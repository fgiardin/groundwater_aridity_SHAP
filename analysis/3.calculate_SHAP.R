# XGB-SHAP method
# script to train PFT-specific XGB models and calculate SHAP values for each model

devtools::load_all(".")
library(tidyverse)
library(xgboost)
library(caret)
library(SHAPforxgboost)
library(rsample)
library(LSD)
library(beepr)
library(doMC) # to run models in parallel
library(sf)
library(maps)
library(data.table)
library(rnaturalearth)


registerDoMC(cores=5) # specify number of cores to run in parallel // 200 for Euler, 5 when local

# load data
df_SHAP <- readRDS("data/reprocessed_intmeans/dataframes/df_SHAP.rds") # see script 2.prep_SHAP.R

dorun = 1 # run model (1) or load results (0) from previous run (default: load results USA)

# if you want SHAP results for the entire world, comment the lines below ("focus on USA")
# it may be necessary to run on a HPC

# loop model training around PFT groups -----------------------------------
foreach(namePFT = c("GRA", "CRO", "Forests", "Savannas")) %dopar% {

  # # option: run locally by specifying PFT:
  # namePFT = "Forests" # GRA / CRO / Forests / Savannas

  set.seed(23) # set seed for reproducibility

  # create directory to save results
  dir_name = sprintf("./%s", namePFT)
  dir.create(dir_name)

if (namePFT == "Forests") {
  filter = c("DBF", "EBF", "ENF", "MF")
} else if (namePFT == "Savannas") {
  filter = c("SAV", "WSA", "CSH", "OSH")
} else {
  filter = namePFT
}

df <- df_SHAP %>%
  dplyr::filter(lon > -125, # focus on USA
                lon < -65,
                lat < 50,
                lat > 24) %>%
  dplyr::filter(PFT %in% filter) %>%   # select PFT
  dplyr::select(
    lon, lat, # keep lon/lat to save df train later
    SIF_over_PAR, WTD, P_over_Rn) %>%   # select predictors
  drop_na() %>%
  mutate(SIF_over_PAR = SIF_over_PAR * 10^5
         )


# XGB model ---------------------------------------------------------------

# training (70%) and testing (30%)
split = initial_split(df, prop = 0.7)
train <- training(split)
test <- testing(split)

# save df and train for later stages (maps etc.)
df <- df %>%
  mutate(ID = row_number() # add ID to merge back to SHAP
  )

train <- train %>%
  mutate(ID = row_number() # add ID to merge back to SHAP
  )

path = sprintf("%s/df_train_test.rds", dir_name)
saveRDS(df, path, compress = "xz") # save shap dataframe

path = sprintf("%s/df_train.rds", dir_name)
saveRDS(train, path, compress = "xz") # save shap dataframe

# drop lon/lat to train the model
df <- df %>% dplyr::select(-lon,-lat,-ID)
train <- train %>% dplyr::select(-lon,-lat,-ID)
test <- test %>% dplyr::select(-lon,-lat)



if (dorun) {
  # define settings of cross validation to validate the model while it's being tuned
  train_control = trainControl(method = "cv",
                               number = 10,
                               search = "grid") # using grid search as random search not efficient with xgbTree (see https://topepo.github.io/caret/random-hyperparameter-search.html)

  # standard HP tuning grid
  gbmGrid <-  expand.grid(
    max_depth = c(3, 6, 9), # limits how deep each tree can grow /// default: 6
    nrounds = c(100, 400, 800),    # number of decision trees to be boosted /// default: 100
    eta = c(0.05, 0.1, 0.2), # learning rate (set 0.01 if sample is smaller, e.g. 5k) /// default: 0.3
    min_child_weight = c(1, 10, 100), # minimum number of instances needed to be in each node. The larger, the more conservative the algorithm will be.
    gamma = 0,
    subsample = 1,
    colsample_bytree = 1
  )

  # # computationally intensive HP tuning tried on HPC --> less or equally effective as the one above
  # gbmGrid <-  expand.grid(
  #   max_depth = c(3, 6, 9, 15), # limits how deep each tree can grow /// default: 6
  #   nrounds = c(100, 500, 1500, 2000, 5000),    # number of decision trees to be boosted /// default: 100
  #   eta = c(0.001, 0.05, 0.1, 0.2), # learning rate (set 0.01 if sample is smaller, e.g. 5k) /// default: 0.3
  #   min_child_weight = c(1, 10, 100, 250), # minimum number of instances needed to be in each node. The larger, the more conservative the algorithm will be.
  #   gamma = 0,
  #   subsample = 1,
  #   colsample_bytree = 1
  # )

  # train model
  model = train(SIF_over_PAR~., # select dependent variable (SIF_over_PAR)
                data = train,
                method = "xgbTree",
                verbose = FALSE,
                trControl = train_control,
                tuneGrid = gbmGrid
  )

  model
  path = sprintf("%s/model.rds", dir_name)
  saveRDS(model, path, compress = "xz") # save model with info concerning tuning iteration
  results <- model$results
  beep(sound = 2) # beep when it's done

  # Check model performance (evaluation of the final model)
  rsq <- function(x,y) summary(lm(y~x))$r.squared
  pred_y <- predict(model, test)
  test_y <- test %>% dplyr::select(SIF_over_PAR) %>% pull # select explained variable
  rsq(test_y, pred_y)
  caret::RMSE(test_y, pred_y)

  # predict using train data
  pred_y_train <- predict(model, train)
  train_y <- train %>% dplyr::select(SIF_over_PAR) %>% pull # select explained variable

  # create df with obs and predicted values
  out_test <- tibble(test_y, pred_y)
  out_train <- tibble(train_y, pred_y_train)

  # scatter plot / fit of XGB model
  filename1 = sprintf("%s/fit_XGB_test.png", dir_name)
  filename2 = sprintf("%s/fit_XGB_train.png", dir_name)
  scatterheat(out_test, "test_y", "pred_y", "fit of XGB model - test", filename1)
  ggsave(filename1, path = "./", width = 4, height = 4, dpi = 600)
  scatterheat(out_train, "train_y", "pred_y_train", "fit of XGB model - train", filename2)
  ggsave(filename2, path = "./", width = 4, height = 4, dpi = 600)

} else {
  # load model of respective PFT (trained with USA data)
  model <- readRDS(sprintf("data/results_SHAP/USA_PFTgroups/WTD/%s/model.rds", namePFT))
}

# SHAP analysis -----------------------------------------------------------

# use whole dataset ("df") instead of just training data ("train") when calculating SHAP value to plot SHAP maps (we need all points)
# otherwise: we compute SHAP only with training dataset to avoid overcrowded SHAP summary plots

dataX <- train %>% # or "df"

  # slice_sample(n = 1000000) %>%  # limit to one million (only when doing global runs)
  dplyr::select(-SIF_over_PAR) %>% # remove dependent variable from df
  as.matrix()


# preparing the data format required by SHAPforxgboost
shap_long <- shap.prep(xgb_model = model$finalModel,
                       X_train = dataX)

path = sprintf("%s/shap_long.rds", dir_name)
saveRDS(shap_long, path, compress = "xz") # save shap dataframe


new_labels <- list(
  SIF_over_PAR = "Vegetation activity (-)",
  WTD = "WTD (m)",
  P_over_Rn = "P/Rn (-)"
)

# summary plot (relative feature importance): The results is influenced by multicolinearity among features.
a <- SHAPforxgboost::shap.plot.summary(shap_long,
                                       # min_color_bound = "#487EBA",
                                       # max_color_bound = "#DA2759"
                                       ) +
  theme( # remove the vertical grid lines
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank())
ggsave("rel_feature_importance.png", path = dir_name, width = 5, height = 3)

# dependence plot
b <- shap.plot.dependence(data_long = shap_long,
                          smooth = F,
                          x = 'WTD',
                          y = 'WTD',
                          color_feature = 'P_over_Rn') +
  theme(axis.line = element_line(color='black'), # remove grid lines
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank()) +
  geom_hline(aes(yintercept=0), alpha = 0.5,
             linetype="dotted")
ggsave("dependence_plot.png", path = dir_name, width = 5, height = 5)

beep(sound = 2) # beep when it's done

}





