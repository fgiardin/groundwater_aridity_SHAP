# Script to train one XGB model for each vegetation group and
# calculate Causal Shapley values for each model. It will
# save raw training summary statistics and Shapley value results for quick reference

# load libraries
rm(list=ls())
library(tidyverse)
library(xgboost)
library(SHAPforxgboost)
library(rsample)
library(caret)
library(doMC)
library(cowplot)
library(scales)
library(ggforce)
library(shapr)

# define number of cores to run locally in parallel
registerDoMC(cores=5)

# load land cover key
source("data-raw/0_land_cover_mapping.R")

# create directory to save model output
current_dir <- getwd() # Get the working directory
dir.create(file.path(current_dir, "model_output"))
model_training_folder <- "model_output/"

# load data
df_SHAP <- readRDS("data/main.rds")

# trim and clean the data for training over the CONUS
df <- df_SHAP %>%
  drop_na(SIF, WTD_Fan, P_over_Rn, elevation, PAR) %>%
  mutate(major_land_cover = recode(land_cover_num, !!!major_land_cover_mapping),
         SIF_over_PAR = SIF * 10^3 / PAR) %>%
  filter(P_over_Rn < 3,
         P_over_Rn > 0,
         major_land_cover != "other", # exclude pixels that are not in our vegetation groups
         land_cover_change == 0) %>%
  filter(lon > -125, # focus on the CONUS
         lon < -65,
         lat < 50,
         lat > 24) %>%
  select(lon, lat, SIF_over_PAR, WTD_Fan, P_over_Rn, elevation, major_land_cover, GDE_frac) %>%
  rename(WTD = WTD_Fan, Aridity = P_over_Rn, Elevation = elevation)

rsq <- function(x,y) summary(lm(y~x))$r.squared
major_types <- c("forests", "grasslands", "croplands", "savannas_and_scrublands")

# using grid search for HP tuning, as random search not efficient with xgbTree
# (for info see https://topepo.github.io/caret/random-hyperparameter-search.html)
train_control = trainControl(method = "cv",
                             number = 10,
                             search = "grid")

# Hyperparameter tuning grid with regularization
gbmGrid <- expand.grid(
  max_depth = c(6,9,12),
  nrounds = c(600,800,1000,1200),
  eta = c(0.1, 0.5),
  min_child_weight = c(1,50),
  gamma = 0,
  subsample = 1,
  colsample_bytree = 1
)

# "forests" "croplands" "grasslands" "savannas_and_shrublands"
# 39%, 20%, 19%, 22%

for (i in major_types) {
  print(i)
  df_type <- df %>% filter(major_land_cover == i)

  # training (80%) and testing (20%)
  split = initial_split(df_type, prop = 0.8)
  train <- training(split)
  test <- testing(split)

  df_type <- df_type %>% mutate(ID = row_number())
  train <- train %>% mutate(ID = row_number())
  test <- test %>% mutate(ID = row_number())
  saveRDS(df_type, paste0(model_training_folder, "df_", i, ".rds"), compress = "xz")
  saveRDS(train, paste0(model_training_folder, "train_",i,".rds"), compress = "xz")
  saveRDS(test, paste0(model_training_folder, "test_",i,".rds"), compress = "xz")

  # drop lon/lat to train the model
  df_type <- df_type %>% dplyr::select(-lon,-lat,-ID, -major_land_cover, -GDE_frac)
  train <- train %>% dplyr::select(-lon,-lat,-ID,-major_land_cover,-GDE_frac)
  test <- test %>% dplyr::select(-lon,-lat,-major_land_cover,-GDE_frac)

  # Train the XGBoost model
  model = train(SIF_over_PAR~.,
                data = train,
                method = "xgbTree",
                verbose = TRUE,
                trControl = train_control,
                tuneGrid = gbmGrid,
                nthread = 5)

  # save model with info concerning tuning iterations
  saveRDS(model, paste0(model_training_folder, "model_",i,".rds"), compress = "xz")

  # Check model performance (evaluation of the final model)
  pred_y <- predict(model, test)
  test_y <- test %>% dplyr::select(SIF_over_PAR) %>% pull # select explained variable

  pred_y_train <- predict(model, train)
  train_y <- train %>% dplyr::select(SIF_over_PAR) %>% pull # select explained variable

  # create df with obs and predicted values
  out_test <- tibble(test_y, pred_y)
  out_train <- tibble(train_y, pred_y_train)

  lm.model.train <- lm(pred_y_train ~ train_y, data = out_train)
  lm.model.test <- lm(pred_y ~ test_y, data = out_test)

  # R², RMSE, p-value for training sets
  r_squared_train <- summary(lm.model.train)$r.squared
  rmse_train <- RMSE(out_train$train_y, out_train$pred_y_train)
  rRMSE_train <- rmse_train / mean(out_train$pred_y_train)
  p_value_train <- summary(lm.model.train)$coefficients[2,4]

  label_text_train <- paste0("R² = ", round(r_squared_train, 2), "\n",
                             "rRMSE = ", round(rRMSE_train, 2), "\n",
                             "p-value = ", format.pval(p_value_train, digits = 3))

  p1 <- ggplot(out_train, aes(x = train_y, y = pred_y_train)) +
    labs(y = expression('Prediction (sr'^-1~'nm'^-1*')'))+
    labs(x = expression(paste("SIF/PAR x 10"^3 * " (sr"^-1 * "nm"^-1 * ")")))+
    geom_point(shape = 16, size = 1, ,alpha = 0.7, color = "#82B1D3") +
    geom_abline(intercept = summary(lm.model.train)$coefficients[1,1],
                slope = summary(lm.model.train)$coefficients[2,1]) +
    annotate("text", x = 0.2, y = 3,
             label = label_text_train, hjust = 0) +
    scale_y_continuous(limits = c(-0.1, 3.5)) +
    scale_x_continuous(limits = c(-0.1, 3.5)) +
    ggtitle(paste0(i, " (train)")) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5),
          panel.grid.minor = element_blank())

  # R², RMSE, p-value for testing sets
  r_squared_test <- summary(lm.model.test)$r.squared
  rmse_test <- RMSE(out_test$test_y, out_test$pred_y)
  rRMSE_test <- rmse_test / mean(out_test$pred_y)
  p_value_test <- summary(lm.model.test)$coefficients[2,4]
  label_text_test <- paste0("R² = ", round(r_squared_test, 2), "\n",
                            "rRMSE = ", round(rRMSE_test, 2), "\n",
                            "p-value = ", format.pval(p_value_test, digits = 3))

  p2 <- ggplot(out_test, aes(x = test_y, y = pred_y)) +
    labs(y = expression('Prediction (sr'^-1~'nm'^-1*')'))+
    labs(x = expression(paste("SIF/PAR x 10"^3 * " (sr"^-1 * "nm"^-1 * ")")))+
    geom_point(shape = 16, size = 1, ,alpha = 0.7, color = "#D5A65B") +
    geom_abline(intercept = summary(lm.model.train)$coefficients[1,1],
                slope = summary(lm.model.train)$coefficients[2,1]) +
    annotate("text", x = 0.2, y = 3,
             label = label_text_test, hjust = 0) +
    scale_y_continuous(limits = c(-0.1, 3.5)) +
    scale_x_continuous(limits = c(-0.1, 3.5)) +
    ggtitle(paste0(i, " (test)")) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5),
          panel.grid.minor = element_blank())

  p_model <- plot_grid(p1, p2, ncol = 2, nrow = 1)
  ggsave(paste0(i,"_model_performance.pdf"), plot = p_model,
         path = model_training_folder, dpi=600, width = 11, height = 5)


# SHAP analysis -----------------------------------------------------------

  p <- mean(df_type$SIF_over_PAR)

  dataX <- as.matrix(subset(df_type,
                            select = -c(SIF_over_PAR))[,c("Elevation", "Aridity", "WTD")])
  explainer_symmetric <- shapr(dataX, model$finalModel)
  partial_order <- list(1, 2, 3)

  explanation_causal <- explain(
    dataX,
    approach = "causal",
    explainer = explainer_symmetric,
    prediction_zero = p,
    ordering = partial_order,
    confounding = FALSE,
    seed = 2024
  )

  saveRDS(explanation_causal, paste0(model_training_folder, "cshap_long_",i,".rds"), compress = "xz")
  all.shap.causal <- explanation_causal$dt
  write.csv(all.shap.causal, file = paste0(model_training_folder, "all_causalSHAP_", i,".csv"), row.names = F)
}
