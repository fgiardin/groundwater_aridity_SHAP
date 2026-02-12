# adapted for highres!

# Script to train one XGB model for each vegetation group and
# calculate Causal Shapley values for each model.

# The script will save raw training summary statistics
# and Shapley value results for quick reference

# load stuff / settings for parallelization ---------------------------------------------------

# load libraries
rm(list=ls())
library(tidyverse)
library(xgboost)
library(SHAPforxgboost)
library(rsample)
library(caret)
library(doParallel)  #  safer on mac/RStudio than forked doMC
library(cowplot)
library(scales)
library(ggforce)
library(shapr) # !important: install version from https://gitlab.science.ru.nl/gbucur/caushapley (follow "Installation Instructions")
library(beepr)
library(rlang)
library(data.table)
library(future)
library(future.apply)

# define number of cores to run locally in parallel
n_workers <- max(1, parallelly::availableCores() - 2)

# registerDoMC(cores=n_workers) # for XGBoost
#  prefer PSOCK clusters to avoid fork issues in RStudio on mac
cl <- parallelly::makeClusterPSOCK(n_workers)
doParallel::registerDoParallel(cl)
on.exit(parallel::stopCluster(cl), add = TRUE)

# Let future use the same cluster for SHAP
parallel::clusterEvalQ(cl, {
  library(shapr)
  library(xgboost)
})

old_plan <- future::plan()                   # remember previous plan
future::plan(cluster, workers = cl)         # use PSOCK cluster for future_lapply
on.exit(future::plan(old_plan), add = TRUE) # restore when script exits

# create directory to save model output
current_dir <- getwd() # Get the working directory
dir.create(file.path(current_dir, "model_output"), showWarnings = FALSE)
model_training_folder <- "model_output/"

# train XGB ---------------------------------------------------------------

# load data XGB
df_SHAP <- readRDS("data/high_res/fpar_wtd_elev_pr_rn_lc_1km_CONUS.rds")
# df_SHAP_old <- readRDS("data/main.rds")

# load land cover key
source("data-raw/0_land_cover_mapping.R")

set.seed(23)  # reproducibility

#### final wrangling ##
# 0) Check that major_land_cover exists (otherwise build it from codes)
if (!"major_land_cover" %in% names(df_SHAP)) {
  if (!exists("major_land_cover_mapping")) {
    stop("major_land_cover missing and mapping not loaded. Source 0_land_cover_mapping.R first.")
  }
  df_SHAP <- df_SHAP %>%
    mutate(major_land_cover = recode(as.character(land_cover_num), !!!major_land_cover_mapping))
}

# 1) Compute P/Rn just like before (NO unit change; NETRAD ~ MJ m^-2 yr^-1)
df_tmp <- df_SHAP %>%
  mutate(
    Aridity = ifelse(!is.na(prcp_mean_mm_yr) & !is.na(NETRAD) & NETRAD > 0,
                     prcp_mean_mm_yr / NETRAD, NA_real_)   # mm per MJ (same as old)
  )

# summary(df_tmp$Aridity)
# summary(df_SHAP_old$P_over_Rn)
# hist(df_tmp$Aridity) # some checks


# 2) Filters: valid vars, Budyko-like range, stable LC, vegetated, CONUS
df <- df_tmp %>%
  filter(!is.na(WTD),
         !is.na(elev_m),
         !is.na(prcp_mean_mm_yr),
         !is.na(NETRAD),
         !is.na(Aridity)) %>%
  filter(Aridity > 0, Aridity < 2.5) %>%
  filter(major_land_cover != "other") %>% # exclude pixels that are not in our vegetation groups
  filter(is.na(land_cover_change) | land_cover_change == 0) %>%
  filter(lon > -125, lon < -65, lat > 24, lat < 50) %>%
  rename(Elevation = elev_m) %>%
  filter(WTD > -150)

rsq <- function(x,y) summary(lm(y~x))$r.squared
major_types <- c("forests", "croplands", "grasslands", "savannas_and_shrublands")

### using grid search for HP tuning, as random search not efficient with xgbTree ##
# (for more info see https://topepo.github.io/caret/random-hyperparameter-search.html)

# 1) Lighter CV
train_control <- trainControl(method = "cv",
                              number = 5,          # was 10
                              search = "grid",
                              allowParallel = TRUE)

# 2) Smaller, faster grid (slightly adjusted)
gbmGrid <- expand.grid(
  max_depth        = c(4, 6),        # was c(6,9,12)
  nrounds          = c(400, 800),    # was c(600,800,1000,1200)
  eta              = c(0.1, 0.3),    # was c(0.1, 0.5)
  min_child_weight = c(1, 5),        # was c(1, 50)
  gamma            = 0,
  subsample        = 0.8,            # was 1
  colsample_bytree = 0.8             # was 1
)


# restricting the model features to match SHAP features exactly
# to avoid any mismatch between model inputs and shapr's dataX
model_features <- c("Elevation", "Aridity", "WTD")  # <- features used in explain()

#  simple runtime log
log_path <- file.path(model_training_folder, "runtime_log.csv")
if (!file.exists(log_path)) {
  write.table(data.frame(group=character(), n_rows=integer(),
                         n_train=integer(), n_test=integer(),
                         train_time_sec=double(), shap_time_sec=double(),
                         timestamp=as.character()),
              file=log_path, sep=",", row.names=FALSE, col.names=TRUE)
}

# "forests" "croplands" "grasslands" "savannas_and_shrublands"
# 4.89M (38%), 2.7M (21%), 2.4M (19%), 2.9M (22%)
# time to run (simplified model): XXX, XXX, 10 mins,

for (i in major_types) {
  print(i)
  df_type <- df %>% filter(major_land_cover == i)

  set.seed(23)  # keep per-group reproducibility

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

  # drop lon/lat to train the model + XXX DROPPED NAs HERE
  df_type <- df_type %>% dplyr::select(-lon,-lat,-ID, -major_land_cover) %>% tidyr::drop_na()
  train <- train %>% dplyr::select(-lon,-lat,-ID,-major_land_cover) %>% tidyr::drop_na()
  test <- test %>% dplyr::select(-lon,-lat,-major_land_cover) %>% tidyr::drop_na()

  #  hard-select features so that model and SHAP use the same predictors
  keep_cols <- c("fpar_mean", model_features)
  train <- train %>% dplyr::select(dplyr::any_of(keep_cols))
  test  <-  test %>% dplyr::select(dplyr::any_of(keep_cols))
  df_type <- df_type %>% dplyr::select(dplyr::any_of(keep_cols))

  # Train the XGBoost model
  #  set xgboost nthread=1 to avoid oversubscription when parallelizing CV folds with caret
  t_train <- system.time({
    # 3) Train call (add fast histogram tree builder)
    model <- train(
      fpar_mean ~ .,
      data = train,
      method = "xgbTree",
      verbose = FALSE,
      trControl = train_control,
      tuneGrid = gbmGrid,
      nthread = 1,               # keep only one thread; folds run in parallel via caret
      tree_method = "hist",      #  much faster on CPU
      max_bin = 256              #  modest speed-up (usually doesn't hurt)
    )
  })["elapsed"]

  # save model with info concerning tuning iterations
  saveRDS(model, paste0(model_training_folder, "model_",i,".rds"), compress = "xz")

  # Check model performance (evaluation of the final model)
  pred_y <- predict(model, test)
  test_y <- test %>% dplyr::select(fpar_mean) %>% pull # select explained variable

  pred_y_train <- predict(model, train)
  train_y <- train %>% dplyr::select(fpar_mean) %>% pull # select explained variable

  # create df with obs and predicted values
  out_test <- tibble(test_y, pred_y)
  out_train <- tibble(train_y, pred_y_train)

  lm.model.train <- lm(pred_y_train ~ train_y, data = out_train)
  lm.model.test <- lm(pred_y ~ test_y, data = out_test)

  # R², RMSE, p-value for training sets
  r_squared_train <- summary(lm.model.train)$r.squared
  rmse_train <- caret::RMSE(out_train$train_y, out_train$pred_y_train)
  rRMSE_train <- rmse_train / mean(out_train$pred_y_train)
  p_value_train <- summary(lm.model.train)$coefficients[2,4]

  label_text_train <- paste0("R² = ", round(r_squared_train, 2), "\n",
                             "rRMSE = ", round(rRMSE_train, 2), "\n",
                             "p-value = ", format.pval(p_value_train, digits = 3))

  p1 <- ggplot(out_train, aes(x = train_y, y = pred_y_train)) +
    labs(y = expression('Prediction (sr'^-1~'nm'^-1*')'))+
    labs(x = expression(paste("SIF/PAR x 10"^3 * " (sr"^-1 * "nm"^-1 * ")"))) +
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
  rmse_test <- caret::RMSE(out_test$test_y, out_test$pred_y)
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

  p_model <- cowplot::plot_grid(p1, p2, ncol = 2, nrow = 1)
  ggsave(paste0(i,"_model_performance.pdf"), plot = p_model,
         path = model_training_folder, dpi=600, width = 11, height = 5)


# SHAP analysis -----------------------------------------------------------

  # run ONLY if not run from before (load model, train and test sets)
  # i = "grasslands"
  # df_type <- readRDS("/Users/fgiardina/Desktop/grassland highres fast settings /model_data/df_grasslands.rds")
  # model <- readRDS("/Users/fgiardina/Desktop/grassland highres fast settings /model_data/model_grasslands.rds")
  # test <- readRDS("/Users/fgiardina/Desktop/grassland highres fast settings /model_data(test_grasslands.rds")
  # train <- readRDS("/Users/fgiardina/Desktop/grassland highres fast settings /model_data/train_grasslands.rds")
  # model_features <- c("Elevation", "Aridity", "WTD")  # <- features used in explain()
  # n_workers <- max(1, parallelly::availableCores() - 2)   # define number of cores to run locally in parallel

  p <- mean(df_type$fpar_mean, na.rm = TRUE)

  dataX <- df_type %>%
    sample_frac(0.05) %>%               # 0.1 crashed with the current settings
    dplyr::select(dplyr::all_of(model_features)) %>%
    as.matrix()

  explainer_symmetric <- shapr(dataX, model$finalModel)
  partial_order <- list(1, 2, 3)

  # ---- Parallel SHAP over row-chunks ------------------------------------

  # choose chunk size (aka rows per SHAP call per worker)
  # tuning based on memory and runtime
  n_rows    <- nrow(dataX)
  target_chunks_total <- min(50L, max(10L, 10L * n_workers))  # aim for about 10 x n_workers chunks overall (between 10 and 50 total)
  chunk_size <- ceiling(n_rows / target_chunks_total)
  chunk_size <- max(2000L, min(50000L, chunk_size))  # clamp to a safe range

  idx_all <- seq_len(n_rows)
  chunks <- split(idx_all, ceiling(idx_all / chunk_size))

  t_shap <- system.time({
    shap_chunks <- future_lapply(chunks,
                                 future.seed = TRUE,
                                 function(idx_chunk) {

                                   dataX_chunk <- dataX[idx_chunk, , drop = FALSE]

                                   ex_chunk <- shapr::explain(
                                     dataX_chunk,
                                     approach        = "causal",
                                     explainer       = explainer_symmetric,
                                     prediction_zero = p,
                                     ordering        = partial_order,
                                     confounding     = FALSE,
                                     seed            = 23  # if needed, this can vary per chunk
                                   )

                                   dt_chunk <- ex_chunk$dt
                                   # keep track of which original rows these correspond to
                                   dt_chunk[, row_id := idx_chunk]

                                   dt_chunk
                                 })
    all.shap.causal <- data.table::rbindlist(shap_chunks)
    })["elapsed"]

  ## --- Build a shapr-like object to save for plotting --------------------------
  data.table::setorder(all.shap.causal, row_id) # 1: Making sure rows are in the same order of row_id

  x_test <- as.data.frame( # 2: extract x_test (feature values) in the same order
    dataX[all.shap.causal$row_id, , drop = FALSE]
  )
  colnames(x_test) <- model_features  # c("Elevation", "Aridity", "WTD")

  dt_out <- as.data.table(all.shap.causal)[, !"row_id"] # 3: drop helper row_id from dt that goes into the object

  cshap_obj <- list(  # 4: build a list mimicking a shapr object
    dt     = dt_out,
    x_test = x_test,
    p      = p,                 # optional (nice to keep)
    model  = model$finalModel   # also optional
  )
  class(cshap_obj) <- c("shapr", "list")

  saveRDS(  # 5: save in the same way as before
    cshap_obj,
    paste0(model_training_folder, "cshap_long_", i, ".rds"),
    compress = "xz"
  )


  # append runtime log (only when running model training and SHAP)
  cat(sprintf("[%s] %s — rows=%d, train=%d, test=%d, train_time=%.1fs, shap_time=%.1fs\n",
              format(Sys.time(), "%Y-%m-%d %H:%M:%S"), i,
              nrow(df_type), nrow(train), nrow(test),
              as.numeric(t_train), as.numeric(t_shap)))
  write.table(data.frame(group = i, n_rows = nrow(df_type),
                         n_train = nrow(train), n_test = nrow(test),
                         train_time_sec = as.numeric(t_train),
                         shap_time_sec = as.numeric(t_shap),
                         timestamp = as.character(Sys.time())),
              file = log_path, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)

  beepr::beep(2)

  #  free RAM between groups
  rm(all.shap.causal, explainer_symmetric, dataX, out_train, out_test,
     train, test, df_type, model, p1, p2, p_model)
  gc()
}
