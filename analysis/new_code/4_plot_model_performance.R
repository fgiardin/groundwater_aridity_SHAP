rm(list=ls())
library(tidyverse)
library(ggpubr)
library(caret)

input_folder <- "/Users/jiangongliu/Desktop/5_WTD_Aridity/new_dataframe/"
model_training_folder <- "/Users/jiangongliu/Desktop/5_WTD_Aridity/new_dataframe/model_training_elevation_US_Fan/"

major_types <- c("savannas_and_scrublands", "forests", "grasslands", "croplands")

common_theme <-  theme(plot.title = element_text(hjust = 0.5, size = 18),
                       panel.grid.minor = element_blank(),
                       legend.title = element_text(size = 12), legend.text = element_text(size = 12),
                       axis.title.y = element_text(size = 14), axis.text.y = element_text(size = 12),
                       axis.title.x = element_text(size = 14, vjust = -1), axis.text.x = element_text(size = 12),
                       plot.margin = unit(c(5, 10, 10, 30), "points")
)


for (i in major_types) {

  if (i == "savannas_and_scrublands") {
    PFTname <- "savannas and scrublands"
  } else {
    PFTname <- i
  }
  
  train <- readRDS(paste0(model_training_folder, "train_",i,".rds"))
  test <- readRDS(paste0(model_training_folder, "test_",i,".rds"))
  model <- readRDS(paste0(model_training_folder, "model_",i,".rds"))
  
  # drop lon/lat to train the model
  train <- train %>% dplyr::select(-lon,-lat,-ID,-major_land_cover,-GDE_frac)
  test <- test %>% dplyr::select(-lon,-lat,-major_land_cover,-GDE_frac)
  
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
  
  fig_name <- paste0("fig_train_", i)
  
  fig <- ggplot(out_train, aes(x = train_y, y = pred_y_train)) +
    geom_bin2d(bins = 50) +  # Heatmap effect based on point density
    scale_fill_gradient(low = 'gold1', high = 'firebrick2') +  # Color gradient
    labs(x = expression(paste("Observed SIF/PAR x 10"^6 * " (sr"^-1 * "nm"^-1 * ")"))) +
    labs(y = expression(paste("Predicted SIF/PAR x 10"^6 * " (sr"^-1 * "nm"^-1 * ")"))) +
    ggtitle(paste0(PFTname, " (train)")) +
    scale_y_continuous(limits = c(-0.1, 4)) +
    scale_x_continuous(limits = c(-0.1, 4)) +
    theme_bw() +
    common_theme +
    geom_abline(intercept = summary(lm.model.train)$coefficients[1, 1], 
                slope = summary(lm.model.train)$coefficients[2, 1], 
                color = "black", linewidth = 1) +
    geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed") + 
    annotate("text", x = 0.3, y = 3.6, label = label_text_train, hjust = 0, size = 4.5)
  
  assign(fig_name, fig)

  
  # R², RMSE, p-value for testing sets
  r_squared_test <- summary(lm.model.test)$r.squared
  rmse_test <- RMSE(out_test$test_y, out_test$pred_y)
  rRMSE_test <- rmse_test / mean(out_test$pred_y)
  p_value_test <- summary(lm.model.test)$coefficients[2,4]
  label_text_test <- paste0("R² = ", round(r_squared_test, 2), "\n",
                            "rRMSE = ", round(rRMSE_test, 2), "\n",
                            "p-value = ", format.pval(p_value_test, digits = 3))
  
  fig_name <- paste0("fig_test_", i)
  
  fig <- ggplot(out_test, aes(x = test_y, y = pred_y)) +
    geom_bin2d(bins = 50) +  # Heatmap effect based on point density
    scale_fill_gradient(low = 'gold1', high = 'firebrick2') +  # Color gradient
    labs(x = expression(paste("Observed SIF/PAR x 10"^6 * " (sr"^-1 * "nm"^-1 * ")"))) +
    labs(y = expression(paste("Predicted SIF/PAR x 10"^6 * " (sr"^-1 * "nm"^-1 * ")"))) +
    ggtitle(paste0(PFTname, " (test)")) +
    scale_y_continuous(limits = c(-0.1, 4)) +
    scale_x_continuous(limits = c(-0.1, 4)) +
    theme_bw() +
    common_theme +
    geom_abline(intercept = summary(lm.model.test)$coefficients[1, 1], 
                slope = summary(lm.model.test)$coefficients[2, 1], 
                color = "black", linewidth = 1) +
    geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed") + 
    annotate("text", x = 0.3, y = 3.6, label = label_text_test, hjust = 0, size = 4.5)
  
  assign(fig_name, fig)
}


# combine graphs
fig <- ggarrange(fig_train_forests, fig_test_forests,
                 fig_train_savannas_and_scrublands, fig_test_savannas_and_scrublands,
                 fig_train_croplands, fig_test_croplands,
                 fig_train_grasslands, fig_test_grasslands,
                 labels = "auto",
                 ncol = 2, nrow = 4,
                 align = "hv"
)

png(filename = paste0("/Users/jiangongliu/Desktop/5_WTD_Aridity/figure/", 
                      "FigS_training_performance.png"), 
    width = 10, height = 18, units = "in", res = 600)
print(fig)
dev.off()





