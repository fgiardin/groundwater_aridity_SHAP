scatterheat <- function(df, X, Y, title, pearson){

  library(tidyverse)
  library(ggplot2)
  library(yardstick)
  library(LSD)

  df <- df %>%
    as_tibble() %>%
    ungroup() %>%
    dplyr::select(X=X, Y=Y) %>%
    tidyr::drop_na(X, Y)

  lm_model <- lm(Y ~ X , data=df, na.action = na.omit)
  slope <- coef(lm_model)["X"]
  intercept <- coef(lm_model)["(Intercept)"]

  r2 = df %>% na.omit %>% yardstick::rsq(Y,X)
  rmse = df %>% na.omit %>% yardstick::rmse(Y,X)

  subtitle <- bquote(italic(R^2)~" = " ~ .(round(r2$.estimate, 2)) ~ ", RMSE = " ~ .(round(rmse$.estimate, 2)))

  if(pearson){
    pearson_cor = cor(df %>% na.omit %>% pull(Y),
            df %>% na.omit %>% pull(X),
            method="pearson")
    subtitle <- bquote(italic(r)~" = " ~ .(round(pearson_cor, 2)))
  }


  if(X == "nn_act"){
    axisX = expression(paste(ET[NN], " (mm d"^"-1"*")"))
  } else if(X == "obs"){
    axisX =  expression(paste(ET[obs], " (mm d"^"-1"*")"))
  } else if(X== "nn_pot"){
    axisX =  expression(paste(PET[NN], " (mm d"^"-1"*")"))
  } else if(X== "NETRAD_mass_coeff"){
    axisX = expression(paste(PET[lm], " (mm d"^"-1"*")"))
  } else if(X== "pet_splash_coeff"){
    axisX = expression(paste(PET[PT], " (mm d"^"-1"*")"))
  } else if(X== "WTD"){
    axisX = "WTD (m)"
  } else if(X== "SIF_over_PAR"){
    axisX = expression(paste("SIF/PAR x 10"^5 * " (sr"^-1 * "nm"^-1 * ")"))
  } else if(X== "P_over_Rn"){
    axisX = expression(paste("λP/R"[n], " (", "-", ")"))
  } else{
     axisX = X
  }

  if(Y == "nn_act"){
    axisY = expression(paste(ET[NN], " (mm d"^"-1"*")"))
  } else if(Y == "obs"){
    axisY = expression(paste(ET[obs], " (mm d"^"-1"*")"))
  } else if(Y == "nn_pot"){
    axisY = expression(paste(PET[NN], " (mm d"^"-1"*")"))
  } else if(Y== "NETRAD_mass_coeff"){
    axisY =  expression(paste(PET[lm], " (mm d"^"-1"*")"))
  } else if(Y== "pet_splash_coeff"){
    axisY = expression(paste(PET[PT], " (mm d"^"-1"*")"))
  } else if(Y== "WTD"){
    axisY = "WTD (m)"
  } else if(Y== "SIF_over_PAR"){
    axisY = expression(paste("SIF/PAR x 10"^5 * " (sr"^-1 * "nm"^-1 * ")"))
  } else if(Y== "P_over_Rn"){
    axisY = expression(paste("λP/R"[n], " (", "-", ")"))
  } else {
    axisY = Y
  }

  range_x <- range(df$X, na.rm = TRUE)
  range_y <- range(df$Y, na.rm = TRUE)

  get_breaks <- function(rng) {
    diff <- rng[2] - rng[1]
    if(diff > 10) {
      return(pretty(rng, 5))
    } else if (diff > 1) {
      return(seq(floor(rng[1]), ceiling(rng[2]), by = 0.5))
    } else {
      return(seq(floor(rng[1]*10)/10, ceiling(rng[2]*10)/10, by = 0.1))
    }
  }

  breaks_x <- get_breaks(range_x)
  breaks_y <- get_breaks(range_y)

  a <- heatscatter(x = df$X, y = df$Y, ggplot = TRUE)
  a <- a +
    labs(
      y = axisY,
      x = axisX,
      title = title,
      subtitle = subtitle
    ) +
    scale_y_continuous(
      breaks = breaks_y,
      expand = c(0, 0)) +
    scale_x_continuous(
      breaks = breaks_x,
      expand = c(0, 0)) +
    theme_classic() +
    theme(
      axis.text=element_text(size = 14),
      axis.title=element_text(size = 14),
      legend.text=element_text(size=14),
      plot.title = element_text(hjust = 0.5, size = 16),
      plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
      axis.line=element_blank(),
      plot.subtitle=element_text(hjust=0.5),
      axis.ticks.length = unit(0.25, "cm"),
      panel.border = element_rect(
        color = "black",
        fill = NA,
        size = 1),
    ) +
    geom_abline(slope = slope, intercept = intercept, color = "red") +
    geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed")


  return(a = a)
}
