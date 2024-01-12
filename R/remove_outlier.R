# script to remove outliers outside of coef*(standard deviation) around mean

library(tidyverse)

remove_outlier <- function(df, col, coef){

  df <- df %>%  dplyr::filter(between(df$col,
                 mean(df$col, na.rm=TRUE) - (coef * sd(df$col, na.rm=TRUE)),
                 mean(df$col, na.rm=TRUE) + (coef * sd(df$col, na.rm=TRUE))))

  return(df)
}
