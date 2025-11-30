library(testthat)
library(ggplot2)
library(ggrepel)
library(dplyr)

source("plot_function.R")


df <- data.frame(
  X_round = rep(1:5, each = 2),
  Y_round = rep(1:2, times = 5),
  count   = 1:10
)

p <- coordinates_plot(df, title = "Test title", top_n = 1)

# Check it's a ggplot
expect_s3_class(p, "ggplot")
