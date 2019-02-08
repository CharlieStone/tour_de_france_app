# Load packages
library(tidyverse)
library(scales)
library(magick)

# Load plot_data
tour_all_loc <- read_csv("plot_data/tour_all_loc.csv")
tour_data_plus_calc <- read_csv("plot_data/tour_data_plus_calc.csv")
tour_not_cyc_stage <- read_csv("plot_data/tour_not_cyc_stage.csv")

# Source plot functions
source("plot_functions.R")

# Produce tibble of ggplots
plots <- tibble(year = seq(1903, 1914, by = 1)) %>%
  bind_rows(tibble(year = seq(1919, 1939, 1))) %>%
  bind_rows(tibble(year = seq(1947, 2018, 1))) %>%
  bind_rows(tibble(year = rep(2019, 10))) %>%
  mutate(plot = purrr::map(year, plot_route_elev))

# Turn ggplots into images
img <- image_graph(400, 250, res = 70, bg = "black")
walk(plots$plot, print)
dev.off()

# Remove whitespace and turn images into animation
animation <- img %>%
  image_animate(fps = 2)

print(animation)

image_write(animation, "tour_elev_2_fps.gif")