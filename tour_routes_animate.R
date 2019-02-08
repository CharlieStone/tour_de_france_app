# Load packages
library(tidyverse)
library(maps)
library(mapproj)
library(magick)

# Source plot functions
source("plot_functions_2.R")

# Load plot_data
tour_all_loc <- read_csv("plot_data/tour_all_loc.csv")
tour_data_plus_calc <- read_csv("plot_data/tour_data_plus_calc.csv")
tour_not_cyc_stage <- read_csv("plot_data/tour_not_cyc_stage.csv")

# Produce tibble of ggplots
plots <- tibble(year = rep(1900, 10)) %>%
  bind_rows(tibble(year = seq(1903, 2019, by = 1))) %>%
  bind_rows(tibble(year = rep(2019, 20))) %>%
  mutate(plot = purrr::map(year, animate_year))

# Turn ggplots into images
img <- image_graph(267, 373, res = 120, bg = "black")
walk(plots$plot, print)
dev.off()

# Remove white bordering
img_2 <- image_draw(img) 
rect(0, 0, 267, 5, border = "black", lwd = 10)
rect(0, 0, 10, 373, border = "black", lwd = 20)
rect(257, 0, 10, 373, border = "black", lwd = 20)
rect(0, 368, 267, 5, border = "black", lwd = 10)
dev.off()

# Remove whitespace and turn images into animation
animation <- img_2 %>%
  image_animate(fps = 10)

print(animation)

image_write(animation, "tour_routes_10_fps.gif")
