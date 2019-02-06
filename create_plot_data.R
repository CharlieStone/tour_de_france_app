# Load libraries
library(tidyverse)
library(lubridate)

# Import tour_data
tour_data <- read_csv("data/tour_data.csv")

# Add tour_progress and elev summary variables % to tour_data
tour_data_plus_calc <- tour_data %>%
  mutate(year = year(date),
         elev_change = end_elev - start_elev,
         elev_mean = (start_elev + end_elev)/2,
         winner_name_short = if_else(is.na(winner_country), winner_name, str_replace(winner_name, "^[A-Z].* ", "")),
                winner_label = paste0(winner_name_short, if_else(is.na(winner_country), "",  paste0(" (", winner_country, ")")))
         ) %>%
  filter(terrain_group != "Rest day") %>%
  group_by(year) %>%
  arrange(date) %>%
  mutate(tour_progress = stage_number / max(stage_number),
         counter = 1,
         section_number = cumsum(counter),
         tour_section_progress = section_number / max(section_number)) %>%
  ungroup() %>%
  select(-counter)

# tour_not_cyc_stage (for dotted lines for parts of Tour not cycled)
tour_not_cyc_stage <- tour_data_plus_calc %>%
  arrange(date) %>%
  group_by(year(date)) %>%
  mutate(end_prev_lat = if_else(date == min(date), 1E9, lag(end_lat)),
         end_prev_long = if_else(date == min(date), 1E9, lag(end_long))) %>%
  ungroup() %>%
  filter(end_prev_lat != 1E9) %>%
  select(year, end_prev_lat, end_prev_long, start_lat, start_long)

# tour_all_loc (each row is a start or end location of a stage of the Tour, instead of each row being a stage)
tour_start_loc <- tour_data_plus_calc %>%
  select(-end_lat,
         -end_long,
         -end_place,
         -end_country,
         -end_gnid,
         -end_elev) %>%
  rename(
    place = "start_place",
    country = "start_country",
    lat = "start_lat",
    long = "start_long",
    elev = "start_elev",
    gnid = "start_gnid"
  ) %>%
  mutate(position = "start")

tour_end_loc <- tour_data_plus_calc %>%
  select(-start_lat,
         -start_long,
         -start_place,
         -start_country,
         -start_gnid,
         -start_elev) %>%
  rename(
    place = "end_place",
    country = "end_country",
    lat = "end_lat",
    long = "end_long",
    elev = "end_elev",
    gnid = "end_gnid"
  ) %>%
  mutate(position = "end")

tour_all_loc <- dplyr::bind_rows(tour_start_loc, tour_end_loc) %>%
  arrange(year, tour_section_progress, position) %>%
  group_by(year) %>%
  mutate(end_x = lead(tour_section_progress),
         end_y = lead(elev)) %>%
  ungroup() %>%
  mutate(mountain_range = case_when(
           lat > 43.28 & lat < 47.38 & long > 5.24 & long < 7.60 ~ "Alps",
           lat > 41.98 & lat < 43.11 & long > -1.69 & long < 2.78 ~ "Pyrenees",
           lat > 44.93 & lat < 45.92 & long > 2.41 & long < 3.08 ~ "Massif Central",
           TRUE ~ "Other"
           ),
         place_elev_1km = if_else(elev > 1000, place, "")
         )
         

# Save files as .csv files in plot_data folder.
write_csv(tour_data_plus_calc, "plot_data/tour_data_plus_calc.csv")
write_csv(tour_not_cyc_stage, "plot_data/tour_not_cyc_stage.csv")
write_csv(tour_all_loc, "plot_data/tour_all_loc.csv")


