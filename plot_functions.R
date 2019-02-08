# Preliminary variables used by all functions

# Plot route for 1 year (assuming tour_all_loc, tour_data_plus_calc, tour_not_cyc_stage already loaded)
leg_labels <- c("Start", "Finish")

# Convert Tour_data_plus_calc mt_range to factor
tour_data_plus_calc <- tour_data_plus_calc %>%
  mutate(mt_range = factor(mt_range, levels = unique(mt_range)))

# Tibble 'notes' contains text annotation and France fill colour for each year
notes <- tibble(year = 1903:2019) %>%
  mutate(text = case_when(
    year %in% c(1915:1918) ~ "WW1",
    year %in% c(1940:1946) ~ "WW2",
    TRUE ~ ""
  ),
  col_code = case_when(
    year %in% c(1915:1918) ~ "gray20",
    year %in% c(1940:1946) ~ "gray20",
    year == 2019 ~ "#FEED00",
    TRUE ~ "gray20"
  )
  )

# Function to produce image for one year for GIF ----
animate_year <- function(year_route) {
if(year_route == 1900){
  intro_label <- c("LES ROUTES", "DU", "TOUR DE FRANCE")
  p <- plot_country(col_code = "black") +
       scale_x_continuous(limits = c(-4.4, 8.4)) +
       scale_y_continuous(limits = c(41.3, 51.3)) +
       labs(title = "") +
       theme(plot.title = element_text(size = 30, face = "bold", colour = "white", hjust = 0.5, vjust = 0.25),
             plot.background = element_rect(fill = "black"),
             panel.border = element_blank()) +
       annotate("text", x = 2, y = c(51, 48.5, 46), label = intro_label, size = 8, colour = "white", family = "Staatliches")
} else { 
  
  # Filter plot_data for year interested in.
  loc_1 <-
    filter(tour_all_loc, year == year_route)
  cyc_1 <-
    filter(tour_data_plus_calc, year == year_route)
  not_cyc_1 <-
    filter(tour_not_cyc_stage, year == year_route)
  
  # Pull out colour code and text for year
  col_code <- notes$col_code[notes$year == year_route]
  annot_text <- notes$text[notes$year == year_route]
  
  # Plot route for this year
  p <- plot_route_year(loc_1, cyc_1, not_cyc_1, plot_country(col_code = col_code)) +
    theme(plot.title = element_text(size = 30, face = "bold", colour = "white", hjust = 0.5, vjust = 0.25),
          plot.background = element_rect(fill = "black"),
          text = element_text(family = "Staatliches"),
          legend.position = "bottom",
          legend.margin = margin(0, 0, 0, 0),
          legend.box.margin=margin(10,10,10,10),
          legend.title = element_blank(),
          legend.text = element_text(size = 14, colour = "white"),
          panel.border = element_blank()
    ) + 
    scale_x_continuous(limits = c(-4.8, 9.5)) +
    scale_y_continuous(limits = c(41.3, 51.3)) +
    labs(title = year_route) +
    scale_colour_gradient(breaks = seq(0, 1, by = 1), limits = c(0, 1), labels = leg_labels, low = "white", high = "gray45") +
    guides(colour = guide_legend(nrow = 1)) +
    annotate("text", x = 2.4, y = 46.9, label = annot_text, size = 18, colour = "white", family = "Staatliches")
}
  return(p)
}

# Produce borders plot for animation (yellow background for france)
plot_country <- function(col_code, country = "France"){
  borders_france <- map_data(map = "world", region = country)
  
  borders_plot <- ggplot() +
    geom_polygon(data = borders_france, aes(x = long, y = lat, group = group), fill = col_code) +
    coord_map() +
    theme_void()
  
  return(borders_plot)
  
}

# Plot tour route for one year (plot_route_year) ----
plot_route_year <- function(loc_single, cyc_single, not_cyc_single, start_plot){

# Plot route for this year(s)
tour_plot <- start_plot +
  # Plot start and end locations of tour
  geom_point(data = loc_single,
             aes(x = long, y = lat, 
                 colour = progress_end),
             size = 0.75,
             show.legend = FALSE) +
  # Plot cycled parts of tour as solid lines
  geom_segment(
    data = cyc_single,
    aes(
      x = start_long,
      y = start_lat,
      xend = end_long,
      yend = end_lat,
      colour = progress_end
    ),
    size = 1.5
  ) +
  # Plot not cycled route between end of previous and start of next stage as dashed line
  geom_segment(
    data = not_cyc_single,
    aes(
      x = end_prev_long,
      y = end_prev_lat,
      xend = start_long,
      yend = start_lat,
      colour = progress_end
    ),
    size = 0.5,
    linetype = 3,
    show.legend = FALSE
  )  

return(tour_plot)

}

# Plot tour route for all year in grey and overlay route for selected year (plot_route_range) ----
plot_route_range <- function(cyc_range, not_cyc_range, start_plot){
  
  # Plot route for this year(s)
  tour_plot_range <- start_plot +
    # Plot cycled parts of tour as solid lines
    geom_segment(
      data = cyc_range,
      aes(
        x = start_long,
        y = start_lat,
        xend = end_long,
        yend = end_lat, 
        group = year
      ),
      size = 0.75,
      colour = "grey"
    ) +
    # Plot not cycled route between end of previous and start of next stage as dashed line
    geom_segment(
      data = not_cyc_range,
      aes(
        x = end_prev_long,
        y = end_prev_lat,
        xend = start_long,
        yend = start_lat,
        group = year
      ),
      size = 0.25,
      colour = "grey",
      linetype = 3
    ) 
  
  return(tour_plot_range)
  
}

# Plot tour elevation profile (plot_route_elev) ----
plot_route_elev <- function(year_route) {
  cyc_1 <- tour_data_plus_calc %>%
    filter(year == year_route) %>%
    mutate(start_elev_km = start_elev / 1000,
           end_elev_km = end_elev / 1000)
  
# Create separate dataset for text labels of Tour locations sorted with highest point for each year at top, to ensure that highest points 
# are labelled first and not removed by check_overlap in geom_text.
  elev_text_start <- cyc_1 %>%
    select(progress_start, start_elev_km, start_place) %>%
    rename(progress = "progress_start", elev_km = "start_elev_km", place = "start_place")
  
  elev_text_end <- cyc_1 %>%
    select(progress_end, end_elev_km, end_place) %>%
    rename(progress = "progress_end", elev_km = "end_elev_km", place = "end_place")
  
  elev_text <- bind_rows(elev_text_start, elev_text_end) %>%
    distinct() %>%
    top_n(3, elev_km) %>%
    arrange(-elev_km)
  
  rm(elev_text_start, elev_text_end)

# Plot segments for cycled parts of Tour    
  route_elev <- cyc_1 %>%
    ggplot() +
    geom_segment(
      aes(
        x = progress_start,
        y = start_elev_km,
        xend = progress_end,
        yend = end_elev_km,
        colour = mt_range
      ),
      size = 2,
      lineend = "round",
      linejoin = "round"
    )  +
    geom_text(
      data = elev_text,
      aes(
        x = progress,
        y = elev_km + 0.1,
        label = place,
        hjust = -0.05,
        angle = 45
      ),
      size = 4,
      colour = "white",
      family = "Staatliches",
      check_overlap = TRUE
    ) +
    scale_color_manual(
      values = c(
        "Alps" = "#00277d",
        "Pyrenees" = "#00773e",
        "Massif Central" = "#8b271f",
        "Other" = "gray25"
      ),
      breaks = c("Alps", "Pyrenees", "Massif Central"),
      drop = FALSE
    ) +
    theme_minimal() +
    theme(plot.title = element_text(size = 30, face = "bold", colour = "white", hjust = 0.5, vjust = 0.25),
          plot.background = element_rect(fill = "black"),
          panel.background = element_rect(fill = "black"),
          plot.margin = unit(c(0.25, 0.5, 0, 0.5), "cm"),
          text = element_text(family = "Staatliches"),
          axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = 10, colour = "white", family = "Staatliches", hjust = 1),
          axis.text.y = element_text(size = 14, colour = "white", family = "Staatliches"),
          legend.position = "bottom",
          legend.margin = margin(0, 0, 0, 0),
          legend.box.margin=margin(10, 10, 10, 10),
          legend.title = element_blank(),
          legend.text = element_text(size = 14, colour = "white"),
          #panel.border = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank()
    ) +
    scale_y_continuous(breaks = seq(0, 3, 1), limits = c(0, 3.2)) +
    scale_x_continuous(limits = c(0, 1.15)) +
    labs(title = year_route, y = "km")
  
return(route_elev)
}
# Plot tour stage winner and stage types (plot_stage_wins) ----
plot_stage_wins <- function(tour_data) {
winner_plot <- tour_data %>%
  ggplot(aes(section_number, distance_km, label = winner_label)) +
  geom_text(hjust = -0.05,
            nudge_x = 0,
            angle = 90,
            size = 5) +
  geom_bar(stat = "identity", aes(fill = terrain_group)) +
  theme(panel.background = element_rect(fill = "white")) +
  ylim(0, 900) +
  scale_fill_manual(values = c("Flat" = "#008B00", "Hilly" = "#0A5CF5", "Mountain" = "#CD0000", "Individual time trial" = "#FFD700",
                                "Team time trial" = "#9400D3", "Mountain time trial" = "#000000"), 
                     breaks = c("Flat", "Hilly", "Mountain", "Individual time trial", "Team time trial", "Mountain time trial")) +
  theme(
    axis.text.y = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.title.x = element_text(size = 16),
    axis.text.x = element_text(size = 16),
    legend.title = element_text(size = 16), 
    legend.text = element_text(size = 16),
    axis.ticks.x = element_blank() 
  ) +
  labs(x = "Stage", y = "Distance cycled (km)", fill = "Stage type")

return(winner_plot)
}