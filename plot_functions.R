
# Plot tour route (plot_route_year) ----
plot_route_year <- function(tour_subset_loc, tour_subset_stage, tour_subset_not_cyc){

## Plot borders of countries
world_map <- map_data(map = "world")

borders_france <- map_data(map = "world", region = "France")

borders_plot <- ggplot() +
  geom_polygon(data = borders_france, aes(x = long, y = lat, group = group), fill = "#FEED00", colour = "#FDFEFE") +
  coord_map() +
  theme_void()

leg_labels <- c("Start", "Finish")

# Plot route for this year(s)
tour_plot <- borders_plot +
  # Plot start and end locations of tour
  geom_point(data = tour_subset_loc,
             aes(x = long, y = lat),
             size = 0) +
  # Plot cycled parts of tour as solid lines
  geom_segment(
    data = tour_subset_stage,
    aes(
      x = start_long,
      y = start_lat,
      xend = end_long,
      yend = end_lat,
      colour = tour_progress
    ),
    size = 1.5
  ) +
  # Plot not cycled route between end of previous and start of next stage as dashed line
  geom_segment(
    data = tour_subset_not_cyc,
    aes(
      x = end_prev_long,
      y = end_prev_lat,
      xend = start_long,
      yend = start_lat
    ),
    size = 0.5,
    colour = "#2E2D2D",
    linetype = 3
  )  +
  theme(
        legend.position = "bottom",
        legend.title = element_text(size = 16, face = "bold", colour = "black"),
        legend.text = element_text(size = 16)) +
  scale_colour_gradient(name = "Tour progress", breaks = seq(0, 1, by = 1), limits = c(0, 1), labels = leg_labels, low = "green", high = "#01061d") +
  guides(colour = guide_legend(nrow = 1, keywidth = 0.1, keyheight = 0.4, default.unit = "inch"))

return(tour_plot)

}

# Plot tour elevation profile (plot_route_elev) ----
plot_route_elev <- function(all_loc) {
  
route_elev <- all_loc %>%
  ggplot() +
  geom_segment(aes(x = tour_section_progress, y = elev, xend = end_x, yend = end_y, colour = mountain_range), 
               size = 2, lineend = "round", linejoin = "round") +
  ylim(0, 2600) +
  theme(panel.background = element_rect(fill = "white")) +
  scale_color_manual(values = c("Alps" = "blue", "Pyrenees" = "green", "Massif Central" = "red", "Other" = "black"), 
                     breaks = c("Alps", "Pyrenees", "Massif Central", "Other")) +
  theme(
    axis.text.y = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.title.x = element_text(size = 16),
    axis.text.x = element_text(size = 0),
    legend.title = element_text(size = 16), 
    legend.text = element_text(size = 16),
    axis.ticks.x = element_blank(), 
  ) +
  labs(x = "Tour % progress by stage", y = "L'altitude (metres)", colour = "Mountain range")

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
  scale_fill_manual(values = c("Flat" = "#008B00", "Hilly" = "#0A5CF5", "Mountain" = "#FFD700", "Individual time trial" = "#CD0000",
                                "Team time trial" = "#000000", "Mountain time trial" = "#9400D3"), 
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