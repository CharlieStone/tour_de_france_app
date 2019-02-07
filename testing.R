loc_1 <-
  filter(tour_all_loc, year == 1913)

cyc_1 <-
  filter(tour_data_plus_calc, year == 1913)

not_cyc_1 <-
  filter(tour_not_cyc_stage, year == 1913)

cyc_range <-
  filter(tour_data_plus_calc, year >= 1903, year <= 1999)

not_cyc_range <-
  filter(tour_not_cyc_stage, year >= 1903, year <= 1999)


source("plot_functions_2.R")

cntry_plot <- plot_country() 
  
range_plot <- plot_route_range(cyc_range, not_cyc_range, cntry_plot)

range_plot

range_plot <- plot_route_year(loc_1, cyc_1, not_cyc_1, range_plot)

range_plot



  