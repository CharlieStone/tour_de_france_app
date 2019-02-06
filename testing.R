loc_1 <-
  filter(tour_all_loc, year == 1913)

cyc_1 <-
  filter(tour_data_plus_calc, year == 1913)

not_cyc_1 <-
  filter(tour_not_cyc_stage, year == 1913)


tour_all_loc_range <-
    filter(tour_all_loc, year >= 1903, year <= 1920)

tour_data_plus_calc_range <-
  filter(tour_data_plus_calc, year >= 1903, year <= 1920)

tour_not_cyc_stage_range <-
  filter(tour_not_cyc_stage, year >= 1903, year <= 1920)

plot_route_range(tour_all_loc_range, tour_data_plus_calc_range, tour_not_cyc_stage_range, loc_1, cyc_1, not_cyc_1)
  