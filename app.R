# Load packages
library(shiny)
library(tidyverse)
library(maps)
library(mapproj)

# Source plot functions
source("plot_functions.R")

# Load data
tour_all_loc <- read_csv("plot_data/tour_all_loc.csv")
tour_data_plus_calc <- read_csv("plot_data/tour_data_plus_calc.csv")
tour_not_cyc_stage <- read_csv("plot_data/tour_not_cyc_stage.csv")

# Filter data for this year
year_sel <- 1934

# User interface ----
ui <- fluidPage(
  titlePanel("Tour de France routes"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("year", "Select a year", value = 1903, min = 1903, max = 2019, step = 1)
    ),
    mainPanel(plotOutput("plot"))
  )
)

# Server logic ----
server <- function(input, output) {
  
  tour_all_loc_y <-
    reactive({
      filter(tour_all_loc, year == input$year)
    })
  tour_data_plus_calc_y <-
    reactive({
      filter(tour_data_plus_calc, year == input$year)
    })
  tour_not_cyc_stage_y <-
    reactive({
      filter(tour_not_cyc_stage, year == input$year)
    })
  
 # output$plot <- renderPlot({
  #  plot_route_year(tour_all_loc_y(), tour_data_plus_calc_y(), tour_not_cyc_stage_y())
  # })
  
 # output$plot <- renderPlot({
  # plot_route_elev(tour_all_loc_y())
#  })
  
  output$plot <- renderPlot({
    plot_stage_wins(tour_data_plus_calc_y())
  })
  
}

# Run the app
shinyApp(ui, server)
