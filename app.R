# Load packages
library(shiny)
library(tidyverse)
library(maps)
library(mapproj)

# Source plot functions
source("plot_functions_2.R")

# Load data
tour_all_loc <- read_csv("plot_data/tour_all_loc.csv")
tour_data_plus_calc <- read_csv("plot_data/tour_data_plus_calc.csv")
tour_not_cyc_stage <- read_csv("plot_data/tour_not_cyc_stage.csv")

# User interface ----
ui <- fluidPage(
  titlePanel("Tour de France routes"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("year", "Select a year", value = 1903, min = 1903, max = 2019, step = 1),
      sliderInput("year_range", "Select years to compare to", min = 1903, max = 2019, step = 1, value = c(1903, 1904))
    ),
    mainPanel(plotOutput("plot"))
  )
)

# Server logic ----
server <- function(input, output) {

# Filter plot data for single year    
  loc_1 <-
    reactive({
      filter(tour_all_loc, year == input$year)
    })
  cyc_1 <-
    reactive({
      filter(tour_data_plus_calc, year == input$year)
    })
  not_cyc_1 <-
    reactive({
      filter(tour_not_cyc_stage, year == input$year)
    })
  
# Filter plot data for range of years  
  cyc_range <-
    reactive({
      filter(tour_data_plus_calc, year >= input$year_range[1], year <= input$year_range[2])
    })
  not_cyc_range <-
    reactive({
      filter(tour_not_cyc_stage, year >= input$year_range[1], year <= input$year_range[2])
    })
  
# Tour routes plot
  cntry_plot <- reactive({plot_country()}) 
  
  range_plot <- reactive({plot_route_range(cyc_range(), not_cyc_range(), cntry_plot())})
  
  all_plot <- reactive({plot_route_year(loc_1(), cyc_1(), not_cyc_1(), range_plot())})
  
  output$plot <- renderPlot({
   all_plot()
   })

# Plot route for selected year
 # output$plot <- renderPlot({
  # plot_route_year(loc_1(), cyc_1(), not_cyc_1())
   # })

# Plot elevation for selected year  
  # output$plot <- renderPlot({
   # plot_route_elev(tour_all_loc_y())
  # })
  
# Plot stage wins for selected year 
  # output$plot <- renderPlot({
  #  plot_stage_wins(tour_data_plus_calc_y())
  # })
  
}

# Run the app
shinyApp(ui, server)
