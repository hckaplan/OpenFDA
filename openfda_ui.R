
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(ggplot2)

#load data outputs for visualizations
load("openfda_example.rds")

#get unique list of adverse events to create dropdown selection
events <- as.list(unique(countries.AEs.cln$term))

shinyUI(fluidPage(
  
  # Application title
  titlePanel("Adverse Events by Country"),
  
  # Sidebar with a dropdowns for events to compare
  sidebarLayout(
    sidebarPanel(
      fluidRow(width='5px',
               #allow the user to compare 2 events at a time
               selectInput("ae1",
                           "Adverse Event 1",
                           choices=events),
               selectInput("ae2",
                           "Adverse Event 2",
                           choices=events)
      )),
    
    # Show a bubble chart of the relative indices of the chosen events
    mainPanel(
      plotOutput("bubblePlot", width="90%", height='500px')
    )
  )
))
