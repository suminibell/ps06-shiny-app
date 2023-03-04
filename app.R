#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)

diamonds <- read_delim("admissionsdata.csv")

ui <- fluidPage(
  titlePanel("Diamond Explorer"),
  p("We have ", nrow(diamonds), "diamonds"),
  sidebarLayout(
    sidebarPanel(
    ),
    mainPanel(
      dataTableOutput("sample")
    )
  )
)

server <- function(input, output) {
  output$sample <- renderDataTable({
    diamonds %>%
      sample_n(6)
  })
}

shinyApp(ui = ui, server = server)


