
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

score <- read_delim("admissionsdata.csv") %>% 
  mutate(selected_state = factor(State)) %>% 
  rename(Admission_Rate = "Admission Rate")
head(score)
state <- unique(score$State)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  #  title
  titlePanel("US College & University Admissions 2020-2021 with SAT / ACT scores"),
  
  # Sidebar with a slider input for number of bins
  mainPanel(
    tabsetPanel(
      tabPanel("About",
               mainPanel(
                 hr("This app displays information about the 75th percentile SAT & ACT scores, admissions rate, and the location of 1,386 institutions in the US."), 
                 hr("It contains 1386 institution and 9 variables. "), 
                 br("Here is a (random) sample of data:"),
                 tableOutput("sampleTable"))),
      
      tabPanel("SAT Plot",
               sidebarPanel(
                 sliderInput("SAT_Range", 
                             "What range of SAT plot: ", 
                             min = 0, 
                             max = 800, 
                             value = c(0,800) 
                 ),
                 checkboxGroupInput("State1",
                                    "Choose what State in the plot: ",
                                    choiceNames = state,
                                    choiceValues = state
                 )
               ),
               mainPanel(
                 plotOutput("satplotENG"),
                 plotOutput("satplotMATH")
               )
               
      ),
      tabPanel("ACT Plot",
               sidebarPanel(
                 sliderInput("ACT_Range",
                             "What range of ACT plot: ",
                             min = 0,
                             max = 36,
                             value = c(0,36)
                 ),checkboxGroupInput("State2",
                                      "Choose which State(s) to plot:",
                                      choiceNames = state,
                                      choiceValues = state
                 )
               ),
               mainPanel(
                 plotOutput("actplotENG"),
                 plotOutput("actplotMATH")
               )
      ),
      tabPanel("Table",
               sidebarPanel(
                 hr("This panel displays the average and range of each region for each ACT and SAT only English subject."),
                 hr(p(strong("SATVR75:"), "75th percentile SAT English")),
                 hr(p(strong("ACTEN75:"), "75th percentile ACT English")),
                 hr("\n"),
                 selectInput("Subject", "Select test type :",
                             choices = c("ACTEN75", "SATVR75"))
               )),
              mainPanel(
                 tableOutput("avgtable")
              )
      ),
  ))




server <- function(input, output) {
  
  ## About page for general information  
  output$sampleTable <- renderTable({
    score %>% 
      sample_n(5)
  })
  
  ## Plot for SAT and ACT 
  
  output$satplotENG <- renderPlot({
    score %>% 
      filter(selected_state %in% input$State1) %>% 
      filter(SATVR75 >= input$SAT_Range[1],
             SATVR75 <= input$SAT_Range[2]) %>% 
      ggplot(aes(SATVR75, Admission_Rate, col = selected_state)) +
      labs(title = "75th Percentile SAT English Score and Admission Rate",
           x = "75th Percentile SAT English Score", y = "Admission Rate") +
      geom_point()
  })
  
  output$satplotMATH <- renderPlot({
    score %>% 
      filter(selected_state %in% input$State1) %>% 
      filter(SATMT75 >= input$SAT_Range[1],
             SATMT75 <= input$SAT_Range[2]) %>% 
      ggplot(aes(SATMT75, Admission_Rate, col = selected_state)) +
      labs(title = "75th Percentile SAT MAth Score and Admission Rate",
           x = "75th Percentile SAT Math Score", y = "Admission Rate") +
      geom_point()
  })
  
  output$actplotENG <- renderPlot({
    score %>% 
      filter(selected_state %in% input$State2) %>% 
      filter(ACTEN75 >= input$ACT_Range[1],
             ACTEN75 <= input$ACT_Range[2]) %>% 
      ggplot(aes(ACTEN75, Admission_Rate, col = selected_state)) +
      labs(title = "75th Percentile ACT English Score and Admission Rate",
           x = "75th Percentile ACT English Score", y = "Admission Rate") +
      geom_point()
  })
  
  output$actplotMATH <- renderPlot({
    score %>% 
      filter(selected_state %in% input$State2) %>% 
      filter(ACTMT75 >= input$ACT_Range[1],
             ACTMT75 <= input$ACT_Range[2]) %>% 
      ggplot(aes(ACTMT75, Admission_Rate, col = selected_state)) +
      labs(title = "75th Percentile ACT MAth Score and Admission Rate",
           x = "75th Percentile ACT Math Score", y = "Admission Rate") +
      geom_point()
  })  
  
  ##Table for avg-page
  
  tableData <- reactive({
    if(input$Subject %in% "SATVR75") {
      score %>% 
        group_by(Region) %>% 
        summarise(avg = mean(SATVR75, na.rm=T))
      
    } else if(input$Subject %in% "ACTEN75") {
      score %>% 
        group_by(Region) %>% 
        summarise(avg = mean(ACTEN75, na.rm=T))
    }
  })
  
  output$avgtable <- renderTable({  
    tableData()

  })

}

# Run the application 
shinyApp(ui = ui, server = server)
