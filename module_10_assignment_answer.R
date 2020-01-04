## Assignment - 10 - Shiny app 
getwd()
setwd("/Users/hareesha/Documents/Hareesha/PracticalDataScience/Course-Resources/Sep26-Jan15/")

# load libraries
library(shiny)
library(dplyr)
library(ggplot2)

income <- read.csv("income.csv")
glimpse(income)

ui <- fluidPage(
  titlePanel("Module 10 Assignment"),
  sidebarLayout(
    sidebarPanel(
      # Interactive piece 1: inputID = "subset_income"
      selectInput(inputId = "subset_income", label = "Select income range", choices = c("<=50K",">50K")),
      # Interactive piece 2: inputId = "set_yaxis"
      selectInput(inputId = "set_yaxis", label = "Select variable", choices = c("hours_per_week", "capital_loss")),
      # Interactive piece 3: inputId = "subset_occupation"
      checkboxGroupInput(inputId = "subset_occupation",
                         label = "Include occupations",
                         choices = unique(income$occupation),
                         selected = unique(income$occupation))
    ),
    mainPanel(plotOutput(outputId = "myfigure"))
  )
)

server <- function(input, output){
  # Create a reactive subset of the data
  create_subset <- reactive(
      income %>%
      filter(capital_loss > 0 
             & income == input$subset_income 
             & occupation %in% input$subset_occupation)
      )
  
output$myfigure <- renderPlot(
    ggplot(create_subset()) +
      # boxplot
      geom_boxplot(aes_string(x = "occupation", y = input$set_yaxis)) +
      theme_bw(18) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
  )
}

shinyApp(ui, server)