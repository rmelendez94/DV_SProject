#ui.R 

library(shiny)

# Define UI for application that plots random distributions 
shinyUI(pageWithSidebar(

# Application title
headerPanel("Portuguese Bank Marketing Campaign"),

# Sidebar with a slider input for number of observations
    sidebarPanel(
      h4("Scatterplot Input"),
      h4("Bar Chart Input"),
      radioButtons("ReferenceLine", 
                   "Reference Lines:",
                   choices = list("Average" = 1, "Minimum" = 2,
                                  "Maximum" = 3, "Sum" = 4), selected = 1),
      h4("Crosstab Inputs"),
      sliderInput("KPI1", 
                  "KPI_Low_Max_value:", 
                  min = 0,
                  max = .15, 
                  value = .1),
      sliderInput("KPI2", 
                  "KPI_Medium_Max_value:", 
                  min = .1,
                  max = .18, 
                  value = .15)
    ),

# Show a plot of the generated distribution
  mainPanel(
    tabsetPanel(
      tabPanel("Scatterplot", plotOutput("scatterPlot")),
      tabPanel("Bar Chart", plotOutput("barPlot")),
      tabPanel("Crosstab", plotOutput("crosstabPlot"))
    )
  )
))
