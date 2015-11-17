#ui.R 

library(shiny)

# Define UI for application that plots random distributions 
shinyUI(pageWithSidebar(

# Application title
headerPanel("CrossTab!"),

# Sidebar with a slider input for number of observations
  sidebarPanel(
    sliderInput("KPI1", 
                "KPI_Low_Max_value:", 
                min = 0,
                max = .1, 
                value = .1),
    sliderInput("KPI2", 
                "KPI_Medium_Max_value:", 
                min = .1,
                max = .15, 
                value = .1)
  ),

# Show a plot of the generated distribution
  mainPanel(
    plotOutput("distPlot")
  )
))
