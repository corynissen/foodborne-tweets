
library(shiny)
library(ggplot2)

# Define UI for miles per gallon application
shinyUI(pageWithSidebar(

  # Application title
  headerPanel("Foodborne Chicago Tweets"),

  sidebarPanel(
    selectInput(inputId = "category",
      label = "Select category label",
      choices = c("All", "Good", "Junk"),
      selected = "All"),
    checkboxInput("rt", "Show Retweets", FALSE),
    br(),
    actionButton("refresh", "Click to Refresh Data")
  ),
  
  mainPanel(
    h3(textOutput("caption")),
    plotOutput("plot"),    
    tableOutput("tweet.table")
  )
))
