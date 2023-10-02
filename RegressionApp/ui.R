library(shiny)
library(ggplot2)

# The  trend is initaly chosen at random 
dynamicValue = function(){
  runif(1, min = -1, max =1.5)
}


# Define the UI
ui <- fluidPage(
  # Application title
  titlePanel("Playing with the regression line"),
  tags$h5("Christophe Bontemps (SIAP)"),
  tags$h5("October 2023 - V2.1"),
  
  HTML("<h3><font color='#2874A6'> Try to adjust the line that fits the data best! </font></h4>"),
  #titlePanel("Scatter Plot with Regression Line"),
  sidebarLayout(
    sidebarPanel(
      HTML("<h4><font color='#2874A6'> Select the coeficients \u03B20 and \u03B21 for the line: </font></h4>"),
      HTML("<h3><font color='#2874A6'> Y =  \u03B20 + \u03B21 X </font></h3>"),
       sliderInput("coef_b0", "Coefficient \u03B20", min = -2, max = 2, value = -0.5, step = 0.1),
       sliderInput("coef_b1", "Coefficient \u03B21", min = -1, max = 2, value = dynamicValue(), step = 0.1),
      br(),
      HTML("<h4><font color='#2874A6'> Let's see the \"right\" result </font></h4>"),
      checkboxInput("show_regression", "Show Regression Line", value = FALSE)
    ),
    mainPanel(
      plotOutput("scatterplot")
    )
  )
)
