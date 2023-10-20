library(shiny)
library(ggplot2)

# The  trend is initaly chosen at random 
dynamicValue = function(){
  runif(1, min = -1, max =1.5)
}


# Define the UI
ui <- fluidPage(
  # Application title
  titlePanel( HTML("<h3><font color='#2874A6'>Playing with the regression line</font></h4>")),
  tags$h5("Christophe Bontemps (SIAP)"),
  tags$h5(HTML("<em>November 2023 - V2.2 </em>")),
  
  HTML("<h3><font color='#2874A6'> Adjust the line that fits the data best... </font></h4>"),
  #titlePanel("Scatter Plot with Regression Line"),
  sidebarLayout(
    sidebarPanel(
      HTML("<h4><font color='#2874A6'> Try by yourself! </font></h4>"),
      checkboxInput("show_line", label = HTML("<font color='#2874A6'> Play with Y =  \u03B20 + \u03B21 X </font>"), value = FALSE), 
      conditionalPanel(
        condition = "input.show_line == true",
        sliderInput("coef_b0",
                    label = HTML("Coefficient \u03B20 <em>(constant)</em>"), min = -2, max = 2, value = -0.5, step = 0.1),
        sliderInput("coef_b1", 
                    label = HTML("Coefficient \u03B21 <em>(slope)</em>"), min = -1, max = 2, value = dynamicValue(), step = 0.1),
        HTML("<h5><em > The first line you see is randomly chosen! </em></h4>"),
      br() ),
      HTML("<h4><font color='#2874A6'> Let's see the \"right\" result </font></h4>"),
      checkboxInput("show_regression", "Show Regression Line", value = FALSE)
    ),
    mainPanel(
      plotOutput("scatterplot")
    )
  )
)
