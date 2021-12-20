#

library(shiny)
library(dplyr)
library(ggplot2)
library(pROC)
library(xtable)
library(caret)
library(PerformanceAnalytics)
library(caret)
library(plotly)
library(e1071)


# Define UI for application that draws a histogram
ui <- fluidPage(
    titlePanel("Logit as a classifier"),
    tags$h5("Christophe Bontemps & Patrick Jonsson (2021)"),
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
        # Sidebar to demonstrate various slider options ----
        sidebarPanel(
            tags$h3("Threshold effect"),
            p("Play with the threshold probability used to classify observations
              as 0 or 1 in the logit model, and observe the impact on the logit
              curve (right), on the confusion matrix and on some quality measures (below) "),
            sliderInput("Threshold", "Predictive threshold: ",
                        min = 0, max = 1, value = 0.5, step = 0.01),
            tags$h3("Confusion Matrix"),
            div(style="width:200px;",fluidRow(verbatimTextOutput("confmat", placeholder = TRUE))),
            tags$h3("Quality of the prediction"),
            tableOutput("values"),
            hr(),
            tags$h4("Details"),
            p("- Circles are data points corresponding to label 1, triangles corresponds to data points with label 0."),
            p("- Red colors if predicted probability is above the threshold, otherwise blue."),
            p("- With perfect predictions all triangles should be red, and all circles should be blue.")
            
        ),
        
        mainPanel(
            plotOutput('plot1'),
            plotlyOutput('plot2')
            
            
        )))

