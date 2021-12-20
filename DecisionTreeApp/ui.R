# New version from DecisionTreeExplore"
#


library(shiny)
library(rpart)
library(caret)
library(dplyr)
library(visNetwork)
#library(shinythemes)
library(sparkline)
library(e1071)


# Define UI for application that draws the network
ui <- fluidPage(
    titlePanel("Decision Tree"),
    tags$h5("Christophe Bontemps & Patrick Jonsson (2021)"),
    sidebarLayout(
        sidebarPanel(
            HTML("<h4><font color='#2874A6'> Model complexity </font></h3>"),
            sliderInput("Complexity", 
                        label = "Complexity parameter value:",
                        min = 0, max = 1.8, step = 0.2, value = 0.5),
            tags$p("The complexity parameter controls the size of the tree by balancing it's cost-complexity."),
            
            
            HTML("<h4><font color='#2874A6'> Confusion Matrix </font></h3>"),
            div(style="width:200px;", fluidRow(verbatimTextOutput("confmat", placeholder = TRUE))),
            
            
            HTML("<h4><font color='#2874A6'> Graphical Preferences </font></h3>"),
            checkboxInput("graphic", 
                          label = "Check to change font sizes",
                          value = FALSE, width = NULL),
            conditionalPanel(condition = "input.graphic == 1", 
                             sliderInput("edgeFS", 
                                         label = "Edge Font size:",
                                         min = 0, max = 40, step = 1, value = 16), 
                             sliderInput("nodeFS", 
                                         label = "Node Font size:",
                                         min = 0, max = 40, step = 1, value = 16)
                             
            )),
        
        
        
        mainPanel(
            visNetworkOutput("network")
        )
    ))

