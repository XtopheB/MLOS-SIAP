
library(shiny)
library(rpart)
library(caret)
library(dplyr)
library(visNetwork)
library(here)
#library(shinythemes)
library(sparkline)
library(e1071)

# Define server logic required to draw a histogram
server <- function(input, output) {
    ChildMarriagedf <- read.csv(here("../data/ChildMarriage.csv"))
    ChildMarriage <- ChildMarriagedf %>% dplyr::select(Before15 = Before15, Residence = HV025, Aridity = Aridity2015, Wealth = aWealthIndex2011, Density = Density2015, Education = Education, Age = Age)
    factor_columns <- c('Before15', 'Residence', 'Education')
    ChildMarriage[factor_columns] <- lapply(ChildMarriage[factor_columns], factor)
    levels(ChildMarriage$Before15) <- c("Unmarried", "Married")
    ChildMarriage  <- ChildMarriage %>% na.omit() 
    
    
    trainIndex <- createDataPartition(ChildMarriage$Before15, p = .8, 
                                      list = FALSE, 
                                      times = 1)
    train_data <- ChildMarriage[ trainIndex,]
    validation_data  <- ChildMarriage[-trainIndex,]

    sliderValues <- reactive({
        data.frame(Name = c("Complexity Parameter", Value = as.character(c(input$Complexity))))  })
    
    output$network <- renderVisNetwork({
        tree_fit <- rpart(Before15~., data=train_data, control = rpart::rpart.control(cp = 0.01*input$Complexity))
        visTree(tree_fit, digits = 2,
                # Some parameters too play with maybe
                edgesFontSize = input$edgeFS,  # default: 16 
                nodesFontSize = input$nodeFS,   # default: 16,
                width = "400%",  # default:  "400%"
                height = "1000px",  # default:"10000px", 
                # Other parameters we do not want to change
                legendNcol = 2,
                legendNodesSize = 20,            
                legendFontSize = 20,
                legendWidth = 0.25
        )})
    
    
    output$confmat <- renderPrint({
        tree_fit <- rpart(Before15~., data=ChildMarriage, control = rpart::rpart.control(cp = 0.01*input$Complexity))
        Predictions <- predict(tree_fit, validation_data, type="class")
        confmat <- caret::confusionMatrix(table(Predictions = Predictions, Actual = validation_data$Before15), positive = "Married")
        confmat$table
    })
    
}