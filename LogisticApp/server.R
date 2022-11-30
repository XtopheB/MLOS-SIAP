# NEW version from LogisticApp.R 

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
library(tidyverse)
library(rpart)

set.seed(1234)
Nobs <- 200
X <-runif(n = Nobs, min = 0, max = 10) 
df <- as.data.frame(X )

df <- df %>%
    mutate(
        Z = X - 0.2* exp(-X^2) + rnorm(n = Nobs,mean = 0, sd = 2), 
        Y = as.factor(ifelse(Z >6, 1, 0))
    )

# Important this is how we NEED to specify to have a CORRECT Confusion matrix
levels(df$Y)[levels(df$Y)==0] <- "Positive"
levels(df$Y)[levels(df$Y)==1] <-"Negative"
df$Y <- relevel(df$Y, ref ="Positive")  # One predicts Positive

# Estimating Logit model ## BEWARE we predict 0 !!!
Log_Fit <- glm(Y ~ X, df, family = binomial)
df$Predictions <- predict(Log_Fit, type="response" ) 

# Define server logic required to draw a histogram
server <- function(input, output) {
    CurrentConfMat <- reactive({ 
        # Prediction with the threshold
        I_Predictions <- as.factor(df$Predictions > input$Threshold) 
        levels(I_Predictions) <-c("TRUE", "FALSE")  # in case of only one value
        levels(I_Predictions)[levels(I_Predictions)==  TRUE] <- "Negative"
        levels(I_Predictions)[levels(I_Predictions)== FALSE] <- "Positive"
        I_Predictions<- relevel(I_Predictions, ref = "Negative")
        
        #levels(I_Predictions)  <- classes  # Need to be specified in case the prediction provide only one class
        confusionMatrix(data = I_Predictions,
                                   reference = df$Y, 
                                   positive = "Negative")   # <<- this is ho to compute specificity
                                                            # And sensitivity correctly
        
    }) 
    
    
    
    
    
    output$plot1 <- renderPlot({
        ggplot(df, aes(x=X, y=Predictions,
                       colour = ifelse(Predictions > input$Threshold, 'Above','Below'))) + 
            stat_smooth(aes(x=X, y=Predictions), 
                        color ="grey",
                        method="glm", 
                        se=FALSE, fullrange=TRUE,
                        method.args = list(family=quasibinomial)) +
            geom_point(aes(shape = as.factor(Y)), size = 3) +
            geom_hline(yintercept=input$Threshold, linetype = 2) + 
            theme_minimal() + 
            theme(legend.position = "none")  +
            labs(title = "Logit curve",
                 subtitle = "(Circles should be blue, triangles red)",
                 x = "X")
    })
    
    
    output$plot2 <- renderPlotly({
        
        # Estimating with a logit classifier
        pprob <- predict(Log_Fit, type = "response")
        
        # Computing the ROC curve (specificity, Sensitivity) for many threshold
        twoclassesROC <- roc(df$Y, pprob)
        
        # Gathering the results
        myROC <- data.frame(cbind(twoclassesROC$specificities,
                                  twoclassesROC$sensitivities, 
                                  twoclassesROC$thresholds)) %>% 
            mutate_if(is.numeric, ~ifelse(abs(.) == Inf,NA,.)) %>%
            mutate(FPR = 1- X1, 
                   t = round(X3, 2)) %>%
            rename(Specificity = X1, 
                   TPR = X2) 
        
        # Computes the point which corresponds to the optimal threshold
        coords <- coords(twoclassesROC, "best", ret = "all", transpose = FALSE)
        
        # Computing the AUC
        twoclassesAUC <-  pROC::auc(twoclassesROC)
        # Visualizing
        pRoc <- myROC%>%
            distinct(FPR, .keep_all = TRUE) %>%
            ggplot() +
            aes(x = FPR, y = TPR, label =  t) +
            geom_line( colour = "red") +
            labs(x = "FPR (1- Specificity)", 
                 y = "TPR (sensitivity)", 
                 title = "ROC curve (interactive)",
                 subtitle =paste("(AUC = ", round(twoclassesAUC, 3), ")") ) +
            theme_minimal()
        
        # Computing the  isoline
        pRoc <- pRoc +  geom_segment(aes(x = 1, xend = 0, y = 1, yend = 0),
                                     color="darkgrey",
                                     linetype="dashed")
        ggplotly(pRoc)
    })
    
    
    output$values <- renderTable({
        ConfMat <- CurrentConfMat()
        sensitivity <- round(ConfMat$byClass["Sensitivity"], 4)
        specificity <- round(1-ConfMat$byClass["Specificity"], 4)
        accuracy <- round(ConfMat$overall["Accuracy"], 4)
        kappa <- round(ConfMat$overall["Kappa"], 4)
        metric.data <- data.frame(
            Metric = c( "Accuracy",
                        "Kappa",
                        "Sensitivity",
                        "1 - Specificity"),
            Value = as.character(c(accuracy,
                                   kappa,
                                   sensitivity,
                                   specificity)),
            stringsAsFactors = FALSE)
       
        metric.data
    })
    output$confmatrix <-renderPrint({
        
        CurrentConfMat()$table
        
    })
    output$confmat <-renderPrint({

        print(CurrentConfMat()$table)
        
    })
}

