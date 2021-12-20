#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

#input$input$k.choice = 250

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    

    output$knnPlot <- renderPlot({
        
        # validate(
        #     need( (input$k.choice <= 400 | input$fhat == FALSE), "Please select LESS than 400 neighbors for the full curve to be computed and displayed")
        # )

        # Scatter plot
        plot(ltexp,FoodShr,type="n",
             main= paste("K-NN regression with k=", input$k.choice,""),
             sub = "In pink the neighbors used for a specific x", 
             xlab="Log(Exp)", ylab = "FoodShare",
             pch=19, cex = 0.5,col = "grey", frame.plot = FALSE )
        points(ltexp,FoodShr,
               pch=19, cex = 0.5,col = "grey" )
        
        library(tidyverse)
        # for a specific x, highlight the points included in computation
        my.index <- input$p.choice  #  <-- value can be changed here
        my.x <- ltexp[my.index]
        my.y <- FoodShr[my.index]
        
        
        # computing x's nearest neighbors
        df <- as.data.frame(cbind(ltexp, FoodShr))
        df <- df %>%
            mutate( dist = abs(ltexp - my.x) ) %>%
            arrange(dist) %>%
            slice(1:input$k.choice)
        
        points(df$ltexp,df$FoodShr,
               pch=19, cex = 0.6,col = "pink" )
        
        rug(df$ltexp, side=1, col = "pink")
        # Original values
        points(my.x,0,
               pch=15, cex = 0.6,col = "red" )
        
        points(my.x,my.y,
               pch=18, cex = 0.9,col = "black" )
        
        
        if (input$yhat == TRUE) {
            # Computing estimation of Y using x's nearest neighbors
            
            rug(df$FoodShr, side=2, col = "pink")
            my.y.hat <- mean(df$FoodShr)
            segments(0, my.y.hat, my.x, my.y.hat,
                     lw = 2, 
                     col= 'pink')
            
            # Original values
            points(my.x,0,
                   pch=15, cex = 0.6,col = "red" )
            
            points(my.x,my.y,
                   pch=18, cex = 0.9,col = "black" )
            
            # Some illustration on the graphic
            segments( my.x, 0, my.x, my.y.hat,
                      lw = 2, 
                      col= 'pink')
            points(my.x,my.y.hat,
                   pch=15, cex = 0.6,col = "red" )
            
            
            points(0,my.y.hat,
                   pch=15, cex = 0.6,col = "red" )
            mtext(paste("For this point (i=", input$p.choice,") the error (vertical distance = yi - f(xi)) is:", round(my.y - my.y.hat, 4),""  )
                  , side=3)
            
        }
        
        if (input$fhat == TRUE & input$k.choice > 400) {
            #Estimating Food Shares using k-NN (FNN package)
            knn.est2 <- knn.reg(ltexp, y=FoodShr, k = max(1,input$k.choice))
            # plotting the k-NN regression line
            lines(ltexp, knn.est2$pred,
                  col=  "#0385a8" , lwd =2)
        }
        if (input$fhat == TRUE & input$k.choice <= 400) {  
            # Estimating Food Shares using k-NN (CARET package) <-- Limited to 400 Neighbors
            knn.est <- knnreg(FoodShr~ltexp, data = MyData, k = max(1,input$k.choice))
            # Defining the sequence of 200 points where we will estimate the k-NN line
            newx <- seq(from=min(ltexp),to=max(ltexp),
                       length.out = 200)
            # Estimating the k-NN regression line
            newy <-predict(knn.est, data.frame(ltexp = newx))
            # plotting the k-NN regression line
            lines(newx, newy,
                  col=  "#0385a8" , lwd =2)
            
           
            
        }
        

    })
    

})
