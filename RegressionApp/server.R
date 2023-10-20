#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
SIAP.color <- "#2874A6"

server <- function(input, output) {
  # Generate the data frame
  data <- reactive({
    # set.seed(2512)  # let it be random
    n <- 150
    x <- rnorm(n)
    y <- 1 + 0.7* x + rnorm(n)
    df <- data.frame(x, y)
    
    df
  })
  
  # Create the scatter plot
  output$scatterplot <- renderPlot({
    # Generate the end points for line 
    x1 = -3
    y1 =  input$coef_b1 * x1 + input$coef_b0
    x2 = 3
    y2=  input$coef_b1 * x2 + input$coef_b0
    
    # Sign of beta1 for display
    sign_b1 <- ifelse(input$coef_b1 < 0, "", " +")
    
    # generate the plot
     p <- ggplot(data(), aes(x, y)) +
      geom_point() +
      theme_minimal() +
      
      coord_cartesian(xlim = c(-3, 3), ylim = c(-3, 3)) 
      if (input$show_line) {
        p <- p+ geom_segment( aes(x = x1, y = y1, xend = x2, yend = y2), colour = SIAP.color, linewidth = 2)
       }
      
      if (input$show_regression) {
        p <- p+ geom_smooth(method = "lm", se = TRUE, color = "darkred")
        
        # Fit the linear regression model for text display 
        model <- lm(y ~ x, data = data())
        # Get the coefficients
        coefs <- summary(model)$coefficients
        # Sign of beta1 for display
        sign_b1_reg <- ifelse(coefs[2,1]< 0, "", " +")
        # output the geom_smooth regression for the ggplot
        p
      }
     
     regression_sub <- ifelse(input$show_regression, 
                               paste("Computed regression line : Y = ",
                                     round(coefs[1,1], 1), sign_b1_reg,
                                     round(coefs[2,1],1), "X"), 
                                     "")
     regression_title <- ifelse(input$show_line, 
                                paste("Line with selected values:  Y = ",input$coef_b0, sign_b1,
                                                       input$coef_b1,"X"), "" )
    p +labs(x = "X", y = "Y", 
            title = regression_title, 
            subtitle = regression_sub) +
        
      theme(plot.title = element_text(color = SIAP.color, size = 16),
            plot.subtitle = element_text(color = "darkred", size = 16))
    })
}
