#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#


library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("K-NN regression visualization"),
    tags$h5("Christophe Bontemps (2021)"),
    tags$p("You can play with the sliders to estimate k-NN on this data set"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            
            sliderInput("p.choice",
                        "Select a Point",
                        min = 1,
                        max = 1109,
                        ticks = FALSE,
                        value = 200),
            tags$i("In black, on the graphic, the corresponding observation"),
            tags$hr(),
            sliderInput("k.choice",
                        "Select the number of neighbors:",
                        min = 1,
                        max = 1100,
                        ticks = FALSE,
                        value = 20),
            tags$i(HTML(paste("In ", tags$b(style="color:pink", "pink"), ", on the graphic, the neighbors", sep = ""))),
            tags$hr(),
            checkboxInput("yhat", "Estimate y for selected point", FALSE),
            checkboxInput("fhat", "Estimate f(.) curve for all points", FALSE)
           
        ),

        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("knnPlot")
        )
    )
))
