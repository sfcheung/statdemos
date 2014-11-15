# Illustrate the residual sum of squares in simple linear regression
# To run in R: runGitHub("statDemos","sfcheung",subdir="lmResidual")

# Global variables
b0def <- 40 # Initial intercept
b1def <- 0  # Initial slope
# Raw data
predictor <- c(3,3,4,4,5,5,6,6,7,7)
outcome <- c(30,40,10,30,40,60,40,60,40,50)
lmData <- data.frame(predictor, outcome)
n <- nrow(lmData) # Sample size 

# UI
ui <- fluidPage(
  titlePanel("Illustrate the residual sum of squares for a linear model"),
  sidebarLayout(
      sidebarPanel(h4(paste("Set the intercept and slope for your model, and ",
                            "see how the residual sum of squares changes",
                            sep="")),
                   br(),
                   sliderInput('b0',
                               label=h5("Intecept (b0)"),
                               min=0, max=max(outcome), value=b0def, step=.5,
                               ticks=TRUE),
                   sliderInput('b1',
                               label=h5("slope (b1)"),
                               min=-30, max=30, value=b1def, step=.5,
                               ticks=TRUE),
                   br(),
                   br(),
                   h5("Technical details:"),
                   paste("[Technical details]", sep="")
                  ),
      mainPanel(plotOutput('plot'))
    )
)

# Server
server <- function(input, output) {
  output$plot <- renderPlot({
    b0i <- input$b0
    b1i <- input$b1
    # Get the range of predicted values
    xlo <- 0
    xhi <- max(predictor)
    ylo <- min(b0i + b1i*xlo,b0i + b1i*xhi)
    yhi <- max(b0i + b1i*xlo,b0i + b1i*xhi)
    # Plot the  graph
    cexAll <- 1.5
    # Don't know why cex cannot control the magnification of all elements
    # So used cexAll here
    # Generate the plot object
    plot(predictor, outcome, 
          xlim=c(xlo,xhi), ylim=c(min(outcome, ylo),max(outcome,yhi)),
          pch="o", cex=2
          )
    abline(a=b0i, b=b1i, lwd=2, col="blue")
  })
}

shinyApp(ui=ui, server=server)
