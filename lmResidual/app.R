# Illustrate the residual sum of squares in simple linear regression
# To run in R: runGitHub("statDemos","sfcheung",subdir="lmResidual")

# To-dos:
#   - Write functions for RSS and R-squared for user model.
#     There are duplicated lines in the render functions.

# Global variables
b0def <- 42 # Initial intercept
b1def <- 0  # Initial slope
# Raw data
predictor <- c(1,1.5,2,2.5,3,3.5,4,4.5,5,5.5)
outcome <- c(30,40,10,30,40,60,40,60,40,70)
lmData <- data.frame(predictor, outcome)
n <- nrow(lmData) # Sample size 
# Linear regression for this data
lm.results <- lm(outcome ~ predictor, data=lmData)
lm.rss <- anova(lm.results)$Sum[2]
lm.tss <- sum(anova(lm.results)$Sum)
lm.rsq <- summary(lm.results)$r.squared

# The following lines are not ready
# Models tried
models <- data.frame(b0=rep(NA,5), b1=rep(NA,5), 
                      RSS=rep(NA,5), Rsquared=rep(NA,5))
models[1,] <- c(b0def, b1def, 0, 0)

# UI
ui <- fluidPage(
  titlePanel("Illustrate the residual sum of squares for a linear model"),
  sidebarLayout(
      sidebarPanel(h4(paste("Set the intercept and slope for your model, and ",
                            "see how the residual sum of squares changes:",
                            sep="")),
                   br(),
                   sliderInput('b0',
                               label=h5("Intecept (b0)"),
                               min=min(outcome)-10, max=max(outcome)+10, value=b0def, step=.5,
                               ticks=TRUE),
                   sliderInput('b1',
                               label=h5("slope (b1)"),
                               min=-30, max=30, value=b1def, step=.5,
                               ticks=TRUE)
                   #br(),
                   #h4("Models explored"),
                   #tableOutput("modelsTab")
                  ),
      mainPanel(
          h5(paste("The \"best\" (least squares) model: Residual sum of squares=", 
              format(lm.rss, nsmall=2), 
              " ; R-squared=",
              sprintf("%3.2f", lm.rsq), sep="")),
          h5(textOutput("rss")),
          p(paste("The R-squared for your model can be less than 0 or greater 1 ",
                  "if it is not the least squares model. ",
                  "This is not an error in computation.", sep="")),
          plotOutput("plot")
        )
    )
)

# Server
server <- function(input, output) {
  output$plot <- renderPlot({
      b0i <- input$b0
      b1i <- input$b1
      # Predicted values
      outcome_hat <- b0i + b1i*predictor
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
            pch="O", cex=cexAll, cex.axis=cexAll, cex.main=cexAll, cex.lab=cexAll,
            xlab="Average hours watched TV daily in last seven days",
            ylab="Happiness score",
            main=paste("Try to find the \"best\" line by setting ",
                    "the intercept(b0) and slope(b1).", sep=""))
      abline(a=b0i, b=b1i, lwd=4, col="blue")
      segments(predictor,outcome,predictor,outcome_hat, col="red", lty="dotted",
                lwd=4)
    })
  output$rss <- renderText({
      b0i <- input$b0
      b1i <- input$b1
      # Predicted values
      outcome_hat <- b0i + b1i*predictor
      rssi <- sum((outcome_hat - outcome)^2)
      rsqi <- 1-rssi/lm.tss
      paste("Your model: Residual sum of squares=", 
            format(rssi, nsmall=2), 
            " ; R-squared=",
            sprintf("%3.2f", rsqi), sep="")
    })
  # The following lines are not ready
  #output$modelsTab <- renderTable({
  #    b0i <- input$b0
  #    b1i <- input$b1
  #    # Predicted values
  #    outcome_hat <- b0i + b1i*predictor
  #    rssi <- sum((outcome_hat - outcome)^2)
  #    rsqi <- 1-rssi/lm.tss
  #    if (rsqi > max(models$Rsquared, na.rm=TRUE)) {
  #        models[2:5,] <- models[1:4,]
  #        models[1,] <- c(b0i, b1i, rssi, rsqi)
  #      }
  #    models
  #  })
}

shinyApp(ui=ui, server=server)
