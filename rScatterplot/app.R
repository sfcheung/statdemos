# Generate a scatterplot for user-specified Pearson's r

rho <- .8 # Population correlation
n <- 500  # Sample size

# UI
ui <- fluidPage(
  titlePanel("Illustrate the data distribution for different degrees of correlation"),
  sidebarLayout(
      sidebarPanel(h4("Set the correlation and see how the scatter plot changes"),
                   br(),
                   sliderInput('rho',
                               label=h5("Correlation (Pearson's r)"),
                               min=-1, max=1, value=rho, step=.1,
                               ticks=TRUE),
                   br(),
                   br(),
                   h5("Technical details:"),
                   paste("500 pairs of random numbers, correlated at ",
                         "the specified Pearson's r, are drawn from ",
                         "a bivariate standard normal distribution.", sep="")
                  ),
      mainPanel(plotOutput('plot'))
    )
)

# Server
server <- function(input, output) {
  output$plot <- renderPlot({
    r <- input$rho
    x <- scale(rnorm(n))
    ifelse (abs(r) == 1,
      e <- 0,
      e <- scale(rnorm(n,0,sqrt(1-r^2)))
      )
    y <- r*x + e
    cexAll <- 1.5
    # Don't know why cex cannot control the magnification of all elements
    # So used cexAll here
    plot(x,y, cex=cexAll, cex.axis=cexAll, cex.lab=cexAll, 
         cex.sub=cexAll, cex.main=cexAll, pch="o", col="blue",
         main=paste("Scatter Plot (Pearson's r = ", r, ")",sep=""),
         sub="The red line is the loess line fitted to the data")
    lines(lowess(x,y), col="red", lwd=4)
  })
}

shinyApp(ui=ui, server=server)
