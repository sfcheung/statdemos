# Generate a scatterplot for user-specified Pearson's r
# To run in R: runGitHub("statDemos","sfcheung",subdir="rScatterplot")

rho <- .8 # Population correlation
n <- 500  # Sample size

ui <- fluidPage(
  titlePanel("Illustrate the data distribution for different degrees of correlation"),
  fluidRow(
    column(12,
      wellPanel(
        p("This page illustrates how the data may look like for a value of ",
          "Pearson's r, ", strong("if"), "the distribution is a linear one.",
          "You can change the correlation and examine the scatterplot, to have",
          "an idea how the Pearson's r can measure a linear association."
          )
        ),
      fluidRow(
        column(4,
          wellPanel(
            h4("Set the correlation and see how the scatter plot changes"),
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
            )
          ),
        column(8,
          plotOutput('plot')
          )
        )
      )
    ),
  fluidRow(
    column(12,
      wellPanel(
        p("The latest version of the code can be found at ",
          a("statDemos at GitHub", 
            href="https://github.com/sfcheung/statDemos/tree/master/rScatterplot"),
          "."
          ),
        p("The whole repository can be downloaded from GitHub and run in R by",
          code("runGitHub(\"statDemos\",\"sfcheung\",subdir=\"rScatterplot\")")
          )
        )
      )
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
         main=paste("Scatter Plot (Pearson's r = ", sprintf("%4.2f", r), ")",
              sep=""),
         sub="The red line is the loess line fitted to the data")
    lines(lowess(x,y), col="red", lwd=4)
  })
}

shinyApp(ui=ui, server=server)
