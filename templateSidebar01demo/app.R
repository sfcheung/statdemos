# [Brief description]
# Global variables

# UI
ui <- fluidPage(
  titlePanel("[Title]"),
  sidebarLayout(
      sidebarPanel(h4("Sibebar text"),
                   br(),
                   sliderInput("test_slider1",
                               label = h4("[Slide title (x)]"),
                               min = -1,
                               max = 1,
                               value = .5,
                               step = .01,
                               ticks = TRUE),
                   sliderInput("test_slider2",
                               label = h5("[Slide title (y)]"),
                               min = -1,
                               max = 1,
                               value = .5,
                               step = .01,
                               ticks = TRUE),
                   sliderInput("rho",
                               label = "Population Correlation (rho)",
                               min = -1,
                               max = 1,
                               value = 0,
                               step = .01,
                               ticks = TRUE),
                   numericInput("n",
                                label = "Sample Size (n)",
                                min = 10,
                                max = 1000,
                                value = 10,
                                step = 10),
                   br(),
                   br(),
                   h5("Technical details:"),
                   paste("[Technical details]",
                         sep = "")
                  ),
      mainPanel(plotOutput("plot"))
    )
)

# Server
server <- function(input, output) {
  output$plot <- renderPlot({
    x <- input$test_slider1
    y <- input$test_slider2
    rho <- input$rho
    n <- input$n
    dat_x <- rnorm(n)
    dat_y <- rho * dat_x + sqrt(1 - rho^2) * rnorm(n)
    # Generate the plot object
    plot(x = x,
         y = y,
         ylim = c(-3, 3),
         xlim = c(-3, 3),
         cex = 5,,
         pch = 20,
         col = "red")
    points(x = dat_x,
           y = dat_y,
           col = "blue",
           cex = 2)
  })
}

shinyApp(ui = ui,
         server = server)
