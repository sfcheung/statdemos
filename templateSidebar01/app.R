# [Brief description]
# To run in R: runGitHub("statDemos","sfcheung",subdir="[folder name]")

# Global variabless

# UI
ui <- fluidPage(
  titlePanel("[Title]"),
  sidebarLayout(
      sidebarPanel(h4("Sibebar text"),
                   br(),
                   sliderInput('[slider name]',
                               label=h5("[Slide title]"),
                               min=-1, max=1, value=default, step=step,
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
    r <- input$[slider name]
    cexAll <- 1.5
    # Don't know why cex cannot control the magnification of all elements
    # So used cexAll here
    # Generate the plot object
    plot()
  })
}

shinyApp(ui=ui, server=server)
