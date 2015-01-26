# Mediation: The effect of scale changes on effect size measures
# To run in R: runGitHub("statDemos","sfcheung",subdir="mediationStdES")

# Work in progress. Not yet fininshed.

# Global variables

# Initial model

set.seed(9879713)
x2m <- 1
m2y <- 80
work_hour_raw <- rnorm(100, 8, 2)
output_weight_raw <- 10 + x2m*work_hour_raw + rnorm(100, 0, 10)
salary_raw <- 3000 + m2y*output_weight_raw + rnorm(100, 0, 50)


# UI
ui <- fluidPage(
  titlePanel("Mediation: Effect of Scale Changes on Effect Size Measures"),
  fluidRow(
    column(12,
      wellPanel(
        h4("Description Panel")
        ),
      fluidRow(
        column(4,
          wellPanel(
            h4("Sibebar Panel"),
            br(),
            radioButtons('time_unit', 'Time Unit for Work Hour (IV)',
                        c('Second'='second',
                          'Minute'='minute',
                          'Hour'='hour'), selected="hour", inline=TRUE),
            radioButtons('weight_unit', 'Weight Unit for Output in Weight (Mediator)',
                         c('Gram'='gram',
                           'Kilogram'='kilogram'), selected="kilogram", inline=TRUE),
            radioButtons('money_unit', 'Money Unit for Salary (DV)',
                         c('MOP'='mop',
                           'USD'='usd'), selected="mop", inline=TRUE),
            br(),
            h5("Technical details:"),
            paste("[Technical details]", sep="")
            )
          ),
        column(8,
          h5("Unstandardized Effect of Work Hour (IV) on Output in Weight (Mediator)"),
          verbatimTextOutput('resultsX2M'),
          h5("Unstandardized Effect of Output in Weight (Mediator) on Salary (DV)"),
          verbatimTextOutput('resultsM2Y'),
          h5("Unstandardized Indirect Effect of Work Hour (IV) on Salary (DV)"),
          verbatimTextOutput('resultsIndirect')
        )
        )
      )
    ),
  fluidRow(
    column(12,
      wellPanel(
        p("The latest version of the code can be found at ",
          a("statDemos at GitHub", 
            href="https://github.com/sfcheung/statDemos/mediationStdES"),
          "."
          ),
        p("The whole repository can be downloaded from GitHub and run in R by",
          code("runGitHub(\"statDemos\",\"sfcheung\",subdir=\"mediationStdES\")")
          )
        )
      )
    )
  )

# Server
server <- function(input, output) {
  output$resultsX2M <- renderPrint({
    work_hour_unit <- switch(input$time_unit,
                             second=60, minute=1, hour=1/60)
    output_weight_unit <- switch(input$weight_unit,
                                 gram=1000, kilogram=1)
    salary_unit <- switch(input$money_unit,
                          mop=1, usd=1/8)
    work_hour <- work_hour_raw * work_hour_unit
    output_weight <- output_weight_raw * output_weight_unit
    salary <- salary_raw * salary_unit
    mean(cbind(work_hour, output_weight, salary))
    model_m <- (lm(output_weight ~ work_hour))
    model_y <- (lm(salary ~ output_weight + work_hour))
    coef_x2m <- coef(model_m)[2]
    coef_m2y <- coef(model_y)[2]
    output$resultsM2Y <- renderPrint(print(coef_m2y, digits=4))
    output$resultsIndirect <- renderPrint(print(coef_x2m*coef_m2y, digits=4))
    print(coef_x2m, digits=4)
    })
  }

shinyApp(ui=ui, server=server)
