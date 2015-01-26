# Mediation: The effect of scale changes on effect size measures
# To run in R: runGitHub("statDemos","sfcheung",subdir="mediationStdES")

# Work in progress. Not yet finished.

# Global variables

# Initial model

set.seed(9879713)
n <- 1000
x2m <- 10
m2y <- 80
work_hour_raw <- 8 + 2*scale(rnorm(n))
output_weight_raw <- 10 + x2m*work_hour_raw + scale(rnorm(n, 0, 10),scale=FALSE)
salary_raw <- 3000 + m2y*output_weight_raw + scale(rnorm(n, 0, 50),scale=FALSE)

# UI
ui <- fluidPage(
  titlePanel("Mediation: Effect of Scale Changes on Effect Size Measures"),
  fluidRow(
    column(12,
      wellPanel(
        h4("Work in progress. Not yet ready."),
        h4("Description Panel", br(), 
            a("Reference", href="http://www.apa.org/pubs/journals/features/met-16-2-93.pdf"))
        ),
      fluidRow(
        column(4,
          wellPanel(
            h4("Sibebar Panel"),
            br(),
            radioButtons('time_unit', 'IV Unit: Time Unit for Work Duration',
                        c('Second'='second',
                          'Minute'='minute',
                          'Hour'='hour'), selected="hour", inline=TRUE),
            radioButtons('weight_unit', 'Mediator Unit: Weight Output in Weight',
                         c('Gram'='gram',
                           'Kilogram'='kilogram'), selected="kilogram", inline=TRUE),
            radioButtons('money_unit', 'DV Unit: Money Unit for Salary (DV)',
                         c('MOP'='mop',
                           'USD'='usd'), selected="mop", inline=TRUE),
            br(),
            h5("Technical details:"),
            paste("[Technical details]", sep="")
            )
          ),
        column(8,
          h4("a Path: Unstandardized Effect of IV (Work Hour) on Mediator (Output in Weight)"),
          verbatimTextOutput('resultsX2M'),
          h4("b Path: Unstandardized Effect of Output in Mediator (Weight) on DV (Salary)"),
          verbatimTextOutput('resultsM2Y'),
          h4("Product a*b: Unstandardized Indirect Effect of IV (Work Hour) on DV (Salary)"),
          verbatimTextOutput('resultsIndirect'),
          h4("Effect Size Measures of Indirect Effect (a*b) of IV (Work Hour) on DV (Salary)"),
          verbatimTextOutput('resultsES')
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
    x2y_ind <- coef_x2m*coef_m2y
    x_sd <- sd(work_hour)
    y_sd <- sd(salary)
    es_pstd <- x2y_ind/y_sd
    es_cstd <- x_sd*x2y_ind/y_sd
    es_all <- data.frame(ES=c(es_pstd,es_cstd))
    rownames(es_all) <- c("Partially Standardized Indirect Effect",
                          "Completely Standardized Indirect Effect")
    output$resultsM2Y <- renderPrint(print(round(coef_m2y, 4)))
    output$resultsIndirect <- renderPrint(print(round(coef_x2m*coef_m2y, 4)))
    output$resultsES <- renderPrint(round(es_all,4))
    print(round(coef_x2m,4))
    })
  }

shinyApp(ui=ui, server=server)
