# Demonstrate sampling distribution of Sample Pearson's r

# if (!(requireNamespace("lavaan", quietly = TRUE) &
#       requireNamespace("semPlot", quietly = TRUE))) {
#     stop(paste0("This app requires both the packages lavaan and semPlot.",
#                 "\n",
#                 "Please install them first."))
#   }

# Global Variables

nrep <- 20000

gen_samples <- function(rho = 0,
                        n = 50,
                        nrep = 10000,
                        r_only = TRUE) {
    out <- replicate(nrep, gen_sample_i(rho = rho,
                                        n = n),
                     simplify = FALSE)
    if (r_only) {
        rs <- sapply(out, function(xx) {cor(xx)[2, 1]})
        return(rs)
      } else {
        return(out)
      }
  }

gen_sample_i <- function(rho = 0,
                         n = 50) {
    x <- rnorm(n)
    y <- rho * x + sqrt(1 - rho^2) * rnorm(n)
    cbind(x, y)
  }

# UI
ui <- fluidPage(
  titlePanel("Sampling Distribution of Pearson's r: Illustration (Work-In-Progress)"),
  fluidRow(
    column(12,
      wellPanel(
          p("This page illustrates ...."),
        ))),
  fluidRow(
      column(6, align = "center",
        sliderInput("n",
                    label = "Sample Size (n)",
                    min = 10,
                    max = 500,
                    value = 50,
                    step = 10)),
      column(6, align = "center",
        sliderInput("rho", label = "Population Correlation",
                    min = -.99,
                    max = .99,
                    value = 0,
                    step = .01))
    ),
  fluidRow(
      column(6, align = "center",
        sliderInput("r",
                    label = "Sample Correlation",
                    min = -.99,
                    max = .99,
                    value = .50,
                    step = .01)),
      column(6, align = "center",
        sliderInput("alpha",
                    label = "Sig. Level",
                    min = .001,
                    max = .10,
                    value = .05,
                    step = .001))
    ),
  fluidRow(
    column(12, submitButton(paste("Generate", nrep, "samples")))
    ),
  fluidRow(
      column(6, plotOutput("hist1")),
      column(6, plotOutput("hist2"))
    ),
  fluidRow(
      column(6, verbatimTextOutput('Lower')),
      column(6, verbatimTextOutput('Upper'))
    ),
  fluidRow(
      column(12,
        wellPanel(
          p("(...)")
          )
        )
    ),
  fluidRow(
    column(12,
      wellPanel(
        p("Version 0.0.1"),
        p("The latest version of the code can be found at ",
          a("statdemos at GitHub",
            href = "https://github.com/sfcheung/statdemos/tree/master/rDistribution"),
            "."
          ),
        p("This app can be run directly by ",
          code("shiny::runUrl(\"https://github.com/sfcheung/statdemos/raw/master/apps/rDistribution.zip\")")
          )
        )
      )
    )
  )

# Server
server <- function(input, output) {
  update_samples <- reactive(gen_samples(rho = input$rho,
                                         n = input$n,
                                         nrep = nrep,
                                         r_only = TRUE))
  output$hist1 <- renderPlot({
      rs <- update_samples()
      hist(rs,
           breaks = 50,
           col = "grey90",
           border = "grey80",
           xlim = c(-1, 1),
           xlab = "Sample r",
           ylab = "Frequency",
           main = as.character(max(rs)))
      cut_lo <- quantile(rs,
                         input$alpha / 2)
      cut_hi <- quantile(rs,
                         1 - input$alpha / 2)
      cut_t <- qt(1 - input$alpha / 2,
                  df = input$n - 2)
      cut_t_lo <- -1 * cut_t / sqrt(input$n - 2 + cut_t^2)
      cut_t_hi <- -1 * cut_t_lo
      tmp1 <- par("usr")
      tmp2 <- (tmp1[4] - tmp1[3]) / 2
      segments(x0 = cut_lo,
               y0 = tmp1[3],
               y1 = tmp2 * 1,
               col = "red")
      segments(x0 = cut_hi,
               y0 = tmp1[3],
               y1 = tmp2 * 1,
               col = "red")
      segments(x0 = cut_t_lo,
               y0 = tmp2 * 1,
               y1 = tmp1[4],
               col = "black",
               lwd = 2)
      segments(x0 = cut_t_hi,
               y0 = tmp2 * 1,
               y1 = tmp1[4],
               col = "black",
               lwd = 2)
      arrows(x0 = cut_lo,
             y0 = tmp2 * .25,
             x1 = cut_lo - (cut_lo - tmp1[1]) * .5,
             col = "red")
      arrows(x0 = cut_hi,
             y0 = tmp2 * .25,
             x1 = cut_hi + (tmp1[2] - cut_hi) * .5,
             col = "red")
      abline(v = input$r,
             col = "blue",
             lwd = 3)
      abline(v = input$rho,
             col = "black",
             lwd = 2,
             lty = "dashed")
      box_color <- rgb(0, 0, 255,
                       maxColorValue = 255,
                       alpha = 100,
                       names = "blue_t")
      box_color2 <- rgb(255, 0, 0,
                       maxColorValue = 255,
                       alpha = 200,
                       names = "red_t")
      r_str <- formatC(input$r, digits = 2, format = "f")
      cut_lo_str <- formatC(cut_lo, digits = 2, format = "f")
      cut_hi_str <- formatC(cut_hi, digits = 2, format = "f")
      cut_t_lo_str <- formatC(cut_t_lo, digits = 2, format = "f")
      cut_t_hi_str <- formatC(cut_t_hi, digits = 2, format = "f")
      cut_t_lo_str <- paste0("t Critical:\n", cut_t_lo_str)
      cut_t_hi_str <- paste0("t Critical:\n", cut_t_hi_str)
      legend(x = input$r,
             y = .50 * (tmp1[4] - tmp1[3]),
             legend = r_str,
             text.col = "white",
             box.col = box_color,
             bg = box_color,
             xjust = .5,
             yjust = .5,
             adj = c(0.25, .5))
      text(x = cut_lo,
           y = .25 * (tmp1[4] - tmp1[3]),
           labels = cut_lo_str)
      text(x = cut_hi,
           y = .25 * (tmp1[4] - tmp1[3]),
           labels = cut_hi_str)
      text(x = cut_t_lo,
           y = .75 * (tmp1[4] - tmp1[3]),
           labels = cut_t_lo_str)
      text(x = cut_t_hi,
           y = .75 * (tmp1[4] - tmp1[3]),
           labels = cut_t_hi_str)
    })
  output$Lower <- renderPrint({
      rs <- update_samples()
      cut_lo <- quantile(rs,
                         input$alpha / 2)
      cut_hi <- quantile(rs,
                         1 - input$alpha / 2)
      cat(round(cut_lo, 3), ", ",
          round(cut_hi, 3))
    })
  output$hist2 <- renderPlot({
      rs <- update_samples()
      qqnorm(rs,
             main = as.character(max(rs)))
      qqline(rs)
    })
  }

shinyApp(ui = ui, server = server)
