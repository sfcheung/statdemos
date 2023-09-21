# Demonstrate sampling distribution of Sample Pearson's r

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

r2z <- function(x) {
    .5 * log((1 + x) / (1 - x))
  }

# UI
ui <- fluidPage(
    titlePanel("Sampling Distribution of Pearson's r"),
    fluidRow(
      column(12,
        wellPanel(
            p("This page illustrates how to find",
              "the critical values (cutoff values)",
              "for testing a sample r (correlation)",
              "using an empirical distribution",
              "of", nrep, "simulated sample r computed from",
              nrep, "samples drawn from a",
              "population with the required population correlation."),
            p("How to Use it:"),
            p("1. Set the sample size, sample r, and level of significance."),
            p("2. Click 'Update the Plots'."),
            p("3. Examine the plots."),
            p("(This app will be discussed in the class to demonstrate",
              "the ideas of sampling distribution, p-value, critical value,",
              "and transformation.)")
        ))),
    fluidRow(
        column(6, align = "center",
          sliderInput("n",
                      label = "Sample Size (n)",
                      min = 10,
                      max = 500,
                      value = 10,
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
                      label = "Sample Correlation (r)",
                      min = -.99,
                      max = .99,
                      value = .40,
                      step = .01)),
        column(6, align = "center",
          sliderInput("alpha",
                      label = "Level of Signifance",
                      min = .001,
                      max = .10,
                      value = .05,
                      step = .001))
      ),
    fluidRow(
      column(12, submitButton(paste("Update the Plots")),
             align = "center")
      ),
    fluidRow(
        column(6, plotOutput("hist1")),
        column(6, plotOutput("hist2"))
      ),
    fluidRow(
#        column(6, verbatimTextOutput('Lower')),
#        column(6, verbatimTextOutput('Upper'))
      ),
    fluidRow(
        column(12,
          wellPanel(
              p("Annotation:"),
              htmlOutput("note")
          )
          )
      ),
    fluidRow(
        column(6, plotOutput("qq1")),
        column(6, plotOutput("qq2"))
      ),
    fluidRow(
      column(12,
        wellPanel(
          p("Version 0.1.0"),
          p("The latest version of the code can be found at ",
            a("statdemos at GitHub",
              href = "https://github.com/sfcheung/statdemos/tree/master/rDistribution"),
              "."
            ),
          p("This app can be run directly by ",
            code("shiny::runUrl(\"https://github.com/sfcheung/statdemos/raw/master/apps/rDistribution.zip\")")
            ),
          p("If you discovered any issues, such as bugs,",
            "it would be nice if you report them at",
            a("GitHub",
              href = "https://github.com/sfcheung/statdemos/issues"))
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
        r_t <- input$r * sqrt((input$n - 2) / (1 - input$r^2))
        r_p <- 2 * pt(abs(r_t),
                      df = input$n - 2,
                      lower.tail = FALSE)
        r_p <- ecdf(rs)(input$r)
        r_p <- ifelse(r_p > .50,
                      2 * (1 - r_p),
                      2 * r_p)
        hist(rs,
             breaks = 50,
             col = "grey90",
             border = "grey80",
             xlim = c(-1, 1),
             xlab = "Simulated Sample r",
             ylab = "Frequency",
             main = paste("Histogram of", nrep, "Simulated Sample rs"))
        cut_lo <- quantile(rs,
                           input$alpha / 2)
        cut_hi <- quantile(rs,
                           1 - input$alpha / 2)
        cut_t <- qt(1 - input$alpha / 2,
                    df = input$n - 2)
        cut_t_lo <- -1 * cut_t / sqrt(input$n - 2 + cut_t^2)
        cut_t_hi <- -1 * cut_t_lo
        tmp1 <- par("usr")
        y_length <- tmp1[4] - tmp1[3]
        tmp2 <- y_length / 2
        segments(x0 = cut_lo,
                 y0 = tmp1[3],
                 y1 = tmp2 * .75,
                 col = "red")
        segments(x0 = cut_hi,
                 y0 = tmp1[3],
                 y1 = tmp2 * .75,
                 col = "red")
        segments(x0 = cut_t_lo,
                 y0 = tmp2 * 1.25,
                 y1 = tmp1[4],
                 col = "blue",
                 lwd = 2)
        segments(x0 = cut_t_hi,
                 y0 = tmp2 * 1.25,
                 y1 = tmp1[4],
                 col = "blue",
                 lwd = 2)
        arrows(x0 = cut_lo,
               y0 = tmp2 * .25,
               x1 = cut_lo - (cut_lo - tmp1[1]) * .5,
               col = "red",
               length = .10)
        arrows(x0 = cut_hi,
               y0 = tmp2 * .25,
               x1 = cut_hi + (tmp1[2] - cut_hi) * .5,
               col = "red",
               length = .10)
        arrows(x0 = cut_t_lo,
               y0 = tmp2 * 1.75,
               x1 = cut_t_lo - (cut_t_lo - tmp1[1]) * .5,
               col = "blue",
               length = .10)
        arrows(x0 = cut_t_hi,
               y0 = tmp2 * 1.75,
               x1 = cut_t_hi + (tmp1[2] - cut_t_hi) * .5,
               col = "blue",
               length = .10)
        if (input$r != input$rho) {
            arrows(x0 = input$r,
                   y0 = tmp2 * .75,
                   x1 = ifelse(input$r > input$rho,
                               input$r + (tmp1[2] - input$r) * .5,
                               input$r - (input$r - tmp1[1]) * .5),
                   col = "black",
                   length = .10)
          }
        abline(v = input$r,
               col = "blue",
               lwd = 2,
               lty = "dotted")
        abline(v = input$rho,
               col = "black",
               lwd = 2,
               lty = "dashed")
        box_color <- rgb(0, 0, 255,
                         maxColorValue = 255,
                         alpha = 50,
                         names = "blue_t")
        box_color2 <- rgb(255, 0, 0,
                          maxColorValue = 255,
                          alpha = 50,
                          names = "red_t")
        r_str <- formatC(input$r, digits = 2, format = "f")
        cut_lo_str <- formatC(cut_lo, digits = 2, format = "f")
        cut_hi_str <- formatC(cut_hi, digits = 2, format = "f")
        cut_t_lo_str <- formatC(cut_t_lo, digits = 2, format = "f")
        cut_t_hi_str <- formatC(cut_t_hi, digits = 2, format = "f")
        cut_t_lo_str <- paste0("t Critical:\n", cut_t_lo_str)
        cut_t_hi_str <- paste0("t Critical:\n", cut_t_hi_str)
        r_p05 <- r_p / 2
        r_p05_str <- formatC(r_p05, digits = 3, format = "f")
        sig <- ((input$r < cut_lo) || (input$r > cut_hi))
        legend(x = input$r,
               y = .50 * y_length,
               legend = paste0("r: ",
                               r_str),
               text.col = "black",
               box.col = ifelse(sig, box_color2, box_color),
               bg = ifelse(sig, box_color2, box_color),
               xjust = .5,
               yjust = .5,
               adj = c(0.25, 0.25))
        text(x = cut_lo,
             y = .25 * y_length,
             labels = paste0("Lower\nCutoff\n",
                            cut_lo_str))
        text(x = cut_hi,
             y = .25 * y_length,
             labels = paste0("Upper\nCutoff\n",
                            cut_hi_str))
        text(x = cut_t_lo,
             y = .75 * y_length,
             labels = cut_t_lo_str)
        text(x = cut_t_hi,
             y = .75 * y_length,
             labels = cut_t_hi_str)
        if (input$r != input$rho) {
          text(x = input$r,
               y = tmp2 * .75,
               labels = ifelse(input$r > input$rho,
                               paste0("Area to right:\n",
                                      r_p05_str),
                               paste0("Area to left:\n",
                                      r_p05_str)),
               adj = ifelse(input$r > 0,
                            c(0, 0),
                            c(1, 0)))
          }
    })
    output$hist2 <- renderPlot({
      rs <- update_samples()
      zs <- r2z(rs)
      z_rho <- r2z(input$rho)
      z <- r2z(input$r)
      z_p <- ecdf(zs)(z)
      z_p <- ifelse(z_p > .50,
                    2 * (1 - z_p),
                    2 * z_p)
      hist(zs,
           breaks = 50,
           col = "grey90",
           border = "grey80",
           xlim = c(-2.7, 2.7),
           xlab = "Fisher's z from Simulated Sample r",
           ylab = "Frequency",
           main = paste("Histogram of", nrep, "Simulated Fisher's zs"))
      cut_lo <- quantile(zs,
                         input$alpha / 2)
      cut_hi <- quantile(zs,
                         1 - input$alpha / 2)
      cut_t <- qnorm(1 - input$alpha / 2)
      cut_t_lo <- -1 * cut_t / sqrt(input$n - 3)
      cut_t_hi <- -1 * cut_t_lo
      tmp1 <- par("usr")
      y_length <- tmp1[4] - tmp1[3]
      tmp2 <- y_length / 2
      segments(x0 = cut_lo,
               y0 = tmp1[3],
               y1 = tmp2 * .75,
               col = "red")
      segments(x0 = cut_hi,
               y0 = tmp1[3],
               y1 = tmp2 * .75,
               col = "red")
      segments(x0 = cut_t_lo,
               y0 = tmp2 * 1.25,
               y1 = tmp1[4],
               col = "blue",
               lwd = 2)
      segments(x0 = cut_t_hi,
               y0 = tmp2 * 1.25,
               y1 = tmp1[4],
               col = "blue",
               lwd = 2)
      arrows(x0 = cut_lo,
             y0 = tmp2 * .25,
             x1 = cut_lo - (cut_lo - tmp1[1]) * .5,
             col = "red",
             length = .10)
      arrows(x0 = cut_hi,
             y0 = tmp2 * .25,
             x1 = cut_hi + (tmp1[2] - cut_hi) * .5,
             col = "red",
             length = .10)
      arrows(x0 = cut_t_lo,
             y0 = tmp2 * 1.75,
             x1 = cut_t_lo - (cut_t_lo - tmp1[1]) * .5,
             col = "blue",
             length = .10)
      arrows(x0 = cut_t_hi,
             y0 = tmp2 * 1.75,
             x1 = cut_t_hi + (tmp1[2] - cut_t_hi) * .5,
             col = "blue",
             length = .10)
      if (z != z_rho) {
        arrows(x0 = z,
               y0 = tmp2 * .75,
               x1 = ifelse(z > z_rho,
                           z + (tmp1[2] - z) * .5,
                           z - (z - tmp1[1]) * .5),
               col = "black",
               length = .10)
      }
      abline(v = z,
             col = "blue",
             lwd = 2,
             lty = "dotted")
      abline(v = z_rho,
             col = "black",
             lwd = 2,
             lty = "dashed")
      box_color <- rgb(0, 0, 255,
                       maxColorValue = 255,
                       alpha = 50,
                       names = "blue_t")
      box_color2 <- rgb(255, 0, 0,
                        maxColorValue = 255,
                        alpha = 50,
                        names = "red_t")
      z_str <- formatC(z, digits = 2, format = "f")
      cut_lo_str <- formatC(cut_lo, digits = 2, format = "f")
      cut_hi_str <- formatC(cut_hi, digits = 2, format = "f")
      cut_t_lo_str <- formatC(cut_t_lo, digits = 2, format = "f")
      cut_t_hi_str <- formatC(cut_t_hi, digits = 2, format = "f")
      cut_t_lo_str <- paste0("z Critical:\n", cut_t_lo_str)
      cut_t_hi_str <- paste0("z Critical:\n", cut_t_hi_str)
      z_p05 <- z_p / 2
      z_p05_str <- formatC(z_p05, digits = 3, format = "f")
      sig <- ((z < cut_lo) || (z > cut_hi))
      legend(x = z,
             y = .50 * y_length,
             legend = paste0("z: ",
                             z_str),
             text.col = "black",
             box.col = ifelse(sig, box_color2, box_color),
             bg = ifelse(sig, box_color2, box_color),
             xjust = .5,
             yjust = .5,
             adj = c(0.25, 0.25))
      text(x = cut_lo,
           y = .25 * y_length,
           labels = paste0("Lower\nCutoff\n",
                           cut_lo_str))
      text(x = cut_hi,
           y = .25 * y_length,
           labels = paste0("Upper\nCutoff\n",
                           cut_hi_str))
      text(x = cut_t_lo,
           y = .75 * y_length,
           labels = cut_t_lo_str)
      text(x = cut_t_hi,
           y = .75 * y_length,
           labels = cut_t_hi_str)
      if (z != z_rho) {
        text(x = z,
             y = tmp2 * .75,
             labels = ifelse(z > 0,
                             paste0("Area to right:\n",
                                    z_p05_str),
                             paste0("Area to left:\n",
                                    z_p05_str)),
             adj = ifelse(z > 0,
                          c(0, 0),
                          c(1, 0)))
      }
    })
    output$note <- renderText({
        tmp <- paste("<ul>")
        tmp <- paste("<li>Lower Cutoff: Value with",
                     100 * input$alpha / 2,
                     "% sample <i>r</i>s to the left.")
        tmp <- paste(tmp,
                     "<li>Upper Cutoff: Value with",
                     100 * input$alpha / 2,
                     "% sample <i>r</i>s to the right.")
        tmp <- paste(tmp,
                     "<li>t Critical: Critical values",
                     "based on a <i>t</i> distributon,",
                     "assuming the population correlation is zero.")
        tmp <- paste(tmp,
                     "<li>z Critical: Critical values",
                     "based on a normal distributon,",
                     "assuming the population correlation is zero.")
        tmp <- paste(tmp,
                     "<li>Area to the left/right is",
                     "based on the distribution of simulated",
                     "sample <i>r</i>s / <i>z</i>s.")
        tmp <- paste(tmp,
                     "<li>2 x the area is the <i>p</i>-value",
                     "based on the distributon of simulated",
                     "sample <i>r</i>s / <i>z</i>s.")
        tmp <- paste(tmp,
                     "<li>(Optional) The QQ-plots below show how close",
                     "the sampling distributions are",
                     "to a normal distribution.")
        tmp <- paste(tmp, "</ul>")
        tmp
      }, sep = "\n")
    output$qq1 <- renderPlot({
        rs <- update_samples()
        qqnorm(rs,
               main = paste("QQ-Plot of the",
                            nrep,
                            "Simulated Sample rs"))
        qqline(rs)
      })
    output$qq2 <- renderPlot({
        rs <- update_samples()
        zs <- r2z(rs)
        qqnorm(zs,
               main = paste("QQ-Plot of the",
                            nrep,
                            "Simulated Sample zs"))
        qqline(zs)
      })
  }

shinyApp(ui = ui, server = server)
