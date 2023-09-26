# Demonstrate bootstrapping in Sample Pearson's r

# Global Variables

nboot <- 20000
n_raw <- 10000
nrep <- 1000

gen_boots <- function(r = 0,
                      n = 50,
                      nboot = 100,
                      x_dist = c("normal",
                                 "chisq",
                                 "uniform",
                                 "beta"),
                      x_df = 1,
                      x_alpha = .5,
                      x_beta = .5) {
    dat <- gen_sample_i(r = r,
                        n = n,
                        x_dist = x_dist,
                        x_df = x_df,
                        x_alpha = x_alpha,
                        x_beta = x_beta)
    rs <- gen_boot_i(data = dat,
                     nboot = nboot)
    rs
  }

gen_boot_i <- function(data,
                       nboot = 100) {
    n <- nrow(data)
    fct_i <- function(data) {
        i <- sample.int(n, replace = TRUE)
        cor(data[i, 1], data[i, 2])
      }
    out <- replicate(nboot,
                     fct_i(data))
    out
  }

gen_sample_i <- function(r = NULL,
                         n = 50,
                         x_dist = c("normal",
                                    "chisq",
                                    "uniform",
                                    "beta"),
                         x_df = 1,
                         x_alpha = .5,
                         x_beta = .5) {
    x_dist <- match.arg(x_dist)
    sd_beta <- sqrt(x_alpha * x_beta) / ((x_alpha + x_beta) * sqrt(x_alpha + x_beta + 1))
    mean_beta <- (x_alpha / (x_alpha + x_beta))
    x <- switch(x_dist,
                normal = rnorm(n),
                chisq = (rchisq(n, df = x_df) - x_df) / sqrt(2 * x_df),
                uniform = runif(n, -.5, .5) / sqrt(12),
                beta = (rbeta(n, x_alpha, x_beta) - mean_beta) / sd_beta)
    y <- switch(x_dist,
                normal = rnorm(n),
                chisq = (rchisq(n, df = x_df) - x_df) / sqrt(2 * x_df),
                uniform = runif(n, -.5, .5) / sqrt(12),
                beta = (rbeta(n, x_alpha, x_beta) - mean_beta) / sd_beta)
    dat0 <- cbind(x, y)
    if (is.null(r)) {
        out <- dat0
        colnames(out) <- c("x", "y")
        return(out)
      } else {
        out <- to_r(r = r, data = dat0)
        colnames(out) <- c("x", "y")
        return(out)
      }
  }

to_r <- function(r = 0,
                 data) {
    sigma0 <- cov(data)
    tmp <- eigen(sigma0)
    dat1 <- data %*% tmp$vectors
    tmp2 <- diag(1 / sqrt(diag(cov(dat1))))
    dat1 <- dat1 %*% tmp2
    x1 <- dat1[, 1]
    y1 <- r * dat1[, 1] + sqrt(1 - r^2) * dat1[, 2]
    out <- cbind(x1, y1)
    colnames(out) <- c("x", "y")
    out
  }

r2z <- function(x) {
    .5 * log((1 + x) / (1 - x))
  }

# UI
ui <- fluidPage(
    titlePanel("Bootstrap Distribution of Pearson's r"),
    fluidRow(
      column(12,
        wellPanel(
            p("This page illustrates the behavior",
              "of the nonparametric percentile bootstrap",
              "confidence interval for a sample r (correlation)",
              "using",
              nboot, "bootstrap samples."),
            p("How to Use it:"),
            p("1. Set the sample size, sample r, and level of confidence."),
            p("2. Click 'Generate a New Sample and Update the Plots'."),
            p("3. Examine the plots."),
            p("(Changing",
              "the distributions of components are optional.)"),
            p("(This app will be discussed in the class to demonstrate",
              "the strengths and weaknesses of bootstrapping.)")
        ))),
    fluidRow(
        column(4, align = "center",
          sliderInput("n",
                      label = "Sample Size (n)",
                      min = 10,
                      max = 500,
                      value = 40,
                      step = 10)),
        column(4, align = "center",
               sliderInput("r",
                           label = "Sample Correlation (r)",
                           min = -.99,
                           max = .99,
                           value = .30,
                           round = -2,
                           step = .01)),
        column(4, align = "center",
               sliderInput("level",
                           label = "Level of Confidence",
                           min = .60,
                           max = .99,
                           value = .95,
                           round = -3,
                           step = .01))
      ),
    fluidRow(
        column(4, align = "center",
            checkboxInput("show_t",
                          label = "Show confidence interval based on t or normal distribution",
                          TRUE)
        ),
        column(4, align = "center",
            checkboxInput("show_cutoff",
                          label = "Show nonparametric bootstrap percentile confidence interval",
                          TRUE)
        ),
        column(4, align = "center",
            checkboxInput("fix_x",
                          label = "Fix the ranges of horizontal axes",
                          TRUE)
        )
      ),
    fluidRow(
      column(12, submitButton(paste("Generate a New Sample and Update the Plots")),
             align = "center")
      ),
    fluidRow(
        column(6, plotOutput("hist1")),
        column(6, plotOutput("hist2"))
      ),
    fluidRow(
        column(12,
            wellPanel(
                htmlOutput("note")
            )
          )
      ),
    fluidRow(
        column(12, align = "center",
            h3("Optional:"),
            p("Click 'Generate a New Sample and Update the Plots' after changing the following")
          )
      ),
    fluidRow(
        column(4, align = "center",
            selectInput("x_dist", "Distribution of the components used to generate the data:",
                        c("Normal" = "normal",
                          "Chi-square" = "chisq",
                          "Uniform" = "uniform",
                          "Beta" = "beta")),
            p("Change to any option other than 'normal' to generate 'some kind of'",
              "nonnormal data. There is no need to know exactly how to control the shape.")
          ),
        column(4, align = "center",
            sliderInput("x_df",
                        label = "df (for Chi-square Distribution)",
                        min = 1,
                        max = 100,
                        value = 1,
                        step = 1)
          ),
        column(4, align = "center",
            sliderInput("x_alpha",
                        label = "alpha (for Beta Distribution)",
                        min = .10,
                        max = 5,
                        value = .50,
                        step = .01),
            sliderInput("x_beta",
                        label = "beta (for Beta Distribution)",
                        min = .10,
                        max = 5,
                        value = .50,
                        step = .01)
          )
      ),
    fluidRow(
      column(12, submitButton(paste("Generate a New Sample and Update the Plots")),
             align = "center")
      ),
    fluidRow(
        column(6, plotOutput("qq1")),
        column(6, plotOutput("qq2"))
      ),
    fluidRow(
        column(4, plotOutput("histx")),
        column(4, plotOutput("histy")),
        column(4, plotOutput("scatterxy"))
      ),
    fluidRow(
        column(12,
            wellPanel(
                p("Note that the two variables are",
                  "generated such that they have exactly",
                  "the sample correlation set above. Because eigenvectors",
                  "are used to force the generated components to be",
                  "uncorrelated, the x and y generated",
                  "may not have the same univariate distributions.",
                  "This is not an issue because the goal of the illustration",
                  "is to generate 'some' nonnormal data. Having",
                  "exact control on the form is not the main concern.")
              )
          )
      ),
    fluidRow(
      column(12,
        wellPanel(
          p("Version 0.1.0"),
          p("The latest version of the code can be found at ",
            a("statdemos at GitHub",
              href = "https://github.com/sfcheung/statdemos/tree/master/rBoot"),
              "."
            ),
          p("This app can be run directly by ",
            code("shiny::runUrl(\"https://github.com/sfcheung/statdemos/raw/master/apps/rBoot.zip\")")
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
    update_samples <- reactive(gen_boots(r = input$r,
                                         n = input$n,
                                         nboot = nboot,
                                         x_dist = input$x_dist,
                                         x_df = input$x_df,
                                         x_alpha = input$x_alpha,
                                         x_beta = input$x_beta))
    # update_boots <- reactive(gen_boots(r = input$r,
    #                                    data = raw_data,
    #                                    nboot = nboot))
    # update_a_sample <- reactive(gen_sample_i(r = NULL,
    #                                          n = input$n,
    #                                          x_dist = input$x_dist,
    #                                          x_df = input$x_df,
    #                                          x_alpha = input$x_alpha,
    #                                          x_beta = input$x_beta))
    update_raw_data <- reactive(gen_sample_i(r = input$r,
                                             n = n_raw,
                                             x_dist = input$x_dist,
                                             x_df = input$x_df,
                                             x_alpha = input$x_alpha,
                                             x_beta = input$x_beta))
    output$hist1 <- renderPlot({
        # raw_data <<- update_a_sample()
        alpha <- 1 - input$level
        rho <- 0
        rs <- update_samples()
        r_t <- input$r * sqrt((input$n - 2) / (1 - input$r^2))
        r_p <- 2 * pt(abs(r_t),
                      df = input$n - 2,
                      lower.tail = FALSE)
        r_p <- ecdf(rs)(input$r)
        r_p <- ifelse(r_p > .50,
                      2 * (1 - r_p),
                      2 * r_p)
        # TODO: Try to reduce code duplication
        if (input$fix_x) {
            hist(rs,
                breaks = 50,
                col = "grey90",
                border = "grey80",
                xlim = c(-1, 1),
                xlab = "Bootstrap r",
                ylab = "Frequency",
                main = paste("Histogram of", nboot, "Bootstrap rs"))
          } else {
            hist(rs,
                breaks = 50,
                col = "grey90",
                border = "grey80",
                xlab = "Bootstrap r",
                ylab = "Frequency",
                main = paste("Histogram of", nboot, "Bootstrap rs"))
          }

        cut_lo <- quantile(rs,
                           alpha / 2)
        cut_hi <- quantile(rs,
                           1 - alpha / 2)
        cut_t <- qt(1 - alpha / 2,
                    df = input$n - 2)
        se_r <- (1 - input$r^2) / sqrt(input$n - 1)
        cut_t_lo <- input$r - se_r * cut_t
        cut_t_hi <- input$r + se_r * cut_t
        tmp1 <- par("usr")
        y_length <- tmp1[4] - tmp1[3]
        tmp2 <- y_length / 2
        if (input$show_cutoff) {
            segments(x0 = cut_lo,
                     y0 = tmp1[3],
                     y1 = tmp2 * 1,
                     col = "red")
            segments(x0 = cut_hi,
                     y0 = tmp1[3],
                     y1 = tmp2 * 1,
                     col = "red")
            arrows(x0 = cut_lo,
                   y0 = tmp2 * .25,
                   x1 = cut_hi,
                   col = "red",
                   length = .10,
                   code = 3)
          }
        if (input$show_t) {
            segments(x0 = cut_t_lo,
                     y0 = tmp2 * 1,
                     y1 = tmp1[4],
                     col = "blue",
                     lwd = 2)
            segments(x0 = cut_t_hi,
                     y0 = tmp2 * 1,
                     y1 = tmp1[4],
                     col = "blue",
                     lwd = 2)
            arrows(x0 = cut_t_lo,
                   y0 = tmp2 * 1.75,
                   x1 = cut_t_hi,
                   col = "blue",
                   length = .10,
                   code = 3)
          }
        abline(v = input$r,
               col = "blue",
               lwd = 2,
               lty = "dotted")
        abline(v = rho,
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
        cut_lo_str <- formatC(cut_lo, digits = 3, format = "f")
        cut_hi_str <- formatC(cut_hi, digits = 3, format = "f")
        cut_t_lo_str <- formatC(cut_t_lo, digits = 3, format = "f")
        cut_t_hi_str <- formatC(cut_t_hi, digits = 3, format = "f")
        cut_t_lo_str <- paste0("t-based\nLower\nLimit:\n", cut_t_lo_str)
        cut_t_hi_str <- paste0("t-based\nUpper\nLimit:\n", cut_t_hi_str)
        r_p_str <- formatC(r_p, digits = 3, format = "f")
        r_p05 <- r_p / 2
        r_p05_str <- formatC(r_p05, digits = 3, format = "f")
        sig <- ((cut_t_lo > 0) || (cut_t_hi < 0))
        legend(x = input$r,
               y = .50 * y_length,
               legend = paste0("r: ",
                               r_str),
               text.col = "black",
               box.col = box_color,
               bg = box_color,
               xjust = .5,
               yjust = .5,
               adj = c(0.25, 0.25))
        if (input$show_cutoff) {
            text(x = cut_lo,
                 y = .25 * y_length,
                 labels = paste0("Lower\nLimit\n",
                                 cut_lo_str))
            text(x = cut_hi,
                 y = .25 * y_length,
                 labels = paste0("Upper\nLimit\n",
                                 cut_hi_str))
          }
        if (input$show_t) {
            text(x = cut_t_lo,
                 y = .75 * y_length,
                 labels = cut_t_lo_str)
            text(x = cut_t_hi,
                 y = .75 * y_length,
                 labels = cut_t_hi_str)
          }
    })
    output$hist2 <- renderPlot({
      # raw_data <<- update_a_sample()
      alpha <- 1 - input$level
      rs <- update_samples()
      zs <- r2z(rs)
      z_rho <- 0
      z <- r2z(input$r)
      z_p <- ecdf(zs)(z)
      z_p <- ifelse(z_p > .50,
                    2 * (1 - z_p),
                    2 * z_p)
      # TODO: Try to reduce code duplication
      if (input$fix_x) {
          hist(zs,
              breaks = 50,
              col = "grey90",
              border = "grey80",
              xlim = c(-2.7, 2.7),
              xlab = "Fisher's z from Bootstrap r",
              ylab = "Frequency",
              main = paste("Histogram of", nboot, "Bootstrap Fisher's zs"))
        } else {
          hist(zs,
              breaks = 50,
              col = "grey90",
              border = "grey80",
              xlab = "Fisher's z from Bootstrap r",
              ylab = "Frequency",
              main = paste("Histogram of", nboot, "Bootstrap Fisher's zs"))
        }
      cut_lo <- quantile(zs,
                         alpha / 2)
      cut_hi <- quantile(zs,
                         1 - alpha / 2)
      cut_t <- qnorm(1 - alpha / 2)
      se_z <- 1 / sqrt(input$n - 3)
      cut_t_lo <- z - se_z * cut_t
      cut_t_hi <- z + se_z * cut_t
      tmp1 <- par("usr")
      y_length <- tmp1[4] - tmp1[3]
      tmp2 <- y_length / 2
      if (input$show_cutoff) {
          segments(x0 = cut_lo,
                   y0 = tmp1[3],
                   y1 = tmp2 * 1,
                   col = "red")
          segments(x0 = cut_hi,
                   y0 = tmp1[3],
                   y1 = tmp2 * 1,
                   col = "red")
          arrows(x0 = cut_lo,
                 y0 = tmp2 * .25,
                 x1 = cut_hi,
                 col = "red",
                 length = .10,
                 code = 3)
        }
      if (input$show_t) {
          segments(x0 = cut_t_lo,
                   y0 = tmp2 * 1,
                   y1 = tmp1[4],
                   col = "blue",
                   lwd = 2)
          segments(x0 = cut_t_hi,
                   y0 = tmp2 * 1,
                   y1 = tmp1[4],
                   col = "blue",
                   lwd = 2)
          arrows(x0 = cut_t_lo,
                 y0 = tmp2 * 1.75,
                 x1 = cut_t_hi,
                 col = "blue",
                 length = .10,
                 code = 3)
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
      cut_lo_str <- formatC(cut_lo, digits = 3, format = "f")
      cut_hi_str <- formatC(cut_hi, digits = 3, format = "f")
      cut_t_lo_str <- formatC(cut_t_lo, digits = 3, format = "f")
      cut_t_hi_str <- formatC(cut_t_hi, digits = 3, format = "f")
      cut_t_lo_str <- paste0("z-based\nLower\nLimit:\n", cut_t_lo_str)
      cut_t_hi_str <- paste0("z-based\nUpper\nLimit:\n", cut_t_hi_str)
      z_p_str <- formatC(z_p, digits = 3, format = "f")
      z_p05 <- z_p / 2
      z_p05_str <- formatC(z_p05, digits = 3, format = "f")
      sig <- ((z < cut_lo) || (z > cut_hi))
      legend(x = z,
             y = .50 * y_length,
             legend = paste0("z: ",
                             z_str),
             text.col = "black",
             box.col = box_color,
             bg = box_color,
             xjust = .5,
             yjust = .5,
             adj = c(0.25, 0.25))
      if (input$show_cutoff) {
          text(x = cut_lo,
               y = .25 * y_length,
               labels = paste0("Lower\nLimit\n",
                               cut_lo_str))
          text(x = cut_hi,
               y = .25 * y_length,
               labels = paste0("Upper\nLimit\n",
                               cut_hi_str))
        }
      if (input$show_t) {
          text(x = cut_t_lo,
               y = .75 * y_length,
               labels = cut_t_lo_str)
          text(x = cut_t_hi,
               y = .75 * y_length,
               labels = cut_t_hi_str)
        }
    })
    output$note <- renderText({
        alpha <- 1 - input$level
        tmp <- ""
        tmp <- paste(tmp, "<h3>Things to Check</h3>")
        tmp <- paste(tmp, "<ul>")
        tmp <- paste(tmp,
                     "<li>Are the two confidence intervals in each plot",
                     "close to each other?")
        tmp <- paste(tmp,
                     "<li>If the two intervals are",
                     "different, what if we use the",
                     "symmetric confidence interval to test the null hypothesis?")
        tmp <- paste(tmp,
                     "<li>Does the <i>t</i>",
                     "confidence interval change if we generate a new sample?")
        tmp <- paste(tmp,
                     "<li>Does the Fisher's <i>z</i>",
                     "confidence interval change if we generate a new sample?")
        tmp <- paste(tmp,
                     "<li>Does the nonparametric bootstrap",
                     "confidence interval change if we generate a new sample?")
        tmp <- paste(tmp, "</ul>")
        tmp <- paste(tmp, "<h3>Annotation</h3>")
        tmp <- paste(tmp, "<ul>")
        tmp <- paste(tmp,
                     "<li>The interval from the <i>Lower Limit</i>",
                     "to the <i>Upper Limit</i> is the percentile bootstrap",
                     input$level * 100, "%",
                     "confidence interval")
        tmp <- paste(tmp,
                     "<li>The interval from the <i>t-based Lower Limit</i>",
                     "to the <i>t-based Upper Limit</i> is the",
                     input$level * 100, "%",
                     "confidence interval based on the <i>t</i> distribution",
                     "and the approximate standard error of Pearson's <i>r</i>.")
        tmp <- paste(tmp,
                     "<li>The interval from the <i>z-based Lower Limit</i>",
                     "to the <i>z-based Upper Limit</i> is the",
                     input$level * 100, "%",
                     "confidence interval based on the normal distribution",
                     "and the standard error of Fisher's <i>r</i>.")
        tmp <- paste(tmp, "</ul>")
        tmp <- paste(tmp, "<h3>Optional / Advanced</h3>")
        tmp <- paste(tmp, "<ul>")
        tmp <- paste(tmp,
                     "<li>The QQ-plots below show how close",
                     "the bootstrap distributions are",
                     "to a normal distribution.")
        tmp <- paste(tmp,
                     "<li>The histograms below show the",
                     "(univariate) distributions of the",
                     "two variables, x and y.")
        tmp <- paste(tmp,
                     "<li>The contour plot",
                     "shows the bivariate distributions of the",
                     "two variables, x and y.")
        tmp <- paste(tmp, "</ul>")
        tmp
      }, sep = "\n")
    output$qq1 <- renderPlot({
        rs <- update_samples()
        qqnorm(rs,
               main = paste("QQ-Plot of the",
                            nboot,
                            "Bootstrap Sample rs"))
        qqline(rs)
      })
    output$qq2 <- renderPlot({
        rs <- update_samples()
        zs <- r2z(rs)
        zs <- zs[(zs > -Inf) & (zs < Inf)]
        qqnorm(zs,
               main = paste("QQ-Plot of the",
                            nboot,
                            "Bootstrap Sample zs"))
        qqline(zs)
      })
    output$histx <- renderPlot({
        dat <- update_raw_data()
        x <- dat[, "x"]
        x_dot <- seq(min(x),
                     max(x),
                     length = 100)
        x_dnorm <- dnorm(x_dot,
                         mean = mean(x),
                         sd = sd(x))
        hist(x,
             breaks = 50,
             prob = TRUE,
             col = "grey90",
             border = "grey80",
             xlab = "x",
             ylab = "Frequency",
             main = paste("Histogram of", n_raw, "Simulated x"),
             sub = "Normal Curve Overlayed")
        lines(x_dot,
              x_dnorm,
              col = "blue",
              lwd = 2)
      })
    output$histy <- renderPlot({
        dat <- update_raw_data()
        x <- dat[, "y"]
        x_dot <- seq(min(x),
                     max(x),
                     length = 100)
        x_dnorm <- dnorm(x_dot,
                         mean = mean(x),
                         sd = sd(x))
        hist(x,
             breaks = 50,
             prob = TRUE,
             col = "grey90",
             border = "grey80",
             xlab = "y",
             ylab = "Frequency",
             main = paste("Histogram of", n_raw, "Simulated y"),
             sub = "Normal Curve Overlayed")
        lines(x_dot,
              x_dnorm,
              col = "red",
              lwd = 2)

      })
    output$scatterxy <- renderPlot({
        dat <- update_raw_data()
        x <- dat[, "x"]
        y <- dat[, "y"]
        corxy <- cor(x,
                     y)
        corxy_str <- formatC(corxy, digits = 3, format = "f")
        xyz <- MASS::kde2d(x,
                           y,
                           n = 50)
        filled.contour(xyz,
                       nlevels = 10,
                       plot.axes = {axis(1);
                                    axis(2);
                                    contour(xyz,
                                            add = TRUE,
                                            lwd = 1)},
                       plot.title = title(main = paste("Contour Plot of", n_raw, "Simulated x and y"),
                                          xlab = "x",
                                          ylab = "y",
                                          sub = paste0("Correlation = ", corxy_str)))
      })
  }

shinyApp(ui = ui, server = server)
