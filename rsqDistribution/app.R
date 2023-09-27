# Demonstrate sampling distribution of R-squares

# Global Variables

nrep <- 20000
n_raw <- 10000
sd_y_sample <- 2 / .8

gen_samples <- function(b0_pop = 0,
                        b1_pop = 0,
                        sd_x = 1,
                        sd_e = 1,
                        n = 50,
                        nrep = 10000,
                        x_dist = "normal",
                        x_df = 1,
                        x_alpha = .5,
                        x_beta = .5,
                        e_dist = "normal",
                        e_df = 1,
                        e_alpha = .5,
                        e_beta = .5) {
    out <- replicate(nrep, gen_sample_i(b0_pop = b0_pop,
                                        b1_pop = b1_pop,
                                        sd_x = sd_x,
                                        sd_e = sd_e,
                                        n = n,
                                        x_dist = x_dist,
                                        x_df = x_df,
                                        x_alpha = x_alpha,
                                        x_beta = x_beta,
                                        e_dist = e_dist,
                                        e_df = e_df,
                                        e_alpha = e_alpha,
                                        e_beta = e_beta),
                     simplify = FALSE)
    lm_out <- sapply(out, function(dat) {
                  out <- lm.fit(cbind(1, dat[, "x"]), dat[, "y"])
                  b <- out$coefficients
                  names(b) <- c("b0", "b1")
                  beta <- b["b1"] * sd(dat[, "x"]) / sd(dat[, "y"])
                  names(beta) <- "beta1"
                  rsq <- cor(out$fitted.values,
                             dat[, "y"])^2
                  n <- nrow(dat)
                  rsq_adj <- 1 - (1 - rsq) * (n - 1) / (n - 1 - 1)
                  c(b, beta, rsq = rsq, rsq_adj = rsq_adj)
                }, simplify = TRUE)
    t(lm_out)
  }

# tmp1 <- gen_samples(b0 = 5, b1 = 2, n = 100, nrep = 1000)
# 2 * 1 / sqrt((2^2 + 1))
# colMeans(tmp1)

gen_sample_i <- function(b0_pop = 0,
                         b1_pop = 0,
                         sd_x = 1,
                         sd_e = 1,
                         n = 50,
                         x_dist = c("normal",
                                    "chisq",
                                    "uniform",
                                    "beta"),
                         x_df = 1,
                         x_alpha = .5,
                         x_beta = .5,
                         e_dist = c("normal",
                                    "chisq",
                                    "uniform",
                                    "beta"),
                         e_df = 1,
                         e_alpha = .5,
                         e_beta = .5) {
    x_dist <- match.arg(x_dist)
    e_dist <- match.arg(e_dist)
    sd_beta_x <- sqrt(x_alpha * x_beta) / ((x_alpha + x_beta) * sqrt(x_alpha + x_beta + 1))
    sd_beta_e <- sqrt(e_alpha * e_beta) / ((e_alpha + e_beta) * sqrt(e_alpha + e_beta + 1))
    mean_beta_x <- (x_alpha / (x_alpha + x_beta))
    mean_beta_e <- (e_alpha / (e_alpha + e_beta))
    x <- switch(x_dist,
                normal = rnorm(n),
                chisq = (rchisq(n, df = x_df) - x_df) / sqrt(2 * x_df),
                uniform = runif(n, -.5, .5) / sqrt(12),
                beta = (rbeta(n, x_alpha, x_beta) - mean_beta_x) / sd_beta_x)
    e <- switch(e_dist,
                normal = rnorm(n),
                chisq = (rchisq(n, df = e_df) - e_df) / sqrt(2 * e_df),
                uniform = runif(n, -.5, .5) / sqrt(12),
                beta = (rbeta(n, e_alpha, e_beta) - mean_beta_e) / sd_beta_e)
    y <- b0_pop + b1_pop * x * sd_x + e * sd_e
    cbind(x, y)
  }

# UI
ui <- fluidPage(
    titlePanel("Sampling Distribution of R-square"),
    fluidRow(
      column(12,
        wellPanel(
            p("This page illustrates how to find",
              "the critical value (cutoff value)",
              "for testing a sample R-square",
              "using an empirical distribution",
              "of", nrep, "simulated sample R-squares computed from",
              nrep, "samples drawn from a",
              "population with the required population B0 and B1."),
            p("How to Use it:"),
            p("1. Set the sample size and sample R-square."),
            p("2. Click 'Update the Plots'."),
            p("3. Examine the plots."),
            p("(Changing the population B0 and B1, and",
              "the distributions of predictor and errors [not residuals] are optional.)"),
            p("(This app will be discussed in the class to demonstrate",
              "the ideas of sampling distribution, p-value, and critical value.)")
        ))),
    fluidRow(
        column(4, align = "center",
          sliderInput("n",
                      label = "Sample Size (n)",
                      min = 10,
                      max = 500,
                      value = 10,
                      step = 10)),
        column(4, align = "center",
               sliderInput("rsq",
                           label = "Sample R-square",
                           min = 0,
                           max = .99,
                           value = .25,
                           step = .01)),
        column(4, align = "center",
               sliderInput("alpha",
                           label = "Level of Significance",
                           min = .001,
                           max = .10,
                           value = .05,
                           round = -3,
                           step = .001))
      ),
    fluidRow(
        column(4, align = "center",
            sliderInput("b0_pop", label = "Population B0 (Intercept)",
                        min = -5,
                        max =  5,
                        value = 0,
                        step = .01)
          ),
        column(4, align = "center",
            sliderInput("b1_pop", label = "Population B1 (Slope)",
                        min = -1.5,
                        max =  1.5,
                        value = 0,
                        step = .01)
          ),
        column(4, align = "left",
            checkboxInput("show_f",
                          label = "Show F critical value",
                          TRUE),
            checkboxInput("show_cutoff",
                          label = "Show cutoff value",
                          TRUE),
            checkboxInput("show_p",
                          label = "Show p value and area",
                          TRUE),
            checkboxInput("fix_x",
                          label = "Fix the ranges of horizontal axes",
                          FALSE)
        )
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
        column(12,
          wellPanel(
              htmlOutput("note")
          )
          )
      ),
    fluidRow(
        column(12, align = "center",
            h3("Optional:"),
            p("Click 'Update the Plots' after changing the following")
          )
      ),
    fluidRow(
        column(4, align = "center",
            selectInput("x_dist", "Distribution of x (predictor):",
                        c("Normal" = "normal",
                          "Chi-square" = "chisq",
                          "Uniform" = "uniform",
                          "Beta" = "beta"))
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
          )),
    fluidRow(
        column(4, align = "center",
            selectInput("e_dist", "Distribution of error:",
                        c("Normal" = "normal",
                          "Chi-square" = "chisq",
                          "Uniform" = "uniform",
                          "Beta" = "beta"))
          ),
        column(4, align = "center",
            sliderInput("e_df",
                        label = "df (for Chi-square Distribution)",
                        min = 1,
                        max = 100,
                        value = 1,
                        step = 1)
          ),
        column(4, align = "center",
            sliderInput("e_alpha",
                        label = "alpha (for Beta Distribution)",
                        min = .10,
                        max = 5,
                        value = .50,
                        step = .01),
            sliderInput("e_beta",
                        label = "beta (for Beta Distribution)",
                        min = .10,
                        max = 5,
                        value = .50,
                        step = .01)
          )
      ),
    fluidRow(
      column(12, submitButton(paste("Update the Plots")),
             align = "center")
      ),
    fluidRow(
        column(4, plotOutput("histx")),
        column(4, plotOutput("histy")),
        column(4, plotOutput("scatterxy"))
      ),
    fluidRow(
      column(12,
        wellPanel(
          p("Version 0.1.0"),
          p("The latest version of the code can be found at ",
            a("statdemos at GitHub",
              href = "https://github.com/sfcheung/statdemos/tree/master/rsqDistribution"),
              "."
            ),
          p("This app can be run directly by ",
            code("shiny::runUrl(\"https://github.com/sfcheung/statdemos/raw/master/apps/rsqDistribution.zip\")")
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
    update_samples <- reactive(gen_samples(b0_pop = input$b0_pop,
                                           b1_pop = input$b1_pop,
                                           n = input$n,
                                           nrep = nrep,
                                           x_dist = input$x_dist,
                                           x_df = input$x_df,
                                           x_alpha = input$x_alpha,
                                           x_beta = input$x_beta,
                                           e_dist = input$e_dist,
                                           e_df = input$e_df,
                                           e_alpha = input$e_alpha,
                                           e_beta = input$e_beta))
    update_raw_data <- reactive(gen_sample_i(b0_pop = input$b0_pop,
                                             b1_pop = input$b1_pop,
                                             n = n_raw,
                                             x_dist = input$x_dist,
                                             x_df = input$x_df,
                                             x_alpha = input$x_alpha,
                                             x_beta = input$x_beta,
                                             e_dist = input$e_dist,
                                             e_df = input$e_df,
                                             e_alpha = input$e_alpha,
                                             e_beta = input$e_beta))
    output$hist1 <- renderPlot({
        lm_out <- update_samples()
        rsq <- input$rsq
        n <- input$n
        sd_x_pop <- 1
        b1_pop <- input$b1_pop
        sd_y_pop <- sqrt((b1_pop^2) + 1)
        rsq_pop <- b1_pop * sd_x_pop / sd_y_pop
        rsq_f <- (rsq / (1 - rsq)) * ((n - 2) / (1))
        f_p <- pf(rsq_f,
                  df1 = 1,
                  df2 = n - 2,
                  lower.tail = FALSE)
        rsqs <- lm_out[, "rsq", drop = TRUE]
        rsqs_mean <- mean(rsqs)
        rsq_p <- 1 - ecdf(rsqs)(input$rsq)
        # TODO: Try to reduce code duplication
        if (input$fix_x) {
            hist(rsqs,
                breaks = 50,
                col = "grey90",
                border = "grey80",
                xlim = c(0, 1),
                xlab = "Simulated Sample R-Square",
                ylab = "Frequency",
                main = paste("Histogram of", nrep, "Simulated Sample R-squares"),
                sub = paste("Mean of", nrep, "Simulated Sample R-squares:",
                            formatC(rsqs_mean, digits = 3, format = "f")))
          } else {
            hist(rsqs,
                breaks = 50,
                col = "grey90",
                border = "grey80",
                xlim = c(0, max(rsq * 1.1, max(rsqs))),
                xlab = "Simulated Sample R-square",
                ylab = "Frequency",
                main = paste("Histogram of", nrep, "Simulated Sample R-squares"),
                sub = paste("Mean of", nrep, "Simulated Sample R-squares:",
                            formatC(rsqs_mean, digits = 3, format = "f")))
          }
        cut_hi <- quantile(rsqs,
                           1 - input$alpha)
        cut_f <- qf(1 - input$alpha,
                    df1 = 1,
                    df2 = n - 2)
        cut_f_hi <- 1 / (1 + 1 / (cut_f * (1) / (n - 2)))
        tmp1 <- par("usr")
        y_length <- tmp1[4] - tmp1[3]
        tmp2 <- y_length / 2
        if (input$show_cutoff) {
            segments(x0 = cut_hi,
                     y0 = tmp1[3],
                     y1 = tmp2 * .65,
                     col = "red")
            arrows(x0 = cut_hi,
                   y0 = tmp2 * .25,
                   x1 = cut_hi + (tmp1[2] - cut_hi) * .5,
                   col = "red",
                 length = .10)
          }
        if (input$show_f) {
            segments(x0 = cut_f_hi,
                     y0 = tmp2 * 1.35,
                     y1 = tmp1[4],
                     col = "blue",
                     lwd = 2)
            arrows(x0 = cut_f_hi,
                   y0 = tmp2 * 1.75,
                   x1 = cut_f_hi + (tmp1[2] - cut_f_hi) * .5,
                   col = "blue",
                   length = .10)
          }
        if ((rsq != rsq_pop) && input$show_p) {
            arrows(x0 = rsq,
                   y0 = tmp2 * .75,
                   x1 = rsq + (tmp1[2] - rsq) * .5,
                   col = "blue",
                   length = .10)
          }
        abline(v = rsq,
               col = "blue",
               lwd = 2,
               lty = "dotted")
        abline(v = rsq_pop,
               col = "black",
               lwd = 2,
               lty = "dashed")
        abline(v = rsqs_mean,
               col = "red",
               lwd = 2,
               lty = "dotted")
        box_color <- rgb(0, 0, 255,
                         maxColorValue = 255,
                         alpha = 50,
                         names = "blue_t")
        box_color2 <- rgb(255, 0, 0,
                          maxColorValue = 255,
                          alpha = 50,
                          names = "red_t")
        rsq_str <- formatC(rsq, digits = 2, format = "f")
        cut_hi_str <- formatC(cut_hi, digits = 3, format = "f")
        cut_f_hi_str <- formatC(cut_f_hi, digits = 3, format = "f")
        cut_f_hi_str <- paste0("F Critical:\n", cut_f_hi_str)
        rsq_p_str <- formatC(rsq_p, digits = 3, format = "f")
        rsq_p05 <- rsq_p
        rsq_p05_str <- formatC(rsq_p05, digits = 3, format = "f")
        rsqs_mean_str <- formatC(rsqs_mean, digits = 3, format = "f")
        sig <- rsq > cut_hi
        legend(x = rsq,
               y = .50 * y_length,
               legend = paste0("R-sq: ",
                               rsq_str),
               text.col = "black",
               box.col = ifelse(sig, box_color2, box_color),
               bg = ifelse(sig, box_color2, box_color),
               xjust = .5,
               yjust = .5,
               adj = c(0.15, 0.25))
        text(x = rsqs_mean,
             y = tmp2 * 1.50,
             col = "red",
             labels = paste0("Mean = ",
                              rsqs_mean_str))
        if (input$show_cutoff) {
            text(x = cut_hi,
                 y = .25 * y_length,
                 labels = paste0("Upper\nCutoff\n",
                                 cut_hi_str))
          }
        if (input$show_f) {
            text(x = cut_f_hi,
                 y = .75 * y_length,
                 labels = cut_f_hi_str)
          }
        if ((rsq != rsq_pop) && input$show_p) {
          text(x = rsq,
               y = tmp2 * 1.20,
               col = "blue",
               labels = paste0("p = ",
                               rsq_p_str))
          text(x = rsq,
               y = tmp2 * .75,
               col = "blue",
               labels = paste0("Area to right:\n",
                               rsq_p05_str),
               adj = ifelse(rsq > 0,
                            c(0, 0),
                            c(1, 0)))
          }
    })
    output$hist2 <- renderPlot({
        lm_out <- update_samples()
        rsq <- input$rsq
        n <- input$n
        rsq <- 1 - (1 - rsq) * (n - 1) / (n - 1 - 1)
        sd_x_pop <- 1
        b1_pop <- input$b1_pop
        sd_y_pop <- sqrt((b1_pop^2) + 1)
        rsq_pop <- b1_pop * sd_x_pop / sd_y_pop
        rsq_f <- (rsq / (1 - rsq)) * ((n - 2) / (1))
        f_p <- pf(rsq_f,
                  df1 = 1,
                  df2 = n - 2,
                  lower.tail = FALSE)
        rsqs <- lm_out[, "rsq_adj", drop = TRUE]
        rsqs_mean <- mean(rsqs)
        rsq_p <- 1 - ecdf(rsqs)(input$rsq)
        # TODO: Try to reduce code duplication
        if (input$fix_x) {
            hist(rsqs,
                breaks = 50,
                col = "lightgreen",
                border = "grey80",
                xlim = c(-.2, 1),
                xlab = "Simulated Sample Adjusted R-Square",
                ylab = "Frequency",
                main = paste("Histogram of", nrep, "Simulated Sample Adjusted R-squares"),
                sub = paste("Mean of", nrep, "Simulated Sample Adjusted R-squares:",
                            formatC(rsqs_mean, digits = 3, format = "f")))
          } else {
            hist(rsqs,
                breaks = 50,
                col = "lightgreen",
                border = "grey80",
                xlim = c(max(-.2, min(rsqs)), max(rsq * 1.1, max(rsqs))),
                xlab = "Simulated Sample Adjusted R-square",
                ylab = "Frequency",
                main = paste("Histogram of", nrep, "Simulated Sample Adjusted R-squares"),
                sub = paste("Mean of", nrep, "Simulated Sample Adjusted R-squares:",
                            formatC(rsqs_mean, digits = 3, format = "f")))
          }
        cut_hi <- quantile(rsqs,
                           1 - input$alpha)
        cut_f <- qf(1 - input$alpha,
                    df1 = 1,
                    df2 = n - 2)
        cut_f_hi <- 1 / (1 + 1 / (cut_f * (1) / (n - 2)))
        tmp1 <- par("usr")
        y_length <- tmp1[4] - tmp1[3]
        tmp2 <- y_length / 2
        # if (input$show_cutoff) {
        if (FALSE) {
            segments(x0 = cut_hi,
                     y0 = tmp1[3],
                     y1 = tmp2 * .65,
                     col = "red")
            arrows(x0 = cut_hi,
                   y0 = tmp2 * .25,
                   x1 = cut_hi + (tmp1[2] - cut_hi) * .5,
                   col = "red",
                 length = .10)
          }
        # if (input$show_f) {
        if (FALSE) {
            segments(x0 = cut_f_hi,
                     y0 = tmp2 * 1.35,
                     y1 = tmp1[4],
                     col = "blue",
                     lwd = 2)
            arrows(x0 = cut_f_hi,
                   y0 = tmp2 * 1.75,
                   x1 = cut_f_hi + (tmp1[2] - cut_f_hi) * .5,
                   col = "blue",
                   length = .10)
          }
        # if ((rsq != rsq_pop) && input$show_p) {
        if (FALSE) {
            arrows(x0 = rsq,
                   y0 = tmp2 * .75,
                   x1 = rsq + (tmp1[2] - rsq) * .5,
                   col = "blue",
                   length = .10)
          }
        abline(v = rsq,
               col = "blue",
               lwd = 2,
               lty = "dotted")
        abline(v = rsq_pop,
               col = "black",
               lwd = 2,
               lty = "dashed")
        abline(v = rsqs_mean,
               col = "red",
               lwd = 2,
               lty = "dotted")
        box_color <- rgb(0, 0, 255,
                         maxColorValue = 255,
                         alpha = 50,
                         names = "blue_t")
        box_color2 <- rgb(255, 0, 0,
                          maxColorValue = 255,
                          alpha = 50,
                          names = "red_t")
        rsq_str <- formatC(rsq, digits = 2, format = "f")
        cut_hi_str <- formatC(cut_hi, digits = 3, format = "f")
        cut_f_hi_str <- formatC(cut_f_hi, digits = 3, format = "f")
        cut_f_hi_str <- paste0("F Critical:\n", cut_f_hi_str)
        rsq_p_str <- formatC(rsq_p, digits = 3, format = "f")
        rsq_p05 <- rsq_p
        rsq_p05_str <- formatC(rsq_p05, digits = 3, format = "f")
        rsqs_mean_str <- formatC(rsqs_mean, digits = 3, format = "f")
        sig <- rsq > cut_hi
        legend(x = rsq,
               y = .50 * y_length,
               legend = paste0("Adj. R-sq: ",
                               rsq_str),
               text.col = "black",
               box.col = ifelse(sig, box_color2, box_color),
               bg = ifelse(sig, box_color2, box_color),
               xjust = .5,
               yjust = .5,
               adj = c(0.10, 0.25))
        text(x = rsqs_mean,
             y = tmp2 * 1.50,
             col = "red",
             labels = paste0("Mean = ",
                              rsqs_mean_str))
        # if (input$show_cutoff) {
        if (FALSE) {
            text(x = cut_hi,
                 y = .25 * y_length,
                 labels = paste0("Upper\nCutoff\n",
                                 cut_hi_str))
          }
        # if (input$show_f) {
        if (FALSE) {
            text(x = cut_f_hi,
                 y = .75 * y_length,
                 labels = cut_f_hi_str)
          }
        # if ((rsq != rsq_pop) && input$show_p) {
        if (FALSE) {
          text(x = rsq,
               y = tmp2 * 1.20,
               col = "blue",
               labels = paste0("p = ",
                               rsq_p_str))
          text(x = rsq,
               y = tmp2 * .75,
               col = "blue",
               labels = paste0("Area to right:\n",
                               rsq_p05_str),
               adj = ifelse(rsq > 0,
                            c(0, 0),
                            c(1, 0)))
        }
    })
    output$note <- renderText({
        b1_pop <- input$b1_pop
        sd_y_pop <- sqrt(b1_pop^2 + 1)
        r_pop <- b1_pop * 1 / sd_y_pop
        rsq_pop <- r_pop ^2
        rsq_pop_str <- formatC(rsq_pop, digits = 3, format = "f")
        b1_pop_str <- formatC(b1_pop, digits = 3, format = "f")
        tmp <- ""
        tmp <- paste(tmp, "<h3>Technical Notes</h3>")
        tmp <- paste(tmp, "<ul>")
        tmp <- paste(tmp,
                     "<li>Population SDs of <i>x</i> and error (<i>e</i>) are fixed to 1.")
        tmp <- paste(tmp,
                     "<li><i>y</i> is computed by b0 + b1 * <i>x</i> + <i>e</i>.")
        tmp <- paste(tmp,
                     "<li>With a population slope of", b1_pop_str,
                     ", the population R-square is", rsq_pop_str, ".")
        tmp <- paste(tmp, "</ul>")
        tmp <- paste(tmp, "<h3>Things to Check</h3>")
        tmp <- paste(tmp, "<ul>")
        tmp <- paste(tmp,
                     "<li>Are the cutoff value and the",
                     "critical value based on an <i>F</i> distribution close to each other?")
        tmp <- paste(tmp,
                     "<li>If the cutoff value and the",
                     "critical value baesd on an <i>F</i> distribution  are very different, what if we use the",
                     "critical value based on an <i>F</i> distribution  to test the null hypothesis?")
        tmp <- paste(tmp,
                     "<li>Is the mean of the",
                     nrep, "simulated sample R-squares close to",
                     "the population R-square,",
                     rsq_pop_str, "?")
        tmp <- paste(tmp,
                     "<li>Is the mean of the",
                     nrep, "simulated sample <i>adjusted</i> R-squares close to",
                     "the population R-square,",
                     rsq_pop_str, "?")
        tmp <- paste(tmp, "</ul>")
        tmp <- paste(tmp, "<h3>Annotation</h3>")
        tmp <- paste(tmp, "<ul>")
        tmp <- paste(tmp,
                     "<li>Note that we only test whether",
                     "a sample <i>R-square</i> is 'too large' if the population",
                     "<i>R-square</i> is zero. Therefore,",
                     "there is only one cutoff value.")
        tmp <- paste(tmp,
                     "<li>Upper Cutoff: Value with",
                     100 * input$alpha,
                     "% sample <i>R-square</i>s to the right.")
        tmp <- paste(tmp,
                     "<li>F Critical: Critical valus",
                     "based on an <i>F</i> distribution,",
                     "assuming the population <i>R-square</i> is zero.")
        tmp <- paste(tmp,
                     "<li>Area to the right is",
                     "based on the distribution of simulated",
                     "sample <i>R-square</i>s.")
        tmp <- paste(tmp,
                     "<li>The area is the <i>p</i>-value",
                     "based on the distribution of simulated",
                     "sample <i>R-square</i>s.")
        tmp <- paste(tmp,
                     "<li><b>IMPORTANT</b>: The area and the <i>p</i>-value",
                     "are based on the histogram, NOT based on",
                     "an <i>F</i> distribution,",
                     "to illustrate how to define them if we",
                     "do not have a theoretical distribution. Therefore,",
                     "the <i>p</i>-value are <i>NOT</i> the usual",
                     "<i>p</i>-value when the population R-square is <i>NOT</i>",
                     "equal to zero (i.e,, the null hypothesis is not the usual one).")
        tmp <- paste(tmp, "</ul>")
        tmp <- paste(tmp, "<h3>Optional / Advanced</h3>")
        tmp <- paste(tmp, "<ul>")
        tmp <- paste(tmp,
                     "<li>The histograms below show the",
                     "(univariate) distributions of the",
                     "two variables, x and y.")
        tmp <- paste(tmp,
                     "<li>The contour plot below show the",
                     "bivariate distributions of the",
                     "two variables, x and y.")
        tmp <- paste(tmp,
                     "<li>In <i>this special</i> case,",
                     "the <i>F</i> distribution still yields",
                     "valid cutoff <i>even if</i> the error is",
                     "nonnormal, <i>if</i> the population",
                     "slope is zero. This may not be the case",
                     "when the number of predictors is more than one.")
        tmp <- paste(tmp, "</ul>")
        tmp
      }, sep = "\n")
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
