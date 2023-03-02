# Illustrate how the hat value depends on the covariance
# Generate a scatterplot for user-specified Pearson's r
# To run in R: runGitHub("statdemos", "sfcheung", subdir = "hatvalue")


if (!requireNamespace("psych", quietly = TRUE)) {
    stop("Please install the package 'psych' first.")
  }

rho <- .8 # Population correlation
n <- 500  # Sample size

hat0 <- function(x) {
    x <- as.matrix(x)
    x <- cbind(1, x)
    diag(x %*% solve(t(x) %*% x) %*% t(x))
  }

xy_dist <- function(p0, p1) {
    sqrt((p0$x - p1$x)^2 + (p0$y - p1$y)^2)
  }

ui <- fluidPage(
  titlePanel("Hat Value of a Case When There Are Two Predictors"),
  fluidRow(
    column(12,
      wellPanel(
        p("This demonstration illustrates how the hat value ",
          "of a case depends on both its distance from ",
          "the centroid and the covariance between variables. ",
          "Click on the plot to position the red diamond point. ",
          "Its literal distance from the centroid",
          "(not taking into account the covariance)",
          "and its hat value ",
          "will be updated.")
        ),
      fluidRow(
        column(4,
          wellPanel(
              p("Move the slider to change the correlation based on all cases",
                "except for the red diamond point.",
                "Then place the red diamond point in ",
                "different locations and observe its hat value",
                "and distance from the centroid."),
              sliderInput("rho",
                          label = h5("Correlation (Pearson's r)"),
                          min = -1,
                          max = 1,
                          value = rho,
                          step = .1,
                          ticks = TRUE)
            )
          ),
        column(8,
            plotOutput("plot",
                      click = "point_clicked")
          )
        )
      )
    ),
  fluidRow(
    column(12,
      wellPanel(
          h5("Technical details:"),
          p("The two ellipses are positions one SD and two SDs from the centroid, respectively. Drawn by psych::ellipses()."),
          p("The number of cases is 500 plus the red diamond point (i.e., 501)."),
          p("The distance to the centroid does not take into account the covariance.")
        ),
      wellPanel(
        p("Version 1.0.0"),
        p("The latest version of the code can be found at ",
          a("statdemos at GitHub",
            href="https://github.com/sfcheung/statdemos/tree/master/hatvalue"),
          "."
          ),
        p("This app can be run directly by ",
          code("shiny::runUrl(\"https://github.com/sfcheung/statdemos/raw/master/apps/hatvalue.zip\")")
          )
        )
      )
    )
  )

# Server
server <- function(input, output) {
    user_point <- reactiveValues(x = NA, y = NA)
    observeEvent(input$point_clicked, {
        user_point$x <- input$point_clicked$x
        user_point$y <- input$point_clicked$y
      })
    output$plot <- renderPlot({
        r <- input$rho
        sigma <- matrix(c(1, r, r, 1), nrow = 2, ncol = 2)
        set.seed(655423)
        dat0 <- data.frame(MASS::mvrnorm(n = n,
                                        mu = c(x = 0, y = 0),
                                        Sigma = sigma,
                                        empirical = TRUE))
        if (!is.na(user_point$x)) {
            dat <- rbind(dat0, data.frame(x = user_point$x,
                                          y = user_point$y))
          } else {
            user_point$x <- mean(dat0$x)
            user_point$y <- mean(dat0$y)
            dat <- dat0
          }
        dat_hat <- hat0(dat)
        x <- dat$x
        y <- dat$y
        cexAll <- 1.5
        user_dist <- xy_dist(list(x = mean(dat$x), y = mean(dat$y)),
                             user_point)
        p_title <- paste0("Hat Value of the diamond point is ",
                          formatC(dat_hat[n + 1], digits = 4, format = "f"),
                          "\n",
                          "Distance from thee centroid is ",
                          formatC(user_dist, digits = 4, format = "f"))
        plot(x, y,
            cex = cexAll / 2,
            cex.axis = cexAll,
            cex.lab = cexAll,
            cex.sub = cexAll,
            cex.main = cexAll,
            pch = 21,
            col = "black",
            bg = "lightblue",
            main = p_title)
        psych::ellipses(dat$x,
                        dat$y,
                        add = TRUE,
                        lm = TRUE,
                        n = 2,
                        lwd = 2,
                        col = "blue")
        points(user_point$x,
               user_point$y,
               cex = cexAll * 2,
               pch = 23,
               bg = "red",
               col = "black")
        if (user_dist > 1e-5) {
            arrows(x0 = mean(dat$x),
                  y0 = mean(dat$y),
                  x1 = user_point$x,
                  y1 = user_point$y,
                  lwd = 4,
                  lty = "solid",
                  col = "red")
          }
        # lines(lowess(x, y),
        #       col = "red",
        #       lwd = 4)
      })
  }

shinyApp(ui = ui, server = server)
