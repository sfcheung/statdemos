# Demonstrate sampling distribution of Sample Pearson's r

# if (!(requireNamespace("lavaan", quietly = TRUE) &
#       requireNamespace("semPlot", quietly = TRUE))) {
#     stop(paste0("This app requires both the packages lavaan and semPlot.",
#                 "\n",
#                 "Please install them first."))
#   }

# Global variables

modellist <- c(
    "0 factor" = "mod0a",
    "1 factor" = "mod1a",
    "2 factors, 6+6 items" = "mod2a",
    "3 factors, 4+4+4 items" = "mod3a",
    "4 factors, 3+3+3+3 items" = "mod4a",
    "2 factors, 9+3 items" = "mod2b",
    "3 factors, 6+3+3 items" = "mod3b",
    "4 factors, 4+3+3+2 items" = "mod4b"
  )

cfap_0a <- "
 #NullModel
 x1 ~~ 0*x2 + 0*x3 + 0*x4 + 0*x5 + 0*x6 + 0*x7 + 0*x8 + 0*x9 + 0*x10 + 0*x11 + 0*x12
 x2 ~~ 0*x3 + 0*x4 + 0*x5 + 0*x6 + 0*x7 + 0*x8 + 0*x9 + 0*x10 + 0*x11 + 0*x12
 x3 ~~ 0*x4 + 0*x5 + 0*x6 + 0*x7 + 0*x8 + 0*x9 + 0*x10 + 0*x11 + 0*x12
 x4 ~~ 0*x5 + 0*x6 + 0*x7 + 0*x8 + 0*x9 + 0*x10 + 0*x11 + 0*x12
 x5 ~~ 0*x6 + 0*x7 + 0*x8 + 0*x9 + 0*x10 + 0*x11 + 0*x12
 x6 ~~ 0*x7 + 0*x8 + 0*x9 + 0*x10 + 0*x11 + 0*x12
 x7 ~~ 0*x8 + 0*x9 + 0*x10 + 0*x11 + 0*x12
 x8 ~~ 0*x9 + 0*x10 + 0*x11 + 0*x12
 x9 ~~ 0*x10 + 0*x11 + 0*x12
 x10 ~~0*x11 + 0*x12
 x11 ~~0*x12
 "
cfap_1a <- "
  f1 =~ (@)*x1 + (@)*x2 + (@)*x3 + (@)*x4 + (@)*x5 + (@)*x6 +
        (@)*x7 + (@)*x8 + (@)*x9 + (@)*x10 + (@)*x11 + (@)*x12
  "
cfap_2a <- "
  f1 =~ (@)*x1 + (@)*x2 + (@)*x3 + (@)*x4 + (@)*x5 + (@)*x6
  f2 =~ (@)*x7 + (@)*x8 + (@)*x9 + (@)*x10 + (@)*x11 + (@)*x12
  "
cfap_3a <- "
  f1 =~ (@)*x1 + (@)*x2 + (@)*x3 + (@)*x4
  f2 =~ (@)*x5 + (@)*x6 + (@)*x7 + (@)*x8
  f3 =~ (@)*x9 + (@)*x10 + (@)*x11 + (@)*x12
  "
cfap_4a <- "
  f1 =~ (@)*x1 + (@)*x2 + (@)*x3
  f2 =~ (@)*x4 + (@)*x5 + (@)*x6
  f3 =~ (@)*x7 + (@)*x8 + (@)*x9
  f4 =~ (@)*x10 + (@)*x11 + (@)*x12
  "
cfap_2b <- "
  f1 =~ (@)*x1 + (@)*x2 + (@)*x3 + (@)*x4 + (@)*x5 + (@)*x6 + (@)*x7 + (@)*x8 + (@)*x9
  f2 =~ (@)*x10 + (@)*x11 + (@)*x12
  "
cfap_3b <- "
  f1 =~ (@)*x1 + (@)*x2 + (@)*x3 + (@)*x4 + (@)*x5 + (@)*x6
  f2 =~ (@)*x7 + (@)*x8 + (@)*x9
  f3 =~ (@)*x10 + (@)*x11 + (@)*x12
  "
cfap_4b <- "
  f1 =~ (@)*x1 + (@)*x2 + (@)*x3 + (@)*x4
  f2 =~ (@)*x5 + (@)*x6 + (@)*x7
  f3 =~ (@)*x8 + (@)*x9 + (@)*x10
  f4 =~ (@)*x11 + (@)*x12
  "

modelnamelist <- c("mod0a", "mod1a", "mod2a", "mod3a", "mod4a",
                   "mod2b", "mod3b", "mod4b")
cfaplist <- c(cfap_0a, cfap_1a, cfap_2a, cfap_3a, cfap_4a,
              cfap_2b, cfap_3b, cfap_4b)

n <- 500
lambda0 <- .5

data1 <- lavaan::simulateData(model = gsub("@", lambda0, cfap_0a),
                              sample.nobs = n, seed = 897981,
                              standardized = TRUE)
data2 <- lavaan::simulateData(model = gsub("@", lambda0, cfap_1a),
                              sample.nobs = n, seed = 452533,
                              standardized = TRUE)
data3 <- lavaan::simulateData(model = gsub("@", lambda0, cfap_2a),
                              sample.nobs = n, seed = 654534,
                              standardized = TRUE)
data4 <- lavaan::simulateData(model = gsub("@", lambda0, cfap_3a),
                              sample.nobs = n, seed = 837634,
                              standardized = TRUE)

eigen_null <- eigen(cor(data1), only.values = TRUE)$values

datalist <- list(data1, data2, data3, data4)

gen_eigen_null <- function(n, nrep = 1000, seed = NULL) {
    if (!is.null(seed)) set.seed(seed)
    dat_null <- t(replicate(nrep,
                    eigen(cor(MASS::mvrnorm(n, mu = rep(0, 12), diag(12))), only.values = TRUE)$values))
    eigen_null_mean <- colMeans(dat_null)
    eigen_null_95 <- apply(dat_null, 2, quantile, probs = .95)
    list(mean = eigen_null_mean,
         p95 = eigen_null_95)
  }

eigen_null_all <- gen_eigen_null(n = n, seed = 51543)
eigen_null <- eigen_null_all$mean
eigen_null_p95 <- eigen_null_all$p95

update_datas <- function(modelselected,
                         cfaplist,
                         modelnamelist,
                         n,
                         lambdas = .5) {
    k <- length(modelselected)
    if (length(lambdas) == 1) lambdas <- rep(lambdas, k)
    datas <- list(rep(NA, k))
    for (i in 1:k) {
        cfapi <- cfaplist[which(modelnamelist == modelselected[i])]
        datas[[i]] <- lavaan::simulateData(model = gsub("@", lambdas[i], cfapi),
                                           sample.nobs = n,
                                           standardized = TRUE)
      }
    datas
  }

my_paths <- function(sem_fit, main) {
  if (length(grep("NullModel", sem_fit)) > 0) {
      semPlot::semPaths(lavaan::lavaanify(sem_fit),
                        whatLabels = "par",
                        sizeMan = 6,
                        sizeLat = 8,
                        nCharNodes = 0,
                        rotation = 2,
                        edge.label.cex = 2.5,
                        edge.color = "black",
                        edge.width = 1,
                        node.width = 1.5,
                        curve = 1.75,
                        exoVar = FALSE,
                        residuals = FALSE,
                        style = "lisrel",
                        fixedStyle = c("white", 0),
                        mar = c(3, 10, 5, 10))
    } else {
      semPlot::semPaths(lavaan::lavaanify(sem_fit),
                        whatLabels = "par",
                        sizeMan = 6,
                        sizeLat = 8,
                        nCharNodes = 0,
                        rotation = 2,
                        edge.label.cex = 2.5,
                        edge.color = "black",
                        edge.width = 1,
                        node.width = 1.5,
                        curve = 1.75,
                        exoVar = FALSE,
                        residuals = FALSE,
                        style = "lisrel",
                        fixedStyle = 1,
                        mar = c(3, 10, 5, 10))
    }
    title(main)
  }

my_scree <- function(data, main, ...) {
    my_fit <- princomp(data, cor = TRUE)
    plot(my_fit,
         type = "lines",
         main = main,
         npcs = 12,
         cex = 2,
         ...)
  }

gen_samples <- function(rho = 0,
                        n = 50,
                        nrep = 10000,
                        r_only = TRUE) {
    out <- replicate(nrep, gen_sample_i(rho = rho, n = n),
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
  # fluidRow(
  #       column(3,
  #         selectInput("efamodel1", "Model 1", modellist, selected = "mod0a")),
  #       column(3,
  #         selectInput("efamodel2", "Model 2", modellist, selected = "mod1a")),
  #       column(3,
  #         selectInput("efamodel3", "Model 3", modellist, selected = "mod2a")),
  #       column(3,
  #         selectInput("efamodel4", "Model 4", modellist, selected = "mod4a"))
  #     ),
  # fluidRow(
  #       column(3,
  #         sliderInput("efalambda1", "Standardized Loadings", .1, .8, .5)),
  #       column(3,
  #         sliderInput("efalambda2", "Standardized Loadings", .1, .8, .5)),
  #       column(3,
  #         sliderInput("efalambda3", "Standardized Loadings", .1, .8, .5)),
  #       column(3,
  #         sliderInput("efalambda4", "Standardized Loadings", .1, .8, .5))
  #     ),
#  fluidRow(
#    column(3,
#      sliderInput("efaphi1", "Factor Correlation(s)", .0, .9, .0)),
#    column(3,
#      sliderInput("efaphi2", "Factor Correlation(s)", .0, .9, .0)),
#    column(3,
#      sliderInput("efaphi3", "Factor Correlation(s)", .0, .9, .0)),
#    column(3,
#      sliderInput("efaphi4", "Factor Correlation(s)", .0, .9, .0))
#      ),
  fluidRow(
      column(4, align = "center",
        sliderInput("n", label = "Sample Size (n)", min = 10, max = 500, value = 50, step = 10)),
      column(4, align = "center",
        sliderInput("rho", label = "Population Correlation", min = -.99, max = .99, value = 0, step = .01)),
      column(4, align = "center",
        sliderInput("alpha", label = "Sig. Level", min = .001, max = .10, value = .05, step = .001)),
    ),
  fluidRow(
    column(12, submitButton("Generate 10000 samples"))
    ),
  # fluidRow(column(12, plotOutput("plot"))),
  # fluidRow(column(12, plotOutput("plot2"))),
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
                                         nrep = 10000,
                                         r_only = TRUE))
  updatedatas_i <- reactive(update_datas(c(input$efamodel1, input$efamodel2,
                                           input$efamodel3, input$efamodel4),
                                           cfaplist = cfaplist,
                                           modelnamelist = modelnamelist,
                                           n = input$n,
                                           lambdas = c(input$efalambda1,
                                                       input$efalambda2,
                                                       input$efalambda3,
                                                       input$efalambda4)))
  output$hist1 <- renderPlot({
      rs <<- update_samples()
      hist(rs,
           breaks = 50,
           xlim = c(-1, 1),
           xlab = "Sample r",
           ylab = "Frequency",
           main = as.character(max(rs)))
      cut_lo <<- quantile(rs, input$alpha / 2)
      cut_hi <<- quantile(rs, 1 - input$alpha / 2)
      abline(v = cut_lo)
      abline(v = cut_hi)
    })
  output$Lower <- renderPrint({
      rs <<- update_samples()
      cut_lo <<- quantile(rs, input$alpha / 2)
      cut_hi <<- quantile(rs, 1 - input$alpha / 2)
      cat(round(cut_lo, 3), ", ",
          round(cut_hi, 3))
    })
  output$hist2 <- renderPlot({
      rs <<- update_samples()
      qqnorm(rs)
      qqline(rs)
      # text(0, 0, as.character(max(rs)))
    })
  output$plot <- renderPlot({
      datas <<- updatedatas_i()
      par(mfrow = c(1, 4))
      my_paths(gsub("@", input$efalambda1,
                    cfaplist[which(modelnamelist == input$efamodel1)]),
              "Model 1")
      my_paths(gsub("@", input$efalambda2,
                    cfaplist[which(modelnamelist == input$efamodel2)]),
              "Model 2")
      my_paths(gsub("@", input$efalambda3,
                    cfaplist[which(modelnamelist == input$efamodel3)]),
              "Model 3")
      my_paths(gsub("@", input$efalambda4,
                    cfaplist[which(modelnamelist == input$efamodel4)]),
              "Model 4")
    })
  output$plot2 <- renderPlot({
      datas <<- updatedatas_i()
      eigen_null_full <- gen_eigen_null(n = input$n, seed = 51543)
      eigenmax <- ceiling(
          max(sapply(datas, function(x) eigen(cor(x), only.values = TRUE)$values))
          )
      #eigenmax <- 5
      par(mfrow = c(1, 4))
      my_scree(datas[[1]], "Model 1", ylim = c(0, eigenmax))
      abline(h = 1, col = "red")
      points(1:12, eigen_null_full$mean, type = "b", col = "blue", cex = 2, pch = 4)
      points(1:12, eigen_null_full$p95, type = "b", col = "#015501", cex = 2, pch = 2)
      my_scree(datas[[2]], "Model 2", ylim = c(0, eigenmax))
      abline(h = 1, col = "red")
      points(1:12, eigen_null_full$mean, type = "b", col = "blue", cex = 2, pch = 4)
      points(1:12, eigen_null_full$p95, type = "b", col = "#015501", cex = 2, pch = 2)
      my_scree(datas[[3]], "Model 3", ylim = c(0, eigenmax))
      abline(h = 1, col = "red")
      points(1:12, eigen_null_full$mean, type = "b", col = "blue", cex = 2, pch = 4)
      points(1:12, eigen_null_full$p95, type = "b", col = "#015501", cex = 2, pch = 2)
      my_scree(datas[[4]], "Model 4", ylim = c(0, eigenmax))
      abline(h = 1, col = "red")
      points(1:12, eigen_null_full$mean, type = "b", col = "blue", cex = 2, pch = 4)
      points(1:12, eigen_null_full$p95, type = "b", col = "#015501", cex = 2, pch = 2)
    })
  }

shinyApp(ui = ui, server = server)
