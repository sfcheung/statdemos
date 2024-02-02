# Demonstrate the computation of the p-value

# UI
ui <- fluidPage(
    titlePanel("Find the p-value of a t statistic"),
    fluidRow(
      column(12,
        wellPanel(
            htmlOutput("note0")
        ))),
    fluidRow(
        column(6, align = "center",
          sliderInput("df",
                      label = "Degrees of Freedom (df)",
                      min = 5,
                      max = 500,
                      value = 10,
                      step = 1)),
        column(6, align = "center",
               sliderInput("t",
                           label = "t Statistic",
                           min = -5,
                           max = 5,
                           value = 1.96,
                           round = -2,
                           step = .01))
      ),
    fluidRow(
      column(12, submitButton(paste("Update the Graph")),
             align = "center")
      ),
    fluidRow(
        column(12, plotOutput("tdist",
                              width = "100%",
                              height = "600px")),
      ),
    fluidRow(
        column(12,
          wellPanel(htmlOutput("note"))
          )
      ),
    fluidRow(
      column(12,
        wellPanel(
          p("Version 0.1.0"),
          p("The latest version of the code can be found at ",
            a("statdemos at GitHub",
              href = "https://github.com/sfcheung/statdemos/tree/master/tpvalue"),
              "."
            ),
          p("This app can be run directly by ",
            code("shiny::runUrl(\"https://github.com/sfcheung/statdemos/raw/master/apps/tpvalue.zip\")")
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
    output$tdist <- renderPlot({
        t1 <- input$t
        df1 <- input$df
        xmin <- qt(.0001, df1)
        xmax <- qt(1 - .0001, df1)
        xmin <- -5
        xmax <- 5
        x <- seq(from = xmin, to = xmax, by = .005)
        y <- dt(x, df = df1)
        plot(x, y,
             type = "l",
             yaxt = "n",
             xlab = "t",
             ylab = "",
             cex.axis = 2,
             cex.lab = 3)
        xrange <- par("usr")[1:2]
        yrange <- par("usr")[3:4]
        y_span <- yrange[2] - yrange[1]
        abline(h = 0)
        abline(v = t1,
               lwd = 2,
               col = "blue")
        if (t1 == 0) {
            text(mean(xrange),
                 mean(yrange),
                 labels = "p-value = 1\nwhen t = 0",
                 cex = 4)
          } else {
            abline(v = -t1,
                  lwd = 2,
                  lty = "dotted",
                  col = "grey")
            if (t1 > 0) {
                t1range <- c(t1, xrange[2])
              } else if (t1 < 0) {
                t1range <- c(xrange[1], t1)
              } else {
                # Tentative
                t1range <- c(t1, xrange[2])
              }
            t1x <- seq(from = t1range[1], to = t1range[2], by = .005)
            t1y <- dt(t1x, df = df1)
            t1len <- length(t1x)
            t1regionx <- c(t1x, rev(t1x))
            t1regiony <- c(rep(0, t1len), rev(t1y))
            polygon(t1regionx, t1regiony, col = "lightblue")
            polygon(-t1regionx, t1regiony, col = "lightgrey")
            txt <- paste0("t = ",
                            formatC(t1, digits = 2, format = "f"))
            text(t1, yrange[1] + .6 * y_span,
                 labels = txt, cex = 3)
            t1area <- pt(abs(t1), df1, lower.tail = FALSE)
            txt <- paste0("Area to the ",
                          ifelse(t1 > 0, "right", "left"),
                          "\nis ",
                          formatC(t1area, digits = 4, format = "f"))
            text(t1, yrange[1] + .5 * y_span,
                 labels = txt, cex = 2)
            txt <- paste0("One-tailed p-value = ",
                          formatC(t1area, digits = 4, format = "f"))
            text(t1, yrange[1] + .38 * y_span,
                 labels = txt, cex = 2)
            txt <- paste0("Two-tailed p-value = \n",
                          formatC(t1area, digits = 4, format = "f"),
                          "*2 = ",
                          formatC(t1area * 2, digits = 4, format = "f"))
            text(t1, yrange[1] + .28 * y_span,
                 labels = txt, cex = 2)
            x1t <- ifelse(t1 > 0,
                          xrange[2],
                          xrange[1])
            arrows(x0 = t1, y0 = yrange[1] + .8 * y_span,
                   x1 = x1t, y1 = yrange[1] + .8 * y_span,
                   length = 1)
          }
      })
    output$note0 <- renderText({
        tmp <- paste0(
            "<p>This page illustrates how the <i>p</i>-value of a <i>t</i> statistic is found using the <i>t</i> distribution. ",
            "<p>Given the <i>df</i> and a <i>t</i> statistic, the one-tailed <i>p</i>-value is the area beyond the <i>t</i> and <b>away from zero</b>. ",
            "That is, if the <i>t</i> is negative, it is the area to the <b>left</b> of the <i>t</i>. ",
            "If the <i>t</i> is positive, it is the area to the <b>right</b> of the <i>t</i>. ",
            "<p>The two-tailed <i>p</i>-value is the total area of <b>two regions</b>, away from zero beyond <i>t</i> and -<i>t</i> ",
            "(e.g., to the right of 2 and to the left of -2). Because the <i>t</i> distribution is symmetric, this is simply ",
            "one of the areas multiplied by two.")
        tmp
      }, sep = "\n")
    output$note <- renderText({
        tmp <- ""
        tmp <- paste(tmp, "<h3>Note on using <b>pt()</b></h3>")
        tmp <- paste(tmp, "<p><b>pt()</b> gives the area to the left (lower.tail) by default.",
                     "This is what we need if the <i>t</i> is negative because this gives the area <i>away from zero</i>.",
                     "However, if the <i>t</i> is positive,",
                     "to get the area <i>away from zero</i>, we need the area to the <i>right</i>.",
                     "To get the area to the right when <i>t</i> is positive, we need to set",
                     "<b>lower.tail = FALSE</b>.",
                     "<p>To always get the area <i>away from 0</i>, whether <i>t</i> is positive or negativve,",
                     "we can use <b>abs()</b> to make the <i>t</i>",
                     "positive and use <b>lower.tail = FALSE</b>",
                     "to ensure that we always get the area away from 0.",
                     "The area is the one-tailed <i>p</i>-value.",
                     "The area multiplied by two is the two-tailed <i>p</i>-value.")
        tmp <- paste(tmp,
                      "<p>This works because the area away from zero for <i>t</i>",
                      "is equal to the area away from zero for <i>-t</i>.")
        if (input$t != 0)  {
            tmp <- paste(tmp,
                         "<p>In the example above, the area to the left of",
                         formatC(-abs(input$t), digits = 2, format = "f"),
                         "and the area to the right of",
                         formatC(abs(input$t), digits = 2, format = "f"),
                         "are the same.")
          }
        tmp
      }, sep = "\n")
  }

shinyApp(ui = ui, server = server)
