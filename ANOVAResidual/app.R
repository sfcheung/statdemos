# In progress. Not ready.

# Illustrate the idea of residual and sum of squares in one-way ANOVA
# To run in R: runGitHub("statDemos","sfcheung",subdir="ANOVAResidual")

# Global variables
anova.data <- read.csv("anova.data.csv", header = TRUE, as.is=1)
gp_means <- tapply(anova.data$score, anova.data$group, mean)
g_mean <- mean(anova.data$score)
gp_f <- factor(levels(anova.data$group))
anova.data$score <- anova.data$score -  
                    rep(gp_means, times=table(anova.data$group))
# Initial gp_means
gp_means <- c(24,16,6,22)
n_total <- nrow(anova.data)
n_gp <- table(anova.data$group)
score_max <- max(anova.data$score)
score_min <- min(anova.data$score)
sdC_max <- 2
sdC_min <- 0
gp_means_max <- max(gp_means) + 5
gp_means_min <- 5
yAxis_max <- sdC_max*score_max + gp_means_max
yAxis_min <- sdC_max*score_min - gp_means_min

# UI
ui <- fluidPage(
  titlePanel("Illustrate the idea of residual and sum of squares in one-way ANOVA"),
  fluidRow(
    column(12,
      wellPanel(
        p("This demonstration shows how the two sums of squares, ",
          "within and between, are computed in one-way between group ANOVA.",
          " You can change the group mean",
          " of each group and see how the deviations of each case, the ",
          " two sums of squares, and hence the sample F ratio, change."),
        p("You can also change within group variation by a factor. For example,",
          "if you set the factor to 2, the deviation of each case from the ",
          "group mean will be doubled (multiplied by 2). This increases the ",
          "within group variation and you can see how the F ratio is affected."),
        p("Scenarios to explore: (1) What if the group means become more and more",
          "similar? (2) What if the group means do not change, but the within ",
          "group variation becomes smaller and smaller (by decreasing the factor)?",
          "What if the group variation becomes larger and larger?")
        ),
      fluidRow(
        column(4,
          wellPanel(
            p("Change the following and see how the residuals change:"),
               h6("Means:"),
               sliderInput('m1', "Group 1", 
                  min=gp_means_min, max=gp_means_max, value=gp_means[1], step=1),
               sliderInput('m2', "Group 2", 
                  min=gp_means_min, max=gp_means_max, value=gp_means[2], step=1),
               sliderInput('m3', "Group 3", 
                  min=gp_means_min, max=gp_means_max, value=gp_means[3], step=1),
               sliderInput('m4', "Group 4", 
                  min=gp_means_min, max=gp_means_max, value=gp_means[4], step=1),
               br(),
               h6("Within Group Variation"),
               sliderInput('sdC', 
                "Increase/decrease variation within a group by this factor",
                  min=sdC_min, max=sdC_max, value=1, step=.05)
              )
          ),
        column(8,
          plotOutput('plot', height="600px")
          )
        )
      )
    ),
  fluidRow(
    column(12,
      wellPanel(
        p("The latest version of the code can be found at ",
          a("statDemos at GitHub", 
            href="https://github.com/sfcheung/statDemos/tree/master/ANOVAResidual"),
          "."
          ),
        p("The whole repository can be downloaded from GitHub and run in R by",
          code("runGitHub(\"statDemos\",\"sfcheung\",subdir=\"ANOVAResidual\")")
          )
        )
      )
    )
  )



# Server
server <- function(input, output, session) {
  output$plot <- renderPlot({
    gp_means[1] <- input$m1
    gp_means[2] <- input$m2
    gp_means[3] <- input$m3
    gp_means[4] <- input$m4
    sdC <- input$sdC
    
    # Create the data frame
    anova.data.i <- anova.data
    anova.data.i$score <- sdC*anova.data.i$score + 
                          rep(gp_means, times=n_gp)
    g_mean <- mean(anova.data.i$score)
    
    # Conduct the one-way ANOVA
    anova.results <- aov(score ~ group, data=anova.data.i) 
    anova.F <- summary(anova.results)[[1]]$F[1]
    dfs <- summary(anova.results)[[1]]$Df
    anova.F.p <- summary(anova.results)[[1]]$Pr
    SSb <- summary(anova.results)[[1]]$Sum[1]
    SSw <- summary(anova.results)[[1]]$Sum[2]
    dfb <- dfs[1]
    dfw <- dfs[2]
    F.cut <- qf(1-.05, dfb, dfw)
    
    # Set graph parameters
    yMin <- min(anova.data.i$score)
    yMax <- max(anova.data.i$score)
    yMin <- yAxis_min
    yMax <- yAxis_max
    
    Flo <- 0
    Fhi <- max(anova.F*1.25, qf(1-.0005, dfb, dfw))
    FRange <- seq(Flo, Fhi, length.out=100)
    Fd <- df(FRange, dfb, dfw)
    FdMax <- max(Fd)
    FpRange <- c(anova.F, seq(anova.F, Fhi, length.out=50), Fhi)
    Fpd <- df(FpRange, dfb, dfw)
    
    cexAll <- 1
    cexPt <- 2
    # Don't know why cex cannot control the magnification of all elements
    # So used cexAll here
    # Generate the plot object
    
    par(mfrow=c(3,2))
    par(mar=c(5,2,3,2))
    
    # Plot group means
    plot(gp_f, gp_means, border="white",
        ylim=c(yMin, yMax),
        ylab="Score",
        xlab="Group",
        main=c("Group Means"))
    points(gp_f, gp_means, cex=cexPt, pch=16)
    lines(gp_f, gp_means, col="blue", lwd=4)
    abline(h=g_mean, lty="dotted", lwd=2, col="red")
    
    # Plot deviation from grand mean
    plot(anova.data.i$score,
        ylim=c(yMin, yMax),
        ylab="Score",
        xlab="Case",
        pch=16, cex=cexPt,
        main=c("Deviation of Each Case from Grand Mean",
                "(Total Sum of Squares)"))
    abline(h=g_mean, lty="dotted", lwd=2, col="grey")
    segments(1:n_total, g_mean, 1:n_total, anova.data.i$score, lty="dotted",
            col="red", lwd=2)

    # Plot sums of squares
    anova.SS <- matrix(c(SSb, SSw), 2, 1)
    rownames(anova.SS) <- c("Between", "Within")
    colnames(anova.SS) <- c("SS")
    barplot(anova.SS,
            xlab="", ylab="",
            col=c(rgb(1,0.5,0.5,.25),rgb(0.5,0.5,1,.25)),
            legend=rownames(anova.SS), horiz=FALSE,
            main=c("Partition the Total Sum of Squares",
                    paste("(Between SS: ",sprintf("%8.2f",SSb),
                          " / Within SS: ",sprintf("%8.2f",SSw),")",
                          sep="")))
            
    # Plot deviation from group mean
    plot(anova.data.i$score,
        ylim=c(yMin, yMax),
        ylab="Score",
        xlab="Case",
        pch=16, cex=cexPt,
        main=c("Deviation of Each Case from Group Mean",
                "(Within-Group Sum of Squares)"))
    abline(h=g_mean, lty="dotted", lwd=2, col="red")
    segments(cumsum(n_gp) - n_gp + 1, gp_means, cumsum(n_gp), gp_means,
              col="black", lwd=2)
    segments(1:n_total, rep(gp_means, times=n_gp), 1:n_total, anova.data.i$score,
              col="blue", lty="solid", lwd=2)

            
    # Plot F distribution
    plot(FRange,Fd,type="l", 
          xlab="F statistic",
          ylab="",yaxt="n",
          main=c("Theoretical distribution of F statistic",
                 paste("(df Between =", dfb, ", df Within =", dfw, ")",
                 sep="")))
    polygon(FRange,Fd,col=rgb(0,1,0,.5))
    abline(v=F.cut, lwd=1, col="black", lty="dotted")
    text(F.cut, FdMax, 
        paste("Critical value\n", sprintf("%3.2f",F.cut), sep=""),
        adj=c(0.5,1))
    abline(v=anova.F, lwd=1, col="red")
    text(anova.F, FdMax*.5, paste("Sample F\n",sprintf("%3.2f",anova.F),sep=""))

              
    # Plot deviation of group mean from grand mean
    plot(anova.data.i$score,
        ylim=c(yMin, yMax),
        ylab="Score",
        xlab="Case",
        pch=16, cex=cexPt,
        main=c("Deviation of Group Mean from Grand Mean",
                "(Between-Group Sum of Squares)"))
    abline(h=g_mean, lty="dotted", lwd=2, col="red")
    segments(cumsum(n_gp) - n_gp + 1, gp_means, cumsum(n_gp), gp_means,
              col="black", lwd=2)
    segments(1:n_total, g_mean, 1:n_total, rep(gp_means, times=n_gp),
              col="red", lty="solid", lwd=2)

              
            
  })
}

shinyApp(ui=ui, server=server)
