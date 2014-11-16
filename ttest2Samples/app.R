# Illustrate two-sample t test
# To run in R: runGitHub("statDemos","sfcheung",subdir="ttest2Samples")

# Global variabless
n1def <- 200
n2def <- 49
m1def <- 70
m2def <- 72
sd1def <- 6
sd2def <- 7
alphadef <- .05

# UI
ui <- fluidPage(
  titlePanel("Illustrate two-sample t test"),
  sidebarLayout(
      sidebarPanel(p("Enter the required information:"),
                   h6("Level of significance (two-tailed):"),
                   numericInput('alpha', "Alpha", min=.001, max=.25, value=alphadef,
                                step=.001),
                   h6("Sample 1:"),
                   numericInput('n1', "Sample size", value=n1def),
                   numericInput('m1', "Mean", value=m1def),
                   numericInput('sd1', "Standard deviation (min .1)", min=.1, step=.1, value=sd1def),
                   h6("Sample 2:"),
                   numericInput('n2', "Sample size", value=n2def),
                   numericInput('m2', "Mean", value=m2def),
                   numericInput('sd2', "Standard deviation (min .1)", min=.1, step=.1, value=sd2def),
                   width=3
                  ),
      mainPanel(plotOutput('plot'))
    )
)

# Server
server <- function(input, output) {
  output$plot <- renderPlot({
    n1 <- input$n1
    n2 <- input$n2
    m1 <- input$m1
    m2 <- input$m2
    sd1 <- input$sd1
    sd2 <- input$sd2
    alpha <- input$alpha
    varPooled <- ((n1-1)*(sd1^2) + (n2-1)*(sd2^2))/(n1+n2-2)
    sdPooled <- sqrt(varPooled)
    se1 <- sdPooled/sqrt(n1)
    se2 <- sdPooled/sqrt(n2)
    seDiff <- sqrt(se1^2 + se2^2)
    mDiff <- m2 - m1
    df <- n1 + n2 - 2
    sampleT <- mDiff/seDiff
    criticalTHi <- qt(1-alpha/2, df)
    criticalTLo <- -1*criticalTHi
    ciDiffHi <- mDiff + criticalTHi*seDiff
    ciDiffLo <- mDiff - criticalTHi*seDiff
    pvalue <- pt(abs(sampleT), df, lower.tail=FALSE)*2
    
    
    # Set the parameters for the graphs
    
    # For raw scores and sample means
    xLo <- min(m1-3*sd1, m2-3*sd2)
    xHi <- max(m1+3*sd1, m2+3*sd2)
    hRange <- seq(xLo, xHi, length.out=100)
    h1     <- dnorm(hRange,m1,sd1)
    h2     <- dnorm(hRange,m2,sd2)
    yMax <- max(c(h1,h2))

    xmLo <- min(m1-3*se1, m2-3*se2)
    xmHi <- max(m1+3*se1, m2+3*se2)
    mhRange <- seq(xmLo, xmHi, length.out=100)
    mh1 <- dnorm(mhRange,m1,se1)
    mh2 <- dnorm(mhRange,m2,se2)
    ymMax <- max(c(mh1,mh2))
    ifelse(m1 < m2, 
            {xadj1 <- 1.25; xadj2 <- -.25}, 
            {xadj1 <- -.25; xadj2 <- 1.25}
          )
    
    # For t distribution
    tHi <- max(abs(sampleT)*1.25, qt(1-.0005, df))
    tLo <- -1*tHi
    thRange <- seq(tLo, tHi, length.out=100)
    th1 <- dt(thRange, df)
    tMax <- max(th1)
    ifelse (sampleT > 0,
        tphRange <- seq(sampleT, tHi, length.out=50),
        tphRange <- seq(tLo, sampleT, length.out=50))
    tph1 <- dt(tphRange, df)
    ifelse (sampleT > 0,
        {tphRange <- c(sampleT, tphRange, tHi);
         tph1 <- c(0, tph1, 0)},
        {tphRange <- c(tLo, tphRange, sampleT);
         tph1 <- c(0, tph1, 0)})
    
    # For confidence interval of difference
    diffHi <- max(ciDiffHi,3*seDiff)
    diffLo <- min(ciDiffLo,-3*seDiff)
    diffhRange <- seq(diffLo, diffHi, length.out=100)
    diffh1 <- dnorm(diffhRange,0,seDiff)
    ydiffMax <- max(diffh1)
    
    
    cexAll <- 1
    # Don't know why cex cannot control the magnification of all elements
    # So used cexAll here
    # Generate the plot object
    
    par(mfrow=c(2,2))
    #par(mar=c(5,1,3,1))
    
    # Plot scores distribution
    plot(hRange,h1,type="l", cex.main=cexAll,
          ylim=c(0,yMax),
          xlab="Score",
          ylab="",yaxt="n",
          main=c("Theoretical distribution of *scores* based on sample statistics",
                 "Red: Sample 1 / Blue: Sample 2"),
          sub="Each sample's own standard deviation is used")
    polygon(hRange,h1,col=rgb(1,0,0,.25))
    polygon(hRange,h2,col=rgb(0,0,1,.25))
    abline(v=m1, lwd=4, col="red")
    abline(v=m2, lwd=4, col="blue")
    text(m1, yMax, m1, adj=c(xadj1,1))
    text(m2, yMax, m2, adj=c(xadj2,1))    

    # Plot sample t distribution
    ifelse(sampleT > 0, 
            t_sub <- paste("Area to the right is ", sprintf("%5.4f",pvalue/2),
                          ", p-value=", sprintf("%5.4f",pvalue), 
                          " (rounded)", sep=""),
            ifelse(sampleT < 0,
              t_sub <- paste("Area to the left is ", sprintf("%5.4f",pvalue/2),
                            ", p-value=", sprintf("%5.4f",pvalue),
                            " (rounded)", sep=""),
              t_sub <- paste("Sample t at the center. p-value = 1."))
          )
            
    plot(thRange,th1,type="l", cex.main=cexAll,
          xlab="t statistic",
          ylab="",yaxt="n",
          main=c("Theoretical distribution of t statistic",
                 paste("(Degrees of freedom=", df, ")", sep="")),
          sub=t_sub
        )
    polygon(thRange,th1,col=rgb(0,1,0,.5))
    abline(v=criticalTLo, lwd=1, col="black", lty="dotted")
    abline(v=criticalTHi, lwd=1, col="black", lty="dotted")
    text(criticalTLo, tMax, 
        paste("Critical value\n", sprintf("%3.2f",criticalTLo), sep=""),
        adj=c(0.5,1))
    text(criticalTHi, tMax, 
        paste("Critical value\n", sprintf("%3.2f",criticalTHi), sep=""),
        adj=c(0.5,1))
    abline(v=sampleT, lwd=1, col="red")
    text(sampleT, tMax*.5, paste("Sample t\n",sprintf("%3.2f",sampleT),sep=""))
    if (sampleT != 0) {
        polygon(tphRange,tph1,col=rgb(1,0,0,.25));
        arrows(sampleT,tMax*.25,ifelse(sampleT > 0, tHi, tLo),tMax*.25,
               lwd=4, length=.125)
      }
           
    # Plot sample mean distribution
    plot(mhRange,mh1,type="l", cex.main=cexAll,
          ylim=c(0,ymMax),
          xlab="Score Mean",
          ylab="",yaxt="n",
          main=c("Theoretical distribution of *sample means*",
                 "Red: Sample 1 / Blue: Sample 2"),
          sub="The pooled estimate of standard deviation is used")
    polygon(mhRange,mh1,col=rgb(1,0,0,.25))
    polygon(mhRange,mh2,col=rgb(0,0,1,.25))
    abline(v=m1, lwd=4, col="red")
    abline(v=m2, lwd=4, col="blue")
    text(m1, ymMax, m1, adj=c(xadj1,1))
    text(m2, ymMax, m2, adj=c(xadj2,1))    

    
    # Plot the confidence interval
    diff_sub <- paste(format(100*(1-alpha), digits=2),"% confidence interval:",
                      format(ciDiffLo,digits=2)," to ", format(ciDiffHi,digits=2),
                      sep="")
    diff_sub <- paste("The ", format(100*(1-alpha), digits=2),
                      "% confidence interval ",
                      ifelse(ciDiffHi < 0 || ciDiffLo > 0, 
                              "does not contain",
                              "contains"),
                      " zero", sep="")
                     
    plot(diffhRange,diffh1,type="l", cex.main=cexAll,
          xlab="Sample mean difference (Mean 2 - Mean 1)",
          ylab="",yaxt="n",
          main=paste("Theoretical distribution of difference of two sample means\n",
               "Standard error of difference = ", format(seDiff, digits=2),"\n",
               sep=""),
          sub=diff_sub
        )
    polygon(diffhRange,diffh1,col=rgb(0,.5,.5,.5))
    abline(v=0, lwd=2, col="red", lty="dotted")
    segments(ciDiffLo, 0, ciDiffLo, ydiffMax*.5, lwd=2, col="black")
    segments(ciDiffHi, 0, ciDiffHi, ydiffMax*.5, lwd=2, col="black")
    text(ciDiffLo, ydiffMax*.5, format(ciDiffLo, digits=2), adj=c(.5,0))
    text(ciDiffHi, ydiffMax*.5, format(ciDiffHi, digits=2), adj=c(.5,0))
    arrows(ciDiffLo, ydiffMax*.25, ciDiffHi, ydiffMax*.25, 
           code=3, lwd=4, length=.125)
  })
}

shinyApp(ui=ui, server=server)
