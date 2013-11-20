shinyServer(function(input, output) {
  library(ggplot2)
  library(ElemStatLearn)
  data(prostate)

  sum.sq <- function(x){
    mean((prostate$lpsa - prostate$lcavol*x[2] - x[1])^2)
  }
  
  grid.1 <- expand.grid(beta0=seq(-2,4,0.1), beta=seq(-2,4,0.1))
  grid.1$rss <- apply(grid.1, 1, sum.sq)
  
  
  output$main_plot <- renderPlot({
    g <- qplot(prostate$lcavol, prostate$lpsa)
    g2 <- g + geom_abline(intercept=as.numeric(input$beta0),
        slope=as.numeric(input$beta), colour='red')+
      geom_segment(
        aes(x=prostate$lcavol, xend=prostate$lcavol,
        y=prostate$lpsa, 
          yend=as.numeric(input$beta0)+as.numeric(input$beta)*prostate$lcavol),
        colour='gray')
    print(g2)
    })
  
  output$residuales <- renderPlot({
    res <- prostate$lpsa - prostate$lcavol*as.numeric(input$beta) -
        as.numeric(input$beta0)
    h <- qplot(res) + xlim(c(-5,5))
    print(h)
    })
   output$rss <- renderPrint({
      res <- prostate$lpsa - prostate$lcavol*as.numeric(input$beta) -
        as.numeric(input$beta0)
    cat(paste('media de residuales al cuadrado: ', round(mean(res^2),2)))
  })
  
  
  output$contour <- renderPlot(
  {
    g <- ggplot(grid.1, aes(x=beta0,y=beta, z=log(rss))) + 
      stat_contour(binwidth=0.5, aes(colour=..level..))+
      geom_point(size=5, x=input$beta0,y=input$beta, colour = 'red')
  print(g)
  }
    )
  
})