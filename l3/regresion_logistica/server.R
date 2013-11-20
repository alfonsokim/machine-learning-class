shinyServer(function(input, output) {
  library(ggplot2)
  library(MASS)
  data(Pima.tr)
  h <- function(x){
    exp(x)/(1+exp(x))
  }
  
  log.like <- function(beta){
    val.1 <- ((Pima.tr$glu-120)/31)*beta[2] + beta[1]
    probs <- h(val.1)
    -(sum(log(probs[Pima.tr$type=='Yes'])) + sum(log(1-probs[Pima.tr$type=='No'])))/length(val.1)
  }
  
  
  grid.1 <- expand.grid(beta0=seq(-20,20,0.5), beta1=seq(-3,6,0.5))
  grid.1$log.like <- apply(grid.1, 1, log.like)
  
  
  
  output$main_plot <- renderPlot({
    glu.grid <- seq(50,200,5)
    val.1 <- ((glu.grid-120)/31)*input$beta + input$beta0
    val.2 <- ((Pima.tr$glu-120)/31)*input$beta + input$beta0
    Pima.tr$class <- factor(val.2 > 0)
    puntos.curva <- data.frame(glu=glu.grid, y=h(val.1))
    set.seed(123)
    g <- ggplot(Pima.tr, aes(x=(glu-120)/31, y=as.numeric(type=='Yes'))) + 
      geom_jitter(position=position_jitter(height=0.05)) + xlim(c(-3,3))
    g2 <- g + geom_line(data=puntos.curva, aes(x=(glu-120)/31, y=y), col='red',size=2)
    print(g2)
    })
  

  output$rss <- renderPrint({
    log.like(c(input$beta0, input$beta))
  })
  
  output$contour <- renderPlot(
  {
    g <- ggplot(grid.1, aes(x=beta0,y=beta1, z=log.like)) + 
      stat_contour(binwidth=0.2, aes(colour=..level..))+
      geom_point(size=5, x=input$beta0,y=input$beta, colour = 'red')
  print(g)
  }
    )
  
})