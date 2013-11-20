

library(knitr)
library(ggplot2)
library(plyr)
library(reshape2)
#options(replace.assign=TRUE,width=55)
#knit_hooks$set(par=function(before, options, envir){if (before) par(mar=c(4,4,.1,.1),cex.lab=.95,cex.axis=.9,mgp=c(2,.7,0),tcl=-.3)})



generar.muestra <- function(n){
  df.1 <- rdply(n, {c(x <- runif(1,-1,1), abs(x))})
  names(df.1) <- c('ind','x','y')
  df.1
}
estimacion.1vmc <- function(x, df){
  df$y[which.min(abs(df$x-x))]
}
set.seed(101)
dat.1 <- generar.muestra(3)
est.1 <- estimacion.1vmc(0, dat.1)
est.1
ecm <- (0-est.1)^2 ## pues f(0)=0^2   
ecm

est.2 <- estimacion.1vmc(0.5, dat.1)
ecm.2 <- (0.5 - est.2) ^ 2

set.seed(101)
calcular.error <- function(){
  dat.1 <- generar.muestra(3)
  est.1 <-  estimacion.1vmc(0, dat.1)
  ecm <- (0-est.1)^2
  data.frame(estimacion=est.1, error=ecm)

}


calcular.error.2 <- function(){
    dat.2 <- generar.muestra(3)
    est.2 <-  estimacion.1vmc(0.5, dat.1)
    ecm <- (0.5-est.2)^2
    data.frame(estimacion=est.2, error=ecm.2)
    
}
reps.2 <- rdply(3000, calcular.error.2())
mean(reps.2$error)
varianza.2 <- var(reps.2$estimacion)
varianza.2

sesgo2.2 <- (mean(reps.2$estimacion)-0.5)^2

sesgo2.2



reps <- rdply(3000, calcular.error())
qplot(reps$error)



mean(reps$error)



varianza <- var(reps$estimacion)
varianza



sesgo2 <- (mean(reps$estimacion)-0)^2
sesgo2



sesgo2+varianza
mean(reps$error)



generar.muestra.3 <- function(n){
  df.1 <- rdply(n, {c(x <- runif(1,-1,1), abs(x)+rnorm(1,0,0.5))})
  names(df.1) <- c('ind','x','y')
  df.1
}
  
salida <- ldply(c(4,6,8,10,12,15,20,30), function(k){
      ldply(1:800, function(rep){
        df.1 <- generar.muestra.3(50)
        mod.vmc <- kknn(y~x, train=df.1, test=data.frame(x=0),k=k, kernel='rectangular',scale=FALSE)
        data.frame(k=k, rep=rep, estimado=mod.vmc$fitted.values)
    })
})
resumen <- ddply(salida,c('k'), summarise,
  sesgo= (mean(estimado)-0)^2, varianza=var(estimado))
resumen$error <- resumen$sesgo + resumen$varianza
  resumen.m <- melt(resumen, id.vars='k')
  ggplot(resumen.m, aes(x=50/k, y=value, colour=variable, group=variable)) + geom_line() +
  xlab('Tamaño de muestra / No. vecinos')



  ggplot(resumen.m, aes(x=50/k, y=value, colour=variable, group=variable)) + geom_line() +
  xlab('Complejidad') + scale_x_log10() + annotate("text",x=2.3, y=0.075, label='Zona\n de\n sesgo') +
  annotate("text", x=10, y=0.06, label='Zona\n de \n varianza')



generar.muestra.3 <- function(n){
  df.1 <- rdply(n, {c(x <- runif(1,-1,1), abs(x)+rnorm(1,0,0.5))})
  names(df.1) <- c('ind','x','y')
  df.1
}
salida <- ldply(c(2,3,4,6,8,10,12,15,20,30,35), function(k){
      ldply(1:200, function(rep){
        df.1 <- generar.muestra.3(40)
        df.prueba <- generar.muestra.3(1500)
        mod.vmc <- kknn(y~x, train=df.1, test=df.1,k=k, kernel='rectangular',scale=FALSE)
        mod.vmc.prueba <- kknn(y~x, train=df.1, test=df.prueba,k=k, kernel='rectangular',scale=FALSE)
        data.frame(k=k, rep=rep, 
                  error.entrena = mean((mod.vmc$fitted.values - df.1$y)^2),
                  error.prueba = mean((mod.vmc.prueba$fitted.values - df.prueba$y)^2))
    })
})
resumen <- ddply(salida,c('k'), summarise,
  error.entrena=mean(error.entrena),
  error.prueba = mean(error.prueba))
resumen.m <- melt(resumen, id.vars=c('k'))
salida.m <- melt(salida, id.vars=c('k','rep'))
ggplot(salida.m, aes(x=50/k, y=value, colour=variable, group=interaction(rep, variable))) + geom_line(alpha=0.5) +
  geom_line(data=resumen.m, aes(x=50/k, y=value, colour=variable, group=variable), size=2, colour='black') +
  scale_x_log10(breaks=c(2,4,8,16))



library(ElemStatLearn)
data(spam)
library(arm)
set.seed(2805)
spam <- spam[sample(1:nrow(spam), nrow(spam)),]
spam.entrena <- spam[entrena.reng <- sample(1:nrow(spam), 2000), ]
entrena.reng.2 <- sample( setdiff(1:nrow(spam), spam.entrena), 2000)
entrena.reng.tot <- c(entrena.reng, entrena.reng.2)
spam.entrena.tot <- spam[entrena.reng.tot, ]
spam.valida <- spam[-entrena.reng.tot, ]
mod.1 <- glm(spam ~ ., data=spam.entrena[,c(1:15,58)], family='binomial')
mean((predict(mod.1, newdata=spam.entrena)>0)== (spam.entrena$spam=='spam'))
mean((predict(mod.1, newdata=spam.valida)>0)== (spam.valida$spam=='spam'))



library(kknn)
generar.muestra.2 <- function(n){
  df.1 <- rdply(n, {c(x <- runif(1,-1,1), abs(x)+runif(1,-0.2,0.2))})
  names(df.1) <- c('ind','x','y')
  df.1
}
set.seed(12855)
muestras <- ldply(c(10,50,100,150,300), function(n){
  muestra.1 <- ldply(1:400, function(i){data.frame(n=n,rep=i,generar.muestra.2(n))})
  muestra.1
})

res <- ddply(muestras, c('n'), function(df){
  ddply(df, c('rep'), function(df.1){
    ldply(c(1,4,8), function(k){
      mod.vmc <- kknn(y~x, train=df.1, test=data.frame(x=0),k=k, kernel='rectangular',scale=FALSE)
      data.frame(k=k, estimado=mod.vmc$fitted.values)
    })
  })
})
res.1 <- ddply(res, c('k','n'), summarise,
  sesgo = (mean(estimado)-0)^2,
  varianza = var(estimado))
res.1$sesgoyvar <- res.1$sesgo+res.1$varianza
res.m <- melt(res.1, id.vars=c('k','n'))
ggplot(res.m, aes(x=n, y=value, colour=factor(k),group=k)) + facet_wrap(~variable) + geom_point()+geom_line()+  scale_y_sqrt()



library(arm)
generar.muestra.2 <- function(n){
  df.1 <- rdply(n, {c(x <- runif(1,-1,1), y <- abs(x)+runif(1,-2,2))})
  names(df.1) <- c('ind','x', 'y')
  df.1
}
set.seed(122855)
muestras <- ldply(c(5,10,20,30,40,50), function(n){
  muestra.1 <- ldply(1:100, function(i){data.frame(n=n,rep=i,generar.muestra.2(n))})
  muestra.1
})

res <- ddply(muestras, c('n'), function(df){
  #print(df$n[1])
  ddply(df, c('rep'), function(df.1){
      ldply(c(0.2,0.5,1,2), function(sigma){
        df.2 <- data.frame(x=df.1$x, x2=(df.1$x^2))
        df.3 <- scale(df.2)
        df.4 <- data.frame(df.3)
        df.4$y <- df.1$y
        medias <- attr(df.3,'scaled:center')
        escala <- attr(df.3,'scaled:scale')
      mod.lineal <- bayesglm(y ~ x + x2 , data=df.4, 
                             prior.scale=sigma, prior.df=Inf, scaled=FALSE)
      data.frame(sigma=sigma, estimado=predict(mod.lineal, 
                                    data.frame(scale(data.frame(x=0.5, x2=0.25), medias, escala))))
  })
})
})  
res.1 <- ddply(res, c('sigma','n'), summarise,
  sesgo = (mean(estimado)-0.5)^2,
  varianza = var(estimado))
res.1$sesgoyvar <- res.1$sesgo+res.1$varianza
res.m <- melt(res.1, id.vars=c('sigma','n'))
ggplot(res.m, aes(x=n, y=value, colour=factor(sigma),group=sigma)) + facet_wrap(~variable) + geom_point()+geom_line()+scale_y_sqrt()




curva.ap <- ldply(seq(50,2000,100), function(i){
  mod.1 <- glm(spam ~ ., data=spam.entrena[1:i, c(1:15,58)], family='binomial')
  corr.entrena <- mean((predict(mod.1, newdata=spam.entrena[1:i, ])>0)== (spam.entrena$spam[1:i]=='spam'))
  corr.valida <- mean((predict(mod.1, newdata=spam.valida)>0)== (spam.valida$spam=='spam'))
 data.frame(i=i, error.entrena=1-corr.entrena, error.valida = 1-corr.valida)
})
curva.ap.m <- melt(curva.ap, id.var='i')
ggplot(curva.ap.m, aes(x=i, y=value, colour=variable)) + geom_point() +
  geom_line() + ylab('Tasa de incorrectos') 



curva.ap <- ldply(seq(50,4000,100), function(i){
  mod.1 <- glm(spam ~ ., data=spam.entrena.tot[1:i, c(1:15,58)], family='binomial')
  corr.entrena <- mean((predict(mod.1, newdata=spam.entrena.tot[1:i, ])>0)== (spam.entrena.tot$spam[1:i]=='spam'))
  corr.valida <- mean((predict(mod.1, newdata=spam.valida)>0)== (spam.valida$spam=='spam'))
 data.frame(i=i, error.entrena=1-corr.entrena, error.valida = 1-corr.valida)
})
curva.ap.m <- melt(curva.ap, id.var='i')
ggplot(curva.ap.m, aes(x=i, y=value, colour=variable)) + geom_point() +
  geom_line() + ylab('Tasa de incorrectos') 



library(nnet)
set.seed(123)
red.1 <- nnet(spam~., data=spam.entrena[, c(1:15,58)], size=20, decay=0.01, maxit=2000, MaxNWts=10000, trace = FALSE)

pred.entrena <- predict(red.1, newdata = spam.entrena, type='class')
pred.prueba <- predict(red.1, newdata = spam.valida, type='class')
mean(pred.entrena!=spam$spam[entrena.reng])
mean(pred.prueba!=spam.valida$spam)



library(nnet)
library(parallel)
cl <- makeCluster(4)
reporte.nodos <- clusterEvalQ(cl, { library(nnet)
})
clusterExport(cl, list('spam.entrena','spam.valida'))
curva.ap <- clusterApplyLB(cl, c(200,seq(500,2000,500)), function(i){
  
  mod.1 <- nnet(spam ~ ., data=spam.entrena[1:i, c(1:15,58)], size=20, MaxNWts=10000,
    maxit=4000, decay=0.01)
  pred.entrena <- predict(mod.1, type='class')
  corr.entrena <- mean(pred.entrena == spam.entrena$spam[1:i])
  
  pred.valida <- predict(mod.1, newdata=spam.valida, type='class')
  corr.valida <- mean(pred.valida == spam.valida$spam)
  df <- data.frame(i=i, error.entrena=1-corr.entrena, 
     error.valida = 1-corr.valida,
    convergence=mod.1$convergence)
  print(df)
  df
})
stopCluster(cl)
curva.ap.df <- Reduce(rbind, curva.ap)
curva.ap.m <- melt(curva.ap.df, id.var='i')
ggplot(curva.ap.m, aes(x=i, y=value, colour=variable)) + geom_point() +
  geom_line() + ylab('Tasa de incorrectos')



library(nnet)
library(parallel)
cl <- makeCluster(4)
reporte.nodos <- clusterEvalQ(cl, { library(nnet)
})
clusterExport(cl, list('spam.entrena.tot','spam.valida'))
curva.ap <- clusterApplyLB(cl, c(200,seq(500,4000,500)), function(i){
  
  mod.1 <- nnet(spam ~ ., data=spam.entrena.tot[1:i, c(1:15,58)], size=25, MaxNWts=10000,
    maxit=4000, decay=0.01)
  pred.entrena <- predict(mod.1, type='class')
  corr.entrena <- mean(pred.entrena == spam.entrena.tot$spam[1:i])
  
  pred.valida <- predict(mod.1, newdata=spam.valida,  type='class')
  corr.valida <- mean(pred.valida == spam.valida$spam)
  df <- data.frame(i=i, error.entrena=1-corr.entrena, 
     error.valida = 1-corr.valida,
    convergence=mod.1$convergence)
  print(df)
  df
})
stopCluster(cl)
curva.ap.df <- Reduce(rbind, curva.ap)
curva.ap.m <- melt(curva.ap.df, id.var='i')
ggplot(curva.ap.m, aes(x=i, y=value, colour=variable)) + geom_point() +
  geom_line() + ylab('Tasa de incorrectos')



library(kknn)

generar.muestra <- function(n){
  x.vars <- sapply(1:10, function(i){rnorm(n)})
  y.var <- (x.vars[,1]) + abs(x.vars[,2]-1) + 1*x.vars[,3]^2 + rnorm(n,0,3)
  data.frame(x.vars, y=y.var)
}
set.seed(12855)
n <- 200
test.df <- muestras[1,-c(1), drop=FALSE]
test.df[1,] <- 0
#test.df <- generar.muestra(1000)
muestras <- ldply(1:400,
                function(i){data.frame(rep=i,generar.muestra(n))})
res <- ddply(muestras, c('rep'), function(df){
    ldply(1:9, function(i){
      df.1 <- df[ , c(2:(i+1),12), drop=FALSE]
      head(df.1)
      mod.vmc <- kknn(y~., train=df.1, test=test.df,
                  k=5, kernel='rectangular')
      data.frame(i=i, estimado=mod.vmc$fitted.values)
    })
  })
res.1 <- ddply(res, c('i'), summarise,
  sesgo = (mean(estimado)-1)^2,
  varianza = var(estimado))
res.1$sesgoyvar <- res.1$sesgo+res.1$varianza
res.m <- melt(res.1, id.vars=c('i'))
ggplot(res.m, aes(x=n, y=value, colour=variable)) + geom_point()+
  geom_line() + scale_x_continuous(breaks=1:9)


