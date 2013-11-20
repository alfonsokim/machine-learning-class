

library(knitr)
library(ggplot2)
library(plyr)
library(reshape2)
#options(replace.assign=TRUE,width=55)
#knit_hooks$set(par=function(before, options, envir){if (before) par(mar=c(4,4,.1,.1),cex.lab=.95,cex.axis=.9,mgp=c(2,.7,0),tcl=-.3)})



beta <- c(-1,2)
beta.0 <- 1
dat.1 <- expand.grid(x.1=seq(-3,3,0.2), x.2=seq(-3,3,0.2))
head(dat.1)
dat.1$valor <- as.matrix(dat.1)%*%beta - beta.0
head(dat.1)
dat.1$color[dat.1$valor < 0] <- 'negativo'
dat.1$color[dat.1$valor > 0] <- 'positivo'
ggplot(dat.1, aes(x=x.1,y=x.2, colour=color)) + geom_point() +
  scale_colour_manual(values=c('red','gray')) +
  geom_abline(intercept=beta.0/beta[2], slope=-beta[1]/beta[2]) +
  annotate('text', x=2.2, y=2.2, label="x^{t}~beta~-~beta[0]==0", parse=T, size=5, angle=30)



set.seed(2805)
dat.1 <- data.frame(x.1=rnorm(10,0,1), x.2=rnorm(10,0,1))
head(dat.1)
dat.1$valor <- as.matrix(dat.1)%*%beta - beta.0
head(dat.1)
dat.1$color[dat.1$valor < 0] <- 'negativo'
dat.1$color[dat.1$valor > 0] <- 'positivo'
ggplot(dat.1, aes(x=x.1,y=x.2, colour=color)) + geom_point() +
  scale_colour_manual(values=c('red','gray')) +
  geom_abline(intercept=beta.0/beta[2], slope=-beta[1]/beta[2]) +
  geom_abline(intercept=beta.0/beta[2]+0.2, slope=-beta[1]/beta[2]) +
  geom_abline(intercept=beta.0/beta[2]+0.1, slope=-beta[1]/beta[2]+0.4)



set.seed(2805)
dat.1 <- data.frame(x.1=rnorm(10,0,1), x.2=rnorm(10,0,1))
head(dat.1)
dat.1$valor <- as.matrix(dat.1)%*%beta - beta.0
head(dat.1)
dat.1$color[dat.1$valor < 0] <- 'negativo'
dat.1$color[dat.1$valor > 0] <- 'positivo'
ggplot(dat.1, aes(x=x.1,y=x.2, colour=color)) + geom_point() +
  scale_colour_manual(values=c('red','gray')) +
  geom_abline(intercept=beta.0/beta[2]+0.3, slope=-beta[1]/beta[2], colour='darkblue', size=1.5) +
  geom_abline(intercept=beta.0/beta[2]+0.1, slope=-beta[1]/beta[2], colour='gray', size=1.5) +
  geom_abline(intercept=beta.0/beta[2]+0.1, slope=-beta[1]/beta[2]+0.4, colour='yellow', size=1.5)




objetivo <- function(X, y){
  func.obj <- function(params){
    beta <- params[1:2]
    beta.0 <- params[3]
    distancias <- (X%*%beta - beta.0)*y*(1/sqrt(sum(beta^2)))
    minimo.margen <- min(distancias)
    -minimo.margen
  }
  func.obj
}
X <- as.matrix(dat.1[,1:2])
y <- (dat.1$color=='positivo') - (dat.1$color=='negativo')
obj.1 <- objetivo(X, y)

res <- optim(par=c(1,1,0), obj.1, method='BFGS')
res
beta <- res$par[1:2]
beta.0 <- res$par[3]
margen <- -res$value
norma <- sqrt(sum(beta^2))
ggplot(dat.1, aes(x=x.1,y=x.2, colour=color)) + geom_point(size=4) +
  scale_colour_manual(values=c('red','gray80')) +
  geom_abline(intercept=beta.0/beta[2], slope=-beta[1]/beta[2], colour='darkblue', size=1.5)+
  geom_abline(intercept=(beta.0+margen*norma)/beta[2], slope=-beta[1]/beta[2], colour='darkblue')+
  geom_abline(intercept=(beta.0-margen*norma)/beta[2], slope=-beta[1]/beta[2], colour='darkblue')



# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }

 if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}




set.seed(2805)
dat.x <- expand.grid(x.1=seq(-4,4,0.1), x.2=seq(-4,4,0.1))
dat.2.1 <- data.frame(x.1=rnorm(10,-1,1), x.2=rnorm(10,-1,1))
dat.2.1$clase <- 1
dat.2.2  <- data.frame(x.1=rnorm(10,1,1), x.2=rnorm(10,1,1))
dat.2.2$ clase <- -1
dat.2 <- rbind(dat.2.1, dat.2.2)
dat.2$clase <- factor(dat.2$clase)

ggplot(dat.2, aes(x=x.1, y=x.2, colour=factor(clase)))+geom_point(size=3)

library(e1071)

svm.1 <- svm(clase ~x.1 + x.2, data=dat.2, kernel = 'linear', cost=0.01 )
svm.2 <- svm(clase ~x.1 + x.2, data=dat.2, kernel = 'linear', cost=1 )
svm.3 <- svm(clase ~x.1 + x.2, data=dat.2, kernel = 'linear', cost=1000 )
preds.1 <- predict(svm.1, newdata = dat.x)
preds.2 <- predict(svm.2, newdata = dat.x)
preds.3 <- predict(svm.3, newdata = dat.x)
dat.x$preds.1 <- preds.1
dat.x$preds.2 <- preds.2
dat.x$preds.3 <- preds.3
g.1 <- ggplot(dat.x, aes(x=x.1, y=x.2, colour=preds.1))+geom_point(size=1) +
  geom_point(data=dat.2, aes(x=x.1, y=x.2, colour=factor(clase)), size=3) +
  labs(title='C grande')
g.2 <- ggplot(dat.x, aes(x=x.1, y=x.2, colour=preds.2))+geom_point(size=1) +
  geom_point(data=dat.2, aes(x=x.1, y=x.2, colour=factor(clase)), size=3)+  labs(title='C mediana')

g.3 <- ggplot(dat.x, aes(x=x.1, y=x.2, colour=preds.3))+geom_point(size=1) + geom_point(data=dat.2, aes(x=x.1, y=x.2, colour=factor(clase)), size=3)+  labs(title='C chica')



multiplot(g.1, g.2, g.3,cols=3)
dat.prueba.1 <- data.frame(x.1=rnorm(500,-1,1), x.2=rnorm(500,-1,1))
dat.prueba.2 <- data.frame(x.1=rnorm(500,1,1), x.2=rnorm(500,1,1))
dat.prueba.1$clase <- 1
dat.prueba.2$clase <- -1
dat.prueba <- rbind(dat.prueba.1, dat.prueba.2)
dat.prueba$clase <- factor(dat.prueba$clase)



mean(predict(svm.1, newdata = dat.prueba)!=dat.prueba$clase)
mean(predict(svm.2, newdata = dat.prueba)!=dat.prueba$clase)
mean(predict(svm.3, newdata = dat.prueba)!=dat.prueba$clase)




svm.1$SV
svm.2$SV
svm.3$SV



z <- rnorm(500)
dat.pos <- data.frame(x.1=rnorm(500)-z, x.2=rnorm(500)+z, y=1)
dat.neg <- data.frame(x.1=2+rnorm(500), x.2=2+rnorm(500), y=-1)
datos <- rbind(dat.pos, dat.neg)
ind.train <- sample(1:nrow(datos), 300)
datos.train <- datos[ind.train, ]
datos.test <- datos[-ind.train, ]
ggplot(datos.train, aes(x=x.1, y=x.2, shape=factor(y))) + geom_point()


#install.packages("kernlab")
library(kernlab)
csv <- ksvm(y~x.1+x.2, data=datos.train, 
  type="C-svc", kernel="vanilladot", C=0.01, scaled=FALSE)
plot(csv, data=datos.train)

?b
?I
pred.1 <- predict(csv, newdata=datos.test)
mean(pred.1==datos.test$y)



### Ejercicio pagina 18
coefs.alpha <- alpha(csv)
indices <- alphaindex(csv)
b0 <- b(csv)
soporte.x <- datos.train[indices[[1]], c("x.1", "x.2")] 
soporte.y <- datos.train[indices[[1]], "y"]
x <- datos.test[3, c(1, 2)]




set.seed(2805)
dat.x <- expand.grid(x.1=seq(-4,4,0.1), x.2=seq(-4,4,0.1))
dat.2.1 <- data.frame(x.1=rnorm(20,-2,1), x.2=rnorm(20,-2,1))
dat.2.1.x <- data.frame(x.1=rnorm(20,2,1), x.2=rnorm(20,2,1))
dat.2.1 <- rbind(dat.2.1, dat.2.1.x)
dat.2.1$clase <- 1
dat.2.2  <- data.frame(x.1=rnorm(40,0,1), x.2=rnorm(40,0,1))
dat.2.2$ clase <- -1
dat.2 <- rbind(dat.2.1, dat.2.2)
dat.2$clase <- factor(dat.2$clase)

ggplot(dat.2, aes(x=x.1, y=x.2, colour=factor(clase)))+geom_point(size=3)

library(e1071)

svm.1 <- svm(clase ~x.1 + x.2 + I(x.1*x.2)+I(x.1^2)+I(x.2^2), data=dat.2, kernel = 'linear', cost=0.01 )
svm.2 <- svm(clase ~x.1 + x.2+ I(x.1*x.2)+I(x.1^2)+I(x.2^2), data=dat.2, kernel = 'linear', cost=1 )
svm.3 <- svm(clase ~x.1 + x.2+ I(x.1*x.2+I(x.1^2)+I(x.2^2)), data=dat.2, kernel = 'linear', cost=1000 )
preds.1 <- predict(svm.1, newdata = dat.x)
preds.2 <- predict(svm.2, newdata = dat.x)
preds.3 <- predict(svm.3, newdata = dat.x)
dat.x$preds.1 <- preds.1
dat.x$preds.2 <- preds.2
dat.x$preds.3 <- preds.3
g.1 <- ggplot(dat.x, aes(x=x.1, y=x.2, colour=preds.1))+geom_point(size=1) +
  geom_point(data=dat.2, aes(x=x.1, y=x.2, colour=factor(clase)), size=3) +
  labs(title='C grande')
g.2 <- ggplot(dat.x, aes(x=x.1, y=x.2, colour=preds.2))+geom_point(size=1) +
  geom_point(data=dat.2, aes(x=x.1, y=x.2, colour=factor(clase)), size=3)+  labs(title='C mediana')

g.3 <- ggplot(dat.x, aes(x=x.1, y=x.2, colour=preds.3))+geom_point(size=1) + geom_point(data=dat.2, aes(x=x.1, y=x.2, colour=factor(clase)), size=3)+  labs(title='C chica')



multiplot(g.1, g.2, g.3,cols=3)
dat.prueba.1 <- data.frame(x.1=rnorm(500,-1,1), x.2=rnorm(500,-1,1))
dat.prueba.2 <- data.frame(x.1=rnorm(500,1,1), x.2=rnorm(500,1,1))
dat.prueba.1$clase <- 1
dat.prueba.2$clase <- -1
dat.prueba <- rbind(dat.prueba.1, dat.prueba.2)
dat.prueba$clase <- factor(dat.prueba$clase)
mean(predict(svm.1, newdata = dat.prueba)!=dat.prueba$clase)
mean(predict(svm.2, newdata = dat.prueba)!=dat.prueba$clase)
mean(predict(svm.3, newdata = dat.prueba)!=dat.prueba$clase)



### Solucion ejercicio p 18
csv
coefs.alpha <- alpha(csv)
indices <- alphaindex(csv)
b0 <- b(csv)
soporte.x <- datos.train[indices[[1]],c('x.1','x.2')]
soporte.y <- datos.train[indices[[1]],'y']
x <- datos.test[3,c(1,2)]



prod.punto <- as.matrix(soporte.x)%*%as.numeric(x)
sum(prod.punto*soporte.y*coefs.alpha[[1]]) - b0
predict(csv, datos.test[1:5, c(1,2)], type='decision')



library(e1071)
ggplot(dat.2, aes(x=x.1, y=x.2, colour=factor(clase)))+geom_point(size=3)
svm.poli.1 <- svm(clase ~ ., data=dat.2, kernel='polynomial', degree=4, cost=100) 
svm.poli.2 <- svm(clase ~ ., data=dat.2, kernel='polynomial', degree=4, cost=0.01) 

preds.1 <- predict(svm.poli.1, newdata = dat.x)
dat.x$preds.1 <- preds.1
 ggplot(dat.x, aes(x=x.1, y=x.2, colour=preds.1))+geom_point(size=1) +
  geom_point(data=dat.2, aes(x=x.1, y=x.2, colour=factor(clase)), size=3)
preds.1 <- predict(svm.poli.2, newdata = dat.x)
dat.x$preds.1 <- preds.1
 ggplot(dat.x, aes(x=x.1, y=x.2, colour=preds.1))+geom_point(size=1) +
  geom_point(data=dat.2, aes(x=x.1, y=x.2, colour=factor(clase)), size=3) 





ggplot(dat.2, aes(x=x.1, y=x.2, colour=factor(clase)))+geom_point(size=3)
svm.1 <- svm(clase ~ ., data=dat.2, kernel='radial', gamma=1, cost=100) 
svm.2 <- svm(clase ~ ., data=dat.2, kernel='radial', gamma=1, cost=0.01) 

preds.1 <- predict(svm.1, newdata = dat.x)
dat.x$preds.1 <- preds.1
 ggplot(dat.x, aes(x=x.1, y=x.2, colour=preds.1))+geom_point(size=1) +
  geom_point(data=dat.2, aes(x=x.1, y=x.2, colour=factor(clase)), size=3)

preds.1 <- predict(svm.2, newdata = dat.x)
dat.x$preds.1 <- preds.1
 ggplot(dat.x, aes(x=x.1, y=x.2, colour=preds.1))+geom_point(size=1) +
  geom_point(data=dat.2, aes(x=x.1, y=x.2, colour=factor(clase)), size=3) 




costo <- function(yf){
  z <- 1-yf
  ifelse(z>=0,z,0)
}
devianza.binomial <- function(yf){
  log(1+exp(-yf))
}
curve(costo, from=-3, to=3)
curve(devianza, from=-3, to=3, add=T, col='red')



library(kernlab)
data(reuters)
x <- reuters
y <- rlabels
y[1:2]
x[1:2]
y[39:40]
x[39:40]

x.1 <- x[4:37]
y.1 <- y[4:37]



library(kernlab)
sk <- stringdot(type="spectrum", length=2, normalize=FALSE)
# incluye un caracter de 'final de cadena'
sk('abcd','abcd')
sk('abcd','abcdx')
sk('abcd','abcxd')
sk('subsucesion','sucesion')
sk('datos a una tabla para poder usar nuestros','de las tablas que hay en común en')
sk('datos a una tabla para poder usar nuestros','que hay en común en')



mod.x <- ksvm(x.1, y.1, kernel='stringdot', kpar=list(type='spectrum',length=4))
predict(mod.x, x[c(1,2,3,38,39,40)])
x[c(1,2,3,38,39,40)]


### TAREA
#install.packages("e1071")
library(e1071)
library(ISLR)
data(OJ)
summary(OJ)
?sample
set.seed(240484)
train.idx <- sample(nrow(OJ), 800)
train.set <- OJ[train.idx, ]
test.set <- OJ[-train.idx, ]

#plot(train.set)
library(caret)
library(kernlab)
library(plyr)
library(ggplot2)

?trainControl
?train

?svm
svm.1 <- svm(Purchase ~ ., data = train.set, kernel = "polynomial", degree = 3, cost = 1)

sum(as.integer(fitted(svm.1) == train.set$Purchase)) / nrow(train.set)


train.grid.poly <- expand.grid(degree=c(3:20), cost=c(0.001, 0.01, 0.1, 1, 10, 100, 1000))

### kernel polinomial ###
results.poly <- ddply(train.grid.poly, .(degree, cost), function(case){
    svm.x <- svm(Purchase ~ ., data=train.set, 
                 kernel="polynomial", degree=case$degree, cost=case$cost)
    err.train <- sum(as.integer(fitted(svm.x) != train.set$Purchase)) / nrow(train.set)
    err.test <- sum(as.integer(predict(svm.x, test.set) != test.set$Purchase)) / nrow(test.set)
    data.frame(err.train, err.test)
})

best.fit.poly <- results.poly[which.min(results.poly$err.train), ]
#   degree cost err.train  err.test
#35      7 1000   0.09125 0.1851852

best.model.poly <- results.poly[which.min(results.poly$err.test), ]
#degree cost err.train  err.test
#5      3   10   0.15375 0.1814815


ggplot(results.poly, aes(x=degree, y=cost, colour=err.train)) + geom_point() +
    geom_line() + ylab('Error de Entrenamiento')


### kernel radial ###
train.grid.radial <- expand.grid(gamma=c(0.0001, 0.001, 0.01, 0.1, 1), 
                                 cost=c(0.001, 0.01, 0.1, 1, 10, 100, 1000))

results.radial <- ddply(train.grid.radial, .(gamma, cost), function(case){
    svm.x <- svm(Purchase ~ ., data=train.set, 
                 kernel="radial", gamma=case$gamma, cost=case$cost)
    err.train <- sum(as.integer(fitted(svm.x) != train.set$Purchase)) / nrow(train.set)    
    err.test <- sum(as.integer(predict(svm.x, test.set) != test.set$Purchase)) / nrow(test.set)
    data.frame(err.train, err.test)
})

best.fit.radial <- results.radial[which.min(results.radial$err.train), ]
#gamma cost err.train  err.test
#35     1 1000   0.02625 0.2296296

best.model.radial <- results.radial[which.min(results.radial$err.test), ]
#gamma cost err.train  err.test
#6 1e-04  100   0.17375 0.1518519



install.packages("~/Dowloads/NewFusion_1.22.4.tar.gz")

#remove.packages("Rcpp")
#install.packages("Rcpp")


library(ISLR)
data(OJ)
summary(OJ)

library(fusionengine)
initialize.fe(4)

level.1 <- matrix(c(rep(runif(100), 3), as.integer(runif(100) > 0.5)), ncol=4, nrow=100)
level.2 <- matrix(c(rep(runif(100), 3), as.integer(runif(100) > 0.5)), ncol=4, nrow=100)

train.fe(x, x)


?prcomp
