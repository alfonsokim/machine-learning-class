

library(knitr)
library(ggplot2)
library(plyr)
library(reshape2)



h <- function(x){
  1/(1+exp(-x))
}
h.deriv <- function(x){
  exp(x)/((1+exp(x))^2)
}




feed.forward <- function(theta.list, x){
  a.1 <- x
  theta.2 <- theta.list[[2]]
  a.2 <- h(z.2 <- theta.2%*%c(1,a.1))
  
  theta.3 <- theta.list[[3]]
  a.3 <- h(z.3 <- theta.3%*%c(1,a.2))

  theta.4 <- theta.list[[4]]
  a.4 <- h(z.4 <- theta.4%*%c(1,a.3))
  
  list(list(a.1, a.2, a.3, a.4), list(NA, z.2,z.3,z.4))
}




delta.calc <- function(theta.list, az.list, y){
  delta.list <- list()
  delta.list[[4]] <- az.list[[1]][[4]] - y
  delta.list[[3]] <- (t(theta.list[[4]][,-1])%*%delta.list[[4]])*h.deriv(az.list[[2]][[3]])
  delta.list[[2]] <- (t(theta.list[[3]][,-1])%*%delta.list[[3]])*h.deriv(az.list[[2]][[2]])
  delta.list
}




library(ElemStatLearn)
zip.1 <- scale(zip.train[1:5000,2:257])
y <- zip.train[1:5000, 1]
y.ind <- sapply(0:9, function(i){as.numeric(y==i)} )
head(y.ind)



## lista de matrices:
set.seed(280572)
pesos.lista.inicial <- list(
  theta.1 <- NA,
  theta.2 <- matrix(rnorm(257*11,0,0.3), ncol=257),
  theta.3 <- matrix(rnorm(12*11,0,0.3), ncol=12),
  theta.4 <- matrix(rnorm(12*10,0,0.3), ncol=12))




pesos.lista.ant <- pesos.lista.inicial
eta <- 2
for(m in 1:100){
  print(m)
  Delta.lista <- list(
  theta.1 <- NA,
  theta.2 <- matrix(rep(0,257*11), ncol=257),
  theta.3 <- matrix(rep(0,12*11), ncol=12),
  theta.4 <- matrix(rep(0,12*10), ncol=12)
  )
  # calcular gradiente
  devianza.suma <- 0
  for(i in 1:5000){
    az.1 <- feed.forward(pesos.lista.ant, zip.1[i,])
    delta.1 <- delta.calc(pesos.lista.ant, az.1, y.ind[i,])
    for(j in 2:4){
      Delta.lista[[j]] <- Delta.lista[[j]] + 
          (delta.1[[j]])%*%t(c(1, az.1[[1]][[j-1]]))
    }
    dev.cont <- -sum(y.ind[i,]*log(az.1[[1]][[4]]) +
        (1-y.ind[i,])*log(1-az.1[[1]][[4]]))
    devianza.suma <- devianza.suma +dev.cont
  }
  print((2/5000)*devianza.suma)
  grad.lista <- lapply(Delta.lista, function(mat){
    (2/5000)*mat
  })
  
  ## ahora descendemos:
  pesos.lista.nueva <- list()
  for(j in 2:4){
   pesos.lista.nueva[[j]] <- pesos.lista.ant[[j]] - eta*grad.lista[[j]]
  }
  pesos.lista.ant <- pesos.lista.nueva
  
}




az.2 <- feed.forward(pesos.lista.nueva, zip.1[1,])
az.3 <- feed.forward(pesos.lista.nueva, zip.1[50,])
az.4 <- feed.forward(pesos.lista.nueva, zip.1[200,])

y[1]
round(az.2[[1]][[4]],3)
y[50]
round(az.3[[1]][[4]],3)

y[200]
round(az.4[[1]][[4]],3)



center <- attr(zip.1, "scaled:center")
scale <- attr(zip.1, "scaled:scale")
dat.test <- scale(zip.test[,2:257], center = center, scale = scale)
clasif.test <- apply(dat.test, 1, function(x){
  a <- feed.forward(pesos.lista.nueva, x)[[1]][[4]]
  which.max(a)
})
table(clasif.test - 1, zip.test[,1])
mean((clasif.test-1)==zip.test[,1])


