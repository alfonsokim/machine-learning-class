
library(kknn)
library(MASS)

#install.packages('ggplot2', dep = TRUE)

library(ggplot2)
Pima.tr
vmc.4 <- kknn(type~glu, train=Pima.tr, 
  test= data.frame(glu=seq(50,200, by=1)),
  k = 5, kernel='rectangular')

data(Pima.tr)

Pima.tr

# npreg glu  bp skin  bmi   ped age type
typeof(Pima.tr)
vmc.4 <- kknn(type~glu, train=Pima.tr, 
              test=Pima.te,
              k = 5, kernel='rectangular')

mean(vmc.4$fitted.values != Pima.te$type)


vmc.4 <- kknn(type~glu, train=Pima.tr, 
              test=Pima.te,
              k = 97, kernel='rectangular')

mean(vmc.4$fitted.values != Pima.te$type)


vmc.diabetes <- function (a) {
  vmc.4 <- kknn(type~glu, train=Pima.tr, 
              test=Pima.te,
              k = a, kernel='rectangular')
  mean(vmc.4$fitted.values != Pima.te$type)
}

plot(sapply(1:100, vmc.diabetes))



dat.pred <- data.frame(glu=seq(50,200, by=1),
                       prob=vmc.4$prob[,2])

ggplot(Pima.tr, aes(x=glu, y=as.numeric(type=='Yes'))) +
  geom_point() +
  geom_line(data=dat.pred, aes(x=glu, y=prob), col='red')

#probabilidades estimadas
vmc.4$prob

dat.pred <- data.frame(glu=seq(50,200, by=1),
  prob=vmc.4$prob[,2])
ggplot(Pima.tr, aes(x=glu, y=as.numeric(type=='Yes'))) +
  geom_point() +
  geom_line(data=dat.pred, aes(x=glu, y=prob), col='red')


