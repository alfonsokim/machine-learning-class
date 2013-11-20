

library(knitr)
library(ggplot2)
library(plyr)
library(reshape2)
options(replace.assign=TRUE,width=55)
#knit_hooks$set(par=function(before, options, envir){if (before) par(mar=c(4,4,.1,.1),cex.lab=.95,cex.axis=.9,mgp=c(2,.7,0),tcl=-.3)})



library(ggplot2)
library(arm)
set.seed(12)
diamonds.sample <- diamonds[sample(1:nrow(diamonds), 1000), ]
ggplot(diamonds.sample, aes(x=price, y=carat)) + geom_point() + geom_smooth(method='lm')



diamonds.sample$log.price <- log(diamonds.sample$price)
diamonds.sample$log.carat <- log(diamonds.sample$carat)
ggplot(diamonds.sample, aes(x=log.price, y=log.carat)) + geom_point() + 
  geom_smooth(method='lm')



diamonds.sample$log.price <- log(diamonds.sample$price)
diamonds.sample$log.carat <- log(diamonds.sample$carat)
ggplot(diamonds.sample, aes(x=price, y=carat)) + geom_point() + 
  geom_smooth(method='lm') + scale_y_log10(breaks=c(0.25,0.5,1,2))+
  scale_x_log10(breaks=c(500,1000,2000,4000,8000))



table(diamonds.sample$cut)
ggplot(diamonds.sample, aes(x=price, y=carat, colour=cut, group=cut)) + geom_point(alpha=0.5) + 
  geom_smooth(method='lm', se=FALSE, size=1.5) + scale_y_log10(breaks=c(0.25,0.5,1,2))+
  scale_x_log10(breaks=c(500,1000,2000,4000,8000))



mat.1 <- diamonds.sample[, c('carat', 'price', 'cut')]
head(mat.1)
mat.1$cut <- as.character(mat.1$cut)
mat.2 <- model.matrix(carat ~ price+cut, data=mat.1)
head(mat.2)
mat.2 <- data.frame(mat.2)[,-1]
mat.2$carat <- mat.1$carat
mod.x <- lm(carat ~ . ,  data = mat.2)
summary(mod.x)



diamonds.sample$cut <- as.character(diamonds.sample$cut)
mod.2 <- lm(carat ~ price + cut, data=diamonds.sample)
display(mod.2)



diamonds.sample$cut <- as.character(diamonds.sample$cut)
mod.2 <- lm(log(carat) ~ log(price) + cut, data=diamonds.sample)
display(mod.2)


#Interacciones
head(airquality)
lm(Ozone ~ Temp, data = airquality)
ggplot(airquality, aes(x=Temp, y=Ozone)) + geom_point()


head(airquality)
library(Hmisc)
?cut2
airquality$Wind.cut <- cut2(airquality$Wind, g=3)

table(airquality$Wind.cut)
ggplot(airquality, aes(x=Temp, y=Ozone)) + geom_point() +
  facet_wrap(~Wind.cut) + geom_smooth(method='lm', se=FALSE)


airquality$Temp.c <- with(airquality,Temp - mean(Temp))
airquality$Wind.c <- with(airquality, Wind - mean(Wind))
airquality$Temp.cxWind.c <- with(airquality, Temp.c*Wind.c)
mod.int <- lm(Ozone ~ Temp.c + Wind.c + Temp.cxWind.c, data=airquality)
display(mod.int)



library(ISLR)
head(Wage)
mod.1 <- lm(wage ~ age, data=Wage)
display(mod.1)
Wage$age.2 <- Wage$age^2
mod.2 <- lm(wage ~ age + age.2, data=Wage)
display(mod.2)
Wage$age.3 <- Wage$age^3
Wage$age.4 <- Wage$age^4
mod.3 <- lm(wage ~ age + age.2 + age.3 + age.4, data=Wage)



age <- seq(20, 80, 1)
X.pred <- data.frame(age = age)
X.pred$wage <- predict(mod.1, newdata = X.pred)
ggplot(Wage, aes(x=age, y=wage)) + geom_jitter() +
  geom_line(data = X.pred, col='red', size=2)



X.pred <- data.frame(age = age, age.2 = age^2)
X.pred$wage <- predict(mod.2, newdata = X.pred)
ggplot(Wage, aes(x=age, y=wage)) + geom_jitter() +
  geom_line(data = X.pred, col='red', size=2)



X.pred <- data.frame(age = age, age.2 = age^2, age.3 = age^3, age.4=age^4)
X.pred$wage <- predict(mod.3, newdata = X.pred)
ggplot(Wage, aes(x=age, y=wage)) + geom_jitter() +
  geom_line(data = X.pred, col='red', size=2)



qplot(Wage$age)



qplot(Wage$age) + geom_vline(xintercept = c(30,65), col = 'red')



Wage$age.cut <- cut2(Wage$age, cuts=c(30,65))
mod.q <- lm(wage ~ age.cut , data=Wage)
display(mod.q)



age <- seq(18, 80, 1)
age.q <- cut2(age, cuts=c(30,65))
X.pred <- data.frame(age.cut= age.q)
X.pred$wage <- predict(mod.q, newdata = X.pred)
ggplot(Wage, aes(x=age, y=wage)) + geom_jitter() +
  geom_line(data = X.pred, col='red', size=2)


