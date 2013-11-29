
image(as.matrix(
    read.csv("~/Documents/MCC/Aprendizaje/Proyecto/cat_array/10.csv", header=F)
))

library(caret)
library(kernlab)
library(plyr)
library(ggplot2)
library(e1071)


todos <- read.csv("~/Documents/MCC/Aprendizaje/Proyecto/todos.csv", header=F)

object.size(todos)

names(todos) <- c("clase", paste("X", 1:128^2, sep=""))

todos.names = names(todos)
head(todos.names)

?svm
svm.1 <- svm(clase ~ ., data=todos, kernel="radial", gamma=0.0001, cost=0.0001)

svm.1 <- svm(clase ~ ., data=todos, kernel="polynomial")




chptdat = read.table("http://www.stat.psu.edu/~mharan/MCMCtut/COUP551_rates.dat",skip=1) 
Y=chptdat[,2] # store data in Y
ts.plot(Y,main="Time series plot of change point data")