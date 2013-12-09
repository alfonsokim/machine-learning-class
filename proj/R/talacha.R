
image(as.matrix(
    read.csv("~/Documents/MCC/Aprendizaje/Proyecto/componentes.csv", header=F)
))


library(caret)
library(kernlab)
library(plyr)
library(ggplot2)
library(e1071)


todos <- read.csv("~/Documents/MCC/Aprendizaje/Proyecto/todos.csv", header=F)

object.size(todos)

names(todos) <- c("clase", paste("X", 1:60^2, sep=""))

todos.names = names(todos)
todos$clase <- as.factor(todos$clase)
str( todos$clase )

head(todos.names)

?svm
svm.1 <- svm(clase ~ ., data=todos, kernel="polynomial")
svm.2 <- svm(clase ~ ., data=todos, kernel="radial")
svm.3 <- svm(clase ~ ., data=todos, kernel="sigmoid")

svm.4 <- svm(clase ~ ., data=todos, kernel="polynomial", degree=5)
svm.5 <- svm(clase ~ ., data=todos, kernel="polynomial", degree=10)

sum(as.numeric(predict(svm.5) != todos$clase))

uno <- todos[1,]
uno$clase <- NULL
tail(names(uno))

str(uno)
uno[1,]
a <- as.matrix(uno)
?reshape
uno.m <- matrix(as.numeric(uno), ncol=60, nrow=60, byrow=T)
?as.matrix
dim(uno)
dim(uno.m)

str(as.numeric(uno))

str(as.numeric(as))

a <- matrix(1:36000, ncol=60, nrow=60, byrow=F)

image(uno.m)

tail(predict(svm.1), 10)
tail(predict(svm.2), 10)
tail(predict(svm.3), 10)
tail(predict(svm.4), 10)
tail(predict(svm.5), 10)

calificados <- data.frame(clase=todos$clase)
calificados$svm1 <- predict(svm.1)
calificados$svm2 <- predict(svm.2)
calificados$svm3 <- predict(svm.3)
calificados$svm4 <- predict(svm.4)
calificados$svm5 <- predict(svm.5)


perros <- subset(calificados, clase=1)
gatos <- subset(calificados, clase=0)

nrow( subset(calificados, clase == 0 & svm1 == 0) )
nrow( subset(calificados, clase == 0 & svm2 == 0) )
nrow( subset(calificados, clase == 0 & svm3 == 0) )
nrow( subset(calificados, clase == 0 & svm4 == 0) )
nrow( subset(calificados, clase == 0 & svm5 == 0) )


pruebas <- read.csv("~/Documents/MCC/Aprendizaje/Proyecto/trim_test_todos.csv", header=F)

60*59
uno <- pruebas[1,1:60]
dim(uno)
uno.m <- matrix(as.numeric(uno), ncol=60, nrow=60, byrow=T)
str(uno.m)

image(uno.m)


### ================================================================
### ================================================================
### ================================================================

setwd("~/Documents/MCC/Aprendizaje/Proyecto/")
cats <- read.csv('cat_components_stack.csv', header=F)
names(cats) <- paste("x", 1:4033, sep="")
cats$class <- as.factor(0)
dim(cats)

cats$class
dogs <- read.csv('dog_components_stack.csv', header=F)
names(dogs) <- paste("x", 1:4033, sep="")
dogs$class <- as.factor(1)
dim(dogs)

all <- rbind(head(dogs, 1000), head(cats, 1000))
svm.1 <- svm(class ~ ., data=all, kernel="polynomial")

sum(predict(svm.1) != all$class)

str(svm.1)

?svm
poly.grid <- expand.grid(degree=3, cost=c(1))

library(plyr)
ddply(poly.grid, c("degree", "cost"), function(p) {
    svm.x <- svm(class ~ ., data=all, kernel="polynomial", 
                 degree=p$degree, cost=p$cost)
    save(svm.x, file=paste("poly.svm_d", p$degree, "_c", p$cost, sep=""))
    NULL
})

?load
load("poly.svm_d3_c1")


sum(predict(svm.x) == all$class)
