## =======================================
## Aprendizaje Maquina
## -------------------
## Maestria en Ciencias de Datos / Computacion
## Jose Ivan Garcia / Alfonso Kim
## =======================================

library(plyr)
library(e1071)
library(ggplot2)

## --------------------------------------------------
## Cargar los datos resultantes del proceso de Python
setwd("~/Documents/MCC/Aprendizaje/Proyecto/")
cats <- read.csv('cat_components_stack.csv', header=F)
names(cats) <- paste("x", 1:4033, sep="")
cats$class <- as.factor(0)

dogs <- read.csv('dog_components_stack.csv', header=F)
names(dogs) <- paste("x", 1:4033, sep="")
dogs$class <- as.factor(1)

## Los primeros 4000 imagenes de cada especie se usan
## para entrenar los modelos
all <- rbind(head(dogs, 4000), head(cats, 4000))

## Generar la matriz de configuracion de modelos
poly.grid <- expand.grid(degree=3:10, cost=c(0.01, 0.1, 1, 10, 100))
radial.grid <- expand.grid(gamma=c(0.01, 0.1, 1, 10), cost=c(0.01, 0.1, 1, 10))

## -----------------------------------------------------
## Generar modelos con las diferentes configuraciones
#  Para optimizar el proceso, cada vez que se entrena un
#  modelo se guarda en un archivo Rdata para despues
#  cargarlo en cuanto este listo
poly.svm <- ddply(poly.grid, c("degree", "cost"), function(p) {
    svm.x <- svm(class ~ ., data=all, kernel="polynomial", 
                 degree=p$degree, cost=p$cost) 
    save(svm.x, file=paste("poly.svm_d", p$degree, "_c", p$cost, ".Rdata", sep=""))
    NULL
})

radial.svm <- ddply(radial.grid, c("gamma", "cost"), function(p) {
    svm.x <- svm(class ~ ., data=all, kernel="radial", 
                 gamma=p$gamma, cost=p$cost)
    save(svm.x, file=paste("radial.svm_g", p$gamma, "_c", p$cost, ".Rdata", sep=""))
    NULL
})

## Separar los ultimos datos del archivo para pruebas
dogs.test <- tail(dogs, 400)
cats.test <- tail(cats, 400)

test.all <- rbind(dogs.test, cats.test)
# Para estar seguros se le quita la clase a los datos
test.class <- test.all$class
test.all$class <- NULL

## Se carga cada modelo entrenado y se obtienen los
## valores de error de entrenamiento y pruebas
## ------------------------------------------------
model.files <- data.frame(file=list.files())

results.svm <- ddply(model.files, "file", function(file){
    print(as.character(file$file))
    load(as.character(file$file))
    train.error <- ( sum(as.numeric( predict(svm.x) != all$class ) ) / nrow(all) )
    preds <- predict(svm.x, newdata=test.all)
    test.error <- ( sum(as.numeric( preds != test.class )) / length(test.class) )
    data.frame(
        model=gsub(".Rdata", "", file$file),
        train.error=train.error, 
        test.error=test.error
    )
})

## Se grafican los errores
## -----------------------
ggplot(results.svm, aes(x=model, y=train.error)) + 
    geom_bar(position="dodge",stat="identity") +
    theme(axis.text.x=element_text(angle=90,hjust=1,vjust=.5,colour='black'))

ggplot(results.svm, aes(x=model, y=test.error)) + 
    geom_bar(position="dodge",stat="identity") +
    theme(axis.text.x=element_text(angle=90,hjust=1,vjust=.5,colour='black'))


?svm

load("~/Documents/MCC/Aprendizaje/Proyecto/rdata/poly.svm_d3_c1.Rdata")

predict.2 <- predict(svm.x, newdata=test.all)
sum(predict.2 != test.class)
