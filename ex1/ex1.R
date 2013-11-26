
data.path <- "/Users/Alfonso/r-workspace/machine-learning/ex1/data"

set.seed(145849)
?read.csv
data <- read.csv(paste(data.path, "data.csv", sep="/"), na.strings=0)

na.counts <- colSums(is.na( data ))
plot(na.counts)
# Hay muchas columnas con un alto numero de NA

# Eliminar las columnas que tienen mas del 5% de valores con NA
max.005 <- nrow(data) * 0.05
na.counts[na.counts > max.005]
good.columns <- names(na.counts[na.counts < max.005])
length(good.columns)

dim(data)
data <- subset(data, select = good.columns)
dim(data)

# Escribir cero en los valores de na
data[is.na(data)] <- 0

## Convertir el Tract a variable categorica
# State, County, Block_Group 
data$Tract <- as.factor(data$Tract)
data$GIDBG <- as.factor(data$GIDBG)
data$State <- as.factor(data$State)
data$County <- as.factor(data$County)
data$Block_Group <- as.factor(data$Block_Group)


# Separar los nombres de las variables segun el censo aplicado
cen.vars <- grep("CEN_2010", names(data), value=TRUE)
acs.vars <- grep("ACS_06_10", names(data), value=TRUE)

length(cen.vars)
length(acs.vars)

cen.vars <- c("GIDBG", "Tract", "State", "County", "Block_Group", cen.vars)
#Probar con solo las variables del CEN2010
data <- subset(data, select=cen.vars)

unique(data$Flag)
subset(data, Flag == 0)
all(!is.na(data$Mail_Return_Rate_CEN_2010))

summary(data$Tract)
table(data$Tract)

summary(data$Mail_Return_Rate_CEN_2010)

plot(data$Mail_Return_Rate_CEN_2010)
ggplot(data, 
       aes(x=Tot_Population_CEN_2010, y=Mail_Return_Rate_CEN_2010)) + 
    geom_point()

ggplot(data, 
       aes(x=Males_CEN_2010, y=Mail_Return_Rate_CEN_2010)) + 
    geom_point()

ggplot(data, 
       aes(x=Renter_Occp_HU_CEN_2010, y=Mail_Return_Rate_CEN_2010)) + 
    geom_point()



## Separar los Tracts
library(plyr)
?count
tract.count <- count(data, "Tract")

sum(tract.count$freq); dim(data)
# la suma de frecuencas es igual al total de datos

## Separar en entrenamiento y pruebas
# 70% de entrenamiento - 30% pruebas
test.size <- ceiling(sum(tract.count$freq) * 0.3)

# Marco como caso de prueba todo el set
data <- cbind(data, train=TRUE)

names(tract.count)
random.tracts <- tract.count[sample(nrow(tract.count)), ] #size=length(tract.count)
nrow(random.tracts); nrow(tract.count)
head(tract.count)
head(random.tracts, 10); head(tract.count, 10)

#consumir tracts hasta tener el 70% de los datos de entrenamiento
test.tracts <- data.frame()
test.count <- 0
random.idx <- 0
str(test.count)
str(test.size)

while(test.count <= test.size){
    test.tracts <- rbind(test.tracts, random.tracts[random.idx, ])
    test.count <- sum(test.count, random.tracts[random.idx, ]$freq)
    random.idx <- random.idx + 1
}

head(random.tracts)
# Verificamos que sea una muestra representativa
test.size; sum(test.tracts$freq)

### Marcar como set de pruebas los tracts generados en el muestre
# Validar
nrow(data[data$Tract == "21618", ]) == test.tracts[test.tracts$Tract == "21618", ]$freq

nrow(data[data$Tract %in% test.tracts$Tract, ])
nrow(data) * 0.3
# ok

# Ahora si, marcar los casos de pruebas 
data[data$Tract %in% test.tracts$Tract, ]$train <- FALSE

nrow(data[data$train, ]) + nrow(data[! data$train, ]) == nrow(data)
#ok

#checar que no haya Tracts en ambas muestras
nrow( data[data$Tract %in% test.tracts$Tract && data$train, ] ) == 0
nrow( data[ data[ data$train, ]$Tract %in% data[! data$train, ]$Tract, ] ) == 0

#### Ejercicio 2: Ajustar un modelo con variables numericas

col.classes <- sapply(data, class)
length(col.classes)
col.classes["Mail_Return_Rate_CEN_2010"]
numeric.col <- col.classes[ col.classes %in% c("integer", "numeric") ]

names(numeric.col)
?subset
numeric.set <- subset(data[data$train, ], select=names(numeric.col))
train.set <- numeric.set[, !( names( numeric.set ) %in% "Mail_Return_Rate_CEN_2010")]
dim(train.set)
train.target <- numeric.set[, "Mail_Return_Rate_CEN_2010"]
head(train.target)
head(train.set)

# Dividir todas las columnas entre la poblacion total
columns <- setdiff(names(numeric.col), c("Tot_Population_CEN_2010", "Mail_Return_Rate_CEN_2010"))
train.set[columns] <- train.set[columns] / train.set$Tot_Population_CEN_2010

numeric.test.set <- subset(data[! data$train, ], select=names(numeric.col))
test.set <- numeric.test.set[, !( names( numeric.test.set ) %in% "Mail_Return_Rate_CEN_2010")]

test.set[columns] <- test.set[columns] / test.set$Tot_Population_CEN_2010

dim(test.set)
dim(train.set)
test.target <- numeric.test.set[, "Mail_Return_Rate_CEN_2010"]
length(test.target)
head(test.set)
names(test.set)

library(glmnet)
?glmnet
model.1 <- glmnet(x=as.matrix(train.set), y=train.target,
              alpha=1, nlambda=100, intercept=T)

plot(model.1, xvar="lambda")

model.1$lambda

coef(model.1)
str(model.1)

?predict.glmnet
ncol(test.set); length(coef(model.1)[, 50])
head(cbind(1, test.set))
as.matrix(cbind(1, test.set)) %*% coef(model.1)[, 50]
length(coef(model.1)[, 50][-1])
nrow(as.matrix(test.set))
model.1$lambda[90]

pred.values.mod.1 <- ldply(1:length(model.1$lambda), function(i){
    m.pred = as.matrix(cbind(1, test.set)) %*% coef(model.1)[, i]
    #m.pred = as.matrix(test.set) %*% coef(model.1)[, i][-1]
    test.results <- data.frame(
        i=i,
        m.lambda = model.1$lambda[i],
        error = mean( (m.pred - test.target) ^ 2 )
    )
    test.results
})

#library(ggplot2)
qplot(log(pred.values.mod.1$m.lambda), pred.values.mod.1$error)

head(train.set)

?cv.glmnet
cv.mod.lasso <- cv.glmnet(x=as.matrix(train.set), y=train.target,
                          alpha = 1, intercept = T, 
                          nfolds = 5, nlambda = 50) 

plot(cv.mod.lasso)

?predict.glmnet
coefs.lasso <- predict(cv.mod.lasso, 
                       s = exp(-2), type = "coef") 

round(coefs.lasso, 2)

test.preds <- predict(cv.mod.lasso, newx=as.matrix(test.set), s=exp(-2))
length(test.target)
length(test.preds)

preds.df <- data.frame(y=test.preds, x=test.target)
head(preds.df)

?geom_abline
library(ggplot2)
ggplot(preds.df, aes(x=x, y=X1)) + geom_point() +
    geom_abline(slope = 1, intercept = 0, colour="red", size=2)


predict.coefs <- as.matrix(coefs.lasso)



plot(data$Mail_Return_Rate_CEN_2010)
ggplot(data, 
       aes(x=Tot_Population_CEN_2010, y=Mail_Return_Rate_CEN_2010)) + 
    geom_point()

ggplot(data, aes(x=Tot_Population_CEN_2010, y=Mail_Return_Rate_CEN_2010)) + 
    geom_point() +
    geom_point(aes(x=Males_CEN_2010, colour="Males_CEN_2010")) + 
    geom_point(aes(x=Pop_18_24_CEN_2010, colour="Pop_18_24_CEN_2010")) +
    geom_point(aes(x=Rel_Child_Under_6_CEN_2010, colour="Rel_Child_Under_6_CEN_2010"))


# Probar con el set de pruebas
test.values.mod.1 <- ldply(1:length(cv.mod.lasso$lambda), function(i){
    lambda <- cv.mod.lasso$lambda[i]
    test.preds <- predict(cv.mod.lasso, newx=as.matrix(test.set), s=lambda)
    test.results <- data.frame(
        lambda = lambda, 
        error = mean( (test.target - test.preds) ^ 2 ) 
    )
})

ggplot(test.values.mod.1, aes(x=lambda, y=error)) + 
    geom_point()

# Agregar variables indicadoras
?model.matrix

?save
save(list=ls(), file="~/r-workspace/machine-learning/ex1/ex1.Rdata")

load("~/r-workspace/machine-learning/ex1/ex1.Rdata")




