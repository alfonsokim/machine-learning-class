library(ggplot2)

data <- read.table("/Users/Alfonso/r-workspace/machine-learning/l2/hw/housing.data", sep="\t")

colnames(data) <- c("CRIM", "ZN", "INDUS", "CHAS", "NOX", "RM", "AGE", "DIS", 
                    "RAD", "TAX", "PTRATIO", "B", "LSTAT", "MEDV")

summary(data)

train.size <- 400
set.seed(840424)
train.idx <- sample(seq_len(nrow(data)), size=train.size)
train.set <- data[train.idx, ]
test.set <- data[-train.idx, ]

summary(train.set)
summary(test.set)

mod.1 <- lm(NOX ~ DIS, data=train.set)
summary(mod.1)

X.pred <- data.frame( DIS=test.set$DIS )
X.pred$NOX <- predict(mod.1, newdata=X.pred )

ggplot(test.set, aes(x=DIS, y=NOX)) +
    geom_jitter() + 
    geom_line(data=X.pred, col="red", size=2)

train.set$DIS.2 <- train.set$DIS^2
mod.2 <- lm(NOX ~ DIS + DIS.2, data=train.set)

X.pred.2 <- data.frame( DIS=test.set$DIS )
X.pred.2$NOX <- predict( mod.2, newdata=X.pred )

ggplot(test.set, aes(x=DIS, y=NOX)) +
    geom_jitter() + 
    geom_line(data=X.pred.2, col="red", size=2)

train.set.2 <- data.frame(DIS=train.set$DIS, NOX=train.set$NOX)
test.set.2 <- data.frame(DIS=test.set$DIS, NOX=test.set$NOX)

#train.set$DIS.2 <- train.set$DIS^2
#mod.2 <- lm(NOX ~ DIS + DIS.2, data=train.set)

polinomial <- function(data.set, train.column, predict.column) {
    values <- list()
    for(i in 1:10){
        values[[ paste("pow", i, sep=".") ]] <- i
        train.data <- data.set[train.column]
        values[[ paste("val", i, sep=".") ]] <- train.data^i
        model <- lm(NOX ~ I(DIS + DIS^i), 
                    data=data.frame(X=train.data, Y=data.set[predict.column]))
        values[[ paste("model", i, sep=".") ]] <- model
    }
    values
}

joe <- polinomial(train.set, "DIS", "NOX")

for (value in joe){
    print(value["pow"])
}



train.set.ext <- data.frame(DIS=train.set$DIS, DIS.2=train.set$DIS^2, DIS.3=train.set$DIS^3,
                            DIS.4=train.set$DIS^4, DIS.5=train.set$DIS^5, DIS.6=train.set$DIS^6,
                            DIS.7=train.set$DIS^7, DIS.8=train.set$DIS^8, DIS.9=train.set$DIS^9,
                            DIS.10=train.set$DIS^10, NOX=train.set$NOX)
mod.ext <- lm(NOX ~ DIS + DIS.2 + DIS.3 + DIS.4 + DIS.5 +
                  DIS.6 + DIS.7 + DIS.8 + DIS.9 + DIS.10, data=train.set.ext)

X.pred.ext <- data.frame( DIS=test.set$DIS )
X.pred.ext$NOX <- predict( mod.ext, newdata=X.pred.ext )

ggplot(test.set, aes(x=DIS, y=NOX)) +
    geom_jitter() + 
    geom_line(data=X.pred.ext, col="red", size=2)


#assign(paste("test.set.dummy$t", 1, sep=""), train.set.2)
