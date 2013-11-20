library(ElemStatLearn)
dim(zip.train)
## [1] 7291 257
dim(zip.test)
## [1] 2007 257
mat.coeficientes <- matrix(NA, nrow = 256 + 1, ncol = 10)
for (i in 0:9) {
    clase.bin <- as.numeric(zip.train[, 1] == i)
    dat <- data.frame(clase = clase.bin, zip.train[, -1])
    modelo <- glm(clase ~ ., data = dat)
    mat.coeficientes[, i + 1] <- coef(modelo)
}


h <- function(x) {
    exp(x)/(1 + exp(x))
}
pred.digito <- function(mat, beta) {
    prob.pred <- h(cbind(1, mat) %*% beta)
    apply(prob.pred, 1, which.max) - 1
}

library(ElemStatLearn)
clasif.entrena <- pred.digito(zip.train[, -1], mat.coeficientes)
head(clasif.entrena)


library(reshape2)
library(Hmisc)
mat.2 <- melt(mat.coeficientes[-1, ])
mat.2$y <- -(mat.2$Var1%/%16)
mat.2$x <- mat.2$Var1%%16
mat.2$Var2 <- mat.2$Var2 - 1
ggplot(mat.2, aes(x = x, y = y)) + geom_tile(aes(fill = cut2(value,
                                                             g = 3))) + facet_wrap(~Var2)

