library(plyr)
library(reshape)
library(ggplot2)
library(kernlab)

setwd('/Users/jigr/Documents/FinalAdM/ModelosFinal')

cats <- read.csv('cat_components_stack.csv', header=F)
names(cats) <- paste("x", 1:4033, sep="")
cats$class <- as.factor(0)
nCats <- nrow(cats)

dogs <- read.csv('dog_components_stack.csv', header=F)
names(dogs) <- paste("x", 1:4033, sep="")
dogs$class <- as.factor(1)
nDogs <- nrow(dogs)

all <- rbind(dogs, cats)
nAll <- nrow(all)
rIndex <- rank(rnorm(nAll))
all <- all[rIndex,]

# vanilla C=1
curva.ap <- ldply(seq(250, 3500, 250), function(i) {
  m <- ksvm(class~., data=head(all, i), type="C-svc", kernel="vanilladot", C = 1, kpar="automatic", scaled=c())  
  data.frame(i = i, error.entrena = error(m), svCount=length(SVindex(m))/i, 
    error.valida = mean(predict(m, tail(all, nAll-i))!=tail(all$class, nAll-i)))
})

curva.ap.m <- melt(curva.ap, id.var = "i")
ggplot(curva.ap.m, aes(x = i, y = value, colour = variable)) + geom_point() + geom_line() + 
  ylab("Tasa de incorrectos")


# vanilla C=0.01
curva.ap <- ldply(seq(500, 3500, 500), function(i) {
  m <- ksvm(class~., data=head(all, i), type="C-svc", kernel="vanilladot", C = 0.01, kpar="automatic", scaled=c())  
  data.frame(i = i, error.entrena = error(m), svCount=length(SVindex(m))/i, 
      error.valida = mean(predict(m, tail(all, nAll-i))!=tail(all$class, nAll-i)))
})

curva.ap.m <- melt(curva.ap, id.var = "i")
ggplot(curva.ap.m, aes(x = i, y = value, colour = variable)) + geom_point() + geom_line() + ylab("Tasa de incorrectos")


# RBF
curva.ap <- ldply(seq(250, 2000, 250), function(i) {
  m <- ksvm(class~., data=head(all, i), type="C-svc", kernel="rbfdot", C = 1, kpar=list(sigma=0.01), scaled=c())  
  data.frame(i = i, error.entrena = error(m), error.valida = mean(predict(m, tail(all, nAll-i))!=tail(all$class, nAll-i)))
  data.frame(i = i, error.entrena = error(m), svCount=length(SVindex(m))/i, 
    error.valida = mean(predict(m, tail(all, nAll-i))!=tail(all$class, nAll-i)))
})

curva.ap.m <- melt(curva.ap, id.var = "i")
ggplot(curva.ap.m, aes(x = i, y = value, colour = variable)) + geom_point() + geom_line() + ylab("Tasa de incorrectos")

# Polynomial
curva.ap <- ldply(seq(500, 4000, 500), function(i) {
  m <- ksvm(class~., data=head(all, i), type="C-svc", kernel="polydot", C = 10, kpar=list(degree=3), scaled=c())  
  data.frame(i = i, error.entrena = error(m), error.valida = mean(predict(m, tail(all, nAll-i))!=tail(all$class, nAll-i)))
  data.frame(i = i, error.entrena = error(m), svCount=length(SVindex(m))/i, 
    error.valida = mean(predict(m, tail(all, nAll-i))!=tail(all$class, nAll-i)))
})

nrow(curva.ap)
curva.ap.m <- melt(head(curva.ap,7), id.var = "i")
ggplot(curva.ap.m, aes(x = i, y = value, colour = variable)) + geom_point() + geom_line() + ylab("Tasa de incorrectos")