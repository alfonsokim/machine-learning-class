library(MASS)
library(plyr)
library(ggplot2)
head(Boston)
library(splines)

modelos <- lapply(4:20, function(grado){
  lm(nox~bs(dis, df=grado), data = Boston)
})


res.1 <- ldply(modelos, function(mod){
  data.frame(grado=(mod$rank-1) ,dis=Boston$dis, nox = Boston$nox, 
    pred.nox = predict(mod, newdata = Boston))
})

#df.etiquetas <- data.frame(x=5,y=0.5, hola=paste('hola',1:10), grado=1:10)

ggplot(res.1, aes(x=dis, y=nox)) + geom_point(size=1) + facet_wrap(~grado) +
  geom_line(aes(y=pred.nox), colour='red',size=1.5) 
#+
#  geom_text(data=df.etiquetas, aes(x=x, y=y, label=hola), col='green')