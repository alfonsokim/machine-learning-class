library(ElemStatLearn)
library(plyr)
library(ggplot2)
library(splines)
library(arm)
data(SAheart)

#' Predecir enfermedad del corazón
#' A retrospective sample of males in a heart-disease high-risk region of the Western Cape, South Africa. There are roughly two controls per case of CHD. Many of the CHD positive men have undergone blood pressure reduction treatment and other programs to reduce their risk factors after their CHD event. In some cases the measurements were made after these treatments. These data are taken from a larger dataset, described in Rousseauw et al, 1983, South African Medical Journal.
#' A data frame with 462 observations on the following 10 variables.
#' sbp systolic blood pressure
#' tobacco cumulative tobacco (kg)
#' ldl low density lipoprotein cholesterol
#' adiposity a numeric vector
#' famhist family history of heart disease, a factor with levels Absent Present
#' typea  type-A behavior
#' obesity  a numeric vector
#' alcohol current alcohol consumption
#' age age at onset
#' chd response, coronary heart disease
head(SAheart)
SAheart$id <- 1:nrow(SAheart)
#Primero construimos la base de splines
dat.1 <- ldply(c('sbp', 'tobacco', 'ldl','typea','obesity','age'), function(nom){
  cuant.1 <- quantile(SAheart[,nom], probs=c(0.05,0.95))
  sp.1 <- ns(SAheart[,nom], df=4, Boundary.knots=cuant.1)  
  dat.temp <- data.frame(sp.1)
  names(dat.temp) <- paste0(nom,1:4)
  dat.temp$var <- nom
  dat.temp$id <- SAheart$id
  melt(dat.temp, id.var=c('id','var'))
})

dat.2 <- dcast(dat.1, id~variable)
head(dat.2)
#agregamos también historia familiar
dat.2$famhist <- SAheart$famhist=='Present'

X <- as.matrix(dat.2[,-1])

library(glmnet)
mod.1 <- cv.glmnet(x=X, y=SAheart$chd, alpha=0, family='binomial')
plot(mod.1)

mod.2 <- cv.glmnet(x=X, y=SAheart$chd, alpha=0, family='binomial',
  type.measure='class')
plot(mod.2)


## Coeficientes óptimos
coefs.1 <- coef(mod.1)
coefs.1
#¿Cómo entender la contribución de cada variable?
#Por ejemplo, ¿Cómo contribuye edad?
rownames(coefs.1)
#Tomamos los de edad
coefs.edad <- coefs.1[22:25]
## multiplicamos por la base de splines:
efecto.edad <- X[,21:24]%*%coefs.edad

qplot(SAheart$age, efecto.edad) + geom_line()+ geom_point()

#Historia de fumar
coefs.fumar <- coefs.1[6:9]
efecto.fumar <- X[,5:8]%*%coefs.fumar
qplot(SAheart$tobacco, efecto.fumar) + geom_line()+ geom_point()

## ldl
coefs.ldl <- coefs.1[10:13]
efecto.ldl <- X[,9:12]%*%coefs.ldl
qplot(SAheart$ldl, efecto.ldl) + geom_line()+ geom_point()


## obesity
coefs.ob <- coefs.1[18:21]
efecto.ob <- X[,17:20]%*%coefs.ob
qplot(SAheart$obesity, efecto.ob) + geom_line()+ geom_point()

## Nota: las unidades verticales están en logits. 
## Se puede interpretar con el siguiente truco: Cuando las
## probabilidades estimadas están alrededor de 50%, 
## un efecto en escala logit de x unidades es alrededor de x/4
## puntos porcentuales.