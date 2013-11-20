

## Aquí usamos las estimaciones de glm
library(MASS)
summary(Pima.tr)



mod.x <- glm(type ~ ., data = Pima.tr, family='binomial')
coef(mod.x)


### Aquí usar Pima.te para calcular especificidad y sensibilidad
prob.pred <- predict(mod.x, newdata=Pima.te, type='response' )

##
##
##

## Comparar con modelo de una variable:


mod.1 <- glm(type ~ glu, data = Pima.tr, family='binomial')
coef(mod.x)

prob.pred.1 <- predict(mod.1, newdata = Pima.te, type='response' )

##
##
##


