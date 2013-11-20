library(glmnet)


# Simular datos
n <- 20
set.seed(2805)

#Construir variables x
# --- Agregar componentes que correlacionan las X's ---
comp.1 <- rnorm(n, 1, 3)
comp.2 <- rnorm(n, 2, 4)
x.1 <- rnorm(n) + comp.1
x.2 <- rnorm(n) + comp.1
x.3 <- rnorm(n) + comp.2
x.4 <- rnorm(n) + comp.2

# construir y
y <- 5 + 2*x.1 + 6*x.2  -2*x.3 - 3*x.4 + rnorm(n,0,10)
datos <- data.frame(x.1,x.2,x.3,x.4,y)



# Regresión ridge, ¿qué coeficientes se encogen juntos?
mod.ridge <- glmnet(x= as.matrix(datos[,1:4]), y=datos$y, alpha=0)
plot(mod.ridge, xvar='lambda')

matplotlib()
