library(shiny)
runApp("/Users/Alfonso/Documents/MCC/Aprendizaje/clase2/regresion_logistica")

rss <- function(beta){
  res <- prostate$lpsa - beta[1] - prostate$lcavol*beta[2]
  sum(res^2)
}

rss.gradiente <- function(beta){
  gradiente <- rep(NA, 2)
  g.beta <- beta[1] + beta[2]*prostate$lcavol
  temp <- g.beta-prostate$lpsa
  gradiente[1] <- 2*mean(temp)
  gradiente[2] <- 2*mean(temp*prostate$lcavol)
  gradiente
}

betas <- list()
z.ant <- c(0,0)
eta <- 0.8
for(i in 1:30){
  z.nueva <- z.ant - eta*rss.gradiente(z.ant)
  betas[[i]] <- c(z.nueva, rss(z.nueva))
  z.ant <- z.nueva
}

plot(sapply(betas, function(x){log(x[3])}))


## Iterar - descenso en gradiente

## regla de paro.