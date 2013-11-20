library(ggplot2)
#install.packages("arm")
library(arm)
set.seed(12)

diamonds.sample <- diamonds[sample(1:nrow(diamonds), 1000),]
ggplot(diamonds.sample, aes(x = price, y = carat)) + geom_point() +
    geom_smooth(method = "lm")

diamonds.sample$log.price <- log(diamonds.sample$price)
diamonds.sample$log.carat <- log(diamonds.sample$carat)
ggplot(diamonds.sample, aes(x = log.price, y = log.carat)) +
    geom_point() + geom_smooth(method = "lm")


diamonds.sample$log.price <- log(diamonds.sample$price)
diamonds.sample$log.carat <- log(diamonds.sample$carat)
ggplot(diamonds.sample, aes(x = price, y = carat)) + geom_point() +
    geom_smooth(method = "lm") + 
    scale_y_log10(breaks = c(0.25,0.5, 1, 2)) + 
    scale_x_log10(breaks = c(500, 1000, 2000, 4000, 8000))

table(diamonds.sample$cut)

ggplot(diamonds.sample, aes(x = price, y = carat, colour = cut, group = cut)) + 
    geom_point(alpha = 0.5) + 
    geom_smooth(method = "lm",se = FALSE, size = 1.5) + 
    scale_y_log10(breaks = c(0.25,0.5, 1, 2)) + 
    scale_x_log10(breaks = c(500, 1000, 2000,4000, 8000))


?as.character
mat.1 <- diamonds.sample[, c("carat", "price", "cut")]
head(mat.1)

mat.1$cut <- as.character(mat.1$cut)
mat.2 <- model.matrix(carat ~ price + cut, data = mat.1)
head(mat.2)


mat.2 <- data.frame(mat.2)[, -1]
mat.2$carat <- mat.1$carat
mod.x <- lm(carat ~ ., data = mat.2)
summary(mod.x)


??display
diamonds.sample$cut <- as.character(diamonds.sample$cut)
mod.2 <- lm(carat ~ price + cut, data = diamonds.sample)
str(mod.2)
display(mod.2)

diamonds.sample$cut <- as.character(diamonds.sample$cut)
mod.2 <- lm(log(carat) ~ log(price) + cut, data = diamonds.sample)
str(mod.2)
display(mod.2)


# Interacciones
head(airquality)
lm(Ozone ~ Temp, data = airquality)
ggplot(airquality, aes(x = Temp, y = Ozone)) + geom_point()


?cut
?cut2
library(Hmisc)
airquality$Wind.cut <- cut2(airquality$Wind, g = 3)
airquality$Wind.cut4 <- cut2(airquality$Wind, g = 4)

?facet_wrap
ggplot(airquality, aes(x = Temp, y = Ozone)) + geom_point() +
    facet_wrap(~Wind.cut) + geom_smooth(method = "lm", se = FALSE)

ggplot(airquality, aes(x = Temp, y = Ozone)) + geom_point() +
    facet_wrap(~Wind.cut4) + geom_smooth(method = "lm", se = FALSE)

table(airquality$Wind.cut)

airquality$Temp.c <- with(airquality, Temp - mean(Temp))
airquality$Wind.c <- with(airquality, Wind - mean(Wind))
airquality$Temp.cxWind.c <- with(airquality, Temp.c * Wind.c)
mod.int <- lm(Ozone ~ Temp.c + Wind.c + Temp.cxWind.c, data = airquality)
str(mod.int)
display(mod.int)


