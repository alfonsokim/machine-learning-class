#' Minicurso de R - febrero 15, 2013. Estadística aplicada I
#' ==========================================================

#' - R es un lenguaje de programación y un ambiente de cómputo estadístico
#' - R es software libre (no dice qué puedes o no hacer con el software), de código abierto (todo el código de R se puede inspeccionar - y se inspecciona).
#' - Cuando instalamos R, instala la base de R. Mucha de la funcionalidad adicional está en *paquetes* que la comunidad contribuye.



#' ## ¿Cómo entender R?
#' 1. Hay una sesión de R corriendo. La **consola** de R es nuestra interfaz entre R y nosotros. 
#' 2. En la sesión hay **objetos**. Todo en R es un objeto: vectores, tablas, funciones, etc.
#' 3. Operamos aplicando funciones a los objetos y creando nuevos objetos.

#' Ejemplo
foo <- 2 + 13
foo
bar <- foo^2 -1
bar

#' Listamos objetos en memoria:
ls()

#' Creamos un vector numérico, calculamos media y sd
vec.1 <- c(1, -1, 0, 2, 0, 3)
vec.1
mean(vec.1)
sd(vec.1)

ls()

qplot(vec.1, vec.1^2)

#' Funcionalidad adicional está en paquetes de R. Podemos instalar nuevos (sólo la primera vez) en la pestaña "packages" en RStudio. Para este ejemplo, instalar plyr, ggplot2, stringr, lubridate, reshape2, ddply.

#' Una vez instalados, los paquetes se pueden cargar usando
library(ggplot2)
library(stringr)
library(lubridate)
library(reshape2)
library(plyr)

#' Por ejemplo, la funcion `qplot` está en `ggplot2`
qplot(vec.1, vec.1^2)

#' Podemos cargar datos de varias maneras. Archivos separados
#' por comas/tabs de texto se pueden cargar con la función read.csv

#' ## IMPORTANTE:
#' Cambiar el directorio de trabajo (de la consola) a donde está el archivo que queremos cargar (navegar en Files, Plots, Packages al directorio y luego en More ->  Set as working directory)

mis.datos <- read.csv("SIMAT_2009.tsv", sep = "\t")

#' La función head muestra las primeras líneas del archivo que cargamos

head(mis.datos)
nrow(mis.datos)
#' Funcionó, pero está hecho un desastre (nombres de variables, valores faltantes codificados con -99, etc.)

#' ## Más de objetos en R

#' Los objetos tienen una clase asociada.
class(foo)
class(vec.1)
mi.cadena <- "Hola"
class(mi.cadena)
class(mis.datos)

#' La clase determina la estructura de los objetos, la forma de los datos que contienen, y cómo se comportan bajo funciones distintas.

mat.1 <- matrix(vec.1, ncol = 2, byrow = TRUE)
mat.1
class(mat.1)

#' Por ejemplo, la función plot hace algo diferente aplicada a un vector o a una matriz
plot(mat.1)
plot(vec.1)

#' Tip: usa la función `str` para averiguar acerca de los objetos
str(mat.1)
str(vec.1)
str(mis.datos)

#' ## Funciones en R.

#' Aplicar funciones:
max(vec.1)
sort(vec.1)
#' Con parámetros adicionales
sort(vec.1, decreasing = TRUE)

#' Ayuda de la función `sort`
?sort

#' En R, las funciones:
#' - Reciben  parámetros por nombre y por posición. Lo mejor es pasarlos por nombre.
#' - No siempre es necesario pasar TODOS los parámetros; tipicamente existen valores por defecto que hacen más fácil la rutina de programar.

#' Por ejemplo, de la ayuda vemos que sort, por defecto, pone decreasing=TRUE. No es necesario pasar este parámetro si este default es aceptable:
sort(vec.1)

#' Pero si queremos ordenar de forma decreciente, pasamos el parámetro
sort(vec.1, decreasing = TRUE)


#' Ejemplo: examinar la función quantile. Parámetros disponibles? Valores por defecto?

#' Es posible crear nuestras propias funciones. 

#' ## Funciones y vectores.
#' Muchas funciones de R están `vectorizadas`

#' Por ejemplo, la función +
vec.1 + 3

#' La función cuadrado
vec.1
vec.2 <- vec.1^2
vec.1
vec.1*vec.1

#' Suma y cuadrado operan componente a componente.

#' Otro ejemplo
vec.1 > 0 
vector.mayor.0 <- vec.1 > 0
vector.mayor.0




#' ## Subindexar vectores
vec.1[c(1,4,5)]
elementos <- c(1,3,2)
vec.1[elementos]

#' Subindexar con  lógicos
elementos.2 <- vec.1 > 0
vec.1[elementos.2]

#' subindexar matrices y tablas
mat.1[1,2] # un elemento
mat.1[c(1,2), ] # dos renglones, todas las columans
mat.1[1:2, ] # lo mismo que el anterior
mat.1[1:2,1] # una columna, dos renglones
mat.1[1:2,1, drop=FALSE] # para que no convierta a vector

#' Y para una tabla:
mis.datos[1:5, c(2,5,6)]
vec.3 <- mis.datos[ , 2]  # esto es un vector, la 2a columna
vec.3[1:20]

#' Subindexar renglones que tengan medición COPED_.ppm. > 1?
indice <- mis.datos[ , 2] > 1
sub.datos <- mis.datos[indice,   ]
nrow(sub.datos)


#'  También se pueden extraer columnas usando notación de $
horas <- mis.datos$HORA
class(horas)
#' Ejercicio: filtrar las mediciones correspondientes a las 2 pm.



######################################################
#' Ahora regresamos a los datos que cargamos:

#la clase de "mis.datos" es un data.frame
class(mis.datos)

?data.frame
# en la ayuda vemos que un data.frame es una lista con
# algunas propiedades y datos adicionales.
# Las componentes de la lista son las variables:

names(mis.datos)
mis.datos$HORA
class(mis.datos$HORA)
length(mis.datos)
nrow(mis.datos)
str(mis.datos)

##Pero también se comporta como matriz: la función
##corchetes de data.frame es como la que vimos arriba para matrices:

mis.datos[1,]
mis.datos[1:5,]
mis.datos[,2]

sub.datos <- mis.datos[1:20, c(1,2)]
class(sub.datos)
nrow(sub.datos)
sub.datos



#' Ejemplo
#' ===========
#' Ahora un ejemplo con todo lo que hemos usado (y algunas cosas adicionales)
#' Pregunta: cómo se relaciona la temperatura con la medición  de ozono para la estación Merced, en cada uno de las horas del día?

names(mis.datos)

#' Identificamos las variables que nos interesan

merced <- mis.datos[ , c(2, 38)]
head(merced)
#' Es más fácil y seguro usar subset
merced <- subset(mis.datos, select = c("FECHA", "HORA", "O3MER_.ppb.", "TMPMER_.gradosC."))
head(merced)


#' Algo raro?
summary(merced)

#' Los -999.00 indican faltantes. Vamos a codificarlos como debe ser:

#' Primero ponemos nombres fáciles:
names(merced) <- c("fecha.texto", "hora", "ozono", "temperatura")

merced$ozono
class(merced$ozono)
plot(merced$ozono)
plot(merced$temperatura)

#' Creamos un índice: recuérdese que esta operación da un vector
faltantes <- merced$ozono == -999
#' Sustituimos
merced$ozono.na <- merced$ozono
merced$ozono.na[faltantes] <- NA # sustituye los elementos de merced$ozono.na que
								  # tales que la componente correspondiente de 
								# faltante es TRUE

#' Igual para temperatura
faltantes.temp <- merced$temperatura == -99.9
merced$temperatura.na <- merced$temperatura
merced$temperatura.na[faltantes.temp] <- NA

#' Vemos que funcionó:
head(merced, 100)
summary(merced$ozono.na)
summary(merced$temperatura.na)

#' Hay 765 faltantes (ozono) y 482 de temperatura, de unos 8mil datos
#' ¿Dónde están los faltantes?

#' Vamos a usar la fecha
class(merced$fecha)
#' factor son variables cualitativas o categóricas - por el momento, pensemos que contiene cadenas de texto. Para asegurarnos, convertimos con as.character:
merced$fecha <- as.character(merced$fecha)
class(merced$fecha)

#' Ahora la convertimos a una fecha 
library(lubridate)
merced$fecha <- dmy(merced$fecha.texto)
class(merced)
#' Cómo se distribuyen los NA's ?

#' Extraemos el mes, que funciona porque la clase de fecha es date;

merced$mes <- month(merced$fecha)

#' Creamos una indicadora de faltantes con la función is.na
merced$faltante.ozono <- is.na(merced$ozono.na)
merced$faltante.temp <- is.na(merced$temperatura.na)

#' Table hace tablas multidimensionales: las celdas se definen por todas las posibles combinaciones de los valores de las variables que pasamos, y en cada celda tenemos el número de sus datos.
tabla.faltantes.o3 <- table(merced$mes, merced$faltante.ozono, merced$hora)
tabla.faltantes.temp <- table(merced$mes, merced$faltante.temp, merced$hora)

#' En estas tablas vemos que tenemos al menos unas veinte observaciones para cada hora/mes tanto de ozono como de temperatura. Por el momento, seguimos con nuestro análisis (pregunta: qué patrones hay en los datos faltantes?)

#' Vamos a analizar desde las 11am hasta las 8pm
merced.2 <- subset(merced, hora >= 11 & hora <= 20)

#' Usamos ggplot para graficar. La sintaxis de ggplot parece rara al principio.
#' Usamos el operador + para agregar 'capas' a la gráfica, y `aes`
#' sirve para mapear variables a objetos de la gráfica
ggplot(merced.2, aes(x=temperatura, y=ozono)) + geom_point()
ggplot(merced.2, aes(x=temperatura.na, y=ozono.na)) + geom_point()

#' Rectas de mínimos cuadrados para cada hora:
ggplot(merced.2, aes(x=temperatura.na, y=sqrt(ozono.na))) + 
  geom_point(alpha = 0.4) + 
  facet_wrap(~hora) +
  geom_smooth(method = "lm")

## Pregunta: ¿Cómo extraer los coeficientes de las rectas de regresión en cada mes? Primero, vemos la función lm, que hace el ajuste de mínimos cuadrados:

mod.total <- lm(ozono.na ~ temperatura.na, data = merced.2)
coefficients(mod.total)

#' Centramos en 22 grados para leer más fácilmente
mean(merced.2$temperatura.na, na.rm =TRUE)
merced.2$temperatura.22 <- merced.2$temperatura.na - 22

mod.total <- lm(ozono.na ~ temperatura.22, data = merced.2)
class(mod.total)
coefficients(mod.total)

#' Dividir, aplicar y combinar
#' ==============================
#' plyr: funciones para dividir, aplicar y combinar:
#' Queremos dividr los datos por hora, hacer mínimos cuadrados de ozono vs. temperatura, y finalmente juntar los resultados en un objeto manejable
library(plyr)


#' Usar datos merced.2, dividir por hora, mínimos cuadrados de ozono vs. temperatura, regresar el resultado en una lista.

#' `dlply` - la d es de dataframe, la l es de list
## nótese la función definida en línea
modelos <- dlply(merced.2, c("hora"), function(df){
  mod.1 <- lm(ozono.na ~ temperatura.22, data = df)
})

#' Cada elemento de la lista contiene objetos `lm`


#' Si sólo queremos coeficientes
#' ddply - la d es de data frame, la d es de dataframe.
coefs.1 <- ddply(merced.2, c("hora"), function(df){
    mod.1 <- lm(ozono.na ~ temperatura.22, data = df)
    coefficients(mod.1)
})

coefs.1

ggplot(coefs.1, aes(x=hora, y=temperatura.22)) + geom_line() + geom_point()



#' Cambiar la forma de las tablas
#' ==============================

#' Ahora queremos calcular medias, para cada estación, hora del día  y mes de las medidas de ozono y de la temperatura. No hay que hacerlo a mano!!!

#' Primero identificamos donde está la medición y la estación:

nombres.var <- names(mis.datos)[3:51]

#' Las estaciones son códigos de 3 letras, justo antes de un underscore y un punto (_)

#' Hay que procesar los nombres de las columnas. Primero vemos la técnica que vamos a usar:

lista.nom <- strsplit(nombres.var, split ="_" )
nombres.limpios <- sapply(lista.nom, function(elem) elem[1])

#' Ahora extraemos los últimos tres caracteres (estación) y el resto es la medición

estaciones <- sapply(nombres.limpios, function(nom) substr(nom, start=nchar(nom)-2, stop=nchar(nom)))
mediciones <- sapply(nombres.limpios, function(nom) substr(nom, start=1, stop=nchar(nom)-3))

#' ¿Qué hacemos con los datos? Primero los ponemos en forma larga

datos.largos <- melt(mis.datos, id.vars=c("FECHA", "HORA"))
nrow(datos.largos)
head(datos.largos)

#' Ahora aplicamos lo que acabamos de hacer para la variable "variable"

nombres.base <- sapply( 
  strsplit(as.character(datos.largos$variable), split ="_"),
  function(elem) elem[1]
  )

datos.largos$estacion <- sapply(nombres.base, function(nom) substr(nom, start=nchar(nom)-2, stop=nchar(nom)))
datos.largos$medicion <- sapply(nombres.base, function(nom) substr(nom, start=1, stop=nchar(nom)-3))

head(datos.largos)
table(datos.largos$medicion)
table(datos.largos$estacion)


#' Agregación 
#' ==============

#' Agregamos con ddply: vamos a cortar por estación y medición, y en cada corte calculamos la media de las observaciones:

tabla.medias <- ddply(datos.largos, c("estacion", "medicion"), 
                      summarise,
                      media = mean(value))

#' Hay algo mal! los valores faltantes. Podemos arreglar fácilmente con:
ddply(datos.largos, c("medicion"), summarise, minimo = min(value))

datos.largos$value[datos.largos$medicion=="TMP" & datos.largos$value==-99.9] <- NA
datos.largos$value[datos.largos$value %in% c(-9.9, -99.9,-99,-999)] <- NA


tabla.medias <- ddply(datos.largos, c("estacion", "medicion"), summarise,
                      media = mean(value),
                      desvest = sd(value))

tabla.medias <- ddply(datos.largos, c("estacion", "medicion"), summarise,
                      media = mean(value, na.rm =TRUE),
                      desvest = sd(value, na.rm =TRUE))

tabla.medias.2 <- ddply(datos.largos, c("estacion", "medicion","HORA"), summarise,
                      media = mean(value, na.rm =TRUE),
                      desvest = sd(value, na.rm =TRUE),
                      validos = sum(!is.na(value)))

#' ¿Cómo quedaron los resultados?
ggplot(tabla.medias.2, aes(x=HORA, y = media, colour=estacion)) + 
  facet_wrap(~medicion, scales="free") +
  geom_point()

#' Que es buen principio. OJO: La dirección del viento está mal:  esta no se puede simplemente promediar. Se puede arreglar, por ejemplo, considerando seno y coseno de la dirección.

## Tips: 
## 1) aprender plyr (ddply-dlply-ldply) y ggplot2 (gráficas) es muy buena idea
## 2) ¿Qué es apply, lapply, sapply?
## 3) Usar class y str cuando algo sale mal es un buen principio.
## 4) data.frame, plyr y ggplot2 puede ser lentos y costosos en memoria 
##    para datos grandes (millones de renglones).
## 5) En este caso: primero, revisar el código y hacer pruebas,
##    luego ver http://cran.r-project.org/web/views/HighPerformanceComputing.html.
##    Y conseguir más memoria/máquina más rápida.
## 6) Otros tips rápidos: no usar ciclos for para hacer cálculos (muchas
## veces hay funciones de R para hacer el trabajo que son mucho más rápidas),
## intentar usar matrices en lugar de data.frame,  
## trabajar con bases de datos SQL. Hay varios paquetes para mejorar
## desempeño o trabajar con bases de datos grandes que no neceariamente
## caben en memoria.
## 7) Google sabe CASI TODO. Copia mensajes de error quitando particulares para buscar soluciones. Pega el nombre de la función, agregando "R", por ejemplo, para buscar ejemplos. Usa stackoverflow.com.
