#################################################################
#   APRENDIZAJE NO SUPERVISADO Y DETECCIÓN DE ANOMALÍAS         #
#  Trabajo de Reglas de asociación. Dataset: Basketball         #
# Autor: Luis Balderas Ruiz                                     #
#################################################################

library(arules)
library(ggplot2)
library(dplyr)

binwd = function(data){
  size = length(data)
  dt = sd(data)
  cr = size^(1/3)
  return(1/(cr)*dt*3.49)
}

# Lectura de datos
data = read.csv('./data/basketball.csv')
basket = data
data$assists = data$assists_per_minuteReal * 40
data$points = data$points_per_minuteReal * 40
data$assists_per_minuteReal = NULL
data$points_per_minuteReal = NULL

################################################################
# EXPLORACIÓN DE DATOS PARA ELEGIR INTERVALOS
library(Hmisc)
summary(data)
hist.data.frame(data)

# assist per minute
ggplot(data=data, aes(x=data$assists)) + geom_histogram(binwidth=binwd(data$assists),fill='blue') +
  ggtitle("Histograma de Asistencias") + labs(x="Asistencias",y="Frecuencia")

# height
ggplot(data=data, aes(x=data$heightInteger)) + geom_histogram(binwidth=binwd(data$heightInteger),fill='blue') +
  ggtitle("Histograma de alturas") + labs(x="Altura",y="Frecuencia")

# time played
ggplot(data=data, aes(x=data$time_playedReal)) + geom_histogram(binwidth=binwd(data$time_playedReal),fill='blue') +
  ggtitle("Histograma de minutos jugados") + labs(x="Minutos",y="Frecuencia")

# age
ggplot(data=data, aes(x=data$ageInteger)) + geom_histogram(binwidth=binwd(data$ageInteger),fill='blue') +
  ggtitle("Histograma de edad") + labs(x="Edad",y="Frecuencia")

# points per minute
ggplot(data=data, aes(x=data$points)) + geom_histogram(binwidth=binwd(data$points),fill='blue') +
  ggtitle("Histograma de puntos") + labs(x="Puntos",y="Frecuencia")

#############################################################################
#############################################################################

############################################################################
# PRIMERA APROXIMACIÓN
data[["heightInteger"]] = ordered(cut(data[["heightInteger"]], c(159,175,182,190,198,203)), labels = c("Muy bajos", "Bajos", "Media altura", "Altos", "Muy altos"))
data[["assists"]] = ordered(cut(data[["assists"]], c(1.97,3.5,5.7,7.2,9,13.8), labels = c("Mal asistente","Poco asistente", "Asistente medio", "Gran asistente", "Pasador nato")))
data[["ageInteger"]] = ordered(cut(data[["ageInteger"]], c(21,25,29,32,34,37), labels = c("Rookie", "Sophomore", "Senior","Experimentado", "Coach")))
data[["time_playedReal"]] = ordered(cut(data[["time_playedReal"]], c(10, 16.50, 22.30,27.90,34.00,41), labels = c("Recambio", "Suplente", "Sexto hombre", "Titular", "Estrella")))
data[["points"]] = ordered(cut(data[["points"]], c(6,12,15,22,27,34), labels = c("Baja anotación", "Discreto", "Anotador", "Determinante", "Amo del parqué")))

data1 = as(data,"transactions")
summary(data1)
image(data1)

# Items más importantes
itemFrequencyPlot(data1, support = 0.1, cex.names=0.8)

# Extraigo los itemsets frecuentes con Apriori
if_data1 = apriori(data1, parameter = list(support=0.01, target="frequent"))
if_data1 = sort(if_data1, by = "support")
inspect(head(if_data1,n=10))

cerrados_data1 = if_data1[is.closed(if_data1)]
maximales_data1 = if_data1[is.maximal(if_data1)]

barplot( c(frequent=length(if_data1), closed=length(cerrados_data1), maximal=length(maximales_data1 )), ylab="count", xlab="itemsets")

# Extracción de reglas con Apriori
reglas1 = apriori(data1, parameter=list(support=0.1, confidence = 0.5, minlen=2))
summary(reglas1)
inspect(head(reglas1))
quality(head(reglas1))


##############################################################
# REGLAS DE ALTURA

# Estudio los Muy Bajos
muybajos = data %>% filter(heightInteger == "Muy bajos")
muybajos$heightInteger = NULL
muybajosT = as(muybajos,"transactions")
summary(muybajosT)
itemFrequencyPlot(muybajosT, support = 0.1, cex.names=0.8)

imuybajos <- apriori(muybajosT, parameter = list(support = 0.1, target="frequent"))
imuybajos <- sort(imuybajos, by="support") # Los ordenamos por el valor del soporte
inspect(head(imuybajos, n=10)) # Inspeccionamos los 10 primeros

rules_muybajos <- apriori(muybajosT, parameter = list(support = 0.1, confidence = 0.8, minlen = 2))
summary(rules_muybajos)
inspect(head(rules_muybajos))
quality(head(rules_muybajos))

# Estudo Media Altura
media.altura = data %>% filter(heightInteger == "Media altura")
media.altura$heightInteger = NULL
media.alturaT = as(media.altura,"transactions")
summary(media.alturaT)
itemFrequencyPlot(media.alturaT, support = 0.1, cex.names = 0.8)

imedia.altura = apriori(media.alturaT, parameter = list(support=0.1, target = "frequent"))
imedia.altura = sort(imedia.altura,by="support")
inspect(head(imedia.altura,n=10))

rules_media.altura = apriori(media.alturaT, parameter = list(support = 0.1, confidence = 0.8, minlen = 2))
summary(rules_media.altura)
inspect(head(rules_media.altura))
quality(head(rules_media.altura))

# Estudio Muy altos
muyaltos = data %>% filter(heightInteger == "Muy alto")

##############################################################
# REGLAS DE ANOTACIÓN

# Estudio Baja Anotación
baja.anotacion = data %>% filter(points == "Baja anotación")
baja.anotacion$points = NULL
banotT = as(baja.anotacion,"transactions")
summary(banotT)
itemFrequencyPlot(banotT, support = 0.1, cex.names = 0.8)

ibanot = apriori(banotT, parameter = list(support=0.1, target = "frequent"))
ibanot = sort(ibanot,by = "support")
inspect(head(ibanot,n=10))

rules_ibanot = apriori(banotT, parameter = list(support = 0.1, confidence = 0.8, minlen = 2))
summary(rules_ibanot)
inspect(head(rules_ibanot))
quality(head(rules_ibanot))

# Estudio determinante
determinante = data %>% filter(points == "Determinante")
determinante$points = NULL
determinanteT = as(determinante,"transactions")
summary(determinanteT)
itemFrequencyPlot(determinanteT, support = 0.1, cex.names = 0.8)

ideterm = apriori(determinanteT, parameter = list(support = 0.1, confidence = 0.8, minlen = 2))
ideterm = sort(ideterm, by = "support")
inspect(head(ideterm, n=10))