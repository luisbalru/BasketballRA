#################################################################
#   APRENDIZAJE NO SUPERVISADO Y DETECCIÓN DE ANOMALÍAS         #
#  Trabajo de Reglas de asociación. Dataset: Basketball         #
# Autor: Luis Balderas Ruiz                                     #
#################################################################

library(arules)
library(ggplot2)

binwd = function(data){
  size = length(data)
  dt = sd(data)
  cr = size^(1/3)
  return(1/(cr)*dt*3.49)
}

# Lectura de datos
data = read.csv('./data/basketball.csv')
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
