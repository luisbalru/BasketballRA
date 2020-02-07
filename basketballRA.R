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

idetermt = apriori(determinanteT, parameter = list(support = 0.1,  target = "frequent"))
idetermt = sort(idetermt, by = "support")
inspect(head(idetermt, n=10))

rules_idetermt = apriori(determinanteT, parameter = list(support = 0.1, confidence = 0.8, minlen=2))
summary(rules_idetermt)
inspect(head(rules_idetermt))
quality(head(rules_idetermt))
############################################################
# REGLAS DE EDAD

# Estudio Coach
coach = data %>% filter(ageInteger == "Coach")
coach$ageInteger = NULL
coachT = as(coach, "transactions")
summary(coachT)
itemFrequencyPlot(coachT, support = 0.1, cex.names = 0.8)


# Estudio Rookie
rookies = data %>% filter(ageInteger == "Rookie")
rookies$ageInteger = NULL
rookT = as(rookies,"transactions")
summary(rookT)
itemFrequencyPlot(rookT, support = 0.1, cex.names = 0.8)

irook = apriori(rookT, parameter = list(support = 0.1,  target = "frequent"))
irook = sort(irook, by = "support")
inspect(head(irook, n=10))

rules_irook = apriori(rookT, parameter = list(support = 0.1, confidence = 0.8, minlen=2))
summary(rules_irook)
inspect(head(rules_irook))
quality(head(rules_irook))

# Estudio Sophomore

soph = data %>% filter(ageInteger == "Sophomore")
soph$ageInteger = NULL
sophT = as(soph, "transactions")
summary(sophT)
itemFrequencyPlot(sophT, support = 0.1, cex.names = 0.8)

isoph = apriori(sophT, parameter = list(support = 0.1,  target = "frequent"))
isoph = sort(isoph, by = "support")
inspect(head(isoph, n=10))

rules_soph = apriori(sophT, parameter = list(support = 0.1, confidence = 0.8, minlen=2))
summary(rules_soph)
inspect(head(rules_soph))
quality(head(rules_soph))
############################################################
# REGLAS DE MINUTOS

# Estudio Recambio
rec = data %>% filter(time_playedReal == "Recambio")
rec$time_playedReal = NULL
recT = as(rec,"transactions")
summary(recT)
itemFrequencyPlot(recT, support = 0.1, cex.names = 0.8)

irec = apriori(recT, parameter = list(support = 0.1,  target = "frequent"))
irec = sort(irec, by="support")
inspect(head(irec, n =10))

rules_rec = apriori(recT, parameter = list(support = 0.1, confidence = 0.8, minlen=2))
summary(rules_rec)
inspect(head(rules_rec, n=15))
quality(head(rules_rec))

# Estudio Sexto hombre
sh = data %>% filter(time_playedReal == "Sexto hombre")
sh$time_playedReal = NULL
shT = as(sh, "transactions")
summary(shT)
itemFrequencyPlot(shT, support = 0.1, cex.names = 0.8)

ish = apriori(shT, parameter = list(support = 0.1,  target = "frequent"))
ish = sort(ish, by="support")
inspect(head(ish, n =10))

rules_sh = apriori(shT, parameter = list(support = 0.1, confidence = 0.8, minlen=2))
summary(rules_sh)
inspect(head(rules_sh, n=15))
quality(head(rules_sh))

###########################################################
# REGLAS DE ASISTENCIAS

# Pasador nato
pn = data %>% filter(assists == "Pasador nato")
pn$assists = NULL
pnT = as (pn, "transactions")
summary(pnT)
itemFrequencyPlot(pnT, support = 0.1, cex.names = 0.8)

ipn = apriori(pnT, parameter = list(support = 0.1,  target = "frequent"))
ipn = sort(ipn, by="support")
inspect(head(ipn, n =10))

rules_pn = apriori(pnT, parameter = list(support = 0.1, confidence = 0.8, minlen=2))
summary(rules_pn)
inspect(head(rules_pn, n=15))
quality(head(rules_pn))

# Poco asistente

pa = data %>% filter(assists == "Poco asistente")
pa$assists = NULL
paT = as(pa, "transactions")
summary(paT)
itemFrequencyPlot(paT, support = 0.1, cex.names = 0.8)

ipa = apriori(paT, parameter = list(support = 0.1,  target = "frequent"))
ipa = sort(ipa, by="support")
inspect(head(ipa, n =10))

rules_pa = apriori(paT, parameter = list(support = 0.1, confidence = 0.8, minlen=2))
summary(rules_pa)
inspect(head(rules_pa, n=15))
quality(head(rules_pa))