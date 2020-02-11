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

# assist
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

# points
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

# Estudio los Bajos
bajos = data %>% filter(heightInteger == "Bajos")
bajos$heightInteger = NULL
bajosT = as(bajos,"transactions")
summary(bajosT)
itemFrequencyPlot(bajosT, support = 0.1, cex.names=0.8)

ibajos <- apriori(bajosT, parameter = list(support = 0.1, target="frequent"))
ibajos <- sort(ibajos, by="support") # Los ordenamos por el valor del soporte
inspect(head(ibajos, n=10)) # Inspeccionamos los 10 primeros

rules_bajos <- apriori(bajosT, parameter = list(support = 0.1, confidence = 0.8, minlen = 2))
summary(rules_bajos)
inspect(head(rules_bajos))
quality(head(rules_bajos))

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

media.altura.discreto = media.altura %>% filter(points == "Discreto")
media.altura.discreto$points = NULL
media.altura.discretoT = as(media.altura.discreto,"transactions")
rules_media.altura.discreto = apriori(media.altura.discretoT, parameter = list(support = 0.1, confidence = 0.8, minlen = 2))
summary(rules_media.altura.discreto)
inspect(head(rules_media.altura.discreto))
quality(head(rules_media.altura.discreto))

# Estudio Muy altos --> no genera interés
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


#######################################################################################
# SEGUNDA APROXIMACIÓN: MOPNAR

# Lectura de datos
data = read.csv('./data/basketball.csv')
basket = data
data$assists = data$assists_per_minuteReal * 40
data$points = data$points_per_minuteReal * 40
data$assists_per_minuteReal = NULL
data$points_per_minuteReal = NULL

library(RKEEL)
algoritmo = MOPNAR_A(data)
algoritmo$run()
algoritmo$sortBy("support")
algoritmo$showRules()


#######################################################################################
# TERCERA APROXIMACIÓN: DIVISIÓN EN RECUBRIMIENTO (NO PARTICIÓN) E ITEMS NEGADOS

# Lectura de datos
data = read.csv('./data/basketball.csv')
basket = data
data$assists = data$assists_per_minuteReal * 40
data$points = data$points_per_minuteReal * 40
data$assists_per_minuteReal = NULL
data$points_per_minuteReal = NULL

# Altura: Bajos (159,183), media altura(178,190), altos(186,197) y muy altos(195,203)
alt_bajo = ifelse(data$heightInteger >= 159 & data$heightInteger <= 183, "Si","No")
alt_ma = ifelse(data$heightInteger >= 178 & data$heightInteger <= 190, "Si", "No")
alt_a = ifelse(data$heightInteger >= 186 & data$heightInteger<=197, "Si", "No")
alt_muya = ifelse(data$heightInteger >= 195 & data$heightInteger <= 203, "Si", "No")
# Asistencias: Baja asistencia(1,4), Asistencia media(3,6), Asistencia alta(4.6,10), Pasador nato(8,14)
asist_ba = ifelse(data$assists>=1 & data$assists<=4, "Si","No")
asist_am = ifelse(data$assists>=3 & data$assists<=6, "Si","No")
asist_aa = ifelse(data$assists>=4.6 & data$assists<=10, "Si","No")
asist_pn = ifelse(data$assists>=8 & data$assists<=14, "Si","No")
# Edad: Rookie(20,25), Sophomore(23,26), Experimentado(24,32), Veterano(28,41)
edad_r = ifelse(data$ageInteger>=20 & data$ageInteger<=25,"Si", "No")
edad_s = ifelse(data$ageInteger>=23 & data$ageInteger<=26,"Si", "No")
edad_e = ifelse(data$ageInteger>=24 & data$ageInteger<=32,"Si", "No")
edad_v = ifelse(data$ageInteger>=28 & data$ageInteger<=41,"Si", "No")
# Tiempo: Suplente(10,19), Sexto hombre(15,27), titular(28,35), estrella(32,41)
temp_s = ifelse(data$time_playedReal>=10 & data$time_playedReal<=19, "Si", "No")
temp_sh = ifelse(data$time_playedReal>=15 & data$time_playedReal<=27, "Si", "No")
temp_t = ifelse(data$time_playedReal>=28 & data$time_playedReal<=35, "Si", "No")
temp_e = ifelse(data$time_playedReal>=32 & data$time_playedReal<=41, "Si", "No")
# Puntos: Baja anotación(6,10), Anotación media(8,17), Protagonista(15,26), Amo del Parqué(22.34)
punt_ba = ifelse(data$points >=6 & data$points<=10,"Si", "No")
punt_am = ifelse(data$points >=8 & data$points<=17,"Si", "No")
punt_p = ifelse(data$points >=15 & data$points<=26,"Si", "No")
punt_ap = ifelse(data$points >=22 & data$points<=34,"Si", "No")

data.ta = data.frame("Altura: Bajo" = c(alt_bajo), "Altura: Media"=c(alt_ma), "Altura: Alto" = c(alt_a), "Altura: Muy alto"=c(alt_muya),
                        "Asistencias: Baja asistencia" = c(asist_ba), "Asistencias: Media" = c(asist_am), "Asistencias: Alta" = c(asist_aa),
                        "Asistencias: Pasador nato" = c(asist_pn), "Edad: Rookie" = c(edad_r), "Edad: Sophomore" = c(edad_s), "Edad: Experimentado" = c(edad_e),
                        "Edad: Veterano" = c(edad_v), "Tiempo: Suplente"= c(temp_s), "Tiempo: Sexto hombre" = c(temp_sh), "Tiempo: Titular" = c(temp_t),
                        "Tiempo: Estrella"= c(temp_e), "Puntos: Baja anotación"= c(punt_ba), "Puntos: Anotación media" = c(punt_am), "Puntos: Protagonista" = c(punt_p),
                        "Puntos: Amo del parqué" = c(punt_ap))


data.ta.t = as(data.ta,"transactions")
summary(data.ta.t)
image(data.ta.t)

# Items más importantes
itemFrequencyPlot(data.ta.t, support = 0.1, cex.names=0.8)

# Extraigo los itemsets frecuentes con Apriori
if_data.ta = apriori(data.ta.t, parameter = list(support=0.01, target="frequent"))
if_data.ta = sort(if_data.ta, by = "support")
inspect(head(if_data.ta,n=10))

cerrados_data.ta = if_data.ta[is.closed(if_data.ta)]
maximales_data.ta = if_data.ta[is.maximal(if_data.ta)]

barplot( c(frequent=length(if_data.ta), closed=length(cerrados_data.ta), maximal=length(maximales_data.ta)), ylab="count", xlab="itemsets")

# Extracción de reglas con Apriori
reglas.ta = apriori(data.ta.t, parameter=list(support=0.2, confidence = 0.5, minlen=2))
summary(reglas.ta)
inspect(head(reglas.ta))
write.PMML(reglas.ta,"reglas-ta.pmml")
write(reglas.ta,"reglas-ta.csv",sep=",")
quality(head(reglas.ta))
