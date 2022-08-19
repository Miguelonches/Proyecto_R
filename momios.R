install.packages("dplyr")
install.packages("ggplot2")
install.packages("remotes")
install.packages("hrbrthemes")
install_github("CRAN/FbRanks")
library(remotes)
library(fbRanks)
library(dplyr)
library(ggplot2)

library(hrbrthemes)

#setwd()

#Obtener links de datasets
u1011 <- "https://www.football-data.co.uk/mmz4281/1011/SP1.csv"
u1112 <- "https://www.football-data.co.uk/mmz4281/1112/SP1.csv"
u1213 <- "https://www.football-data.co.uk/mmz4281/1213/SP1.csv"
u1314 <- "https://www.football-data.co.uk/mmz4281/1314/SP1.csv"
u1415 <- "https://www.football-data.co.uk/mmz4281/1415/SP1.csv"
u1516 <- "https://www.football-data.co.uk/mmz4281/1516/SP1.csv"
u1617 <- "https://www.football-data.co.uk/mmz4281/1617/SP1.csv"
u1718 <- "https://www.football-data.co.uk/mmz4281/1718/SP1.csv"
u1819 <- "https://www.football-data.co.uk/mmz4281/1819/SP1.csv"
u1920 <- "https://www.football-data.co.uk/mmz4281/1920/SP1.csv"

#Descargar Archivos
setwd("/cloud/project/Data")
download.file(url = u1011, destfile ="SP1-1011.csv", mode = "wb")
download.file(url = u1112, destfile ="SP1-1112.csv", mode = "wb")
download.file(url = u1213, destfile ="SP1-1213.csv", mode = "wb")
download.file(url = u1314, destfile ="SP1-1314.csv", mode = "wb")
download.file(url = u1415, destfile ="SP1-1415.csv", mode = "wb")
download.file(url = u1516, destfile ="SP1-1516.csv", mode = "wb")
download.file(url = u1617, destfile ="SP1-1617.csv", mode = "wb")
download.file(url = u1718, destfile ="SP1-1718.csv", mode = "wb")
download.file(url = u1819, destfile ="SP1-1819.csv", mode = "wb")
download.file(url = u1920, destfile ="SP1-1920.csv", mode = "wb")

#Tenemos todos los archivos en una lista
lista_de_CV <- lapply(list.files(),read.csv)

#Existen dos tipos de archivos entonces hay que separarlos
lista_de_CV_formato1 <- lista_de_CV[1:9]
lista_de_CV_formata2 <- lista_de_CV[10]

#Seleccionamos los datos Date:FTAG, BbMx.2.5:BbAv.2.5.1

datos_columnas_seleccionadas <- lapply(lista_de_CV_formato1,select,Date:FTAG,BbMx.2.5:BbAv.2.5.1) #Seleccionamos para los del formato tipo 1
datos_columnas_seleccionadas_formato2 <- lapply(lista_de_CV_formata2,select,Date:FTAG, Max.2.5:Avg.2.5.1)
#Quitamos la variable time del formato 2
datos_columnas_seleccionadas_formato2 <- lapply(datos_columnas_seleccionadas_formato2,
                                                select,-Time)
#Renombramos las columnas de ambos formatos para tenerlos igual
datos_columnas_seleccionadas <- lapply(datos_columnas_seleccionadas,rename, #Para formato1
                                       Max2.5_under = BbMx.2.5,
                                       Avg2.5_under = BbAv.2.5,
                                       Max2.5_over = BbMx.2.5.1,
                                       Avg2.5_over = BbAv.2.5.1)
datos_columnas_seleccionadas_formato2 <-lapply(datos_columnas_seleccionadas_formato2,rename, #Para formato2
                                               Max2.5_under = Max.2.5,
                                               Avg2.5_under = Avg.2.5,
                                               Max2.5_over = Max.2.5.1,
                                               Avg2.5_over = Avg.2.5.1)
#Ya que los dos tipos de datasets podemos combinarlos
data_complete <- c(datos_columnas_seleccionadas,datos_columnas_seleccionadas_formato2)

#Modificamos el formato de la columna Date para las fechas a día/mes/año
data_complete <- lapply(data_complete,mutate,Date=as.Date(Date,format = "%d/%m/%y"))

#Juntamos todo en un solo dataset

#EN caso de tener que ejecutar el for varias veces hay que borrar la data
#data <- NULL
for(i in 1:length(data_complete)){
  data <- rbind(data,data_complete[[i]])
}

#Renombramos las variables de goles para local y visitante
data <- rename(data,Home_score = FTHG,
               Away_score = FTAG)

#Ordenamos las columnas para tener el data set listo
data <- select(data, Date, HomeTeam, Home_score, AwayTeam,Away_score,Max2.5_under:Avg2.5_over)


#Graficas de goles local
data %>%
  ggplot()+
  aes(Home_score)+
  geom_histogram(binwidth = 1.5,colour="black",fill = "blue")+
  labs(x = "Goles anotados", y = "Frecuencia")

#Grafica de goles visitante
data %>%
  ggplot()+
  aes(Away_score)+
  geom_histogram(binwidth = 1.5,colour="black",fill = "yellow")+
  labs(x = "Goles anotados", y = "Frecuencia")

#Probabilidad conjunta
tabla_prob_goles = table(data$Home_score,data$Away_score)
matriz_prob = prop.table(tabla_prob_goles)

#Mapa de calor de la probabilidad conjunta
heatmap(matriz_prob,xlab = "Goles visita",ylab = "Goles local")

ggplot(data,aes(x = Home_score, y = Away_score, fill = (Home_score + (Away_score))  ))+geom_tile()+
  scale_fill_gradient(low="white", high="blue") +
  labs(x = "Goles local", y = "Goles visitante", fill = "Goles")

#Geometria goles anotados de local por cada gol de visitante
data %>%
  ggplot()+
  aes(Home_score)+
  geom_bar()+
  facet_wrap("Away_score")+
  theme(axis.title=element_text(size=10,face="bold"))+
  labs(x = "Goles Local",y="Frecuencia")

#Preparamos el data para el FbRanks
#Tenemos que obtener la fecha y los resultados de los partidos

data_matches <- select(data,Date:Away_score)
#Tenemos que renombrar las columnas porqué sino el fbRanks no lo va a poder leer
data_matches <- rename(data_matches,date=Date,home.team = HomeTeam,
                       home.score = Home_score,
                       away.team = AwayTeam,
                       away.score = Away_score)
#Creamos un csv con los partidos
write.csv(data_matches,"data_partidos.csv",row.names = FALSE)
fdata_matches <- create.fbRanks.dataframes(scores.file = "data_partidos.csv")
#Obtenemos las filas de scores y teams
resultados <- fdata_matches$scores
equipos <- fdata_matches$teams

#Separamos las fechas para los partidos
fechas <- resultados$date
fechas_unique <- unique(fechas) #Fechas sin repetir
#Nos quedamos solamente con el año y el mes
anio_mes <- format(fechas_unique,"%Y-%m")
anio_mes_unique <- unique(anio_mes) #Fechas sin repetir
length(anio_mes_unique)
anio_mes_unique

#Agarramos 13 meses para entrenar -> 2010-08-2011-10
index_partidos_fechas <- which(anio_mes_unique[13] == format(resultados$date,"%Y-%m"))
#Calculamos el ultimo partido del ultimo mes de entramiento 2011-10
fecha_final <- resultados$date[max(index_partidos_fechas)]

######Entrenamiento del modelo########
#Datos de entrenamiento del inicio 2010 al año final 2011
#Datos de prueba del año final 2011 al final de la data
entrenamiento <- filter(data, resultados$date <= fecha_final)
prueba <- filter(data, resultados$date > fecha_final)

#Prueba de entrenamiento Poisson con rankTeams
ranks <- rank.teams(scores = resultados,
                    teams = equipos,
                    min.date = entrenamiento$Date[1],
                    max.date = max(entrenamiento$Date),
                    date.format = "%Y-%m-%d")
#Predecimos con el modelo que nos genero ranksteams
prediccion <- predict(ranks,date = prueba$Date[1])

#Guardamos las predicciones
predicted_home_goals <- prediccion$scores$home.score
predicted_hometeams <- prediccion$scores$home.team
predicted_awaygoals <- prediccion$scores$away.score
predicted_awayteams <- prediccion$scores$away.team

#Vemos el dataFrame obtenido
data_predecidad <- data.frame(predicted_hometeams,predicted_home_goals,
                              predicted_awayteams,predicted_awaygoals)
data_predecidad

#Vamos a predecir para cada una de las fechas del dataset
#Con un pivote vamos armar datos de entrenamiento y datos de prueba
predicted_home_goals <- NULL
predicted_hometeams <- NULL
predicted_awaygoals <- NULL
predicted_awayteams <- NULL

#Mantemos un porcentaje de 70-30
for(i in 1:length(unique(resultados$date)-355)){
  ranks <- rank.teams(scores = resultados,
                      teams = equipos,
                      min.date = resultados$date[i],
                      max.date = resultados$date[i+354],
                      date.format = "%Y-%m-%d")
  prediccion <- predict(ranks,date = resultados$date[i+355])
  
  predicted_home_goals <- c(predicted_home_goals,prediccion$scores$home.score)
  predicted_hometeams <- c(predicted_hometeams,prediccion$scores$home.team)
  predicted_awaygoals <- c(predicted_awaygoals,prediccion$scores$away.score)
  predicted_awayteams <- c(predicted_awayteams,prediccion$scores$away.team)
}

#Vemos el dataFrame obtenido
data_predecidad <- data.frame(predicted_hometeams,predicted_home_goals,
                              predicted_awayteams,predicted_awaygoals)
data_predecidad


#Verificamos NA
matrizNA <- is.na(predicted_awaygoals | predicted_home_goals)
matrizNA
#No hay datos NA

#Obtenemos los momios
momios <- filter(data,data$Date >= unique(resultados$date[355]))
#Probando los momios
mean(predicted_home_goals == momios$Home_score) #Promedio de No. de goles predecidos que coinciden con los anotados
mean(predicted_awaygoals == momios$Away_score)

mean(predicted_awaygoals+predicted_home_goals > 3) #Proporcion de partidos que anotaron más de 3

mean(momios$Home_score + momios$Away_score > 3)/mean(momios$Away_score >= 1)
