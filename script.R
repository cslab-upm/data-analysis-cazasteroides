# Script para obtener la base de datos, mongodb server debe de estar en ejecucion.

# Importamos paquetes necesarios.
library(mongolite)
library(scales)
library(ggplot2)
library(factoextra)

# Variables globales mediante las cuales se obtiene el set de datos. 
# Se va a dividir en tres partes, detecciones, votos y grupo mixto.
db_name <- "caza"
users_collection <-  "users"
url_db <- "mongodb://localhost:27017"
users_fields <-  '{ "_id" : true, "sDetections" : true,  "points" : true, "sVotes" : true}'
queryy <- '{ "sDetections" : true,  "points" : true, "sVotes" : true}'


# Funcion que permite obtener un et de datos de una tabla de mongoDb.
getCollectionDB <- function(db_name, db_collection, url_db, list_fields) {
  ret <-  mongo(collection = db_collection, db = db_name, url = url_db)
  ret <-getSetCollectionDb(ret, list_fields)
  ret <- setFormat(ret)
  return(ret)
}

# Funcion que permite obtener un set de datos sobre una coleccion de mongoDb.
# Se establece por defecto, un limite de 100000 filas y 0 saltados por el numero de usuarios de la plataforma, en la bd actual hay <4000.
getSetCollectionDb <- function(collection, list_fields){
  ret <- collection$find(limit = 100000, skip = 0, fields = list_fields)
  ret <- setFormat(ret)
  return(ret)
}

# Funcion auxiliar con la que se obtiene el conjunto de datos con formato correcto.
setFormat <- function(dataSet){

  
  dataSet$points[is.na(dataSet$points)] <- 0
  dataSet$points[dataSet$points < 0] <- 0
  dataSet$points <- round(dataSet$points, 0)

  dataSet$sVotes$wrongs[is.na(dataSet$sVotes$wrongs)] <- 0
  dataSet$sVotes$corrects[is.na(dataSet$sVotes$corrects)] <- 0
  dataSet$sVotes$total[is.na(dataSet$sVotes$total)] <- 0 
  dataSet$sVotes$points[is.na(dataSet$sVotes$points)] <- 0
 dataSet$sVotes$points <- round(dataSet$sVotes$points, 0) 

  dataSet$sDetections$approved[is.na(dataSet$sDetections$approved)] <- 0
  dataSet$sDetections$rejected[is.na(dataSet$sDetections$rejected)] <- 0
  dataSet$sDetections$total[is.na(dataSet$sDetections$total)] <- 0 
  dataSet$sDetections$points[is.na(dataSet$sDetections$points)] <- 0
   dataSet$sDetections$points <- round( dataSet$sDetections$points, 0) 
  
  return (dataSet)
}


# Funcion para obtener la efectividad de todos los usuarios sobre una coleccion de usuarios dada.
# Mode toma valores 0 y 1 en funcion de si se desea calcular la efectividad de sus detecciones o votos
get_effect_percentage <- function(users, mode) {
  if(mode == 0) {
    efect <- users$sDetections$approved / users$sDetections$total
  }
  else {
     efect <- users$sVotes$corrects / users$sVotes$total
  }
  efect[is.na(efect)] <- 0
  ret <- as.numeric(format(round(efect, 2), nsmall =2))
    return(ret)
}

# get_user_effect_percentage <- function(id, data){
#   return (data[data$`_id` == id,]) 
# }

grafico <- function(x, y, header, xtag, ytag, color){
  plot(x,y, main = header, xlab = xtag, ylab = ytag, col = color)
}

#grafico(users_by_points$points, get_effect_percentage(users_by_points),"'Actives' users by points/effectivity", "Points", "Effectivity" )

# Obtenemos el conjunto de datos de todos los usuarios
users <- getCollectionDB(db_name, users_collection, url_db, users_fields)

# Separamos en tres grupos, usuarios por detecciones, por votos y total.
# Eliminamos los usuarios con puntuación 0. Pasamos de 3668 elementos a 1072.
users <- users[c(users$points > 0),] #1072 usuarios
usersByDetections <- users[c(users$sDetections$points > 0),] #541 usuarios
usersByVotes <- users[c(users$sVotes$points > 0),] #921 usuarios

# Una opcion interesante es mostrar los usuarios en funcion de su puntuacion por deteccion / efectividad. Usaremos por tanto el grupo de los usuarios con puntuacion por deteccion > 0.
avgPointsDetec <- trunc(usersByDetections$sDetections$points / usersByDetections$sDetections$total , 0)
avgPointsDetec[c(avgPointsDetec > 100)] <-100
userEffect <- get_effect_percentage(usersByDetections,0)
userEffect <- (trunc(userEffect/ 0.02,0) * 0.02)
# grafico(avgPointsDetec, userEffect,"Users by user activity/effectivity", "Points", "Effectivity" )


# Creamos una estructura de datos sobre la que trabajaremos 
# str[id,points, pointsDetec, avgPointsDetec, effectDetec, votes, votesPoints]
str <- data.frame(usersByDetections$`_id`,usersByDetections$points, usersByDetections$sDetections$total, avgPointsDetec, userEffect, usersByDetections$sVotes$total,usersByDetections$sVotes$points, 0)
names(str) <- c("id", "points", "pointsDetec", "avgPointsDetec", "effectDetec", "votes", "votesPoints", "cluster")

# Nos interesa clusterizar en base a su media de puntos por deteccion y su efectividad en detecciones
dataUser <- as.data.frame(scale(str[,4:5]))
km <- kmeans(dataUser, centers = 6, nstart=50)
str$cluster <- km$cluster
plot(str$avgPointsDetec, str$effectDetec, col = str$cluster, xlab="Average points per detection", ylab="Effectiveness")
aggregate(str[,2:8], by = list(str$cluster), mean)

# Como vemos en el grafico, se ha clusterizado sobre el conjunto de datos str, categorizando a los usuarios, asignandoles un grupo. En este caso se han establecido 6 clusters.
