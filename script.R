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

grafico <- function(x, y, header, xtag, ytag){
  plot(x,y, main = header, xlab = xtag, ylab = ytag)
}


#grafico(users_by_points$points, get_effect_percentage(users_by_points),"'Actives' users by points/effectivity", "Points", "Effectivity" )

# Obtenemos el conjunto de datos de todos los usuarios
users <- getCollectionDB(db_name, users_collection, url_db, users_fields)

# Separamos en tres grupos, usuarios por detecciones, por votos y total.
# Eliminamos los usuarios con puntuación 0. Pasamos de 3668 elementos a 1072.
users <- users[c(users$points > 0),] #1072 usuarios
usersByDetections <- users[c(users$sDetections$points > 0),] #541 usuarios
usersByVotes <- users[c(users$sVotes$points > 0),] #921 usuarios

# Nos centramos en los usuarios que han obtenido puntuación mediante detecciones, estamos clasificando mediante puntos por detecciones / efecitivdad.
# grafico(usersByDetections$sDetections$points, effectivityUsers,"'Actives' users by points/effectivity", "Points", "Effectivity" )
# Se sigue que, la gran mayoria de usuarios se concentran en el intervalo [0-10000].

# Se obtiene que la media de puntos por deteccion es de 28.02 puntos por deteccion y la media de detecciones por usuario es de 123. 
avgPD <- sum(usersByDetections$sDetections$points / usersByDetections$sDetections$total) / length(usersByDetections$points)
avgD <- sum(usersByDetections$sDetections$total) / length(usersByDetections$points) 
# usersByDetectionsOverRatio <- usersByDetections[c((usersByDetections$sDetections$points / usersByDetections$sDetections$total > 28) | (usersByDetections$sDetections$total > 60) ),]
# grafico(usersByDetectionsOverRatio$sDetections$points, get_effect_percentage(usersByDetectionsOverRatio, 0),"'Actives' users by points/effectivity", "Points", "Effectivity" )
# Viendo que asi obtenemos los usuarios activos, se ve que a pesar de ello la grafica sigue con la misma dinamica, el motivo es por la visualizacion 
# en funcion de los puntos de los usuarios.
# Veamos que ocurre si, el eje X, pasa a ser un valor relacionado con la actividad de los usuarios, es decir, tratar de cuantificar como de activos son los usuarios.
# De esta forma, podemos ver claramente que un usuario que ha realizado multiples detecciones y votaciones,
# es un usuario activo al igual que un usuario que ha obtenido por ejemplo 150000 puntos. De esta manera, ampliaremos la grafica y podemos realizar una clusterizacion mas precisa.
# La cuatificacion sobre 10, una primera parte que se calcula en funcion de puntos obtenidos por deteccion. avgPointsDetec y la segund parte,
# pointsActivity es una valoracion entre [0,5] que se asigna al usuario en funcion de su actividad dentro de la plataforma
# pointsActivity[i] (0.1)*numeroDetecciones - (0.1)*deteccionesRechazadas + (1, si esta por encima de la media de detecciones) + (0.1)*votosTotales. 
# Por tanto userActivity sera la suma de ambas partes, como maximo cada parte puede sumar 5 puntos y nunca restar.  
avgPointsDetec <- (usersByDetections$sDetections$points / usersByDetections$sDetections$total) / 30
avgPointsDetec[is.na(avgPointsDetec)] <- 0
avgPointsDetec[c(avgPointsDetec > 5)] <- 5 # Caso en el que un usuario ha obtenido mas de 150 puntos por detenccion. En principio no se puede dar el caso.
avgPointsDetec <- round(avgPointsDetec, 1)
pointsActivity <- ifelse(((0.1)*usersByDetections$sDetections$approved - (0.1)*usersByDetections$sDetections$rejected + (0.1)*usersByDetections$sVotes$total + (ifelse(usersByDetections$sDetections$total > avgD, 1,0))) 
 > 4.99, 5, ((0.1)*usersByDetections$sDetections$approved - (0.1)*usersByDetections$sDetections$rejected + (0.1)*usersByDetections$sVotes$total + (ifelse(usersByDetections$sDetections$total > avgD, 1,0))) 
)
pointsActivity[is.na(pointsActivity)] <- 0
pointsActivity <- round(pointsActivity, 1)
pointsActivity[c(pointsActivity < 0)] <-0
userActivity <- avgPointsDetec + pointsActivity

grafico(userActivity, get_effect_percentage(usersByDetections,0),"Users by user activity/effectivity", "Points", "Effectivity" )
# Se ve claramente que al puntuar la actividad de los usuarios, la usuarios representados en la grafica estan mas dispersos. 
# Se esta actuando sobre el set de datos de usuarios con puntuacion de detecciones > 0, si lo aplicamos sobre el set de usuarios con puntuacion global > 0,
# apareceran los restantes osbre el eje X dispersados en funcion de sus votos.



 


