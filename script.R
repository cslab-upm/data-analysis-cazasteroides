# Script para obtener la base de datos, mongodb server debe de estar en ejecucion.

# Importamos paquetes necesarios.
library(mongolite)
library(scales)
library(ggplot2)
library(factoextra)

# Variables globales mediante las cuales se obtiene el set de datos.
db_name <- "caza"
users_collection <-  "users"
url_db <- "mongodb://localhost:27017"
users_fields <-  '{ "_id" : true, "sDetections" : true,  "points" : true, "sVotes" : true}'


# Funcion que permite obtener una collecion de una tabla de mongoDb.
getCollectionDB <- function(db_name, db_collection, url_db) {

  ret <-  mongo(collection = db_collection, db = db_name, url = url_db)
  return(ret)
}

# Funcion que permite obtener un set de datos sobre una coleccion de mongoDb.
# Se establece por defecto, un limite de 100000 filas y 0 saltados por el numero de usuarios de la plataforma, en la bd actual hay <4000.
getSetCollectionDb <- function(collection, list_fields){
  ret <- collection$find(limit = 100000, skip = 0, fields = list_fields)
  return(ret)
}


# Funcion para obtener la efectividad de todos los usuarios sobre una coleccion de usuarios dada
get_effectiveness_percentage <- function(users) {
  efect <- (users$sDetections$approved / users$sDetections$total)
  efect[is.na(efect)] <- 0
  ret <- as.numeric(format(round(efect, 5), nsmall = 5))
    return(ret)
}

get_user_effectiveness_percentage <- function(user_id, data){
  return (data[data$`_id` == user_id,]) 
}



users <- getCollectionDB(db_name, users_collection, url_db)
users_set <- getSetCollectionDb(users, users_fields)

ids <- users_set$`_id`

effectiveness <- get_effectiveness_percentage(users_set)

points <- users_set$points
points[is.na(points)] <- 0

#points <- rnorm(points)
#points <- scales::rescale(points, to=c(0,1000))
#points <- as.numeric(format(round(points, 5), nsmall = 5))


effectiveness <- as.numeric(format(round(effectiveness, 5), nsmall = 5))

plot(points, effectiveness, main = "Usuarios en funcion de sus puntos/efectividad", xlab = "Puntos", ylab = "Efectividad")


