# Script para obtener la base de datos, mongodb server debe de estar en ejecucion.

# Importamos paquetes necesarios.
library(mongolite)
library(scales)
library(ggplot2)
library(factoextra)
library(cluster)

# Variables globales mediante las cuales se obtiene el set de datos. 
# Se va a dividir en tres partes, detecciones, votos y grupo mixto.
db_name <- "caza"
users_collection <-  "users"
url_db <- "mongodb://localhost:27017"
users_fields <-  '{ "_id" : true, "sDetections" : true,  "points" : true, "sVotes" : true}'
queryy <- '{ "sDetections" : true,  "points" : true, "sVotes" : true}'
numClusters <-6 # (Por defecto) El analista decide este valor, posteriormente con los diferentes metodos sera modificado.


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
grafico(avgPointsDetec, userEffect,"Users by user activity/effectivity", "Points", "Effectivity",1 )


# Creamos una estructura de datos sobre la que trabajaremos 
# str[id,points, pointsDetec, avgPointsDetec, effectDetec, votes, votesPoints]
str <- data.frame(usersByDetections$`_id`,usersByDetections$points, usersByDetections$sDetections$total, avgPointsDetec, userEffect, usersByDetections$sVotes$total,usersByDetections$sVotes$points, 0)
names(str) <- c("id", "points", "pointsDetec", "avgPointsDetec", "effectDetec", "votes", "votesPoints", "cluster")

# Nos interesa clusterizar en base a su media de puntos por deteccion y su efectividad en detecciones
dataUser <- as.data.frame(scale(str[,4:5]))


km <- kmeans(dataUser, centers = numClusters, nstart=50)
kmeansData <- str
kmeansData$cluster <- km$cluster
plot(kmeansData$avgPointsDetec, kmeansData$effectDetec, col = kmeansData$cluster, xlab="Average points per detection", ylab="Effectiveness")
# aggregate(kmeansData[,2:8], by = list(kmeansData$cluster), mean)
# Como vemos en el grafico, se ha clusterizado sobre el conjunto de datos str, categorizando a los usuarios, asignandoles un grupo. En este caso se han establecido 6 clusters.
numClusters <- 6


# Usando la libreria factoextra podemos observar el numero optimo de cluster a crear, de tal forma que se ve a partir de 6 clusters, la diferencia con un numero de clusters mayor, 
# es muy cercana a 0 por lo que en princpio, se deberian establecer entre 6-10 clusters
# pamData <- as.data.frame(scale(str[,4:5]))
# fviz_nbclust(x = dataUser, FUNcluster = pam, method = "wss", k.max = 15, diss = dist(dataUser, method = "manhattan"))

pam_clusters <- pam(x = dataUser, k = numClusters, metric = "manhattan")
# fviz_cluster(object = pam_clusters, data = dataUser, ellipse.type = "t",repel = TRUE) +  theme_bw() +  labs(title = "Resultados clustering PAM") +  theme(legend.position = "none")
# Para mostrar los medoids, con el objecto calculado podemos obtenerlos y resaltarlos.
# Se seleccionan únicamente las proyecciones de las observaciones que son medoids
# Se emplean los mismos nombres que en el objeto ggplot
# Creación del gráfico
# Se resaltan las observaciones que actúan como medoids
medoids <- prcomp(dataUser)$x
medoids <- medoids[rownames(pam_clusters$medoids), c("PC1", "PC2")]
medoids <- as.data.frame(medoids)
colnames(medoids) <- c("x", "y")
fviz_cluster(object = pam_clusters, data = dataUser, ellipse.type = "t", repel = FALSE, show.clust.cent = TRUE, geom = "point")  + theme_bw() + geom_point(data = medoids, color = "firebrick", size = 5) +  labs(title = "Resultados clustering PAM") + theme(legend.position = "none")
# Como se puede ver en el grafico, obtenemos los diferentes clusters y su shape en forma de eclipse, mediante la cual podemos ver cual es el medoid asociado a cada cluster.


# Aplicando Hierarchical Kmeans
hc_euclidea_completo <- hclust(d = dist(x = dataUser, method = "euclidean"), method = "complete")
fviz_dend(x = hc_euclidea_completo, cex = 0.5, main = "Linkage completo", sub = "Distancia euclídea") + theme(plot.title =  element_text(hjust = 0.5, size = 15))
# Viendo el dendograma, podemos observar que hay 5 grandes grupos, al ser las ramificaciones del estilo de 2^n, se deberá ver si vemos el segundo nivel, podemos observar que,
# tendremos 4 clusters, sin embargo, vemos que eligiendo 4 clusters, la amplitud es muy alta por lo que tendremos grupos con mucha variacion, por lo que se seleccionaran los
# clusters asociados al siguiente nivel, es decir 2^3, tendremos 8 clusters.
numClusters <- 8
hkmeans_cluster <- hkmeans(x = dataUser, hc.metric = "euclidean", hc.method = "complete", k = numClusters)
fviz_cluster(object = hkmeans_cluster, pallete = "jco", repel = FALSE) + theme_bw() + labs(title = "Hierarchical k-means Clustering")

# Hasta este punto se ha visto como a cada observacion, se le asigna un solo cluster, mediante una clusterizacion difusa podemos saber con que grado, una observacion
# pertenece a un cluster o a otro.
fuzzy_cluster <- fanny(x = dataUser, diss = FALSE, k = numClusters, metric = "euclidean", stand = FALSE)
fviz_cluster(object = fuzzy_cluster, repel = FALSE, ellipse.type = "norm", pallete = "jco") + theme_bw() + labs(title = "Fuzzy Cluster plot")


