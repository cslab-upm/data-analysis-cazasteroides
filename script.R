# Script para obtener la base de datos, mongodb server debe de estar en ejecucion.
# Importamos paquetes necesarios.
library(mongolite)
library(scales)
library(ggplot2)
library(factoextra)
library(cluster)
library(jsonlite)

# Variables globales mediante las cuales se obtiene el set de datos. 
db_name <- "cazasteroides"
users_collection <-  "users"
url_db <- "mongodb://localhost:27017"
users_fields <-  '{ "_id" : true, "sDetections" : true,  "sVotes" : true, "username" : true}'
set.seed(100)

# Metodo que permite obtener un set de datos de una tabla de mongoDb.
getCollectionDB <- function(db_name, db_collection, url_db, list_fields) {
  ret <-  mongo(collection = db_collection, db = db_name, url = url_db)
  ret <-  ret$find(limit = 100000, skip = 0, fields = list_fields)
  ret <- setFormat(ret)
  return(ret)
}

# Metodo auxiliar con la que se obtiene el conjunto de datos con formato correcto.
# La base de datos proporcionada posee usuarios con puntuaciones negativas o sin puntuacion por defecto.
# Se sometera al conjunto de datos a un formateado forzoso.
setFormat <- function(dataSet){
    dataSet$sDetections[is.na(dataSet$sDetections)] <- 0
    dataSet$sVotes[is.na(dataSet$sVotes)] <- 0
    dataSet$sDetections[is.na(dataSet$sDetections)] <- 0
  return (dataSet)
}

getSTR <- function(target, mode){
  str <- data.frame(target$`_id`, target$username,(ifelse(target$sVotes$total < 1,0,(target$sVotes$points / target$sVotes$total)/2) + ifelse(target$sDetections$total < 1,0,(target$sDetections$points / target$sDetections$total)/2)),  (ifelse(target$sVotes$total < 1,0,(target$sVotes$corrects / target$sVotes$total)/2) + ifelse(target$sDetections$total < 1,0,(target$sDetections$approved / target$sDetections$total)/2)),target$sDetections$points,target$sVotes$points,target$sDetections$total , target$sVotes$total,0)
  names(str) <- c("id",  "username", "Puntacion por accion", "Efectividad por accion ","Puntos por detecciones", "Puntos por votos","Detecciones totales" , "Votos totales", "cluster")
  if(mode == 1) {
    str[3] <- ifelse(target$sDetections$total < 1,0,(target$sDetections$points / target$sDetections$total))
    str[4] <- ifelse(target$sDetections$total < 1,0,(target$sDetections$approved / target$sDetections$total))
  } 
  if(mode == 2) {
    str[3] <- ifelse(target$sVotes$total < 1,0,(target$sVotes$points / target$sVotes$total))
    str[4] <- ifelse(target$sVotes$total < 1,0,(target$sVotes$approved / target$sVotes$total))
  }  
  return (str)
}

# Metodo que permite obtener la informacion media de cada cluster de un conjunto de datos.
# Se utilizara para comparar los resultados obtenidos de los diferentes metodos de clusterizacion.
getDataAlg <- function (algObject) {
  ret <- aggregate(algObject[,3:length(algObject)], by = list(algObject$cluster), mean)
  for(e in 1:10) {
    ret$totalUsr[e] <- length(which(algObject$cluster == e))
  }
  ret <- round(ret, 2)
  return (ret)
}

getClusterAPI <- function(object, str){
  ret <- object$username
  for(i in 1:length(ret)) {
    ret[i] <- ifelse( length(str[c(str["username"] == ret[i]),]$cluster)>0, str[c(str["username"] == ret[i]),]$cluster, 0)
  }
  return(ret)
}

getIdAPI <- function(object, str){
  ret <- object$username
  for(i in 1:length(ret)) {
    ret[i] <- ifelse( length(str[c(str["username"] == ret[i]),]$id)>0, str[c(str["username"] == ret[i]),]$id, 0)
  }
  return(ret)
}

# Se obtiene el conjunto de datos de todos los usuarios y se agrupan en datos pre-procesados.
users <- getCollectionDB(db_name, users_collection, url_db, users_fields)
totalUsers <- users
users <- users[c(users$sDetections$total > 0 | users$sVotes$total > 0),]
users <- users[c((users$sDetections$points + users$sVotes$points) > 0),]
usersByDetections <- users[c(users$sDetections$points > 0),] 
usersByVotes <- users[c(users$sVotes$points > 0),] 
# usersByAPI <- users[c(users$sDetections$approved > 50 | users$sVotes$corrects > 50),]
# [target: users , mode : 0]
# [target: usersByDetections , mode : 1]
# [target: usersByVotes , mode : 2]
target <- users
mode <- 0
str <- getSTR(target, mode)
dataUser <- as.data.frame(str[,3:4])
dataUserScaled <- as.data.frame(scale(str[,3:4]))
names(dataUserScaled) <- c("x", "y") 

# Calculo numero mas eficiente de clusters
clusters <- kmeans(dataUser, centers = 1, nstart=50)$betweenss
for(i in 2:13) clusters[i] <- kmeans(dataUser, centers = i, nstart=50)$betweenss
clusters.diff <- clusters[2] - clusters[1]
for(i in 2:12) clusters.diff[i] <- clusters[i+1] - clusters[i]
# grafico.nOptimo.kmeans <- plot(1:13, clusters, type="b", xlab="Numero de clusters", ylab="Suma de cuadrados inter grupos")
# grafico.nOptimo.factoextra <- fviz_nbclust(x = dataUser, FUNcluster = pam, method = "wss", k.max = 12, diss = dist(dataUser, method = "manhattan")) + labs(title = "Busqueda numero optimo de clusters", x = "Numero de clusters", y = "Valor suma de cuadrados", color ="Cluster")
numClusters <- 10


# Obtencion grafico poblacion total de usuarios.
data.noClusters <- dataUserScaled
# grafico.sin_clusterizacion <- ggplot(data.noClusters, aes(x = data.noClusters$x, y = data.noClusters$y)) + geom_point(size = 1)+ theme_bw() + labs(x = "Puntuacion media", y = "Media de efectividad")


# Aplicacion del algoritmo KMeans, asignacion de clusters a la estructura str
data.kmeans.data <- dataUserScaled
data.kmeans <-  kmeans(data.kmeans.data, centers = numClusters, nstart=50)
data.kmeans.cluster <- data.kmeans$cluster
data.kmeans.str <- str
data.kmeans.str$cluster <-data.kmeans.cluster
grafico.kmeans <- ggplot(data = data.kmeans.data, aes(x = data.kmeans.data$x, y = data.kmeans.data$y, group = data.kmeans.cluster ,color = as.factor(data.kmeans.cluster)))+ geom_point(size = 1) + theme_bw() + labs(x = "Puntuacion media", y = "Media de efectividad") + theme(legend.position = "none")


# Aplicacion del algoritmo Kmedoid emplando metodo PAM, calculo de medoids y asignacion de clusters.
data.kmedoid.data <- dataUser
data.kmedoid <- pam(x = data.kmedoid.data, k = numClusters, metric = "manhattan")
data.kmedoid.medoids <- prcomp(data.kmedoid.data)$x
data.kmedoid.medoids <- data.kmedoid.medoids[rownames(data.kmedoid$medoids), c("PC1", "PC2")]
data.kmedoid.medoids <- as.data.frame(data.kmedoid.medoids)
colnames(data.kmedoid.medoids) <- c("x", "y")
data.kmedoid.cluster <- data.kmedoid$clustering
data.kmedoid.str <- str
data.kmedoid.str$cluster <- data.kmedoid.cluster
# grafico.pam.poly <-fviz_cluster(object = data.kmedoid, pallete = "jco", repel = FALSE, geom= "point") + theme_bw() + geom_point(data = data.kmedoid.medoids, color = "firebrick", size = 1) +  labs(x = "Puntuacion media", y = "Media de efectividad", colour = "Cluster", title ="") + theme(legend.position = "none")
# grafico.pam.ellip <-fviz_cluster(object = data.kmedoid, repel = FALSE, ellipse.type = "t", pallete = "jco", geom = "point") + theme_bw() + geom_point(data = data.kmedoid.medoids, color = "firebrick", size = 1) +  labs(x = "Puntuacion media", y = "Media de efectividad", colour = "Cluster", title ="") + theme(legend.position = "none")
grafico.pam_no_medoids <-fviz_cluster(object = data.kmedoid, data = data.kmedoid.data, ellipse = "FALSE",repel = TRUE, geom = "point") +  theme_bw() +  labs(x = "Puntuacion media", y = "Media de efectividad", color = "Cluster") +  theme(legend.position = "none")


data.hkmeans.data <- dataUserScaled
data.hkmeans.dendograma <- hclust(d = dist(x = data.hkmeans.data, method = "euclidean"), method = "complete")
data.hkmeans <- hkmeans(x = data.hkmeans.data, hc.metric = "euclidean", hc.method = "complete", k = numClusters)
data.hkmeans.str <- str
data.hkmeans.cluster <- data.hkmeans$cluster
data.hkmeans.str$cluster <- data.hkmeans.cluster
# grafico.dendograma <- fviz_dend(x = data.hkmeans.dendograma, cex = 0.5, main = "Dendograma clusterizacion HKmeans", sub = "Distancia euclidea", rect =TRUE, horiz = TRUE, repel=FALSE) + theme(plot.title =  element_text(hjust = 0.5, size = 15))
grafico.hkmeans <- fviz_cluster(object = data.hkmeans, data = data.hkmeans.data ,pallete = "jco", ellipse = FALSE, repel = TRUE, show.clust.cent = TRUE, geom = "point")  + theme_bw() + labs(x = "Puntuacion media", y = "Media de efectividad", colour = "Cluster", title ="") + theme(legend.position = "none")


# # Aplicacion del algoritmo difuso Fuzzy junto con asignacion de clusters.
data.fuzzy.data <- dataUser
data.fuzzy <- fanny(x = data.fuzzy.data, k = numClusters, memb.exp = 2, metric = "SqEuclidean", stand = FALSE, iniMem.p = NULL, cluster.only = FALSE, maxit = 500, tol = 1e-15, trace.lev = 0)
data.fuzzy.str <- str
data.fuzzy.cluster <- data.fuzzy$clustering
data.fuzzy.str$cluster <- data.fuzzy.cluster
# grafico.fuzzy.poly <- fviz_cluster(object = data.fuzzy, pallete = "jco", repel = FALSE, geom= "point") + theme_bw() +  labs(x = "Puntuacion media", y = "Media de efectividad", colour = "Cluster", title ="") + theme(legend.position = "none")
grafico.fuzzy <- fviz_cluster(object = data.fuzzy, data = data.fuzzy.data ,pallete = "jco", ellipse = FALSE, repel = TRUE, show.clust.cent = FALSE, geom = "point")  + theme_bw()  +  labs(x = "Puntuacion media", y = "Media de efectividad", colour = "Cluster", title ="") + theme(legend.position = "none")
# grafico.fuzzy.ellip <- fviz_cluster(object = data.fuzzy, repel = FALSE, ellipse.type = "norm", pallete = "jco", geom = "point") + theme_bw() +  labs(x = "Puntuacion media", y = "Media de efectividad", colour = "Cluster", title ="") + theme(legend.position = "none")


# Obtencion informacion media de cada cluster por algoritmos
info.kmeans <- getDataAlg(data.kmeans.str)
info.kmedoid <- getDataAlg(data.kmedoid.str)
info.hkmeans <- getDataAlg(data.hkmeans.str)
info.fuzzy <- getDataAlg(data.fuzzy.str)

# Obtencion usuarios mediante el API
api.users.effectiveDetections <- fromJSON("http://api.cazasteroides.org/statistics/top/500/moreEffectiveDetections")
api.users.effectiveVotes <- fromJSON("http://api.cazasteroides.org/statistics/top/500/moreEffectiveVotes")

api.users.id <- getIdAPI(api.users.effectiveDetections, data.kmeans.str) 
api.kmeans.effectiveDetections <- getClusterAPI(api.users.effectiveDetections, data.kmeans.str)
api.kmeans.effectiveVotes <- getClusterAPI(api.users.effectiveVotes, data.kmeans.str)

api.kmedoid.effectiveDetections <- getClusterAPI(api.users.effectiveDetections, data.kmedoid.str)
api.kmedoid.effectiveVotes <- getClusterAPI(api.users.effectiveVotes, data.kmedoid.str)

api.hkmeans.effectiveDetections <- getClusterAPI(api.users.effectiveDetections, data.hkmeans.str)
api.hkmeans.effectiveVotes <- getClusterAPI(api.users.effectiveVotes, data.hkmeans.str)

api.fuzzy.effectiveDetections <- getClusterAPI(api.users.effectiveDetections, data.fuzzy.str)
api.fuzzy.effectiveVotes <- getClusterAPI(api.users.effectiveVotes, data.fuzzy.str)

# Comparacion de usuarios del API y su asignacion mediante los diferentes algoritmos
api.compare.effectiveDetections <- data.frame(api.kmeans.effectiveDetections ,api.kmedoid.effectiveDetections , api.hkmeans.effectiveDetections,api.fuzzy.effectiveDetections,api.users.id)
api.compare.effectiveVotes <- data.frame(api.kmeans.effectiveVotes ,api.kmedoid.effectiveVotes , api.hkmeans.effectiveVotes,api.fuzzy.effectiveVotes, api.users.id)


