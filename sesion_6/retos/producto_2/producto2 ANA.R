## PRODUCTO 2

#establecer directorio
setwd("~/ecoinformatica_2014_2015/sesion_6/retos/producto_2")

#cargar paquetes
library(sp)
library(rgdal)
library(raster)
library(Kendall)
library(classInt)#para pintar los mapas
library(RColorBrewer)

#leer archivo
robles<-read.csv("robles_ecoinfo.csv", sep = ",", dec=".", header = T)
#visualizar archivo
View(robles)

## Cluster
#INDICACIONES-> "Para el cluster no debéis usar esas dos primeras columnas (coordenadas X,Y).
#Probad con varios número de clusters (p.e. 3, 4 y 5), decidir en función de los resultados
#y justificarlo de forma ecológica."

#seleccionar las columnas de las variables
vBiofisic<- robles[,3:33]

#posibles números del clúster
n<-3
n<-4
n<-5

#particionar entre n números de clúster
cluster<-kmeans(vBiofisic,n, iter.max=200)
#tomar las columnas de coordenadas
roblesCoor<- robles[,c(1,2)]
#concatenar las columnas de coordenadas con las del clúster, 
#así obtener las coordenadas clasificadas
roblesCoor<-cbind(roblesCoor, cluster[[1]])
#nombrar la columna en que se clasifican los clúster
colnames(roblesCoor)[3]<-"cluster"

## Pintar mapa
# definimos las coordenadas de los puntos
coordinates(roblesCoor) =~x+y
# definimos el sistema de coordenadas WGS84
proj4string(roblesCoor)=CRS("+init=epsg:23030")
# obtenemos n_cluster colores para una paleta de colores que se llama "Spectral", para cada cluster creado
plotclr <- rev(brewer.pal(n, "Spectral"))
#salida del mapa en pdf
pdf(file="mapaRoblesN5.pdf",height=8, width=10)
# plot, asignando el color en función del cluster al que pertenece
plot(roblesCoor, col=plotclr[roblesCoor$cluster], pch=19, cex = .6, main = "Grupos de roble - 5 clusters")
##fin orden pdf
dev.off()

#######################
#JUSTIFICACIÓN EJERCICIO

#Trás la observación de los diferentes mapas generados ser recomienda
#el uso de un n=3, pues un número superior de clusters genera posiblemente
#más tipos de robledal de los que sea necesario precisar, se observan
#demasiado disgregados en 4 clústers, mientras que 2 sería muy impreciso.

######################