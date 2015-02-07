## PRODUCTO UNO

#establecer el directorio de trabajo
setwd("~/ecoinformatica_2014_2015/sesion_6/retos/producto_1")

#instalar paquetes
install.packages(c("Kendall", "classInt"))

#cargar paquetes
library(sp)
library(rgdal)
library(raster)
library(Kendall)
library(classInt)#para pintar los mapas
library(RColorBrewer)


#leer archivos
ndvi<- read.csv("ndvi_robledal.csv", sep=";", dec=".",header=T)
nieve<- read.csv("nieve_robledal.csv", sep=";", dec=".",header=T)

#crear la variable tendencias para cada archivo
tendNdvi<-data.frame()
tendNieve<-data.frame()

#crear data.frames respectivos con los campos:
  #identificador de pixel, y los componentes del 
  #análisis Mann-Kendall (tau y pvalue) vacíos
tendNdvi2<-data.frame(iv_malla_modi_id=NA,tauNdvi=NA,pvalueNdvi=NA)
tendNieve2<-data.frame(nie_malla_modi_id=NA,tauNieve=NA,pvalueNieve=NA)

#eliminar las filas repetidas de la columna identificador
  #en la matriz de datos, dejando sólo uno de cada pixel y extraerlo
identNdvi<-unique(ndvi$iv_malla_modi_id)
identNieve<-unique(nieve$nie_malla_modi_id)


#### ANÁLISIS DE TENDENCIAS 

## BUCLE TENDENCIAS del NDVI

for(pixel in identNdvi)
  {
  #data.frame con todas las tuplas para un mismo identificador de pixel
  cadapixelNDVI<-ndvi[ndvi$iv_malla_modi_id==pixel,]
  #en este data.frame, realizar MannKedall del valor del NDVI (así, ver tendencia en cada pixel)
  mk<-MannKendall(cadapixelNDVI$ndvi_i)
  #asignar el identificador de un pixel a dicha columna en el data.frame vacío
  tendNdvi2$iv_malla_modi_id<-pixel
  #tomar la columna de tau del análisis M-K y asignarla a la columna correspondiente en tendencias
  tendNdvi2$tauNdvi<-mk$tau[1]
  #tomar la columna de pvalor del análisis M-K y asignarla a la columna correspondiente en tendencias
  tendNdvi2$pvalueNdvi<-mk$sl[1] 
  #para concatenar todas las tendencias en el data.frame inicial
  tendNdvi<-rbind(tendNdvi,tendNdvi2)
}

## BUCLE TENDENCIAS de NIEVE

for(pixel in identNieve)
{
  #data.frame con todas las tuplas para un mismo identificador de pixel
  cadapixelNIEVE<-nieve[nieve$nie_malla_modi_id==pixel,]
  #en este data.frame, realizar MannKedall de la duración de la nieve (así, ver tendencia en cada pixel)
  mkNIEVE<-MannKendall(cadapixelNIEVE$scd)
  #asignar el identificador de un pixel a dicha columna en el data.frame vacío
  tendNieve2$nie_malla_modi_id<-pixel
  #tomar la columna de tau del análisis M-K y asignarla a la columna correspondiente en tendencias
  tendNieve2$tauNieve<-mkNIEVE$tau[1]
  #tomar la columna de pvalor del análisis M-K y asignarla a la columna correspondiente en tendencias
  tendNieve2$pvalueNieve<-mkNIEVE$sl[1] 
  #para concatenar todas las tendencias en el data.frame inicial
  tendNieve<-rbind(tendNieve,tendNieve2)
}


####TRATAMIENTO DE DATOS

#instalar y cargar el paquete "plyr" ->comando "join"
install.packages("plyr")
library("plyr")

 #NDVI
#selecionar las columnas identificador, longitud y latitud del ndvi
ndviCoor<- ndvi[,c(1,4:5)]
#eliminar las tuplas repetidas en este data.frame, 
  #dejando cada coordenada con un identificador de pixel
coordNDVI<-unique(ndviCoor)
#unir el resultado del análisis M-K con el data.frame de coordenadas,
  #por la columna común identificador del pixel
mapaNdvi<-join(tendNdvi,coordNDVI, by="iv_malla_modi_id")
head(mapaNdvi)

 #NIEVE
#selecionar las columnas identificador, longitud y latitud del archivo nieve
nieveCoor<- nieve[,c(2,10,11)]
#eliminar las tuplas repetidas en este data.frame, 
  #dejando cada coordenada con un identificador de pixel
coordNIEVE<-unique(nieveCoor)
#unir el resultado del análisis M-K con el data.frame de coordenadas,
  #por la columna común identificador del pixel
mapaNieve<-join(tendNieve,coordNIEVE, by="nie_malla_modi_id")
head(mapaNieve)

####PINTAR LOS MAPAS

#MAPA NDVI
## definimos las coordenadas de los puntos
coordinates(mapaNdvi) =~lng+lat
## definimos el sistema de coordenadas WGS84
proj4string(mapaNdvi)=CRS("+init=epsg:4326")


## partimos los valores de tau en 5 clases
clases <- classIntervals(mapaNdvi$tauNdvi, n = 5)
## obtenemos cinco colores para una paleta de colores que se llama "Spectral"
plotclr <- rev(brewer.pal(5, "Spectral"))
## Asociamos los valores de tau a su valor correspondiente
colcode <- findColours(clases, plotclr)
#salida del mapa en pdf
pdf(file="mapaNDVI.pdf",height=8, width=10)
## plot sin tener en cuenta
plot(mapaNdvi, col=colcode, pch=19, cex = .6, main = "Mapa de tendencias NDVI")
## mostramos la leyenda
legend("topright", legend=names(attr(colcode, "table")), fill=attr(colcode, "palette"), bty="n")
##fin orden pdf
dev.off()

###### Otra forma de pintar el mapa

mapaNdvi$significativa <- ifelse(mapaNdvi$pvalueNdvi < 0.05, 1, 2)
#salida del mapa en pdf
pdf(file="mapaNDVI2.pdf",height=8, width=10)
## plot sin tener en cuenta
plot(mapaNdvi, col=colcode, pch=c(17, 19)[as.numeric(mapaNdvi$significativa)], cex = .6, main = "Mapa de tendencias NDVI 2")
## mostramos la leyenda
legend("topright", legend=names(attr(colcode, "table")), fill=attr(colcode, "palette"), bty="n")
##fin orden pdf
dev.off()

#MAPA NIEVE

mapaNieve$significativa <- ifelse(mapaNieve$pvalueNieve < 0.05, 1, 2)
#salida del mapa en pdf
pdf(file="mapaNIEVE.pdf",height=8, width=10)
## plot sin tener en cuenta
plot(mapaNieve, col=colcode, pch=c(17, 19)[as.numeric(mapaNieve$significativa)], cex = .6, main = "Mapa de tendencias NIEVE")
## mostramos la leyenda
legend("topright", legend=names(attr(colcode, "table")), fill=attr(colcode, "palette"), bty="n")
##fin orden pdf
dev.off()
