# Correlacion-de-SSM-dia-exacto-NDVI-con-NDVI
Aquí se muestra la correlación entre humedad (SSM) y índice de vegetación (NDVI), pero la humedad es el día exacto del día de NDVI sin promedio de los 5 días anteriores 
#MES ENERO

#### ubicación del directorio de trabajo
setwd("~/Análisis de Tesis en Rstudio y SAGA GIS/Variables/Vegetación/NDVI_1km_v2_2020/1_Enero_NDVI_1km_v2_2020")

###### Cargar librería raster 
library(raster)
library(maps)
library(dichromat)
library(spatialEco)
library(mapview)
library(rasterVis)
library(RColorBrewer)
###### Kmeans 
library("raster")  
library("cluster")
library("randomForest")
###### Regresión lineal
library(moonBook)
library(ggiraphExtra)
library(devtools)

## MES DE ENERO

#### ndvi Enero
ESP_NDVI_E <- stack(list.files("C:/Users/Usuario/Documents/Análisis de Tesis en Rstudio y SAGA GIS/Variables/Vegetación/NDVI_1km_v2_2020/1_Enero_NDVI_1km_v2_2020/3 DIAS DE VEGETACIÓN CON ESP, FRA, AFRICA", full.names = TRUE))
plot(ESP_NDVI_E)


## DATOS de SSM ENERO

SSM_enero <- stack(list.files("C:/Users/Usuario/Documents/Análisis de Tesis en Rstudio y SAGA GIS/Variables/Humedad/SSM_Bir  2020/1_Enero_SSM_Bir_2020/Imag SSM solo el dia de NDVI", full.names = TRUE))
plot(SSM_enero)

![image](https://user-images.githubusercontent.com/78845785/117970623-29212080-b329-11eb-91b2-fe3eb653d065.png)
![image](https://user-images.githubusercontent.com/78845785/117984022-c08d7000-b337-11eb-8ac5-264283d785da.png)


### METODO ESTADISTICO DE CORRELACIÓN

##### Primero se Ajustan los datos para el mismo mes (cambio de tamaño de pixel a uno mismo), es la misma capa Ajuste_NDVIenero, para que quede en el mismo tamaño ssm 'ngb' este metodo se refiere que ajusta a los vecinos cercanos 
Ajuste_NDVI_Enero <- resample(ESP_NDVI_E, SSM_enero, method='ngb')
plot(Ajuste_NDVI_Enero)
![image](https://user-images.githubusercontent.com/78845785/117984171-e61a7980-b337-11eb-9b0c-cbfb7009957a.png)

##### Mediana de los 3 dias de enero NDVI
medianaNDVIenero <- calc(Ajuste_NDVI_Enero, median, na.rm=TRUE)
writeRaster(medianaNDVIenero, file="medianadiaexacNDVIenero.tif")
![image](https://user-images.githubusercontent.com/78845785/117983673-773d2080-b337-11eb-9ce9-d14798d70eec.png)

###### Mediana de los 3 dias de enero SSM (Cuando na.rm es TRUE, la función omite cualquier valor de NA.)
medianaSSMenero <- calc(SSM_enero, median, na.rm=TRUE)
writeRaster(medianaSSMenero, file="medianadiaexactoSSMenero.tif")
![image](https://user-images.githubusercontent.com/78845785/117983854-9d62c080-b337-11eb-8b79-fb02c2737b81.png)

#Desviación estandar de los 3 dias de enero NDVI
SDNDVIenero <- calc(Ajuste_NDVI_Enero, sd, na.rm=TRUE)
writeRaster(SDNDVIenero, file="DesvstandiaexactNDVIenero.tif")

![image](https://user-images.githubusercontent.com/78845785/117983403-35ac7580-b337-11eb-90a2-17a2db55a40b.png)

#Desviación estandar de los 3 dias de enero SSM
SDSSMenero <- calc(SSM_enero, sd, na.rm=TRUE)
writeRaster(SDSSMenero, file="DesviacionstandardiaexactoSSMenero.tif")

![image](https://user-images.githubusercontent.com/78845785/117983534-58d72500-b337-11eb-9f96-268d02aff6ea.png)

###### Raster correlation (s es el tamaño de la ventana móvil para la correlación, x=SSM; y=NDVI)
correlaciónSSMNDVIenero <-rasterCorrelation(medianaSSMenero, medianaNDVIenero,  s = 9, type = "pearson")
#Eliminar los infinitos
correlaciónSSMNDVIenero[!is.finite(correlaciónSSMNDVIenero)] <- NA
plot(correlaciónSSMNDVIenero)

![image](https://user-images.githubusercontent.com/78845785/117971638-73ef6800-b32a-11eb-80d0-bd5357d7f759.png)

#
mapview(correlaciónSSMNDVIenero)

![image](https://user-images.githubusercontent.com/78845785/117971805-a305d980-b32a-11eb-8448-777f8d440db8.png)

![image](https://user-images.githubusercontent.com/78845785/117971913-c29d0200-b32a-11eb-8bdd-a3b1491ebdd0.png)

writeRaster(correlaciónSSMNDVIenero, file='correlacionenerodiaexactoSSMNDVIs9_diaNDVI.tif')
#cambio de colores 
pal <- colorRampPalette(brewer.pal(9, "BrBG"))
mapview(correlaciónSSMNDVIenero, col.regions = pal(100), legend = TRUE)

# VARIABLE TOPOGRAFICA

#KmeasnTopo muestra los 3 grupos topograficos
kmeansTopo <- raster("C:/Users/Usuario/Documents/Análisis de Tesis en Rstudio y SAGA GIS/Variables/Georfometria R_SAGA GIS/4 ATRIBUTOS TOPOGRAFICOS/kmeans_3gruposTopograficos_bueno.tif")
limn <- getData('GADM', country='ESP', level=2)
Km_Topografic <- mask(kmeansTopo,limn)
plot(Km_Topografic)

#### Correlación de 3 Variables 

#NDVI Proyectada
medianNDVI <- mask(medianaNDVIenero,limn)
NDVI <- projectRaster(medianNDVI, Km_Topografic)

#Humedad proyectada
medianSSM <- mask(medianaSSMenero,limn)
ssm <- projectRaster(medianSSM,Km_Topografic)

#### Unificación 3 variables 

Unión3variable <- stack(NDVI, ssm, Km_Topografic)
plot(Unión3variable)

![image](https://user-images.githubusercontent.com/78845785/117972229-26272f80-b32b-11eb-8ef4-4d43014228f8.png)


df <- na.omit(as.data.frame(Unión3variable, xy=TRUE))
write.csv(df, file='tableUnion_Enero.csv')
###### contiene esta cantidad de pixeles 612641
![image](https://user-images.githubusercontent.com/78845785/117972501-861dd600-b32b-11eb-9b24-6d66fac84ccf.png)

#
fit <- lm(layer.1 ~ layer.2 + as.factor(kmeans_3gruposTopograficos_bueno), df)
summary(fit)

###### luego correr este codigo
ggPredict(fit, se=TRUE,interactive=TRUE)



