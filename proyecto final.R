library(dplyr)
library(tidyverse)
library(glmnet)
library(haven)
library(stringr)
library(ggplot2)
library(tseries)
library(grid)
library(tibble)
library(gtable)
library(httr)
library(jsonlite)
library(rjson)
library(PerformanceAnalytics)
library(zoo)

datos<-read.csv("C:\\Users\\fcolo\\OneDrive\\Documentos\\Codes\\R_codes\\pcd\\afluencias.csv")
datos$fecha <- as.Date(datos$fecha)
datos$anio <- format(datos$fecha, "%Y")

lista_lineas <- split(datos, datos$linea)

suma<-function(linea){linea %>%
    group_by(anio) %>%
    summarise(total_afluencias = sum(afluencia))
}

#afluencias anuales totales
suma_afluencia_anual<-suma(datos)
View(suma_afluencia_anual) 
plot(suma_afluencia_anual)


#afluencias anuales por lineas
linea1 <- lista_lineas[["Linea 1"]]
  suma_afluencias_l1 <- suma(linea1)

linea2 <- lista_lineas[["Linea 2"]]
  suma_afluencias_l2 <- suma(linea2)

linea3 <- lista_lineas[["Linea 3"]]
  suma_afluencias_l3 <- suma(linea3)

linea4 <- lista_lineas[["Linea 4"]]
    suma_afluencias_l4 <- suma(linea4)
    
linea5 <- lista_lineas[["Linea 5"]]
    suma_afluencias_l5 <- suma(linea5)

linea6 <- lista_lineas[["Linea 6"]]
    suma_afluencias_l6 <- suma(linea6)

linea7 <- lista_lineas[["Linea 7"]]
    suma_afluencias_l7 <- suma(linea7)
    
linea8 <- lista_lineas[["Linea 8"]]
    suma_afluencias_l8 <- suma(linea8)

linea9 <- lista_lineas[["Linea 9"]]
    suma_afluencias_l9 <- suma(linea9)
    
lineaA <- lista_lineas[["Linea A"]]
    suma_afluencias_lA <- suma(lineaA)

lineaB <- lista_lineas[["Linea B"]]
    suma_afluencias_lB <- suma(lineaB)

linea12 <- lista_lineas[["Linea 12"]]
    suma_afluencias_l12 <- suma(linea12)
    
    
#intervalo de confianza
    
n<- length(datos$afluencia)
  media<-mean(datos$afluencia)
  desv<-sd(datos$afluencia)
  valor_critico<-qt(0.95, df=n-1)
  error<-desv/sqrt(n)
  intervalo_de_confianza<-media+c(-1,1)*valor_critico*error
  intervalo_de_confianza
  

  
#Calculamos la capacidad máxima del metro

capacidad_tren<-1530
linea <- c("l1","l2","l3","l4","l5","6","l7","l8","l9","lA","lB","l12")
unidades<- c(50,41,54,14,25,15,32,30,34,33,36,30)
dur_reco<-
unidades_linea <- data.frame(linea,unidades)

dr<-c(29, 34, 34, 24, 24, 21, 30, 32, 23, 27, 33)
trenes_pm<-unidades/dr
trenes_pm

cm<-function(trenes_pm, linea, capacidad_tren){
  capacidad_tren*(60*19)/ trenes_pm[linea]
}

cm_pl<-c()
for(i in 1:12){
  cm_pl[i]<-cm(trenes_pm, i, capacidad_tren)
}
cm_pl
#capmax nos da la capacidad máxima de las lineas al día
capmax<-sum(cm_pl)



#Obtenemos los datos del crecimiento poblacional
                                                                    
personas <- as.integer(c(8851080,8877932,8904784,8931635,8958487, 8985339, 9030260,9075181,9120102,9165023, 9209944))
plot(personas)

afluencia<-suma_afluencia_anual$total_afluencias[1:11]
plot(afluencia)

#calculmos correlación
cor(personas, afluencia)
dat<-data.frame(personas, afluencia)

cor.test(personas, afluencia)

chart.Correlation(dat)


años<-2010:2020
df<-data.frame(afluencia, personas,años)
afluencia<-suma_afluencia_anual$total_afluencias[1:11]

modelo<-lm(personas~.,
           data=df)
print(modelo)
summary(modelo)
predicciones <- predict(modelo, newdata = df)
predicciones

chart.Correlation( dat,
  method = c("pearson")
)