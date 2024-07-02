setwd("C:/Users/Laura/Documents/INCLIVA TRABAJO/PICS/datos")


# Instalación de paquetes

install.packages("zoo")
install.packages("rts", repos = "http://cran.us.r-project.org")
install.packages("ggplot2", repos = "http://cran.us.r-project.org")
install.packages("tidyverse")
install.packages("plotly")
install.packages('na.tools')
install.packages("lubridate")
install.packages('caTools')
install.packages("gdata")
install.packages ("PCAmixdata")
install.packages("EHRtemporalVariability")
install.packages("clock")
install.packages("tidyr")
install.packages("openxlsx")
install.packages("writexl")
install.packages("plyr")
install.packages("svDialogs")
install.packages("dplyr")
install.packages("xlsx")

#Librerias

library(zoo)
library(rts)
library(ggplot2)
library(tidyverse)
library(datacheck)
library(plotly)
library(knitr)
library(na.tools)
library(caTools)
library(lubridate)
library(gdata)
library(PCAmixdata)
library(EHRtemporalVariability)
library(clock)
library(tidyr)
library(openxlsx)
library(writexl)
library(plyr)
library(dplyr)
library(svDialogs)
library(xlsx)



# Funciones
source("/Users/Laura/Documents/INCLIVA TRABAJO/AVI SepAsT/Datos/is_empty.R")
source ("/Users/Laura/Documents/INCLIVA TRABAJO/AVI SepAsT/Datos/crear_tablas_union_2v.R")
source ("/Users/Laura/Documents/INCLIVA TRABAJO/AVI SepAsT/Datos/obtener_clases_bbdd.R")
source("/Users/Laura/Documents/INCLIVA TRABAJO/AVI SepAsT/Datos/obtener_etiquetas_bbdd.R")
source("/Users/Laura/Documents/INCLIVA TRABAJO/AVI SepAsT/dimCompletenessByColumn.R")
source("/Users/Laura/Documents/INCLIVA TRABAJO/PICS/dimCompleteness.R")
source("/Users/Laura/Documents/INCLIVA TRABAJO/PICS/media_sd.R")

## borrar todo (variables y funciones) del environment 
# rm(list=ls())

## borrar solo las variables del environment 
# rm(list= setdiff(ls(), lsf.str()))

## borrar solo las funciones del environment 
# rm(list=lsf.str())


### Cargar datos
fileName = "PICS_v3.csv" #nombre del fichero a ser analizado
HasHeader = TRUE #Si la primera columna contiene el nombre de las variables

repository_og <- read.csv(fileName,header = HasHeader, na.strings=c("NA"))
repo_sepsis_PICS <-repository_og
N_pacientes<- nrow(repo_sepsis_PICS)
M_variables<- ncol(repo_sepsis_PICS)
variable_names= colnames(repo_sepsis_PICS)



### Dar formato a las variables/datos

# Fecha


# Obtenemos la columna con las fechas de ingreso y vemos que hay diferentes 
# formatos de fechas

dateColumn<- which(colnames(repo_sepsis_PICS)== "fechaingreso")

#dateFormat = "%m/%d/%Y"
dates <- repo_sepsis_PICS[,dateColumn] 

# Transformamos la fecha al formato año-mes-dia con format y después con as.Date
repo_sepsis_PICS_Dates <- (as.Date(dates,"%Y-%m-%d"))

repo_sepsis_PICS [,dateColumn]<-repo_sepsis_PICS_Dates

repo_sepsis_PICS <- formatDate(
  input = repo_sepsis_PICS,
  dateColumn ="fechaingreso",
  dateFormat = "%Y-%m-%d")

dates_PESET<- which(colnames(repo_sepsis_PICS)== dateColumn)

# Ordenamos el respositorio de datos según las fechas
repo_sepsis_PICS = repo_sepsis_PICS[order(dates_PESET),]
minDate = min(dates_PESET,na.rm = TRUE)
maxDate = max(dates_PESET,na.rm = TRUE)


dateBatches = seq(as.Date(minDate),as.Date(maxDate),by = "month")
#Para ejecutar el comando read.zoo es necesario que todas las filas tengan una fecha de ingreso
#por lo que buscaremos aquellas filas en las que la fecha sea NAN y las eliminaremos para poder realizar 
#el análisis

missing_dates_ind <- which(is.na(repo_sepsis_PICS[,dateColumn]))
missing_dates_rows <- repo_sepsis_PICS[missing_dates_ind,dateColumn]

repo_sepsis_PICS_noNaDates <- repo_sepsis_PICS[!(is.na(repo_sepsis_PICS$fechaingreso)),]

zooRepository <- read.zoo(repo_sepsis_PICS_noNaDates,format = "%Y-%m-%d",index.column = dateColumn)

##### DQ dimensions

#######Part 1: Reviewing data variability

#### Completeness

# Plots
par(mar=c(1,1,1,1))

## Completeness by column
resCompletenessByColumn = apply.monthly(zooRepository, FUN=dimCompletenessByColumn)

#for (i in 1:(M_variables -6)){
#plot(resCompletenessByColumn[, 1:6],xlab = "Date", ylab = names(resCompletenessByColumn[, 1:6]), main = "Completeness (%)", ylim=c(0,100), cex.lab=0.5)
#plot(resCompletenessByColumn[, 6:12],xlab = "Date", ylab = names(resCompletenessByColumn[, 6:12]), main = "Completeness (%)", ylim=c(0,100), cex.lab=0.5)
#}
plot(resCompletenessByColumn ,xlab = "Date", ylab = names(resCompletenessByColumn), main = "Completeness (%)", ylim=c(0,100), cex.lab=0.5)

## Completeness of the data set
resCompleteness = apply.monthly(zooRepository, FUN=dimCompleteness)
plot(resCompleteness,xlab = "Date", ylab = names(resCompleteness), main = "Completeness of the dataset (%)", 
     ylim=c(0,100), cex.lab=0.5)

## Mean Completeness of the data set
meanResCompleteness = mean(resCompleteness)

#### Temporal stability

####Part 2: Fixing inconsistent data 

## Consistency

etiquetas <- lapply(repo_sepsis_PICS, unique)

n.obs <- sapply(etiquetas, length)
seq.max <- seq_len(max(n.obs))
mat <- (sapply(etiquetas, "[", i = seq.max))

write_xlsx(as.data.frame(mat), "etiquetas_pics.xlsx")

### Cambiar las clases de las columnas a su formato correspondiente

##Comprobar de qué clase es cada columna og

clases_og<- as.data.frame(sapply(repo_sepsis_PICS, class))
clases_og<- cbind(variable_names,clases_og) 
write_xlsx(clases_og, "clases_og_pics.xlsx")

data_prueba_clases <-type.convert(repo_sepsis_PICS,as.is = TRUE)

#todas las clases están bien

# Ahora cambiaremos a factor las variables categóricas formadas por más de dos o 
# 3 categorias (por tanto no estarán codificadas)




## Coherencia: Exploración de las categorias de las variables cualitativas




####Part 3: Outlier removal

# Correctness

# Multi-source stability



# Timeliness

#####Part 4: Fixing data with regular expressions

#### Recording linkage (finding duplicates)

#We can find duplicates in the dataset and remove them.

ndup <- nrow(repo_sepsis_PICS[duplicated(repo_sepsis_PICS), ])
if (ndup != 0) {
  repository_sepsis_2 <- repo_sepsis_PICS[!duplicated(repo_sepsis_PICS), ]
}

## Datos a corregir

# quitar el de ANF: (influenza B, galacto -), el galacto - porque el valor del 
# aspiado traqueal es una colonización del paciente ID: 138, la neumonia era por influenza


######### Estudio pacientes correción datos
