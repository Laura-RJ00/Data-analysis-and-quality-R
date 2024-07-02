setwd("C:/Users/Laura/Documents/INCLIVA TRABAJO/AVI SepAsT/Datos")
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


## borrar todo (variables y funciones) del environment rm(list=ls())
## borrar solo las variables del environment rm(list= setdiff(ls(), lsf.str()))
## borrar solo las funciones del environment rm(list=lsf.str())


### Cargar datos
fileName = "BD Sepsis - H. Peset.csv" #nombre del fichero a ser analizado
HasHeader = TRUE #Si la primera columna contiene el nombre de las variables
naFormat_str = c("x","X","¿?","#","","999","9999")
repository_og <- read.csv(fileName,header = HasHeader, na.strings= naFormat_str)
repo_sepsis_PESET <-repository_og
N_pacientes<- nrow(repo_sepsis_PESET)
M_variables<- ncol(repo_sepsis_PESET)
variable_names= colnames(repo_sepsis_PESET)


## Arreglar error tamaño muestra
# La variable estancia en dias tiene datos de iguales a 0 que no corresponden a ningun paciente,
# ni está acompañado del valor de ninguna otra variable, por lo que se eliminarán estos registros extras

repo_sepsis_PESET<-repo_sepsis_PESET[(!is.na(repo_sepsis_PESET$Código)),]
repository_og <-repo_sepsis_PESET
                     
#La variable T3 tiene un valor equivalente a NA en el paciente 52, 
ind_t3<- which(colnames(repo_sepsis_PESET)== "T3")
repo_sepsis_PESET[repo_sepsis_PESET$Código == "SEP-0052", ind_t3]<- NA


                     
### Dar formato a las variables/datos

# Fecha


# Obtenemos la columna con las fechas de ingreso y vemos que hay diferentes 
# formatos de fechas

dateColumn<- which(colnames(repo_sepsis_PESET)== "Fecha.ingreso")

#dateFormat = "%m/%d/%Y"
dates <- repo_sepsis_PESET[,dateColumn] 

# Transformamos la fecha al formato año-mes-dia con format y después con as.Date
repo_sepsis_PESET_Dates <- (as.Date(dates,"%d/%m/%Y"))

repo_sepsis_PESET_Dates <- format(as.Date(dates,"%d/%m/%Y"),"%d/%m/%y")

repo_sepsis_PESET [,dateColumn]<-repo_sepsis_PESET_Dates

# Transformamos el resto de variables de fecha en el formato que correponde

datesVariables <- c("T1","T2","T3","T4","Fecha.alta")


# Ordenamos el respositorio de datos según las fechas
repo_sepsis_PESET = repo_sepsis_PESET[order(repo_sepsis_PESET_Dates),]
minDate = min(repo_sepsis_PESET_Dates,na.rm = TRUE)
maxDate = max(repo_sepsis_PESET_Dates,na.rm = TRUE)


dateBatches = seq(as.Date(minDate),as.Date(maxDate),by = "month")
#Para ejecutar el comando read.zoo es necesario que todas las filas tengan una fecha de ingreso
#por lo que buscaremos aquellas filas en las que la fecha sea NAN y las eliminaremos para poder realizar 
#el análisis

missing_dates_ind <- which(is.na(repo_sepsis_PESET[,dateColumn]))
missing_dates_rows <- repo_sepsis_PESET[missing_dates_ind,dateColumn]

repo_sepsis_PESET_noNaDates <- repo_sepsis_PESET[!(is.na(repo_sepsis_PESET$Fecha.ingreso)),]

zooRepository <- read.zoo(repo_sepsis_PESET_noNaDates,format = "%d/%m/%y",index.column = dateColumn)

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

etiquetas_PESET <- lapply(repo_sepsis_PESET, unique)

n.obs <- sapply(etiquetas_PESET, length)
seq.max <- seq_len(max(n.obs))
etiquetas_PESET <- (sapply(etiquetas_PESET, "[", i = seq.max))

write_xlsx(as.data.frame(etiquetas_PESET), "etiquetas_peset.xlsx")

### Cambiar las clases de las columnas a su formato correspondiente

##Comprobar de qué clase es cada columna og

clases_og<- as.data.frame(sapply(repo_sepsis_PESET, class))
clases_og_PESET<- cbind(variable_names,clases_og) 
write_xlsx(clases_og, "clases_og_peset.xlsx")

data_prueba_clases <-type.convert(repo_sepsis_PESET,as.is = TRUE)

#todas las clases están bien

# Ahora cambiaremos a factor las variables categóricas formadas por más de dos o 
# 3 categorias (por tanto no estarán codificadas)


## Coherencia: Exploración de las categorias de las variables cualitativas

#Creamos una nueva variable denominada EXITUS (1=muerte, 0= no) a partir de la variable ESTADO
repo_sepsis_PESET <- repo_sepsis_PESET %>%
  mutate(Exitus = if_else((repo_sepsis_PESET$Estado == "COMPLETADO - Exitus ingreso "| repo_sepsis_PESET$Estado == "COMPLETADO (EXITUS CANCER 3m)"),1,0))
    

# Corrección de las palabras mal escritas
repo_sepsis_PESET$Estado[repo_sepsis_PESET$Estado %in% 'COMPLETADO'] = 'COMPLETADO'
repo_sepsis_PESET$Estado[repo_sepsis_PESET$Estado %in% 'COMPLETADO - Exitus ingreso '] = 'COMPLETADO'
repo_sepsis_PESET$Estado[repo_sepsis_PESET$Estado %in% 'COMPLETADO (EXITUS CANCER 3m)'] = 'COMPLETADO'
repo_sepsis_PESET$Estado[repo_sepsis_PESET$Estado %in% 'COMPLETADO (FALTA ÚLTIMA ANALITICA)'] = 'COMPLETADO'
repo_sepsis_PESET$Estado[repo_sepsis_PESET$Estado %in% 'COMPLETADO (FALTA ÚLTIMA ANALÍTICA)'] = 'COMPLETADO'
#repo_sepsis_PESET$Estado[repo_sepsis_PESET$Estado %in% 'COMPLETADO - NO ACUDE A CONTROL 12M'] = 'COMPLETADO'
repo_sepsis_PESET$Estado[repo_sepsis_PESET$Estado %in% 'COMPLETADO '] = 'COMPLETADO'


repo_sepsis_PESET$Estado[repo_sepsis_PESET$Estado %in% 'PENDIENTE CONTROL 12M'] = 'PENDIENTE CONTROL'
repo_sepsis_PESET$Estado[repo_sepsis_PESET$Estado %in% 'NO ACUDE A CONTROL DE 6M – PENDIENTE CONTROL 12M'] = 'PENDIENTE CONTROL'
repo_sepsis_PESET$Estado[repo_sepsis_PESET$Estado %in% 'PENDIENTE CONTROL 12M'] = 'PENDIENTE CONTROL'

repo_sepsis_PESET$Estado[repo_sepsis_PESET$Estado %in% 'SEGUIMIENTO PERDIDO (RETIRA CI SEG 6-12M) '] = 'SEGUIMIENTO PERDIDO'
repo_sepsis_PESET$Estado[repo_sepsis_PESET$Estado %in% 'SEGUIMIENTO PERDIDO (RETIRA CI SEG 6-12M)'] = 'SEGUIMIENTO PERDIDO'


repo_sepsis_PESET$Estado <- as.factor(repo_sepsis_PESET$Estado)

levels(repo_sepsis_PESET$Estado)


repo_sepsis_PESET$GRUPO.DE.ESTUDIO[repo_sepsis_PESET$GRUPO.DE.ESTUDIO %in% "No sepsis"] ="NO SEPSIS"
repo_sepsis_PESET$GRUPO.DE.ESTUDIO[repo_sepsis_PESET$GRUPO.DE.ESTUDIO %in% "NO sepsis (Excluido)"] ="NO SEPSIS"
repo_sepsis_PESET$GRUPO.DE.ESTUDIO[repo_sepsis_PESET$GRUPO.DE.ESTUDIO %in% "NO sepsis"] ="NO SEPSIS"
repo_sepsis_PESET$GRUPO.DE.ESTUDIO[repo_sepsis_PESET$GRUPO.DE.ESTUDIO %in% "NO SEPSIS"] ="NO SEPSIS"
repo_sepsis_PESET$GRUPO.DE.ESTUDIO[repo_sepsis_PESET$GRUPO.DE.ESTUDIO %in% "No sepsis (Excluido)"] ="NO SEPSIS"
repo_sepsis_PESET$GRUPO.DE.ESTUDIO[repo_sepsis_PESET$GRUPO.DE.ESTUDIO %in% "SEPSIS"] ="SEPSIS"

repo_sepsis_PESET$GRUPO.DE.ESTUDIO <- as.factor(repo_sepsis_PESET$GRUPO.DE.ESTUDIO)

levels(repo_sepsis_PESET$GRUPO.DE.ESTUDIO)
# FOCO Y FOCO.GLOBAL TABLA 


# MOTIVINGRESO Y MO.tabla2.reagrupados







####Part 3: Outlier removal

# Correctness

# Multi-source stability



# Timeliness

#####Part 4: Fixing data with regular expressions

#### Recording linkage (finding duplicates)

#We can find duplicates in the dataset and remove them.

ndup <- nrow(repo_sepsis_PESET[duplicated(repo_sepsis_PESET), ])
if (ndup != 0) {
  repository_PESET_2 <- repo_sepsis_PESET[!duplicated(repo_sepsis_PESET), ]

}else { print("No hay registros repetidos")
  
}

## Datos a corregir

#la respuesta correcta es SI


######### Estudio pacientes correción datos
write_xlsx(repo_sepsis_PESET, "RepoPeset.xlsx")
