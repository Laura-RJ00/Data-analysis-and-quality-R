setwd("C:/Users/Laura/Documents/INCLIVA TRABAJO/AVI SepAsT/Datos")
install.packages("zoo")
library("zoo")
install.packages("rts", repos = "http://cran.us.r-project.org")
library("rts")
install.packages("ggplot2", repos = "http://cran.us.r-project.org")
library("ggplot2")
install.packages("tidyverse")
library("tidyverse")
#install.packages("datacheck", repos = "http://cran.us.r-project.org")
#library(datacheck)
install.packages("plotly")
library("plotly")
library(knitr)
install.packages('na.tools')
library(na.tools)
install.packages('caTools')
library("caTools")
install.packages("lubridate")
library(lubridate)
install.packages("gdata")
library("gdata")
install.packages ("PCAmixdata")
library("PCAmixdata")
install.packages("EHRtemporalVariability")
library("EHRtemporalVariability")
install.packages("clock")
library(clock)
install.packages("tidyr")
library(tidyr)
install.packages("openxlsx")
library(openxlsx)
install.packages("writexl")
library(writexl)
install.packages("plyr")
library(plyr)
install.packages("dplyr")
library(dplyr)
install.packages("svDialogs") 
library(svDialogs)



## borrar todo (variables y funciones) del environment rm(list=ls())
## borrar solo las variables del environment rm(list= setdiff(ls(), lsf.str()))
## borrar solo las funciones del environment rm(list=lsf.str())


### Cargar datos
fileName = "junio 21 ITIbase.csv" #nombre del fichero a ser analizado
HasHeader = TRUE #Si la primera columna contiene el nombre de las variables
naFormat_str =c("")

repository_og <- read.csv(fileName,header = HasHeader, na.strings= naFormat_str)
repository_sepsis <-repository_og
N_pacientes<- nrow(repository_sepsis)
M_variables<- ncol(repository_sepsis)
variable_names= colnames(repository_sepsis)

## Llamar a funciones
source(dimCompleteness)
source(dimCompletenessByColumn)

### Dar formato a las variables/datos

# Fecha


# Obtenemos la columna con las fechas de ingresoy vemos que hay diferentes 
# formatos de fechas
dateColumn = 12
#dateFormat = "%m/%d/%Y"
dates <- repository_sepsis[,dateColumn] 

# Transformamos la fecha al formato año-mes-dia con format y después con as.Date
#repository_sepsis_Dates <- (as.Date(dates,"%m/%d/%Y"))
repository_sepsis_Dates <- format(as.Date(dates,"%m/%d/%Y"),"%m/%d/%y")
repository_sepsis_Dates <- as.Date(repository_sepsis_Dates,format= "%m/%d/%y")
repository_sepsis [,dateColumn]<-repository_sepsis_Dates

# Ordenamos el respositorio de datos según las fechas
repository_sepsis = repository_sepsis[order(repository_sepsis_Dates),]
minDate = min(repository_sepsis_Dates,na.rm = TRUE)
maxDate = max(repository_sepsis_Dates,na.rm = TRUE)


dateBatches = seq(as.Date(minDate),as.Date(maxDate),by = "month")
#Para ejecutar el comando read.zoo es necesario que todas las filas tengan una fecha de ingreso
#por lo que buscaremos aquellas filas en las que la fecha sea NAN y las eliminaremos para poder realizar 
#el análisis

missing_dates_ind <- which(is.na(repository_sepsis [,dateColumn]))
missing_dates_rows <- repository_sepsis[missing_dates_ind,dateColumn]

repository_sepsis_noNaDates <- repository_sepsis[!(is.na(repository_sepsis$FECHAINGRES)),]

zooRepository <- read.zoo(repository_sepsis_noNaDates,format = "%Y-%m-%d",index.column = dateColumn)

##### DQ dimensions

#######Part 1: Reviewing data variability

#### Completeness

# Plots
par(mar=c(1,1,1,1))

## Completeness by column
resCompletenessByColumn = apply.monthly(zooRepository, FUN=dimCompletenessByColumn)

#for (i in 1:(M_variables -6)){
plot(resCompletenessByColumn[, 1:6],xlab = "Date", ylab = names(resCompletenessByColumn[, 1:6]), main = "Completeness (%)", ylim=c(0,100), cex.lab=0.5)
plot(resCompletenessByColumn[, 6:12],xlab = "Date", ylab = names(resCompletenessByColumn[, 6:12]), main = "Completeness (%)", ylim=c(0,100), cex.lab=0.5)
#}

## Completeness of the data set
resCompleteness = apply.monthly(zooRepository, FUN=dimCompleteness)
plot(resCompleteness,xlab = "Date", ylab = names(resCompleteness), main = "Completeness of the dataset (%)", 
     ylim=c(0,100), cex.lab=0.5)

## Mean Completeness of the data set
meanResCompleteness = mean(resCompleteness)

#### Temporal stability

####Part 2: Fixing inconsistent data 

## Consistency

etiquetas <- lapply(repository_sepsis, unique)

n.obs <- sapply(etiquetas, length)
seq.max <- seq_len(max(n.obs))
mat <- (sapply(etiquetas, "[", i = seq.max))

write_xlsx(as.data.frame(mat), "etiquetas.xlsx")

### Cambiar las clases de las columnas a su formato correspondiente

##Comprobar de qué clase es cada columna og

clases_og<- as.data.frame(sapply(repository_sepsis, class))
clases_og<- cbind(variable_names,clases_og)
clases_og <- rename(clases_og,clase= "sapply(repository_sepsis, class)")
write_xlsx(clases_og, "clases_og.xlsx")

data_prueba_clases <-type.convert(repository_sepsis,as.is = TRUE)
 
#todas las clases están bien

# Ahora cambiaremos a factor las variables categóricas formadas por más de dos o 
# 3 categorias (por tanto no estarán codificadas)

FOCO
FOCO.GLOBAL.TABLA.6
MOTIVINGRESO
Microorganismo
Microorganismo_2
MO.tabla2.reagrupados
JUSTIFICA
ATB1
ATB2
ATB3
ANTIVIRAL_NOMBRE
ANTIFUNGICO
HEMOCULTIVOS
ASPTRAQUEAL
AGSURIN
URINOCULTIVO
ABDOMINAL
CATETER
LCR
OTROS
CUAL
SOFA3PRIMERDIA
DVA1Dcual
DVA3DCUAL
DVA5DCUAL
ATB1D
ATB3D
ATB5D
ATBALTA


## Coherencia: Exploración de las categorias de las variables cualitativas

# FOCO Y FOCO.GLOBAL TABLA 

Tabla_focos<-crear_tablas_union_2v("FOCO.GLOBAL.TABLA.6","FOCO",repository_sepsis)

# Podemos ver en la tabla obtenida que hay varias etiquetas que pese a referir a la misma situación
# se han guardado como datos diferentes debido a: mayusculas/minisculas, con/sin acentos, abreviaturas, errores
# ortográficos


# MOTIVINGRESO Y MO.tabla2.reagrupados

Tabla_motivos<-crear_tablas_union_2v("MO.tabla2.reagrupados","MOTIVINGRESO",repository_sepsis)


## Microorganismos
Tabla_microorganismo<-crear_tablas_union_2v("MO.tabla2.reagrupados","Microorganismo",repository_sepsis)





# Corrección de casos de pacientes 


# Tal y como se ve hay varias categorias que aluden al mismo valor pero está escrito de distintas maneras,
# En concreto trataremos el caso del paciente con la siguiente ID
iD_pac <- repository_og[which(repository_sepsis$FOCO == "URIN VS ARTIC"),1]
# Despues de hablar con los clinicos se ha estudiado mas profundidad el caso llegando a la conclusion de que,
# obedece a un caso de sepsis urinario, por lo que sustituimos el valor de URIN VS ARTIC por URINARIO
ind_FOCO<- which(colnames(repository_sepsis)== "FOCO")
repository_sepsis[which(repository_sepsis$FOCO == "URIN VS ARTIC"),ind_FOCO] <- "URINARIO"


# PARA PODER REALIZAR MEJOR UNA CORRECCIÓN DE LAS VARIABLES CUALITATIVAS TRANSFORMAREMOS 
# TODOS LOS STRING DE MINUSCULA A MAYÚSCULA MEDIANTE UN BUCLE


for (i in 1: M_variables){
  
  if (class(repository_sepsis[,i]) == "character"){
    repository_sepsis[,i] <- toupper(repository_sepsis[,i])
  }
  
}


# Corrección de las palabras mal escritas de la VARIABLE FOCO
repository_sepsis$FOCO[repository_sepsis$FOCO %in% 'RESPI GRIP'] = 'RESPIRATORIO'
repository_sepsis$FOCO[repository_sepsis$FOCO %in% 'RESPI GRIPE'] = 'RESPIRATORIO'
repository_sepsis$FOCO[repository_sepsis$FOCO %in% 'RESP'] = 'RESPIRATORIO'

# # IMPORTANTE ANTES DE TRANSFORMAR EN UN FACTOR HAY QUE LIMPIAR TODA LA VARIABLE 
# YA QUE LOS VALORES CON LOS QUE CUENTE LA VARIABLE EN EL MOMENTO DE LA TRANSFORMACION SE QUEDARÁN FIJOS AUNQUE NO TENGA REGISTROS PARA DICHA CATEGORIA
# Convertimos la variable foco en un factor para poder observar todas los valores que puede tomar
repository_sepsis$FOCO <- as.factor(repository_sepsis$FOCO)
levels(repository_sepsis$FOCO)

# Corrección de las palabras mal escritas de la VARIABLE MOTIVINGRESO
repository_sepsis$MOTIVINGRESO[repository_sepsis$MOTIVINGRESO %in% 'COLECISTITS'] = 'COLECISTITIS'




####Part 3: Outlier removal

# Correctness

# Multi-source stability



# Timeliness

#####Part 4: Fixing data with regular expressions

#### Recording linkage (finding duplicates)

#We can find duplicates in the dataset and remove them.

ndup <- nrow(repository_sepsis[duplicated(repository_sepsis), ])
if (ndup != 0) {
  repository_sepsis_2 <- repository_sepsis[!duplicated(repository_sepsis), ]

}else { print("No hay registros repetidos")
  
}
  
## Datos a corregir
pac_x<- repository_sepsis[repository_sepsis$DVA1D==0.5,]
#la respuesta correcta es SI


######### Estudio pacientes correción datos

#Casos particulares de FOCO

focos_particulares <-c("URIN VS ARTIC", "ÓTICO", "MENINGOCOCCEMIA")
ID_pacs <-vector("character",length(focos_particulares))

for (i in 1:length(focos_particulares)){

ID_pacs[i]<- repository_sepsis[which(repository_sepsis$FOCO==focos_particulares[i]),1]  

}

#Casos particulares fechas con formato final yy

Pacientes_ID <- repository_og[,1] 
pacs_ID_dates <-cbind(Pacientes_ID,dates)

#El simbolo '$' permite que se seleccionen los caracteres "que acaben en..." aquello que se haya escrito delante

pacs_2013<- subset(dates_ID_pacs,grepl('/13$',dates_ID_pacs[,2]))
pacs_2014<- subset(dates_ID_pacs,grepl('/14$',dates_ID_pacs[,2]))
pacs_2015<- subset(dates_ID_pacs,grepl('/15$',dates_ID_pacs[,2]))
pacs_2016<- subset(dates_ID_pacs,grepl('/16$',dates_ID_pacs[,2]))
pacs_2017<- subset(dates_ID_pacs,grepl('/17$',dates_ID_pacs[,2]))
pacs_2018<- subset(dates_ID_pacs,grepl('/18$',dates_ID_pacs[,2]))
pacs_2019<- subset(dates_ID_pacs,grepl('/19$',dates_ID_pacs[,2]))

pacs_fechas_yy <-as.data.frame(rbind(pacs_2013,pacs_2014,pacs_2015,pacs_2016,pacs_2017,pacs_2018,pacs_2019))

write_xlsx(pacs_fechas_yy, "pacs_fechas_yy.xlsx")



## Comprobar las relaciones entre variables

# Se busca saber si la detección de bacteremia está relacionado con la variable GRAM, sin embargo 
ind<- which(colnames(repository_sepsis)== "GRAM")
enfermos_bacter<-repository_sepsis[which(repository_sepsis$BACTER==1),c(1,ind)]
no_enfermos_bacter<-repository_sepsis[which(repository_sepsis$BACTER==2),c(1,ind)]




x <- repository_sepsis$ECOCA.VCI


