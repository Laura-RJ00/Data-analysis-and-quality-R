dqFramework <- function(fileName, hasHeader, dateColumn_ingreso, dateFormat,naFormat_str){
  
  #Leemos el repositorio de datos 
  repository <- read.csv(fileName,header=hasHeader,na.strings=na_formats)
  N_pacs <- nrow(repository)
  M_variables <- ncol(repository)
  
   
  repositoryDates <- as.Date(repository[,dateColumn],format=dateFormat)
  repository = repository[order(repositoryDates),]
  minDate = min(repositoryDates)
  maxDate = max(repositoryDates)
  dateBatches = seq(as.Date(minDate),as.Date(maxDate),by = "month")
  zooRepository <- read.zoo(repository,format = dateFormat,index.column = dateColumn)
  
  resCompletenessByColumn = apply.monthly(zooRepository, FUN=dimCompletenessByColumn)
  png("resCompletenessByColumn.png", width=900, height=1000)
  
  plot(resCompletenessByColumn, xlab = "Date", ylab = names(resCompletenessByColumn),
       main = "Completeness (%)", ylim=c(0,100), cex.lab=0.5)
  dev.off()  
  
  NAmatrix <- !is.na(repository) 
  sumNAmatrix <- sum(NAmatrix)
  completenessDataSet <- round(sumNAmatrix/(N*D)*100,2)
  return(completenessDataSet)
}