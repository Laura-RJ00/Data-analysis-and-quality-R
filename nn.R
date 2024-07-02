media_sd_porgrupos <- function (data, nGrupos,nVariables){
  
  estadisticos <-matrix("",nrow(data),length(data))
  
  for (i in 1:nGrupos){
    for (j in 1:nVariables){}
    
    grupo<- subset(data,Grupo == i)
    mean(grupo[,j],na.rm = TRUE)
    sd(grupo[,j],nVariables)
    
    estadisticos[,j] <- 
  }
  
  }
  
  return()
}
  
  