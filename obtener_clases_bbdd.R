obtener_clases_bbdd <- function(repository){
  
  repository <- as.data.frame(repository)
  variable_names= colnames(repository)
  clases_og<- as.data.frame(sapply(repository, class))
  
    if (nrow(clases_og)<ncol(clases_og)){
      clases_og <- t(clases_og)
    }
  clases_og<- cbind(variable_names,clases_og)
  colnames(clases_og)[2]<- "Clase"
  rownames(clases_og)<- NULL
  
  #clases_og <- rename(clases_og,c("sapply(repository, class)" = "Clase"))
    #clases_og = clases_og %>% `rownames<-`( NULL )
  #write_xlsx(clases_og, "clases_og.xlsx")  
  
  return(clases_og)
}


