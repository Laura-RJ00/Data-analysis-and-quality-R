media_sd <- function(repository,name_variables,name_groups,num_groups){
  
  ind_group<- which(colnames(repository)== name_groups)
  
  num_variables = length(name_variables)
  
  datos_media_sd <- (data.frame(matrix(nrow = num_groups, ncol=num_variables*2)))
  
  ind_variable <- matrix(,length(name_variables))
  columnNames <- c()
  var <- 0
  
    for (k in seq(1,num_variables*2, by=2) ) {
      h = k + 1
      var = var + 1
      
      ind_variable[var]<- which(colnames(repository)== name_variables[var]) 
      columnNames[k] <-paste(name_variables[var],"Media",sep = "_")
      columnNames[h] <-paste(name_variables[var],"SD",sep = "_") 
      
    }
  
  colnames(datos_media_sd) <- columnNames

  m= 1
  n= 2
  for (i in 1:num_variables) {
     
      for (j in 1:num_groups) {
      
      group <- repository[which(repository[,ind_group]== j),]
      
      datos_media_sd[j,m] = mean(as.numeric(unlist(group[,ind_variable[i]])),na.rm =TRUE)
      
      datos_media_sd[j,n] = sd(as.numeric(unlist(group[,ind_variable[i]])),na.rm =TRUE)
        
      }
    m = m + 2
    n = n + 2
  }
  
return(datos_media_sd)
}
