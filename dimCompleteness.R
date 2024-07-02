dimCompleteness <- function(repository){
  N = dim(repository)[1]*dim(repository)[2]
  NAmatrix <- !is.na(repository)
  sumNAmatrix <- sum(NAmatrix)
  completeness <- round(sumNAmatrix/N*100,2)
  return(completeness)
}