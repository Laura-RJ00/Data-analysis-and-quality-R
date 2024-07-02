obtener_etiquetas_bbdd <- function (repository){

  variable_names= colnames(repository)
  etiquetas <- lapply(repository, unique)
  n.obs <- sapply(etiquetas, length)
  seq.max <- seq_len(max(n.obs))
  mat <- (sapply(etiquetas, "[", i = seq.max))

  #write_xlsx(as.data.frame(mat), "etiquetas.xlsx")

return(mat)
}