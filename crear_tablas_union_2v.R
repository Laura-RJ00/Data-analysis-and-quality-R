crear_tablas_union_2v <- function(name_cabezeras_str,name_entradas_str,repository) {
  
    #Parametros de entrada
    #name_cabezeras: STRING nombre de la variable que actuará como cabezara y calsificador
    #name_entradas: STRING nombre de la variable que se clasificara en función de la primera variable
    #repository: DATAFRAME en el que están contenidas ambas variables
    
    #Primero identificamos el numero de columna que corresponde a la variable que actuará como cabezera
    ind_cabezeras<-which(colnames(repository)==name_cabezeras_str)
    
    #A continuación para evitar la apariación de valores perdidos o repetidos utilizaremos na.omit y unique
    cabezeras<-unique(na.omit(repository[,ind_cabezeras]))
                    
    #En segundo lugar preparamos una matriz vacia en la que volcaremos las entradas 
    #de la variable a clasificar bajo su correspondiente cabezera, con un mxn de 
    # nºfilas de repository (ya que es el tamaño máximo que puede llegar a tener una variable) x nºcabezeras
    
    
    datosTabla <-matrix("",nrow(repository),length(cabezeras))
    
    #Debido a esta medida para generar el numero de filas en caso de que ninguna de 
    #las entradas de una cabezera llegue a tener el tamaño igual al tamaño de la muestra, 
    #guardaremos en un vector vacio cada uno de los tamaños que se generan tras cada clasificación
    num_entradas<-numeric(length(cabezeras))
    
    # Para poder generar el bucle for que pasará por todas las entradas de nuestra 
    # columna/variable a clasificar se identifica el numero de columna que le corresponde en el repository
    ind_entradas<- which(colnames(repository)== name_entradas_str)
    
                    
          for (i in 1:length(cabezeras)) {
            #Fijamos el for en la columna correspondiente ind_entradas
            #A continuación indexamos repository para comparar todas las entradas de la columna correspondiente a ind_cabezeras
            # que sean iguales a la cabezera que en ese momento este siendo loopeada (i)
            #Aplicamos también unique y na.omit con tal de no tener valores repetidos ni perdidos
            #x<-na.omit(unique(repository[(which(repository[,ind_cabezeras]==cabezeras[i])),ind_entradas]))
            
            x<- unique(repository[repository[,ind_cabezeras]==cabezeras[i],ind_entradas])
            x<- x[!is.na(x)]
            
            # Dado que puede que haya veces que no todos las etiquetas de a variable clasificadora 
            # este presente en todos los valores de la variable de entradas, es necesario crear una condición IF, así en caso de que la
            # la variable de cabezera no este presente, el bucle for puede continuar con el siguiente valor sin arrojar un error
            
              if (!is_empty(x)) {
              
                #Estos valores de entradas guardados en el vector x son introducidos en la 
                #matriz datosTabla indexando según el tamaño que tiene el vector que correspondera a la numero de filas completas
                #en la columna i
                
                datosTabla[1:length(x),i]<- x
                
                #A su vez guardamos el tamaño del vector x en la variable num_entradas
                num_entradas[i]<-length(x)
              } else {
                next # en caso de que quisieramos romper el bucle utilizariamos la palabra reservada break
                
                
              }
          }
                    
    # Transformamos la matriz que contine la tabla clasificadora en un dataframe para facilitar su manipulación y escritura
    datosTabla<-as.data.frame(datosTabla)
    #nombramos a las columnas con los noombres contenidos en el vector cabezeras
    colnames(datosTabla)<-cabezeras
    
    #Por último para eliminar las filas vacias (en caso de que contengan un solo 
    #dato serán guardadas), partimos del dígito máximo almacenado en num_entradas +1 
    #para que eliminen a partir de la fila siguiente a la seleccionada como último registro con almenos un valor
    
    datosTabla<-datosTabla[-((max(num_entradas,na.rm=TRUE)+1):nrow(datosTabla)),]
    
    #Escribimos la tabla obtenida en excel para facilitar su visualización
    name_doc<-paste("Tdatos",name_cabezeras_str,name_entradas_str,sep = "_")
    excel_doc<-paste(name_doc,"xlsx",sep = ".")
    
    write_xlsx(datosTabla, excel_doc)
    
    return(datosTabla)
  
  
  
  
}
  
  