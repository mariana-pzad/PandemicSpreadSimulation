#Inicializar la matriz
Tablero <- matrix()

#Filas representan a cada individuo
#Columna 1: Identificador del individuo (número:ID)
#Columna 2: Posicion dentro del Tablero (fila)
#Columna 3: Posicion dentro del Tablero (columna)
#Columna 4: Estado del individuo [0:susceptible, 1:enfermo, 2:vacunado, 3:recuperado, 4:latente, 5:muerto]
#Columna 5: Edad [años]
#Columna 6: Tasa de contagio [probabilidad]
#Columna 7: Tasa de letalidad [probabilidad]
#Columna 8: Tasa de vacunación [probabilidad]
#Columna 9: Periodo de latencia [días]
#Columna 10: Periodo de enfermedad [días]
#Columna 11: Radio de movilidad
#Columna 12: Trabaja
#Columna 13: Tipo de traslado
#Columna 14: Tipo de trabajo
#Columna 15: Tasa de recuperación [probabilidad]
#Columna 16: Cuarentena [binaria]



#####################################################
FuncionGraficar <- function(matriz){

    return()
}
# Autores:
  #Maricela Alejandra Valero Fuentes
  #Edwin Martin Romero Silva
  #Erick Hinojosa Aguirre

######################################################
MedidasPrev <- function(matriz){

    return(matriz)
}  
#Luis David Dávila Torres
#Carlos Antonio Espinosa Bravo
#Alan Gerardo Garza Muro

###################################################
cuarentena <- function(poblacion, no_pruebas)
    # Obtenemos los ids de los individuos con latencia obtenidos
    # aleatoriamente para ponerlos en cuarentena.
    filas <- prueba(poblacion, no_pruebas);
    # Iteramos sobre todos los individuos en latencia para ponerlos en cuarentena.
    for (fila in filas) {
        # Lo ponemos en cuarentena.
        poblacion[fila,16] <- 1;
        # Iniciamos el contador de cuarentena en 0.
        poblacion[fila,17] <- 0;
    }
    return(poblacion);
}

salir_cuarentena <- function(poblacion, id) {
    # La columna que contiene los ids es la primer columna.
    columna_ids <- poblacion[,1];
    # Obtenemos el registro de este individuo.
    fila <- match(c(id), columna_ids);
    # Lo sacamos de cuarentena.
    poblacion[fila,16] <- 0;
    # Reiniciamos su contador de cuarentena
    poblacion[fila,17] <- 0;
    return(poblacion);
}

prueba <- function(poblacion, no_pruebas) {
    # Lista para guardar individuos ya considerados para que no se repitan
    # en la prueba.
    individuos_considerados <- c();
    # Lista para guardar los individuos que pondremos en cuarentena.
    individuos_en_latencia <- c();
    # El total de individuos en la población es necesario como límite para la 
    # generación de número aleatorios.
    total_individuos <- dim(poblacion)[0];
    # La columna que contiene los ids es la primer columna.
    columna_ids <- poblacion[,1];
    # Iteramos sobre el número total de pruebas a realizar
    for (i in 1..no_pruebas) {
        # Obtenemos un id aleatorio.
        id <- randint(total_individuos) + 1;
        # Obtenemos el registro de este individuo.
        fila <- match(c(id), columna_ids);
        if (is.element(id, individuos_considerados) || poblacion[fila,16] == 1) {
            # Si este id ya fue considerado, o si este individuo ya se encuentra
            # en cuarentena aumentamos el número total de pruebas
            # para que no se vean afectadas por encontrar un individuo
            # ya considerado.
            no_pruebas = no_pruebas + 1;
            # Continuamos a la siguiente iteración.
            next;
        } else {
            # Consideramos a este individuo para ser puesto en cuarentena
            individuos_en_latencia <- c(individuos_en_latencia, fila);
        }
        individuos_considerados <- c(individuos_considerados, id);
    }
    return(individuos_en_latencia);
}
# Autores:
# Victor Francisco Carrizales Castor
# Natalia ALejandra García Armijo
# Fabián Gandarilla López

###################################################
Contagio <- function(matriz){
    
    return(matriz)
}
#Autores:
#Araceli Montserrat Rodriguez Crespo
#Ivan Rigoberto Ibarra Rodriguez
#Fabiola Jasso Valadez
#####################################################
Vacunacion <- function(matriz){
return(matriz)
}
# Autores:
# Gerardo Perez Arriega
# Mariana Perez Arredondo

##########################################################

RecuperacionMuerte <- function(matriz){
 
return(matriz)
}
#Autores:
#Enrique Martinez
#Sandra Negrete

#####################################################
FuncionAsintomaticos <- function(matriz){
  
    return()
}
#Autores:
  #Luis Alberto Guerrero Zuñiga
  #Alan Fernando Mejía Aranda
  #Jose Armando Jara Rodriguez 
