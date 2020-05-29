source("helperfunctions_vacunation.R")

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
#Columna 17: Tiempo tras vacunación
#Columna 18: Tiempo recuperado


#####################################################
FuncionGraficar <- function(matriz){#La funcion graficar funciona tomando como parametro la matriz generada de las demas funciones y graficandola cada una de una color diferente
par(mfrow=c(1,1))
  plot(which(matriz==0, arr.ind=TRUE),col="white", pch=15,xlab="Años",xlim=c(0,25), ylim=c(0,25))
  par(new=T)#Se toman los valores de cada columna en base a la estructura dada por el profesor contra el numero de años.
  plot(which(matriz==1, arr.ind=TRUE),col="red", pch=15,xlab="Años",xlim=c(0,25), ylim=c(0,25))
  par(new=T)
  plot(which(matriz==2, arr.ind=TRUE),col="yellow", pch=15,xlab="Años",xlim=c(0,25), ylim=c(0,25))
  par(new=T)
  plot(which(matriz==3, arr.ind=TRUE),col="gray50", pch=15,xlab="Años",xlim=c(0,25), ylim=c(0,25))
  par(new=T)
  plot(which(matriz==4, arr.ind=TRUE),col="green", pch=15,xlab="Años",xlim=c(0,25), ylim=c(0,25))
  par(new=T)
  plot(which(matriz==5, arr.ind=TRUE),col="black", pch=15,xlab="Años",xlim=c(0,25), ylim=c(0,25))
  par(new=T)
  title("REPRESENTACIÓN PANDEMIA")
Sys.sleep(.1)
return(matriz)
}
# Autores:
  #Maricela Alejandra Valero Fuentes
  #Edwin Martin Romero Silva
  #Erick Hinojosa Aguirre

<<<<<<< HEAD
######################################################
MedidasPrev <- function(matriz){
=======
###############################################################################################################################################################
#Descripcion de la funcion
# La función de Medidas de prevención nos permite modificar la probabilidad de cada individuo en base a la cantidad de infectados que existe en cada tiempo t, 
# así como una variable que nos ayuda a ingresar medidas especiales según las medidas levantadas por el gobierno, 
# es importante mencionar que la probabilidad se puede modificar según características de cada individuo como si esta trabajando o si usa transporte publico
# y si su trabajo es de alto, medio o bajo riesgo. 

MedidasPrev=function(i,Poblacion){
  FASE1<-20   # Numero de infectados para activar la fase
  FASE2<-200  # Numero de infectados para activar la fase
  FASE3<-4000 # Numero de infectados para activar la fase
  ControlDeFronteras <- 0 #0 Desactivado: La vigilancia es minima, 1 Activado: Cuarentena y estricta revision 
 
 #---------------
  x<-1/Poblacion[,6][Poblacion[,12]==1]
  d<-(x-1)
  T_Trabaja<-(1+(d*.2)) #Proporcion de aumento de la tasa de infeccion por trabajo
 #---------------
  x<-1/Poblacion[,6][Poblacion[,12]==1 & Poblacion[,13]==1]
  d<-(x-1)
  T_Transporte<-(1+(d*.2)) #Proporcion de aumento de la tasa de infeccion por transporte publico
 #---------------
  x<-1/Poblacion[,6][Poblacion[,12]==1 & Poblacion[,14]==2]
  d<-(x-1)
  T_MedioR<-(1+(d*.2)) #Proporcion de aumento de la tasa de infeccion por trabajo de medio riesgo
  #---------------
  x<-1/Poblacion[,6][Poblacion[,12]==1 & Poblacion[,14]==3]
  d<-(x-1)
  T_AltoR<-(1+(d*.3)) #Proporcion de aumento de la tasa de infeccion por trabajo de alto riesgo

  Infectados<-length(Poblacion[,4][Poblacion[,4]==1]) #Numero de infectados

  if(i==1){ #Actualizacion de las probabilidades de contagio segun las caracteristicas de las personas
  Poblacion[,6][Poblacion[,12]==1]<-Poblacion[,6][Poblacion[,12]==1]*T_Trabaja
  Poblacion[,6][Poblacion[,12]==1 & Poblacion[,13]==1]<-Poblacion[,6][Poblacion[,12]==1 & Poblacion[,13]==1]*T_Transporte
  Poblacion[,6][Poblacion[,12]==1 & Poblacion[,14]==2]<-Poblacion[,6][Poblacion[,12]==1 & Poblacion[,14]==2]*T_MedioR
  Poblacion[,6][Poblacion[,12]==1 & Poblacion[,14]==3]<-Poblacion[,6][Poblacion[,12]==1 & Poblacion[,14]==3]*T_AltoR
  }
>>>>>>> 930e3ee2a3bf14449243b88e386233026bf0c176

    return(matriz)
}  
#Luis David Dávila Torres
#Carlos Antonio Espinosa Bravo
#Alan Gerardo Garza Muro

###################################################
<<<<<<< HEAD
PonerEnCuarentena <- function(m) {
    
    return(matriz);
=======
# La función de cuarentena funciona tomando como parámetro el número de pruebas a realizar para detectar a 
# individuos en periodo de latencia. Se toman aleatoriamente individuos de la población y los que estén en latencia
# son puestos en cuarentena.
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
>>>>>>> 930e3ee2a3bf14449243b88e386233026bf0c176
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
  # Recorreremos la matriz, por individuo
  for(id in 1:(rownames(matriz)) {
    # Obtenemos un vector por persona, con todos los parametros de la columna de la matriz
    atributos_persona <- matriz[,id]
    # Obtenemos el estado de la persona
    estado_persona <- which(atributos_persona["estado"] == 1)
    edad_persona <- atributos_persona["edad"]
    tiempo_tras_vacunacion <- atributos_persona["tiempo_tras_vacunacion"]
    tiempo_recuperado <- atributos_persona["tiempo_recuperado"]

    ################################################### Susceptible
    if (estado_persona == 0)
    {
      # Si se vacuna, calculamos ahora la efectividad
      if (calcular_vacunacion_susceptible(edad_persona))
      {
        # Vacunado, se cambia el estado de la persona a vacunado
        matriz["estado", id] <- 2
      }
    }
    ################################################### Enfermo
    else if (estado_persona == 1)
    {
        # Enfermos NO se vacunan
    }
    ################################################### vacunado
    else if (estado_persona == 2)
    {
      # Despues del año se vuelve a vacunar
      if (tiempo_tras_vacunacion>=365)
      {
          se_vuelve_a_vacunar <- sample(c(0,1), size=1, replace=TRUE, prob=c(0.3,0.7))

          # Si no se vacuna, pasa nuevamente a ser susceptible
          if (se_vuelve_a_vacunar != 1)
          {
            matriz["estado", id] <- 0
          }
      }
    }
    ################################################### Recuperado
    else if (estado_persona == 3)
    {
      # A los 90 dias de recuperado se puede vacunar
      if (tiempo_recuperado>=90)
      {
          se_vacuna <- sample(c(0,1), size=1, replace=TRUE, prob=c(0.6,0.4))

          # Si se vacuna, ahora pasa a estado vacunado
          if (se_vacuna)
          {
            matriz["estado", id] <- 2
          }
      }
    }
    else{
      # Cualquier otro estado, no se evalúa
    }

  }
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
Latencia<-function(Matriz){

    for (i in 1:length(Matriz$latencia)) {
      if(Matriz$estado[i]==4){
        if(Matriz$latencia[i]>=0&Matriz$latencia[i]<=6){
          Matriz$latencia[i]<-Matriz$latencia[i]+1
      }
       if(Matriz$latencia[i]>6){
         Matriz$estado[i]<-sample(c(1,3),1,prob=c(2,8))
       } 
    }
  
  }
  Matriz
  }
#La funcion latencia nos sirve para llevar a cabo un conteo del tiempo 
#que la persona estara en periodo de latencia, en cada iteracion se 
# agrega 1 y cuando se cumpla el periodo variable pasa a ser 
#recuperado(1) o infectado(3) dependiendo la probabilidad que tiene 
#cada uno, en este caso 80% para infectado y 20% para recuperado 
#Autores:
  #Luis Alberto Guerrero Zuñiga
  #Alan Fernando Mejía Aranda
  #Jose Armando Jara Rodriguez 
