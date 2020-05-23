#Inicializar la matriz
Tablero <- matrix(0, nrow=10000,ncol=16)

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
    par(mfrow=c(1,1))
  plot(which(M==0, arr.ind=TRUE),col="white", pch=15,xlab="Años",xlim=c(0,21), ylim=c(0,21))
  par(new=T)
  plot(which(M==1, arr.ind=TRUE),col="red", pch=15,xlab="Años",xlim=c(0,21), ylim=c(0,21))
  par(new=T)
  plot(which(M==2, arr.ind=TRUE),col="yellow", pch=15,xlab="Años",xlim=c(0,21), ylim=c(0,21))
  par(new=T)
  plot(which(M==3, arr.ind=TRUE),col="gray50", pch=15,xlab="Años",xlim=c(0,21), ylim=c(0,21))
  par(new=T)
  plot(which(M==4, arr.ind=TRUE),col="green", pch=15,xlab="Años",xlim=c(0,21), ylim=c(0,21))
  par(new=T)
  plot(which(M==5, arr.ind=TRUE),col="black", pch=15,xlab="Años",xlim=c(0,21), ylim=c(0,21))
  par(new=T)
  title("REPRESENTACIÓN PANDEMIA")
Sys.sleep(.1)

    return(M)
}
# Autores:
  #Maricela Alejandra Valero Fuentes
  #Edwin Martin Romero Silva
  #Erick Hinojosa Aguirre

######################################################
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

  if(Infectados==FASE1){
    sum(Poblacion[,12])
    Trabaja<-which(Poblacion[,12]==1)
    Cambio<-sample(c(0,1),size=length(Trabaja),TRUE,prob=c(.05,.95)) 
    NoTrabajaPox<-which(Cambio==0)
    PY_t<-Trabaja[NoTrabajaPox]
    Poblacion[PY_t,12]<-0
    
    Poblacion[PY_t,6]<-Poblacion[PY_t,6]/T_Trabaja[NoTrabajaPox]
    Poblacion[,11]<-10
    Poblacion[,6]<-Poblacion[,6]*.95
  }
  if(Infectados==FASE2){
    sum(Poblacion[,12])
    Trabaja<-which(Poblacion[,12]==1)
    Cambio<-sample(c(0,1),size=length(Trabaja),TRUE,prob=c(.1,.9)) 
    NoTrabajaPox<-which(Cambio==0)
    PY_t<-Trabaja[NoTrabajaPox]
    Poblacion[PY_t,12]<-0
    Poblacion[PY_t,6]<-Poblacion[PY_t,6]/T_Trabaja[NoTrabajaPox]
    
    Poblacion[,11]<-5
    Poblacion[,6]<-Poblacion[,6]*.90
    ControlDeFronteras<-1
  }
  if(Infectados==FASE3){
    sum(Poblacion[,12])
    Trabaja<-which(Poblacion[,12]==1)
    Cambio<-sample(c(0,1),size=length(Trabaja),TRUE,prob=c(.3,.7)) 
    NoTrabajaPox<-which(Cambio==0)
    PY_t<-Trabaja[NoTrabajaPox]
    Poblacion[PY_t,12]<-0
    Poblacion[PY_t,6]<-Poblacion[PY_t,6]/T_Trabaja[NoTrabajaPox]
    
    Poblacion[,11]<-1
    Poblacion[,6]<-Poblacion[,6]*.80
  }
  if(ControlDeFronteras==0){
    A<-sample(c(3,0), 1, prob=c(0.5,0.5)) #Probabilidad de .5 que el que llegue al mapa este infectado
    PX<-sample(1:sqrt(Pob),1)
    PY<-sample(1:sqrt(Pob),1)
    if(A==3){
      Mapa[PX,PY]<-A #Se generan mas infectados que llegan a cualquier lugar del mapa  
    }
  }
  if(ControlDeFronteras==1){ 
    A<-sample(c(3,0), 1, prob=c(0.1,0.9)) #Probabilidad de .1 que el que llegue al mapa este infectado
    PX<-sample(1:sqrt(Pob),1)
    PY<-sample(1:sqrt(Pob),1)
    if(A==3 && Poblacion[PY,4]== 0 ){
      Mapa[PX,PY]<-A #Se generan Menos infectados que llegan a cualquier lugar del mapa  
      Poblacion[PY,4]<-6
    }
  }
  return(Poblacion)
}
#Luis David Dávila Torres
#Carlos Antonio Espinosa Bravo
#Alan Gerardo Garza Muro

###################################################
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
}
# Autores:
# Victor Francisco Carrizales Castor
# Natalia ALejandra García Armijo
# Fabián Gandarilla López

###################################################
Contagio <- function(matriz){
    NF= 10#NUMERO DE FILAS
  NC= 10#NUMERO DE COLUMNAS
  pc= #PROBABILIDAD DE CONTAGIO
  Matriz=matrix(NA,nrow=NF,ncol=NC)
  
#LLENANDO LA MATRIZ CON EL ESTADO DEL INDIVIDUO
for(i in 1:NF){ #Filas
  for (j in 1:NC){ #Columnas
     Matriz[i,j]<-Tablero[i,4]
  }
}

#BUSCANCO LOS SUSCEPTIBLES
sus<-which(Matriz==0, arr.ind = TRUE)
sus[which(sus[,]),]

#VERIFICANDO EL ESTADO PARA VER SI SE CONTAGIA O NO
for(i in 1:NF){ #Filas
  for (j in 1:NC){ #Columnas
    for(y in 1:length(sus)){
      if(Matriz[i-1,])
      
      
}

}
Contagio
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
  m<-matriz
   m[,15]<-.12 #Agregando a la matriz la prob de recuperación
  #m[,15,drop=FALSE] #Revisando que se haya agregado bien 
  for(i in 1:dim(m)[1]){
    if(m[i,4]==1){ #Si el sujeto se encuentra infectado entra a esta clausula
      periodoenfermedad<- 0
      while(m[i,4]==1){ #Se crea un loop para ver cuantos dias se tarda en recuperarse
        simulacion<-sample(1:100,1)
        if (simulacion/100<=m[i,15]){
          m[i,4]<-3
          m[i,10]<-periodoenfermedad
        }else{
          periodoenfermedad<-periodoenfermedad+1
        }
      }
    }else{
      if(m[i,4]==4){  #Si el sujeto se encuentra latente entra a esta clausula
        periodolatencia<- 0
        while(m[i,4]==4){ #Se crea un loop para ver cuantos dias se tarda en recuperarse
          simulacion<-sample(1:100,1)
          if (simulacion/100<=m[i,15]){
            m[i,4]<-3
            m[i,9]<-periodolatencia
          }else{
            periodolatencia<-periodolatencia+1
          }
        }
      }
    }
  }
  return(m)
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
