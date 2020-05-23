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

    return()
}
# Autores:
  #Maricela Alejandra Valero Fuentes
  #Edwin Martin Romero Silva
  #Erick Hinojosa Aguirre

######################################################
MedidasPrev <- function(matriz){
    
 FASE1<-20   # Numero de infectados para activar la fase
  FASE2<-200  # Numero de infectados para activar la fase
  FASE3<-4000 # Numero de infectados para activar la fase
  ControlDeFronteras <- 0 #0 Desactivado: La vigilancia es minima, 1 Activado: Cuarentena y estricta revision 
 
 #---------------
  x<-1/matriz[,6][matriz[,12]==1]
  d<-(x-1)
  T_Trabaja<-(1+(d*.2)) #Proporcion de aumento de la tasa de infeccion por trabajo
 #---------------
  x<-1/matriz[,6][matriz[,12]==1 & matriz[,13]==1]
  d<-(x-1)
  T_Transporte<-(1+(d*.2)) #Proporcion de aumento de la tasa de infeccion por transporte publico
 #---------------
  x<-1/matriz[,6][matriz[,12]==1 & matriz[,14]==2]
  d<-(x-1)
  T_MedioR<-(1+(d*.2)) #Proporcion de aumento de la tasa de infeccion por trabajo de medio riesgo
  #---------------
  x<-1/matriz[,6][matriz[,12]==1 & matriz[,14]==3]
  d<-(x-1)
  T_AltoR<-(1+(d*.3)) #Proporcion de aumento de la tasa de infeccion por trabajo de alto riesgo

  Infectados<-length(matriz[,4][matriz[,4]==1]) #Numero de infectados

  if(i==1){ #Actualizacion de las probabilidades de contagio segun las caracteristicas de las personas
  matriz[,6][matriz[,12]==1]<-matriz[,6][matriz[,12]==1]*T_Trabaja
  matriz[,6][matriz[,12]==1 & matriz[,13]==1]<-matriz[,6][matriz[,12]==1 & matriz[,13]==1]*T_Transporte
  matriz[,6][matriz[,12]==1 & matriz[,14]==2]<-matriz[,6][matriz[,12]==1 & matriz[,14]==2]*T_MedioR
  matriz[,6][matriz[,12]==1 & matriz[,14]==3]<-matriz[,6][matriz[,12]==1 & matriz[,14]==3]*T_AltoR
  }

  if(Infectados==FASE1){
    sum(matriz[,12])
    Trabaja<-which(matriz[,12]==1)
    Cambio<-sample(c(0,1),size=length(Trabaja),TRUE,prob=c(.05,.95)) 
    NoTrabajaPox<-which(Cambio==0)
    PY_t<-Trabaja[NoTrabajaPox]
    matriz[PY_t,12]<-0
    
    matriz[PY_t,6]<-matriz[PY_t,6]/T_Trabaja[NoTrabajaPox]
    matriz[,11]<-10
    matriz[,6]<-matriz[,6]*.95
  }
  if(Infectados==FASE2){
    sum(matriz[,12])
    Trabaja<-which(matriz[,12]==1)
    Cambio<-sample(c(0,1),size=length(Trabaja),TRUE,prob=c(.1,.9)) 
    NoTrabajaPox<-which(Cambio==0)
    PY_t<-Trabaja[NoTrabajaPox]
    matriz[PY_t,12]<-0
    matriz[PY_t,6]<-matriz[PY_t,6]/T_Trabaja[NoTrabajaPox]
    
    matriz[,11]<-5
    matriz[,6]<-matriz[,6]*.90
    ControlDeFronteras<-1
  }
  if(Infectados==FASE3){
    sum(matriz[,12])
    Trabaja<-which(matriz[,12]==1)
    Cambio<-sample(c(0,1),size=length(Trabaja),TRUE,prob=c(.3,.7)) 
    NoTrabajaPox<-which(Cambio==0)
    PY_t<-Trabaja[NoTrabajaPox]
    matriz[PY_t,12]<-0
    matriz[PY_t,6]<-matriz[PY_t,6]/T_Trabaja[NoTrabajaPox]
    
    matriz[,11]<-1
    matriz[,6]<-matriz[,6]*.80
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
    if(A==3 && matriz[PY,4]== 0 ){
      Mapa[PX,PY]<-A #Se generan Menos infectados que llegan a cualquier lugar del mapa  
      matriz[PY,4]<-6
    }
  }
  return(matriz)
}  
#Luis David Dávila Torres
#Carlos Antonio Espinosa Bravo
#Alan Gerardo Garza Muro

###################################################
PonerEnCuarentena <- function(m) {
    
    return(matriz);
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
