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
#Columna 12: Trabaja [0:No,1:Si]
#Columna 13: TipoTraslado [0:CochePropio, 1:Transporte publico]
#Columna 14: TipodeTrabajo [0:Notrabja, 1:BajoRiesgo,2:MedioRiesgo,3:AltoRiesgo]
#Columna 15: Tasa de recuperación [probabilidad]
#Columna 16: Cuarentena [binaria]
#Columna 17: Tiempo tras vacunación
#Columna 18: Tiempo recuperado
#Columna 19: Esperado periodo latencia
############## Ciclo Principal ############################


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
    return(matriz)
}  
        } else {
          no_pruebas = no_pruebas + 1;
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

    columna_ids <- poblacion[,1];
    # Obtenemos el registro de este individuo.
    fila <- madim(matriz)[]a_ids);
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
        }  else if (poblacion[fila,4] == 4 || poblacion[fila,4] == 1]){
            # Si este individuo esta latente o infectado lo consideramos para cuarentena
            individuos_en_latencia <- c(individuos_en_latencia, fila);
        } else {
          no_pruebas = no_pruebas + 1;
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
NF<-length(Matriz[,1])#NUMERO DE FILAS
NC<-length(Matriz[1,])#NUMERO DE COLUMNAS

#TOMANDO EN CUENTA LOS LATENTES
for(i in 2:NF-1){ #Filas
  for (j in 2:NC-1){ #Columnas
    if(Matriz[i,j]==0){
      if(Matriz[i-1,j]==4){
        Matriz[i,j]=4}
         else if (Matriz[i+1,j]==4){
             Matriz[i,j]=4}
           else if (Matriz[i,j-1]==4){
             Matriz[i,j]=4}
             else if (Matriz[i,j+1]==4){
               Matriz[i,j]=4
       }
    }
  }
}

#TOMANDO EN CUENTA LOS INFECTADOS
for(i in 2:NF-1){ #Filas
  for (j in 2:NC-1){ #Columnas
    if(Matriz[i,j]==0){
      if(Matriz[i-1,j]==1){
        Matriz[i,j]=4}
      else if (Matriz[i+1,j]==1){
        Matriz[i,j]=4}
      else if (Matriz[i,j-1]==1){
        Matriz[i,j]=4}
      else if (Matriz[i,j+1]==1){
        Matriz[i,j]=4
      }
    }
  }
}

return(Matriz);
#Autores:
#Araceli Montserrat Rodriguez Crespo
#Ivan Rigoberto Ibarra Rodriguez
#Fabiola Jasso Valadez
#####################################################

Contagio <- function(matriz){
NF<-length(Matriz[,1])#NUMERO DE FILAS
NC<-length(Matriz[1,])#NUMERO DE COLUMNAS

#TOMANDO EN CUENTA LOS LATENTES
for(i in 2:NF-1){ #Filas
  for (j in 2:NC-1){ #Columnas
    if(Matriz[i,j]==0){
      if(Matriz[i-1,j]==4){
        Matriz[i,j]=4}
         else if (Matriz[i+1,j]==4){
             Matriz[i,j]=4}
           else if (Matriz[i,j-1]==4){
             Matriz[i,j]=4}
             else if (Matriz[i,j+1]==4){
               Matriz[i,j]=4
       }
    }
  }
}

#TOMANDO EN CUENTA LOS INFECTADOS
for(i in 2:NF-1){ #Filas
  for (j in 2:NC-1){ #Columnas
    if(Matriz[i,j]==0){
      if(Matriz[i-1,j]==1){
        Matriz[i,j]=4}
      else if (Matriz[i+1,j]==1){
        Matriz[i,j]=4}
      else if (Matriz[i,j-1]==1){
        Matriz[i,j]=4}
      else if (Matriz[i,j+1]==1){
        Matriz[i,j]=4
      }
    }
  }
}

return(Matriz);

# Autores:
# Gerardo Perez Arriega
# Mariana Perez Arredondo

##########################################################

RecuperacionMuerte <- function(m){
   m[,15]<-.12 #Agregando a la matriz la prob de recuperación
  #m[,15,drop=FALSE] #Revisando que se haya agregado bien 
  for(i in 1:dim(m)[1]){
    if(m[i,4]==1){ #Si el sujeto se encuentra infectado entra a esta clausula
      simulacion<-sample(1:100,1) # Se genera una probabilidad para que le sujeto se cure
      m[i,10]<-mm[i,10]+1 #Se agrega un dia a la cuenta del periodo de enfermedad
      if (simulacion/100<=m[i,15]){ #¿Se curara?
        m[i,4]<-3
        }
    }else{
      if(m[i,4]==4 and m[i,19]==0){  #Si el sujeto se encuentra latente entra a esta clausula
        esperadoperiodolatencia<- 0
        while(m[i,4]==4){ #Se crea un loop para ver cuantos dias se tarda en recuperarse
          simulacion<-sample(1:100,1)
          if (simulacion/100<=m[i,15]){
            m[i,19]<-esperadoperiodolatencia
          }else{
            esperadoperiodolatencia<-esperadoperiodolatencia+1
          }
        }
      }
    }
    }
return(m) #Es muy importante que la matriz que este valor regresa se aplique a alguna variable dentro del código general 
}
#Autores:
#Enrique Martinez
#Sandra Negrete

#####################################################
Latencia<-function(Matriz){
    for (i in 1:in 1:dim(Matriz)[1]) {
      if(Matriz[i,4]==4){
        if(Matriz[i,9]>=0&Matriz[i,9]<Matriz[i,19]){
          Matriz[i,9]<-Matriz[i,9]+1
      }
       if(Matriz[i,9]>=Matriz[i,19]){
         Matriz[i,9]<-sample(c(1,3),1,prob=c(2,8))
       } 
    }
  }
  return(Matriz)
  }
#La funcion latencia nos sirve para llevar a cabo un conteo del tiempo 
#que la persona estara en periodo de latencia, en cada iteracion se 
# agrega 1 y cuando se cumpla el periodo variable pasa a ser 
#recuperado(3) o infectado(1) dependiendo la probabilidad que tiene 
#cada uno, en este caso 80% para recuperado y 20% para infectado 
#Autores:
  #Luis Alberto Guerrero Zuñiga
  #Alan Fernando Mejía Aranda
  #Jose Armando Jara Rodriguez 
