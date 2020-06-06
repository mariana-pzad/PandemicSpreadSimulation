source("helperfunctions_vacunation.R")
Pob <- 10000;
# 50 pruebas por dia
NoPruebas <- 50;
# 14 dias de cuarentena
LimiteCuarentena <- 14;

#Inicializar la matriz
EstadoDelSistema <- matrix(0, nrow=Pob, ncol=22)
EstadoDelSistema[,1] <- 1:Pob #Columna 1: Identificador del individuo (número:ID)
for(j in 1:sqrt(Pob)){ #CICLO PARA LLENAR EL TABLERO
  EstadoDelSistema[,2][EstadoDelSistema[,1] <= (j*sqrt(Pob)) & EstadoDelSistema[,1] > ((j-1)*sqrt(Pob)) ] <- j
  EstadoDelSistema[,3] <- c(1:sqrt(Pob))
}
#Columna 2: Posicion dentro del Tablero (fila)
#Columna 3: Posicion dentro del Tablero (columna)
EstadoDelSistema[sample(1:10000,1),4] <- 1
#Columna 4: Estado del individuo [0:susceptible, 1:enfermo, 2:vacunado, 3:recuperado, 4:latente, 5:muerto]
EstadoDelSistema[,5] <- abs(round(rnorm(10000,14,10)))
#Columna 5: Edad [años]
EstadoDelSistema[,6] <- 0.5
#Columna 6: Tasa de contagio [probabilidad]
EstadoDelSistema[,7] <- 0.12
#Columna 7: Tasa de letalidad [probabilidad]
#Columna 8: Tasa de vacunación [probabilidad]
#Columna 9: Periodo de latencia [d�?as]
#Columna 10: Periodo de enfermedad [d�?as]
EstadoDelSistema[,11] <- 30
#Columna 11: Radio de movilidad []
EstadoDelSistema[,13] <- 1
L<-length(EstadoDelSistema[,5][EstadoDelSistema[,5]>=18 & EstadoDelSistema[,5]<=65])
EstadoDelSistema[,12][EstadoDelSistema[,5]>=18 & EstadoDelSistema[,5]<=65]<-sample(c(0,1),size=L,TRUE,prob=c(.6,.4)) #De los mayores de 18 asigna quienes trabajan y quienes no
LC<-length(EstadoDelSistema[,5][EstadoDelSistema[,5]>=18 & EstadoDelSistema[,5]<=85])
EstadoDelSistema[,13][EstadoDelSistema[,5]>=18 & EstadoDelSistema[,5]<=85]<-sample(c(0,1),size=LC,TRUE,prob=c(.4,.6)) #De los mayores de 18 que conducen
LT<-length(EstadoDelSistema[,12][EstadoDelSistema[,12]==1])
EstadoDelSistema[,14][EstadoDelSistema[,12]==1]<-sample(c(1,2,3),size=LT,TRUE,prob=c(0.3,0.6,0.1)) #De los que trabajan que tipo de riesgo
#Columna 12: Trabaja [0:No,1:Si]
#Columna 13: TipoTraslado [0:CochePropio, 1:Transporte publico]
#Columna 14: TipodeTrabajo [0:Notrabja, 1:BajoRiesgo,2:MedioRiesgo,3:AltoRiesgo]
EstadoDelSistema[,15] <- 0.12
#Columna 15: Tasa de recuperación [probabilidad]
#Columna 16: Cuarentena [binaria]
#Columna 23: Contador del periodo de cuarentena [dias]
#Columna 17: Tiempo tras vacunación
#Columna 18: Tiempo recuperado [dias]
Coordenadas<-CorTrabajador(LT) # Numero de aleatorios dentro del centro
EstadoDelSistema[,19]<-rep(0)
EstadoDelSistema[,19][EstadoDelSistema[,12]==1]<-Coordenadas[,3]  #Posicion de trabajo en Y Fila
EstadoDelSistema[,20]<-rep(0)
EstadoDelSistema[,20][EstadoDelSistema[,12]==1]<-Coordenadas[,2] #Posicion de trabajo en XColumna
EstadoDelSistema[,21][EstadoDelSistema[,12]!=1]<-rep(0) #Posicion de trabajo en XColumna
EstadoDelSistema[,21][EstadoDelSistema[,12]==1]<-Coordenadas[,1] #Id del trabajo
#Columna 19: Lugar de trabajo coordenada Y (fila)
#Columna 20: Lugar de trabajo coordenada X (columna)
#Columna 21: Id_LugarTrabajo
#Columna 22: Esperado periodo latencia

#####################################################
#IMPRIME LA INFORMACIÓN DE LOS DATA FRAMES (NO LA METIMOS EN LA FUNCION YA QUE ESTO SOLO DEBERÍA GRAFICARSE AL FINAL)
#par(mfrow=c(1,1))
#plot(gsus,col="gray",pch=15,xlab="DIAS",ylab="DATOS",xlim=c(0,dias),ylim=c(0,1000))
#par(new=T)
#plot(ginf,col="red",pch=15,xlab="DIAS",ylab="DATOS",xlim=c(0,dias),ylim=c(0,1000))
#par(new=T)
#plot(gvac,col="yellow",pch=15,xlab="DIAS",ylab="DATOS",xlim=c(0,dias),ylim=c(0,1000))
#par(new=T)
#plot(grec,col="orange",pch=15,xlab="DIAS",ylab="DATOS",xlim=c(0,dias),ylim=c(0,1000))
#par(new=T)
#plot(glat,col="magenta",pch=15,xlab="DIAS",ylab="DATOS",xlim=c(0,dias),ylim=c(0,1000))
#par(new=T)
#plot(gmue,col="green",pch=15,xlab="DIAS",ylab="DATOS",xlim=c(0,dias),ylim=c(0,1000))
#title("CONTAGIOS VS OTROS FACTORES")

funcion_graficar <- function(M){#La funcion graficar funciona tomando como parametro la matriz generada de las demas funciones y graficandola cada una de una color diferente

   #INICIALIZACIÓN DE VARIABLES
  dia<-1          #solo se corre una vez!!! es un contador
  dias<-14        #solo se corre la primera vez
  vdias<-(1:dias) #solo se corre la primera vez
  vcont<-(1:dias) #solo se corre la primera vez
  gsus<-data.frame(vdias,vcont)
  ginf<-data.frame(vdias,vcont)
  gvac<-data.frame(vdias,vcont)
  grec<-data.frame(vdias,vcont)
  glat<-data.frame(vdias,vcont)
  gmue<-data.frame(vdias,vcont)
  
  #SE CREA LA MATRIZ DE 16 COLUMNAS
  n<-1000 #numero de individuos
  #MA<-matrix(0,n,16)
  #CUENTA EL NUMERO DE CONTAGIADOS, VACUNADOS, LATENTES, ETC Y LOS COLECTA EN DATA FRAMES (USA EL CONTADOR "DIA")
  sus<-sum(MA[,4] == 0)
  gsus[dia,2]<-sus
  inf<-sum(MA[,4] == 1)
  ginf[dia,2]<-inf
  vac<-sum(MA[,4] == 2)
  gvac[dia,2]<-vac
  rec<-sum(MA[,4] == 3)
  grec[dia,2]<-rec
  lat<-sum(MA[,4] == 4)
  glat[dia,2]<-lat
  mue<-sum(MA[,4] == 5)
  gmue[dia,2]<-mue
  #TOMA LOS VALORES DE LA MATRIZ MA DE 16 COLUMNAS Y EN BASE A SUS COORDENADAS LAS COLOCA EN LA MATRIZ M
   M<-matrix(6,100,100)
  for(j in 1:n){
    fila<-MA[j,2]
    columna<-MA[j,3]
    estado<-MA[j,4]
    M[fila,columna]<-estado
  }
  #TOMA LOS VALORES DE LA MATRIZ MA DE 16 COLUMNAS Y LEE SOLO LOS QUE ESTAN EN ESTADO "1" (EN CUARENTENA) Y LOS ALMACENA EN LA MATRIZ B
   B <- matrix(0,100,100)
   for (i in 1:100) {
     for(j in 1:100){
       if (M[i,j]!=6){
         fil = i #fila columna 2
         col = j #columna columna 3
         for (k in 1:n) {
           if(MA[k,2]==fil & MA[k,3]== col & MA[k,16]==1){
             fill<-MA[k,2]
             coll<-MA[k,3]
             cuarentena<-MA[k,16]
             B[fill,coll]<-cuarentena
           }
         }
       }
     }
   }
  
  #IMPRIME A LOS INDIVIDUOS DE LA MATRIZ M SEGÚN SU ESTADO 
  par(mfrow=c(1,1))
  plot(which(M==6, arr.ind=TRUE),col="white", pch=15,xlab="Años",xlim=c(0,150), ylim=c(0,100))
  par(new=T)
  plot(which(M==0, arr.ind=TRUE),col="gray", pch=15,xlab="Años",xlim=c(0,150), ylim=c(0,100))
  par(new=T)
  plot(which(M==1, arr.ind=TRUE),col="red", pch=15,xlab="Años",xlim=c(0,150), ylim=c(0,100))
  par(new=T)
  plot(which(M==2, arr.ind=TRUE),col="yellow", pch=15,xlab="Años",xlim=c(0,150), ylim=c(0,100))
  par(new=T)
  plot(which(M==3, arr.ind=TRUE),col="orange", pch=15,xlab="Años",xlim=c(0,150), ylim=c(0,100))
  par(new=T)
  plot(which(M==4, arr.ind=TRUE),col="magenta", pch=15,xlab="Años",xlim=c(0,150), ylim=c(0,100))
  par(new=T)
  plot(which(M==5, arr.ind=TRUE),col="green", pch=15,xlab="Años",xlim=c(0,150), ylim=c(0,100))
  par(new=T)
  
  #IMPRIME RECUADROS NEGROS SOBRE CADA INDIVIDUO EN ESTADO "1" (CUARENTENA)
  plot(which(B==1, arr.ind=TRUE), pch=0,xlab="Años",xlim=c(0,150), ylim=c(0,100))
  par(new=T)
  title("REPRESENTACIÓN PANDEMIA")
  legend(cex = .8, x = "topright",legend = c("Sus","En","Va", "Re","La","Mu") , fill =  c("gray","Red","yellow","orange","magenta","green"))
  Sys.sleep(.1)
  return(M)
  dia<-dia+1 #ES EL CONTADOR, PARA QUE LA SIGUIENTE VES QUE CORRA LA FORMULA Y CAMBIEN LOS DATOS DE MA, RECOLECRE NUEVA INFORMACIÓN
            #Y PONER ESA INFORMACIÓN EN LOS DATA FRAMES
  }
funcion_graficar(M)
# Autores:
  #Maricela Alejandra Valero Fuentes
  #Edwin Martin Romero Silva
  #Erick Hinojosa Aguirre

###############################################################################################################################################################
Contagio <- function(Matriz){
  Rp<-sqrt(Pob)
  
  DiasCamEstatus<-15
  for(j in 1:Pob){
    B<-0
    Arriba<- j+Rp
    Abajo<- j-Rp
    Izqu<-(j-1)
    Der<-(j+1)
    InfIzq<- j-(Rp+1) 
    InfDer<- j-(Rp-1)
    SupIzq<- j+(Rp-1)
    SupDer<- j+(Rp+1)
    
    if(Matriz[j,4]==0 & Matriz[j,16]==0 ){
      #0
      if((Matriz[j,2]!=1 & Matriz[j,2]!= Rp) & (Matriz[j,3]!=1 &  Matriz[j,3]!=Rp) ){ 
        if((Matriz[Arriba,4]==1 || Matriz[Arriba,4]==4) & Matriz[Arriba,16]==0) {B=1}  #POSICION ARRIBA
        if((Matriz[Abajo,4]==1 || Matriz[Abajo,4]==4) & Matriz[Abajo,16]==0) {B=1} #POSICION ABAJO
        if((Matriz[Der,4]==1 || Matriz[Der,4]==4) & Matriz[Der,16]==0) {B=1} #POSICION DERECHA
        if((Matriz[Izqu,4]==1 || Matriz[Izqu,4]==4) & Matriz[Izqu,16]==0) {B=1} #POSICION IZQUIERDA
        if((Matriz[InfIzq,4]==1 || Matriz[InfIzq,4]==4) & Matriz[InfIzq,16]==0) {B=1} #DIAGONAL INFERIOR IZQUIERDA
        if((Matriz[InfDer,4]==1 || Matriz[InfDer,4]==4) & Matriz[InfDer,16]==0) {B=1} #DIAGONAL INFERIOR DERECHA
        if((Matriz[SupIzq,4]==1 || Matriz[SupIzq,4]==4) & Matriz[SupIzq,16]==0) {B=1} #DIAGONAL SUPERIOR IZQUIERDA
        if((Matriz[SupDer,4]==1 || Matriz[SupDer,4]==4) & Matriz[SupDer,16]==0) {B=1} #DIAGONAL SUPERIOR DERECHA
      }
      #1 Ezquina
      if(Matriz[j,2]==1 & Matriz[j,3]== 1){ #Arriba, Sup Derecha, Derecha
        if((Matriz[Arriba,4]==1 || Matriz[Arriba,4]==4) & Matriz[Arriba,16]==0) {B=1}  #POSICION ARRIBA
        if((Matriz[SupDer,4]==1 || Matriz[SupDer,4]==4) & Matriz[SupDer,16]==0) {B=1} #DIAGONAL SUPERIOR DERECHA
        if((Matriz[Der,4]==1 || Matriz[Der,4]==4) & Matriz[Der,4]==0) {B=1} #POSICION DERECHA
      }
      #2 Ezquina
      if(Matriz[j,2]==Rp & Matriz[j,3]== 1){ #abajo, derecha, inferior derecha
        if((Matriz[Abajo,4]==1 || Matriz[Abajo,4]==4) & Matriz[Abajo,4]==0) {B=1} #POSICION ABAJO
        if((Matriz[Der,4]==1 || Matriz[Der,4]==4) & Matriz[Der,4]==0) {B=1} #POSICION DERECHA
        if((Matriz[InfDer,4]==1 || Matriz[InfDer,4]==4) & Matriz[InfDer,16]==0) {B=1} #DIAGONAL INFERIOR DERECHA 
      }
      #3 Ezquina
      if(Matriz[j,2]==1 & Matriz[j,3]== Rp){ #Arriba, Izquierda, Superior Izquierda
        if((Matriz[Arriba,4]==1 || Matriz[Arriba,4]==4) & Matriz[Arriba,16]==0) {B=1}  #POSICION ARRIBA
        if((Matriz[Izqu,4]==1 || Matriz[Izqu,4]==4) & Matriz[Izqu,4]==0) {B=1} #POSICION IZQUIERDA
        if((Matriz[SupIzq,4]==1 || Matriz[SupIzq,4]==4) & Matriz[SupIzq,16]==0) {B=1} #DIAGONAL SUPERIOR IZQUIERDA
      }
      #4 Ezquina
      if(Matriz[j,2]== Rp & Matriz[j,3]== Rp){ #Abajo, Izquierda, Inferior Izquierda
        if((Matriz[Abajo,4]==1 || Matriz[Abajo,4]==4) & Matriz[Abajo,4]==0) {B=1} #POSICION ABAJO
        if((Matriz[Izqu,4]==1 || Matriz[Izqu,4]==4) & Matriz[Izqu,4]==0) {B=1} #POSICION IZQUIERDA
        if((Matriz[InfIzq,4]==1 || Matriz[InfIzq,4]==4) & Matriz[InfIzq,4]==0) {B=1} #DIAGONAL INFERIOR IZQUIERDA
      }
      #5 Debajo
      if(Matriz[j,2]==1 & (Matriz[j,3]!= 1  & Matriz[j,3]!= Rp)){ #arriba, izquierda, derecha, sup izquierda, sup derecha 
        if((Matriz[Arriba,4]==1 || Matriz[Arriba,4]==4) & Matriz[Arriba,16]==0) {B=1}  #POSICION ARRIBA
        if((Matriz[Der,4]==1 || Matriz[Der,4]==4) & Matriz[Der,4]==0) {B=1} #POSICION DERECHA
        if((Matriz[Izqu,4]==1 || Matriz[Izqu,4]==4) & Matriz[Izqu,4]==0) {B=1} #POSICION IZQUIERDA
        if((Matriz[SupIzq,4]==1 || Matriz[SupIzq,4]==4) & Matriz[SupIzq,16]==0) {B=1} #DIAGONAL SUPERIOR IZQUIERDA
        if((Matriz[SupDer,4]==1 || Matriz[SupDer,4]==4) & Matriz[SupDer,16]==0) {B=1} #DIAGONAL SUPERIOR DERECHA
      }
      #6 izquierda
      if(Matriz[j,3]==1 & (Matriz[j,2]!= 1  & Matriz[j,2]!= Rp)){ #arriba,abajo,derecha,superior derecha, inferior derecha
        if((Matriz[Arriba,4]==1 || Matriz[Arriba,4]==4) & Matriz[Arriba,16]==0) {B=1}  #POSICION ARRIBA
        if((Matriz[Abajo,4]==1 || Matriz[Abajo,4]==4) & Matriz[Abajo,4]==0) {B=1} #POSICION ABAJO
        if((Matriz[Der,4]==1 || Matriz[Der,4]==4) & Matriz[Der,4]==0) {B=1} #POSICION DERECHA
        if((Matriz[InfDer,4]==1 || Matriz[InfDer,4]==4) & Matriz[InfDer,16]==0) {B=1} #DIAGONAL INFERIOR DERECHA
        if((Matriz[SupDer,4]==1 || Matriz[SupDer,4]==4) & Matriz[SupDer,16]==0) {B=1} #DIAGONAL SUPERIOR DERECHA
      }
      #7 Arriba
      if(Matriz[j,2]==Rp & (Matriz[j,3]!= 1  & Matriz[j,3]!= Rp)){ 
        if((Matriz[Abajo,4]==1 || Matriz[Abajo,4]==4) & Matriz[Abajo,4]==0) {B=1} #POSICION ABAJO
        if((Matriz[Der,4]==1 || Matriz[Der,4]==4) & Matriz[Der,4]==0) {B=1} #POSICION DERECHA
        if((Matriz[Izqu,4]==1 || Matriz[Izqu,4]==4) & Matriz[Izqu,4]==0) {B=1} #POSICION IZQUIERDA
        if((Matriz[InfIzq,4]==1 || Matriz[InfIzq,4]==4) & Matriz[InfIzq,4]==0) {B=1} #DIAGONAL INFERIOR IZQUIERDA
        if((Matriz[InfDer,4]==1 || Matriz[InfDer,4]==4) & Matriz[InfDer,16]==0) {B=1} #DIAGONAL INFERIOR DERECHA
      }
      
      #8 Derecha
      if(Matriz[j,3]==Rp & (Matriz[j,2]!= 1  & Matriz[j,2]!= Rp)){ #arriba,abajo,izquierda,superior izquierda, inferior izquierda
        if((Matriz[Arriba,4]==1 || Matriz[Arriba,4]==4) & Matriz[Arriba,16]==0) {B=1}  #POSICION ARRIBA
        if((Matriz[Abajo,4]==1 || Matriz[Abajo,4]==4) & Matriz[Abajo,4]==0) {B=1} #POSICION ABAJO
        if((Matriz[Izqu,4]==1 || Matriz[Izqu,4]==4) & Matriz[Izqu,4]==0) {B=1} #POSICION IZQUIERDA
        if((Matriz[InfIzq,4]==1 || Matriz[InfIzq,4]==4) & Matriz[InfIzq,4]==0) {B=1} #DIAGONAL INFERIOR IZQUIERDA
        if((Matriz[SupIzq,4]==1 || Matriz[SupIzq,4]==4) & Matriz[SupIzq,16]==0) {B=1} #DIAGONAL SUPERIOR IZQUIERDA
      }
      
    }
    Pc<-Matriz[j,6] #PROB DE CONTAGIO
    if (B==1){
      Matriz[j,4]<-sample(c(0,4),1,prob=c((1-Pc),Pc))  
      #Matriz[j,9]<-Matriz[j,9]+1 #Empieza el periodo de latencia
    }
    if(Matriz[j,9]>DiasCamEstatus){
      Matriz[j,4]<-1
      Matriz[j,9]<-0
      Matriz[j,10]<-Matriz[j,10]+1
    }
    Matriz<-ContagioTrabajo(Matriz,j)
    
  }
  return(Matriz);
}
#Autores:
#Araceli Montserrat Rodriguez Crespo
#Ivan Rigoberto Ibarra Rodriguez
#Fabiola Jasso Valadez

#####################################################
#Descripcion de la funcion
# La función de Medidas de prevención nos permite modificar la probabilidad de cada individuo en base a la cantidad de infectados que existe en cada tiempo t, 
# as�? como una variable que nos ayuda a ingresar medidas especiales según las medidas levantadas por el gobierno, 
# es importante mencionar que la probabilidad se puede modificar según caracter�?sticas de cada individuo como si esta trabajando o si usa transporte publico
# y si su trabajo es de alto, medio o bajo riesgo. 

MedidasPrecaucion=function(i,Poblacion){
  Rp<-sqrt(Pob)
  FASE1<-20   # Numero de infectados para activar la fase
  FASE2<-200  # Numero de infectados para activar la fase
  FASE3<-4000 # Numero de infectados para activar la fase
  
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
    ControlDeFronteras <- 0 #0 Desactivado: La vigilancia es minima, 1 Activado: Cuarentena y estricta revision 
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
    A<-sample(c(1,0), 1, prob=c(0.5,0.5)) #Probabilidad de .5 que el que llegue al mapa este infectado
    PX<-sample(1:Rp,1)
    PY<-sample(1:Rp,1)
    P<-sample(1:Pob,1)
    if(A==1 && Poblacion[PY,4]== 0 ){
      Poblacion[P,2]<-PY
      Poblacion[P,3]<-PX
      Poblacion[P,4]<-A
    }
  }
  if(ControlDeFronteras==1){ 
    A<-sample(c(1,0), 1, prob=c(0.1,0.9)) #Probabilidad de .1 que el que llegue al mapa este infectado
    PX<-sample(1:Rp,1)
    PY<-sample(1:Rp,1)
    P<-sample(1:Pob,1)
    if(A==1 && Poblacion[PY,4]== 0 ){
      Poblacion[P,2]<-PY
      Poblacion[P,3]<-PX
      Poblacion[P,16]<-1 #Se envia a cuarentena
    }
  }
  return(Poblacion)
}
#Luis David D�vila Torres
#Carlos Antonio Espinosa Bravo
#Alan Gerardo Garza Muro

CorTrabajador=function(N){
  Ciudad<-(sqrt(Pob))/2 # Se estable la mitad del mapa como el centro de la ciudad 
  R<-(sqrt(Pob))%/%4 #Simulando el tama�o de una ciudad
  P=0
  h<-Ciudad
  k<-Ciudad
  Coorde<-matrix(0,N,3)
  ID<-1
  while (P<N) {
    Punto<- c(sample((Ciudad-R):(Ciudad+R),1),sample((Ciudad-R):(Ciudad+R),1))
    if (((Punto[1]-h)^2 + (Punto[2]-k)^2)<R^2)
    {
      Coorde[P+1,2]<-Punto[1]
      Coorde[P+1,3]<-Punto[2]
      
      posx<-which(Coorde[,2]==Punto[1])
      if(length(posx)>1){
        posx<-posx[-length(posx)]
        Pos<-which(Coorde[,3][posx]==Punto[2])
        IDP<-posx[Pos]
        if(length(Pos)!=0){
          if(length(Pos)>1){
            Coorde[P+1,1]<-Coorde[IDP[1],1]
          }else{ 
            Coorde[P+1,1]<-Coorde[IDP,1]
          }
        }else{
          Coorde[P+1,1]<-ID
          ID<-ID+1
        }
      }else{
        Coorde[P+1,1]<-ID
        ID<-ID+1
      }
      P<-P+1
      
    }
  }
  return(Coorde)
}#Luis David D�vila Torres

ContagioTrabajo=function(Poblacion,j){
  B2=0
  Pc<-Matriz[j,6] #PROB DE CONTAGIO
  Idtrabajo<-which(Poblacion[,21]==Poblacion[j,21])
  
  if(Poblacion[j,4]==0){
    Idtrabajo<-Idtrabajo[-which(Idtrabajo==j)]
    if (sum(Poblacion[Idtrabajo,4]==1)>0 || sum(Poblacion[Idtrabajo,4]==4)>0){ B2=1}
  }
  
  
  if (B2==1){
    Poblacion[j,4]<-sample(c(0,4),1,prob=c((1-Pc),Pc))  
    #Matriz[j,9]<-Matriz[j,9]+1 #Empieza el periodo de latencia
  }
  return(Poblacion)
}#Luis David D�vila Torres

GraficaTrabajo<-function(Poblacion){ #ejemplo de grafico de hogar a trabajo
  plot(Poblacion[,2],Poblacion[,3])
  
  for (k in 1:LT) {
    Trabaja<-which(Poblacion[,12]==1)
    Trabaja[k]
    ey<-c(Poblacion[Trabaja[k],2],Poblacion[Trabaja[k],19]) 
    ex<-c(Poblacion[Trabaja[k],3],Poblacion[Trabaja[k],20])
    
    lines(ex,ey,col=k)
    points(Poblacion[Trabaja[k],3],Poblacion[Trabaja[k],2],col="gray",lwd=7)
  }
}#Luis David D�vila Torres

###################################################

cuarentena <- function(poblacion, no_pruebas)
    # Obtenemos los individuos con latencia o infectados obtenidos
    # aleatoriamente para ponerlos en cuarentena.
    filas <- prueba(poblacion, no_pruebas);
    # Iteramos sobre todos los ids para poner estos individuos en cuarentena.
    for (fila in filas) {
        # Lo ponemos en cuarentena e iniciamos el contador de 
        # cuarentena en 0. 
        poblacion[fila,16] <- 1;
        poblacion[fila,23] <- 0;
    }
    return(poblacion);
}

avanzar_cuarentena <- function(poblacion) {
  # Iteramos sobre todos los individuos en busca de los que estan en cuarentena.
  for (i in 1..dim(poblacion)[0]) {
    # Si ha cumplido el limite de cuarentena, es sacado de cuarentena.
    if (poblacion[i,23] == LimiteCuarentena) {
      poblacion[i,16] <- 0;
      poblacion[i,23] <- 0;
    }
    # Si no ha cumplido el plazo, y esta en cuarentena aumentamos su contador.
    else if (poblacion[i,16] == 1) {
      poblacion[i,23] <- poblacion[i,23] + 1;
    }
  }
  return(poblacion);
}

prueba <- function(poblacion, no_pruebas) {
    # Lista para guardar individuos ya considerados para que no se repitan
    # en la prueba.
    individuos_considerados <- c();
    # Lista para guardar los individuos que pondremos en cuarentena.
    individuos_en_latencia <- c();
    # El total de individuos en la población es necesario como limite para la 
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
          # Si no está latente o infectado aumentamos el número total de pruebas
          # para que no se vean afectadas.
          no_pruebas = no_pruebas + 1;
        }
        individuos_considerados <- c(individuos_considerados, id);
    }
    return(individuos_en_latencia);
}
# Autores:
# Victor Francisco Carrizales Castor
# Natalia ALejandra Garcia Armijo
# Fabián Gandarilla López

#####################################################

Vacunacion <- function(matriz){
  # Recorreremos la matriz, por individuo
  for(persona in 1:dim(matriz)[1]) {
    # Obtenemos el estado de la persona
    anti_vacunas<- sample(c(0,1), size=1, replace=TRUE, prob=c(0.95,0.05)) #Valor de {0,1} que indica si la persona pertenece al movimiento antivacunas
    estado_persona <- matriz[persona,4]
    edad_persona <- matriz[persona,5]
    tiempo_tras_vacunacion <- matriz[persona,17]
    tiempo_recuperado <- matriz[persona,18]
    prob_vacuna<-matriz[persona,8]
    fila_persona<-matriz[persona,2]
    columna_persona<-matriz[persona,3]
    ################################################### Susceptible
    if (anti_vacunas == 0){
      if (estado_persona == 0)
      {
        # Si se vacuna, calculamos ahora la efectividad
        if (calcular_vacunacion_susceptible(edad_persona) == 1)
        {
          # Vacunado, se cambia el estado de la persona a vacunado
          estado_persona <- 2
          tiempo_tras_vacunacion<-0
        }
      }
      ################################################### Vacunado
      else if (estado_persona == 2)
      {
        # Despues del a�o se vuelve a vacunar
        if (tiempo_tras_vacunacion>=365)
        {
          prob_vacuna<-0.7
          se_vuelve_a_vacunar <- sample(c(0,1), size=1, prob=c(1-prob_vacuna,prob_vacuna))
          
          # Si no se vacuna, pasa nuevamente a ser susceptible
          if (se_vuelve_a_vacunar == 0)
          {
            estado_persona <- 0
            tiempo_tras_vacunacion<-0
          }
        }
      }
      ################################################### Recuperado
      else if (estado_persona == 3)
      {
        # A los 90 dias de recuperado se puede vacunar
        if (tiempo_recuperado>=90)
        {
          prob_vacuna=0.4
          se_vacuna <- sample(c(0,1), size=1, prob=c(1-prob_vacuna,prob_vacuna))

          # Si se vacuna, ahora pasa a estado vacunado
          if (se_vacuna == 1)
          {
            estado_persona <- 2
            tiempo_tras_vacunacion<-0
            tiempo_recuperado<-0
          }
        }
      }
    }
  }
return(matriz)
}


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
      m[i,10]<-m[i,10]+1 #Se agrega un dia a la cuenta del periodo de enfermedad
      if (simulacion/100<=m[i,15]){ #¿Se curara?
        m[i,4]<-3
      }
    }else{
      if(m[i,4]==4 && m[i,22]==0){  #Si el sujeto se encuentra latente entra a esta clausula
        esperadoperiodolatencia<- 0
        while(m[i,22]==0){ #Se crea un loop para ver cuantos dias se tarda en recuperarse
          simulacion<-sample(1:100,1)
          if (simulacion/100<=m[i,15]){
            m[i,22]<-esperadoperiodolatencia
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
        if(Matriz[i,9]>=0&Matriz[i,9]<Matriz[i,22]){
          Matriz[i,9]<-Matriz[i,9]+1
      }
       if(Matriz[i,9]>=Matriz[i,22]){
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
  #Alan Fernando Mej�?a Aranda
  #Jose Armando Jara Rodriguez 

############## Ciclo Principal ############################

for() {

  # Cuarentena
  # Hace pruebas aleatorias y poner en cuarentena a personas que se encuentran en latencia o enfermas.
  EstadoDelSistema <- cuarentena(EstadoDelSistema, NoPruebas);
  # Incrementar el contador de cuarentena en las personas que estan en ese estado y sacar de cuarentena a las que cumplieron
  # el periodo de cuarentena.
  EstadoDelSistema <- avanzar_cuarentena(EstadoDelSistema);
  ############

}
