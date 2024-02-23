##################### Sumación Númerica ##################### 
######################## Lubbock ############################


# Argumentos:
# a: Fila de la tabla a partir de la cual se inicia la suma
#    indicaría el argumento 0 de la fórmula
# n: Cantidad de períodos incluídos en la sumatoria
# m: Cantidad de subperiodos de cada período
# o: Opción, 
# o=1  para fórmula únicamente con diferencias 
# o=2 para fórmula con diferencias y nablas.

u<-seq(0,50,by=5)
fi_u<-c(3,28,103,228,403,628,903,1228,1603,2028,2503)
i<-c(1:length(u))
Datos<-cbind(i,u,fi_u)

Lubbock<-function(Datos,a,n,m,o){

  A1<-c((m-1)/2,-(m^2-1)/(12*m),(m^2-1)/(24*m),-((m^2-1)*(19*m^2-1))/(720*m^3),((m^2-1)*(9*m^2-1))/(480*m^3))           
  x<-Datos[,2]                                          
  fx<-Datos[,3]
  l<-length(x)
  # Verifico si hay suficientes datos
  if(l<a+n){print("No hay suficientes datos"); break}
  
  # Genero la tabla de diferencias completa
  # Armo una matriz de ceros y la completo
  diferencia <-matrix(NA,l,5)
  diferencia[,1]<-fx
  for (j in 2:5) {
    for (i in 1:(l-j+1)){
      diferencia[i,j] <-(diferencia[i+1,j-1]-diferencia[i,j-1])
    }
  }
  #***************************************
  #* #Sumatoria con nablas y diferencias.
  #***************************************
  if(o==2){ 
    # Calculo el primer término
    Primero=m*sum(Datos[a:(a+n-1),3])
    n_mn=rep(0,5)    # Nablas con argumento m*n
    k=5
    if(n<4){
      k=n+1     
      print("No es posible realizar la sumatoria con las 4 diferencias")
      print(c("se utilizarán sólo" ,  n ,"diferencias"))      
    }
    for(j in 1:k){
      n_mn[j]=diferencia[(a+n-j+1),j]
    }
    
    d_0=diferencia[a,1:k]      # Diferencias con argumento 0
    terminos=c(Primero)
   for(h in 1:k){  
    if(h==3|h==5){
     terminos=c(terminos,(-A1[h]*(n_mn[h]+d_0[h])))
    }
     else
       terminos=c(terminos,(A1[h]*(n_mn[h]-d_0[h])))
   }
  }
  else
    #***************************************
    #* #Sumatoria con diferencias solamente
    #***************************************
    if(o==1){
    # Calculo el primer término
    Primero=m*sum(Datos[a:(a+n-1),3])
    if(length(Datos[a:nrow(Datos),1])<(4+n)){
    nd=length(Datos[a:nrow(Datos),1])-n   # Número de diferencias a utilizar
    print("No es posible realizar la sumatoria con las 4 diferencias")
    print(c("se utilizarán sólo" ,  nd ,"diferencias"))
 }
     else {nd=4}
     d_mn=diferencia[(a+n),0:nd+1]  # Diferencia con argumento m*n
     print(d_mn)
     d_0=diferencia[a,0:nd+1]       # Diferencia con argumento 0
     print(d_0)
     terminos=c(Primero,A1[0:nd+1]*(d_mn - d_0))
    }
  Resultado=sum(terminos)
  print(Resultado)
  Salida=list(Datos,a,n,m,o,A1,diferencia,terminos,Resultado)
  names(Salida)=c("Datos","a","n","m","o","Coeficientes A","diferencias","terminos","Resultado")
  return(Salida)
  
}
  
u<-seq(0,30,by=5)
fi_u<-u^4
c<-c(1:length(u))
Datos<-cbind(c,u,fi_u)  ; Datos

L1=Lubbock(Datos,1,3,5,1)
L2=Lubbock(Datos,1,3,5,2)
