####################################################
########      Resolución de Ecuaciones      ########
########  Método por Partes Proporcionales  ########
####################################################

#Se debe generar una función "func" con la ecuación deseada.
#func<-expression(Ecuación deseada)
#Ejemplo
func<-expression(x^2-2)

#Necesitamos de los siguientes parametros:
#n=Cantidad de Iteraciones
#a=Extremo izquierdo del intervalo
#b=Extremo derecho del intervalo
#x0=Defino por cual lado comienza el método: 0 = Comienza por extremo izquierdo, 
#                                            1 = Comienza por extremo derecho

partesprop<-function(a,b,n,x0){
result<-matrix(0,n,2)

 #Para simplificar código, creamos una función que nos permita
# valuar la función de forma f(valor)  

 f=function(x){
   eval(func)
   }
  
#Verificamos el Terorema de Bolzano  
  
  if(f(a)*f(b)<0){

    if(x0==0){ #Se comienza por el lado izquierdo, "a" es x0
      x0<-a
      fx0<-f(a)
      xn<-b  
    }
    else { if(x0==1){  #Se comienza por el lado derecho, "b" es x0
      x0<-b
      fx0<-f(b)
      xn<-a  }
      
      else {
        print("Se debe indicar 0 o 1 (El metodo comienza por izquierda o derecha)")
      }
    }
    
#Realizamos las n iteraciones y guardamos los cambios de "xn" junto a "func(xn)"
    
    for (i in 1:nrow(result)) {
      xn<-xn-f(xn)*((xn-x0)/(f(xn)-fx0))
      result[i,]=c(xn,f(xn))
    }
    colnames(result)<-c("x","f(x)")
    rownames(result)<-c(1:nrow(result))
    print(result)
  }
  else{
    print("No se verifica el Teorema de Bolzano. Defina otro intervalo")
  }
}

# partesprop<-function(a,b,n,x0){
# partesprop(0,2,6,b)