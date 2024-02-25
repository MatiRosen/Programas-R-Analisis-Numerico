###########################################
######   Resolución de Ecuaciones  ########
######  Metodo por Newton  Raphson ########
###########################################

#Defino la ecuación manualmente con expression
Y<-expression(x^2-2)     

#Argumentos de la función:

#Se crea la función "NewtonR" con argumentos:
#n: Total de iteraciones
#a: Límite Inferior del intervalo en el que se busca raíz
#b: Límite Superior del intervalo 
#xo: Indica cual es el xo. Si es 0, xo=a , si es 1 xo=b.

NewtonR<-function(a,b,n,x0,Y){
  options(digits=15)
#  Y=func
    D<-D(Y,"x")
  if(a>=b){
    print(" 'a' debe ser el valor mas chico, b el valor mas grande del intervalo [a,b]")
  }
  else{
    
    #Teorema de Bolzano
    
    x<-c(a,b)
    t=eval(Y)
    D2<-eval(D(D,"x"))
    if(t[1]*t[2]<0){                              #Si se cumple procederá a la búsqueda de las raíces. 
      cont=1
      m<-0
      vv<-c()
      Iteracion<-c()
      
      
      if(sign(t[1])==sign(D2[1])){                 #Elige el x0 y verifica que el elegido es correcto.
         x<-a                                      #Utilizamos el extremo izquierdo. 
         if(x0==1){
           print("El valor elegido de x0 es incorrecto, x0 es A")
           }
        while(cont<=n){
          x<-a-eval(Y)/eval(D) 
          if(eval(Y)==0){ 
            cont<-n       
            break
          }else{
            a<-x          
            aprox=eval(Y)
            cont=cont+1    
          }
          
          v<-c(x,eval(Y))
          vv<-rbind(vv,v)
        }
      }
        else{                                                 #Utilizamos el extremo derecho.
        if(x0==0){
          print("El valor elegido de x0 es incorrecto,x0 es B")
          }
        x<-b                                                  
        while(cont<=n){
          x<-b-eval(Y)/eval(D) 
          if(eval(Y)==0){ 
            cont<-n       
            break
          }else{
            b<-x          
            aprox=eval(Y)
            cont=cont+1    
          }
          
          v<-c(x,eval(Y))
          vv<-rbind(vv,v)
        }
      }
      vv<-as.data.frame(vv)
      colnames(vv)<-c("xi","f(xi)")
      rownames(vv)<-c(paste(1:nrow(vv)))            #La salida es una matriz conformada por las n iteraciones 
      print(vv)                                     # y sus respectivos valores en la función.
    }else print("No se cumple el teorema de Bolzano")
    
  }
}

# NewtonR<-function(a,b,n,x0,Y)
# NewtonR(0,2,6,1,Y)

