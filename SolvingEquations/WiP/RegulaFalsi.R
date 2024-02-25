############################################
######   Resolución de Ecuaciones   ########
######   Método por Regula Falsi    ########
############################################

#Se debe generar una función "func" con la ecuación deseada.
#func<-expression(Ecuación deseada)
#Ejemplo
func<-expression(x^2-2)

#Se crea la función "regulafalsi" con argumentos:
#n: Total de iteraciones
#a: Limite Inferior del intervalo en el que se busca raiz
#b: Limite Superior del intervalo

regulafalsi<-function(a,b,n){
  options(digits=15)

#Para simplificar código, creamos una función que nos permita valuar la función 
#de forma f(valor)
  
f=function(x){
  eval(func)
  }   

#Para que la función no produzca error,"a" debe ser mayor a "b". Igualmente si se 
#ingresan al revés, la función lo corregirá
    
  if(a>=b){
      print("a debe ser el valor más chico, b el valor más grande del intervalo [a,b]")
    c<-a
    a<-b
    b<-a
  }
  
#Verificamos el Terorema de Bolzano  
  
  if(f(a)*f(b)<0){
    cont=1
    m<-0
    Iteracion<-c(0)
    vv<-c(0)
    
#Ingresamos la condición que realice el proceso un máximo de "n" iteraciones.
        
      while(cont<=n){

#Generamos los "xn" valores,se irán acercando a la raíz de la función 
# comprendida en el intervalo.
        
        xn<-b-f(b)*((b-a)/(f(b)-f(a)))
        if(f(xn)==0){
          cont=n
        }else{
          if(f(a)*f(xn)<0){
            a<-a
            b<-xn
            cont=cont+1
            aprox=f(xn)
          }else{
            a<-xn
            b<-b
            aprox=f(xn)
            cont=cont+1
          }
        }
        
        m=m+1
        Iteracion<-c(Iteracion,m)
        v<-c(xn,aprox)
        vv<-rbind(vv,v)
      }
    
#Guardamos los valores de "xn" en cada iteración junto a "func(xn)".
    
    vv<-as.data.frame(vv)
    colnames(vv)<-c("xi","f(xi)")
    rownames(vv)<-Iteracion
    print(vv[2:nrow(vv),])
    
    }else{
      print("No se verifica el Teorema de Bolzano")
    }
 
}  

# regulafalsi<-function(a,b,n)
# regulafalsi(0,2,6)
