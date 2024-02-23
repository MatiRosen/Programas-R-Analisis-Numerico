#####################################################################################################
#############################  Resolución de Sumas Subperiódicas  ###################################
#############################       Fórmula de Woolhouse          ###################################
#####################################################################################################

# Se debe generar una funcion "func" con la ecuacion deseada.
# UTILIZAR EL COMANDO EXPRESSION(funcion que se desee integrar)
# NO FUNCIONA CON 3X,UTILIZAR 3*X
# Ej:  func=expression("Funcion deseada")

func=expression(1/(x+2))
u<-c(0,7,14,21)
fi_u<-c(0.5,1/9,1/16,1/23)
c<-c(1:length(u))
Datos<-cbind(c,u,fi_u)
#Se crea la funcion "woolhouse" con argumentos:
# a: Fila de la tabla a partir de la cual se inicia la suma
#    indicaría  el argumento 0 de la fórmula
#n: Cantidad de periodos
#m: Cantidad de subperiodos
#u: Cantidad de terminos de Bernoulli a utilizar en el ajuste
#   Por defecto u=3 (corresponde a b6 y derivada 5ta.Maximo u=11

Woolhouse=function(Datos,a,n,m,u=3){
  d<-c(Datos[,3])
  
  #SEPARAREMOS LA FORMULA EN 3 PARTES
  
  #PRIMERA PARTE
  sum=0
  for (i in 1:n) {
    I=d[a+i]
    sum=sum+(m*I)
  }
  terminos=c(sum)
  coef=c(m)
  funcion=c(sum/m)
  #SEGUNDA PARTE
  
  x=c(Datos[a,2],Datos[a+n,2])
  fx=eval(func)
  sum1=-(m-1)/2*(fx[2]-fx[1])
  terminos=c(terminos,sum1)
  coef=c(coef,-(m-1)/2 )
  funcion=c(funcion,(fx[2]-fx[1]))
  #TERCERA PARTE
  
  #Generamos un vector con los numeros de bernoulli a utilizar a partir de b2
  B=c(1,-1/2,1/6,0,-1/30,0,1/42,0,-1/30,0,5/66,0,-691/2730,0,7/6,0,-3617/510,0,43867/798,0,-174611/330)
  be=c()
  for (i in 1:u) {
    be=c(be,B[2*i+1]/factorial(2*i))
  }
  
  #Realizamos las derivadas necesarias
  dfunc=func
  for (j in 1:(2*u-3)) {
    dfunc=c(dfunc,D(dfunc[j],"x"))
  }
  
  sum2=0
  for (s in 1:(u-1)) {
    fx=eval(dfunc[2*s])
    parcial=-be[s]*(m^(2*s)-1)*(fx[2]-fx[1])
    sum2=sum2+parcial
    terminos=c(terminos,parcial)
    coef=c(coef,-be[s]*(m^(2*s)-1))
    funcion=c(funcion,(fx[2]-fx[1]))
  }
  
  #Juntamos los resultados que hemos obtenido y resolvemos la ecuacion de Woolhouse
  
  y=sum(terminos,na.rm=TRUE)
  print(y)
  salida=list(Datos,n,m,u,coef, funcion,terminos,y)
  names(salida)=c("Datos","n","m","u","coeficientes", "funciones","terminos", "resultado")
  return(salida)
}

