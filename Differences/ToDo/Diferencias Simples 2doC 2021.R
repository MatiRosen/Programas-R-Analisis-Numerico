############################################################################################
#############################  TABLA DE DIFERENCIAS  ###################################
############################################################################################

x<-c(1,2,3,4,5,6,7)
fx<-c(144,56,35,22,78,3,17)
A<-cbind(x,fx)

# A es una matriz con los vectores de datos (x y f(x)). 
# k es el orden de diferencia simple que se desea.
############################################################################################
#                               PROGRAMA
############################################################################################
# Argumentos:
# A : matriz con los vectores de datos (x y f(x))
# k : es el orden de la diferencia simple que se desea.

diferencias2<-function(A,k){    #establezco los argumentos
  if(nrow(A)<=k){ #verifico que la cantidad de diferencias que deseo obtener tenga sentido
    print("Revise el numero de diferencias que solicita") #Cartel de error que se emite
    }else{ #En caso de que la cantidad de diferencias solicitada pueda ser calculada...
      b<-nrow(A)-1    #el vector b es un vector auxiliar. 
      m<-A[,2]        #m es otro vector auxiliar
      names<-c("x","f(x)")  #el vector names va a concatenar todos los titulos de las columnas con las diferencias
      for(i in 1:k){  #la operacion se va a realizar tantas veces como diferencias querramos
        a<-rep(0,nrow(A)) #el vector auxiliar A es en el cual se van a cargar los datos de la nueva columna de diferencias.
        for(j in 1:b){  #Accion a realizar tantas veces ("b") como valores haya para la diferencia ("i")
          a[j]<-m[j+1]-m[j] #calculo de la diferencia utilizando los vectores auxiliares
        }
      A<-cbind(A,a) #el vector "a" contiene las diferencias (y valor 0 en las demas celdas)
      u<-"Dif" #vector auxiliar que se utiliza para nombrar la columna
      uu<-i #otro vector auxiliar utilizado para formar el nombre de la columna. Toma el valor "i", es decir el valor de la dif. a la que corresponda
      name<-paste(u,uu,sep = "", collapse = "") #funcion paste para concatenar string de caracteres
      names<-c(names,name) #armo, para cada iteracion del for, el vector con los nombres de las columnas
      b<-b-1 #para la siguiente iteracion, el valor de b deberá reducirse en una unidad puesto que habra un valor menos para dicha diferencia
      m<-A[,2+i] #el vector auxiliar m debe redefinirse dado que a la matriz A se la ha añadido una nueva columna con la nueva diferencia a partir de la cual se calculara la siguiente diferencia.
    }
    colnames(A)<-names #asigno el vector de nombres de columnas a la matriz A
    print(A) #Imprimo la matriz A, con los resultados pedidos. 
  }
}

#Explicacion detallada de cómo trabaja la función:
#El programa parte de una matriz A. Toma la segunda columna. 
#Crea un vector "a" lleno con "0" con longitud igual a tantos datos haya
#en la matriz A. Se crea un valor "b" que comenzará tomando un valor
#de una unidad menos que la longitud del vector "a". 
#Tambien se utiliza un vector auxiliar "m" que tomara los valores de la ultima columna de A
#a partir de la cual habra que tomar valores para el calculo de las diferencias. 
#Se utiliza la funcion "for" para realizar una accion tantas veces como
#diferencias se busque calcular. 
#Para cada iteracion del "for", el programa correrá otro "for"
#que se repetira tantas veces como valores deban existir para 
#esa diferencias (es decir, si hay n valores de la f(x), para la 
#primera diferencia debe haber n-1 valores, por lo que para la primera iteracion
#del primer for, el segundo for se correrá hasta n-1). 
#Este segundo for permite calcular las diferencias a partir del vector m
#y grabarlas en el vector a. 
#Luego se procede a unir el vector a (vector con los valores de las diferencias y ceros)
#con la matriz A. 
#Por ultimo, se le resta una unidad al vector b (porque la siguiente dif
#tendra una unidad menos) y se reasigna el vector m, de modo que
#en la siguiente iteracion del for -es decir, para el calculo de la proxima
#diferencia- se tomen los valores de la nueva columna que se la ha
#añadido a la matriz (las diferencias recien calculadas)


#Tambien seria posible prescindir del vector "m". Esto se puede hacer
#bindeando el vector a (o los muchos vectores a) a la matriz A antes de calcular las diferencias,
#y calcular las diferencias tomando los valores de una columna de A
#y grabandolos en la siguiente (la que corresponde).
#Pruebe hacerlo!


############################################################################################
#                               EJEMPLO
############################################################################################
#Ejemplo:
diferencias2(A,6)
