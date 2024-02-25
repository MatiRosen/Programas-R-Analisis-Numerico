# Al archivo le pondria el nombre completo (Lagrange)

# Por un tema de hacer todos los programas iguales, pondría el ejemplo al
# final de todo.

# Ademas, usar siempre = o siempre <-. Yo no los mezclaria. (Yo uso siempre =) 
x<-c(1,2,3,4,5,6,7)
fx<-c(144,56,35,22,78,3,17)
A=cbind(x,fx)

# Tambien el showGraph lo pondría como último argumento.
intL=function(A,xs,showGraph=F,showExpr=F,showMatrix=F){

# Quizas a las variables le pondria nombres mas descriptivos.
  m<-A[,1]
  n<-A[,2]
  l=length(m) 
  terms<-c()
  den<-c()
  
  for(i in 1:l){
    den[i] = 1
    for(y in 1:l){
      if(y != i){
        den[i]=den[i]*(m[i]-m[y])
      }
    }
    num = 1
    for(y in 1:l){
      if(y != i){
        num=num*(xs-m[y])
      }
    }
    terms[i]=n[i]*num/den[i]
  }
  
  fxs=(sum(terms))
#******************************************************
  if(showGraph==T | showExpr==T){
    expr=c()
    
    for (i in 1:l) {
      expr[i]=paste("(",as.character(n[i]))
    
      for(j in 1:l){
        if(j != i){  
          expr[i] = paste(expr[i],paste(")*(x-",as.character(m[j],")")))
        }
      }
    
      expr[i] = paste(expr[i],")","/",as.character(den[i]))
    }
    
    expr=paste(expr,collapse = " + ")
    
    poly=function(x) {
      eval(parse(text=paste(expr,collapse = " + ")))
    }
    #******************************************************
    # Si no me equivoco, todo el codigo anterior no es necesario para el graph.
    # Por lo tanto, yo el if de arriba lo separaria:
    # if (showExpr) entonces haces todo el codigo de arriba y el de abajo en
    # el que mostras.
    
    # Y abajo de ese if, meteria este del showGraph, pero me parece que no
    # es necesario que esté uno dentro del otro.
    if(showGraph==T){
      curve(poly, from = min(m), to = max(m), main = "Lagrange Polynomial",
        col = "black", lwd = 2,type = "l",xlab = "X",ylab = "P(x)")
      points(m,n,pch = 21,cex=1.5,lwd=0.5,bg="red")
      points(xs, poly(xs), pch = 21, cex = 1.8, lwd = 0.9, bg = "yellow")
    }
    #******************************************************
    if(showExpr==T){
      print(" Expression : ")
      cat("A(x) = ", expr, "\n")
      # Yo para las lineas uso el siguiente codigo:
      # Primero, arriba de todo en la funcion, genero la siguiente variable:
      # separatorLine = paste(rep("-", 10), collapse = "")
      # despues, para que todos los prints sean iguales, los uso asi:
      # print(separatorLine)
      print("---------------------------------")
    } 
  }
   #******************************************************
  if(showMatrix==T){
     matrix = matrix(c(seq(1,l),terms), byrow=F,nrow = l, ncol = 2)
     colnames(matrix) = list("L(i)", "Term")
     print("Coefficients Matrix: ")
     print(matrix)
     print("---------------------------------")
   }
  #******************************************************
  names(fxs)="f(xs)"
  return(fxs)  
}