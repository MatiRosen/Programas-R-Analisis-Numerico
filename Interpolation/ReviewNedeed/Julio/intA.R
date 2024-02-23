x<-c(1,2,3,4,5,6,7)
fx<-c(144,56,35,22,78,3,17)
A=cbind(x,fx)

# A es la matriz de datos
# xs es el punto en el cual se desea interpolar
# La función devuelve el valor interpolado f(xs)
# Si el argumento Graph = T, devuelve un gráfico
# Si el argumento Expr=T devuelve el polinomio con 
# los coeficientes redondeados a "dec" decimales

#******************************************
intA<-function(A,xs,showGraph=F,showExpr=F,showMatrix=F,dec=2){
  m<-A[,1]
  n<-A[,2]
  l=length(m)
  M<-matrix(rep(0,l*l),l,l)
  for(i in 1:l){
    for(j in 1:l){
      M[i,j]<-m[i]^(l-j)
    }
  }
  invCoef<-solve(M,as.matrix(n))
  a=xs
  fxs=0
  coef=rev(invCoef)
  for(i in 1:length(coef)){
    fxs=fxs+a^(i-1)*(coef[i])
  }
  #******************************************************* 
  if(showGraph==T){
    for (i in 1:length(coef)-1) {
      expr = as.character(coef[1])
      for (j in 1:i) {
        expr = paste(expr," + ",as.character(coef[j+1]), " * ", "(x^",as.character(j),")",sep="")
      }
    }
    poly=function(x) {eval(parse(text=expr))}

    curve(poly, from = min(m), to = max(m), main = "Algebraic Polynomial",
          col = "black", lwd = 2,type = "l",xlab = "X",ylab = "P(x)")
    points(m,n,pch = 21,cex=1.5,lwd=0.5,bg="red")
    points(xs, poly(xs), pch = 21, cex = 1.8, lwd = 0.9, bg = "yellow")
  }
#*******************************************************
  if(showMatrix==T){
    matrix = matrix(c(seq(0,l-1),coef), byrow=F,nrow = l, ncol = 2)
    colnames(matrix) = list("Degree", "Coeff")
    print("Coefficients Matrix: ")
  print(matrix)
  print("---------------------------------")
  }
#*******************************************************   
  if(showExpr==T){
    print(" Expression : ")
  rCoef=round(coef,dec)
  
  for (i in 1:length(rCoef)-1) {
    expr = as.character(rCoef[1])
    for (j in 1:i) {
      expr = paste(expr," + ",as.character(rCoef[j+1]), " * ", "(x^",as.character(j),")",sep="")
    }
  }
  cat("A(x) = ", expr, "\n")
  print("---------------------------------")
  }
  
  #*******************************************************   
  names(fxs)="f(xs)"
  return(fxs)  
  }

intA(A,5.5)
