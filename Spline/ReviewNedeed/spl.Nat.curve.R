##EJEMPLO DATOS

m<-c(1,2,3,4,5,6,7)
n<-c(144,56,35,22,78,3,17)
A=cbind(m,n)

#Los argumentos de la función son de una matriz "datos" que incluya dos columnas.
#Una columna con valores de x ,y otra columna con sus respectivos f(x) 
# El argumento enlarge permite fijar el intervalo vertical del gráfico en 
# caso de que por default no quedara todo visible

spl.Nat=function(A,xs,showMatrix=FALSE,showPoly=FALSE,showGraph=FALSE,enlarge=FALSE,limits.y=c(-100,100)){
    x=A[,1]
    fx=A[,2]
    n=length(x)-1
    a=fx
    h=matrix(0,n,1)
    for (i in 1:n) {
      h[i]=(x[i+1]-x[i])}
    A=matrix(0,n+1,n+1)
    A[1,1]=1
    A[n+1,n+1]=1
    F=(matrix(0,n+1,1))
    
    for (i in 2:n) {
      A[i,i-1]=h[i-1]
      A[i,i]=2*(h[i-1]+h[i])
      A[i,i+1]=h[i]
      F[i]=3*((a[i+1]-a[i])/h[i])-3*((a[i]-a[i-1])/h[i-1])}
#-----------------------------------------------------------
    c=solve(A)%*%F
     d=c()
     b=c()
        for (i in 1:n) {
      d[i]=(c[i+1]-c[i])/(3*h[i])
      b[i]=(a[i+1]-a[i])/h[i]-h[i]*(2*c[i]+c[i+1])/3 
    }
#-----------------------------------------------------------    
    for(i in 1:length(b)){
      if(xs>=x[i] & xs<=x[i+1]){
        fxs=a[i]+b[i]*(xs-x[i])+c[i]*(xs-x[i])^2+d[i]*(xs-x[i])^3
      }
    }
#*******************************************************
    if(showMatrix==TRUE){
      print("#---------------------------------------")
      cat("\n")
      sep=rep("|",length(F))
      Frame=as.data.frame(cbind(sep,A,sep,F,sep))
      colnames(Frame)=c()
      rownames(Frame)=c()
      print("Matrix")
      print(Frame)
    }
#*****************************************************    
   if(showPoly==TRUE){
     print("#---------------------------------------")
     cat("\n")
     print("Polynomials  Si(x)")
     cat("\n")
    orden=seq(0,(length(b)-1))
    M=cbind(orden,a[1:(length(a)-1)],b,c[1:(length(c)-1)],d)
    colnames(M)=c("i", "ai","bi","ci", "di")
    print(M)
 }
#******************************************************* 
     if(showGraph==TRUE){
if(enlarge==TRUE){
  plot(x,fx,pch = 21,cex=1.5,lwd=0.5,bg="red", 
       main = "Spline Natural",xlab = "X",ylab = "SplNat(x)",
       ylim=limits.y)
     
}else{
  plot(x,fx,pch = 21,cex=1.5,lwd=0.5,bg="red", 
           main = "Spline Natural",xlab = "X",ylab = "SplNat(x)")
}
      for(p in 1:length(b)){
        expr=as.character(a[p])
        expr=paste(expr,"+",as.character(b[p])," * (x-",as.character(x[p]),")")
        expr=paste(expr,"+",as.character(c[p]),"*(x-",as.character(x[p]),")^2")   
        expr=paste(expr,"+",as.character(d[p]),"*(x-",as.character(x[p]),")^3") 
        poly=function(x) {eval(parse(text=expr))}
        curve(poly, from = x[p], to = x[p+1], col = "black", lwd = 2,type = "l",add=T)
        points(xs, fxs, pch = 21, cex = 1.8, lwd = 0.9, bg = "yellow")
      }
}
#*******************************************************
    names(fxs)="f(xs)"
    return(fxs)  
    }

# Ejemplo
spl.Nat(A,3.5)
  

