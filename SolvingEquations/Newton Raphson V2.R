f = function(x, expr){
  eval(expr)
}

checkBolzano = function(expr, lower, higher){
  return(f(lower, expr) * f(higher, expr) < 0)
}

newtonR = function(expr, n, limA, limB){
  options(digits=15)
  
  if (limA == limB)
    return("Limits should not be the same.")
  
  if (n < 1) 
    return("n should be greater than 0.")
  
  lowerLim = limA
  higherLim = limB
  
  if (lowerLim > higherLim){
    lowerLim = limB
    higherLim = limA
  }
  
  if (!checkBolzano(expr, lowerLim, higherLim)) 
    return("Bolzano's theorem is not verified.")
  
  derivate = D(expr, "x")
  secondD = D(derivate, "x")
  
  x = lowerLim
  if (f(higherLim, expr) * f(higherLim, secondD) > 0){
    x = higherLim
  }
  
  i = 0
  fx = f(x, expr)
  result = matrix(c(x, fx), ncol=2)
  
  while (i < n && fx != 0){
    xDerivate = D(expr, "x")
    
    x = x - (fx / f(x, xDerivate))
    fx = f(x, expr)
    result = rbind(result, c(x, fx))
    
    i = i + 1
  }
  
  colnames(result) = list("xi", "f(xi)")
  return(result)
  
}

func<-expression(x^2-2)     
newtonR(func, 6, 0, 2)

