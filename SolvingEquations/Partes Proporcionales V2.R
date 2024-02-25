f = function(x, expr){
  eval(expr)
}

checkBolzano = function(expr, lower, higher){
  return(f(lower, expr) * f(higher, expr) < 0)
}

propParts = function(expr, n, limA, limB){
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
  
  x0 = lowerLim
  x = higherLim
  if (f(higherLim, expr) * f(higherLim, secondD) > 0){
    x0 = higherLim
    x = lowerLim
  }
  
  i = 0
  fx = f(x, expr)
  result = matrix(0, ncol=2)
  fx0 = f(x0, expr)
  
  while (i < n && fx != 0){
    x = x - fx * ((x - x0) / (fx - fx0))
    fx = f(x, expr)
    result = rbind(result, c(x, fx))
    
    i = i + 1  
  }
  
  colnames(result) = list("xi", "f(xi)")
  return(result[-1,])
  
}

func<-expression(x^2-2)     
propParts(func, 6, 0, 2)

