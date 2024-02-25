f = function(x, expr){
  eval(expr)
}

checkBolzano = function(expr, lower, higher){
  return(f(lower, expr) * f(higher, expr) < 0)
}

regulaFalsi = function(expr, n, limA, limB){
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
  
  i = 0
  fx = 1
  result = matrix(0, ncol=2)
  
  while (i < n && fx != 0){
    fHigherLim = f(higherLim, expr)
    fLowerLim = f(lowerLim, expr)
    x = higherLim - fHigherLim * (higherLim - lowerLim) / (fHigherLim - fLowerLim)
    fx = f(x, expr)
    result = rbind(result, c(x, fx))
    
    if (f(x, expr) * f(lowerLim, expr) < 0){
      higherLim = x
    } else {
      lowerLim = x
    }
    
    i = i + 1
  }
  
  colnames(result) = list("xi", "f(xi)")
  return(result[-1,])
  
}

func<-expression(x^2-2)

regulaFalsi(func, 6,2,0)
