f = function(x, expr){
  eval(expr)
}

newtonR = function(expr, lowerLim, upperLim, iterations, digits = 15, stepByStep = F){
  options(digits=digits)
  
  if (lowerLim == upperLim)
    return("Limits should not be the same.")
  
  if (iterations < 1) 
    return("Iterations should be greater than 0.")

  
  if (f(lowerLim, expr) * f(upperLim, expr) >= 0) 
    return("Bolzano's theorem is not verified.")
  
  separatorLine = paste(rep("-", 10), collapse = "")
  
  if(lowerLim >= upperLim){
    print("The interval limits are reversed! Fixing it...")
    aux = lowerLim
    lowerLim = upperLim
    upperLim = aux
  }
  
  derivate = D(expr, "x")
  secondD = D(derivate, "x")
  x = lowerLim
  
  if (f(upperLim, expr) * f(upperLim, secondD) > 0){
    x = upperLim
  }
  
  counter = 1
  fx = f(x, expr)
  result = matrix(c(x, fx), ncol=2)
  
  while (counter <= iterations && fx != 0){
    xDerivate = D(expr, "x")
    
    x = x - (fx / f(x, xDerivate))
    fx = f(x, expr)
    result = rbind(result, c(x, fx))
    
    counter = counter + 1
  }
  
  if (stepByStep){
    colnames(result) = c("xi", "f(xi)")
    rownames(result) = c(0 : (nrow(result) - 1))
    print(result)
    print(separatorLine)
  }
  

  return(result[nrow(result), 1])
}

# Example
newtonR(expression(x^2-2), 0, 2, 6, 15, T)

