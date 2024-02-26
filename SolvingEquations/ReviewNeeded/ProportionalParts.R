f = function(x, expr){
  eval(expr)
}

propParts = function(expr, lowerLim, upperLim, iterations, digits = 15, stepByStep = F){
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
  x0 = lowerLim
  x = upperLim
  
  if (f(upperLim, expr) * f(upperLim, secondD) > 0){
    x0 = upperLim
    x = lowerLim
  }
  
  counter = 1
  fx = f(x, expr)
  result = matrix(0, ncol=2)
  fx0 = f(x0, expr)
  
  while (counter <= iterations && fx != 0){
    x = x - fx * ((x - x0) / (fx - fx0))
    fx = f(x, expr)
    result = rbind(result, c(x, fx))
    
    counter = counter + 1  
  }
  
  if (stepByStep){
    colnames(result) = c("xi", "f(xi)")
    rownames(result) = c(0 : (nrow(result) - 1))
    print(result[-1, ])
    print(separatorLine)
  }
  
  return(result[nrow(result), 1])
}

# Example
propParts(expression(x^2-2), 0, 2, 6, 15, T)