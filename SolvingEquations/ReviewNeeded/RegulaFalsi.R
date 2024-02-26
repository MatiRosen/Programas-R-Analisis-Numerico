regulaFalsi = function(expr, lowerLim, upperLim, iterations, digits = 15, stepByStep = F){
  options(digits=digits)
  
  if (lowerLim == upperLim)
    return("Limits should not be the same.")
  
  if (iterations < 1) 
    return("Iterations should be greater than 0.")
  
  f = function(x){
    eval(expr)
  }
  
  if (f(lowerLim) * f(upperLim) >= 0) 
    return("Bolzano's theorem is not verified.")
  
  separatorLine = paste(rep("-", 10), collapse = "")
  
  if(lowerLim >= upperLim){
    print("The interval limits are reversed! Fixing it...")
    aux = lowerLim
    lowerLim = upperLim
    upperLim = aux
  }

  counter = 0
  fx = 1
  result = matrix(0, ncol=2)
  
  while (counter < iterations && fx != 0){
    fUpperLim = f(upperLim)
    fLowerLim = f(lowerLim)
    x = upperLim - fUpperLim * (upperLim - lowerLim) / (fUpperLim - fLowerLim)
    fx = f(x)
    result = rbind(result, c(x, fx))
    
    if (f(x) * f(lowerLim) < 0){
      upperLim = x
    } else {
      lowerLim = x
    }
    
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
regulaFalsi(expression(x^2-2), 0, 2, 6, 15, T)
