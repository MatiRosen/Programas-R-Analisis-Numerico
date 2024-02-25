bisection = function(expr, lowerLim, upperLim, iterations, digits = 15){
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

  
  if(lowerLim >= upperLim){
    print("The interval limits are reversed! Fixing it...")
    aux = lowerLim
    lowerLim = upperLim
    upperLim = aux
  }
  
  counter = 1
  result = matrix(0, ncol=2)
  
  while(counter <= iterations){
    x = (lowerLim + upperLim) / 2
    result = rbind(result, c(x, f(x))) 
    
    if(f(lowerLim) * f(x) < 0){
      upperLim = x
    } else{
      lowerLim = x
    }
      
      counter = counter + 1
  }
  
  colnames(result) = c("xi", "f(xi)")
  rownames(result) = c(0 : (nrow(result) - 1))
  
  return(result[-1, ])
    
}

# Example
res = bisection(expression(x^2-2), 0,2,6)
res