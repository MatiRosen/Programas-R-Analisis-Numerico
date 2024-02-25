bisection = function(func, lowerLim, upperLim, iterations, digits = 15){
  options(digits=digits)

  f = function(x){
    eval(func)
  }
  
  if (f(lowerLim) * f(upperLim) >= 0){
    return("Bolzano's theorem is not verified.")
  }
    
  if(lowerLim >= upperLim){
    print("The interval limits are reversed!")
    aux = lowerLim
    lowerLim = upperLim
    upperLim = aux
  }
  
  counter = 1
  result = c(0)
  
  while(counter <= iterations){
    x1 = (lowerLim+upperLim) / 2
    
    if(f(x1) == 0){
      counter = iterations
    } else{
      if(f(lowerLim) * f(x1) < 0){
        upperLim = x1
      } else{
        lowerLim = x1
      }
      
      aprox = f(x1)
      counter = counter + 1
    }
    
    result = rbind(result, c(x1, f(x1)))
  }
  
  colnames(result) = c("xi", "f(xi)")
  rownames(result) = c(0 : (nrow(result) - 1))
  
  return(result[-1, ])
    
}

# Example
res = bisection(expression(x^2-2), 0,2,6)
res