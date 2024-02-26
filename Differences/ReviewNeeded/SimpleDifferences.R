simpleDiff = function(A, order, showTable = F){
  if (nrow(A) <= order)
    return("The order of differences cannot be greater than the number of rows in the matrix.")
  
  if (order < 1)
    return("Order should be greater than 0.")
  
  separatorLine = paste(rep("-", 10), collapse = "")
  simpleDiff = A
  
  for (i in 1:order){
    result = rep(0, nrow(A))
    
    for (j in 1:(nrow(A) - i)){
      currentDiff = simpleDiff[j + 1, ncol(simpleDiff)]
      previousDiff = simpleDiff[j, ncol(simpleDiff)]

      result[j] = currentDiff - previousDiff
    }
    
    simpleDiff = cbind(simpleDiff, result)
    
    colnames(simpleDiff)[i+2] = paste("D", i)
  }
  
  if (showTable){
    print("Differences Table: ")
    print(simpleDiff)
    print(separatorLine)
  }
  
  return(simpleDiff[1, ncol(simpleDiff)])
}

x<-c(1,2,3,4,5,6,7)
fx<-c(144,56,35,22,78,3,17)
simpleDiff(cbind(x,fx), 6, T)
