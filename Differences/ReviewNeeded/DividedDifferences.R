divDiff = function(A, order, showTable = F){
  if (nrow(A) <= order)
    return("The order of differences cannot be greater than the number of rows in the matrix.")
  
  if (order < 1)
    return("Order should be greater than 0.")
  
  separatorLine = paste(rep("-", 10), collapse = "")
  divDiff = A
  
  for (i in 1:order){
    result = rep(0, nrow(A))
    
    for (j in 1:(nrow(A) - i)){
      currentDiff = divDiff[j + 1, ncol(divDiff)]
      previousDiff = divDiff[j, ncol(divDiff)]
      
      currentX = divDiff[j + i, 1]
      prevX = divDiff[j, 1]
      
      result[j] = (currentDiff - previousDiff) / (currentX - prevX)
    }
    
    divDiff = cbind(divDiff, result)
    
    colnames(divDiff)[i+2] = paste("DD", i)
  }
  
  if (showTable){
    print("Divided differences Table: ")
    print(divDiff)
    print(separatorLine)
  }
  
  return(divDiff[1, ncol(divDiff)])
}

x<-c(1,2,3,4,5,6,7)
fx<-c(144,56,35,22,78,3,17)
divDiff(cbind(x,fx), 6, T)
