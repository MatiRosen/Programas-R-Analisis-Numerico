intN = function(A, xs, showTable = F, showExpr = F, showMatrix = F, showGraph = F){
  degree = nrow(A) - 1
  x = xs
  divDiff = A
  
  separatorLine = paste(rep("-", 10), collapse = "")
  
  for (i in 1:degree){
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
  
  expr = as.character(divDiff[1, 2])
  matrix = matrix(c(0, divDiff[1, 2], divDiff[1, 2]), 
                  nrow = nrow(A), ncol = 3)
  
  colnames(matrix) = list("Order", "Coef", "Aprox")

  for (i in 1:degree) {
    term = "1"

    for (j in 1:i) {
      term = paste(term, "*", paste("(x - ", divDiff[j, 1], ")", sep = ""))
    }
    
    expr = paste(expr, "+", as.character(divDiff[1, i + 2]), "*", term)
    aprox = eval(parse(text = expr))
    matrix[i+1,] = c(i, divDiff[1, i + 2], aprox)
  }
  expr = gsub("1 \\*", "", expr)
  
  if (showExpr){
    cat("P(x) = ", expr, "\n")
    print(separatorLine)
  }
  
  polyFunc = function(x) {
    eval(parse(text = expr))
  }
  
  # Graphs
  if (showGraph){
    curve_results <- curve(polyFunc, from = min(A[,1]), to = max(A[,1]))
    
    plot(A[,1], A[,2], pch = 21, cex = 1.5, lwd = 0.5, bg = "red", 
         main = "Newton Polynomial", xlab = "X", ylab = "P(x)", 
         ylim = c(min(curve_results$y), max(curve_results$y)))
    
    lines(curve_results, col = "black", lwd = 2)
    
    points(xs, polyFunc(xs), pch = 21, cex = 1.8, lwd = 0.9, bg = "yellow")
  }
 

  if (showMatrix){
    print("Matrix: ")
    print(matrix)
    print(separatorLine)
  }
  
  cat("Aprox: ")
  
  return(polyFunc(xs))
}


# Ejemplo
x<-c(1,2,3,4,5,6,7)
fx<-c(144,56,35,22,78,3,17)
A=cbind(x,fx) ; A

intN(A, 5.5, T,T,T,T)
