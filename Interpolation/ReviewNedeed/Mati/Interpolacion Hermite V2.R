intH = function(A, xs, showTable = FALSE, showExpr = FALSE, showMatrix = FALSE){
  A = A[order(A[, 1]), ]
  
  expandedTable = A
  pos = 0
  for (i in 1:nrow(A)){
    for (j in ncol(A):3){
      if (!is.na(A[i,j])){
        if ((sum(is.na(A[i,3:j])) == 0)){
          newRow = A[i,]
          newRow[(j):length(newRow)] = NA
          isLastRow = (i + 1 + pos) > nrow(expandedTable)
          if (isLastRow){
            expandedTable = rbind(expandedTable, newRow)
            
          } else{
            expandedTable = rbind(expandedTable[1:(i + pos), ], newRow, expandedTable[(i + 1 + pos):nrow(expandedTable), ])
          }
          pos = pos + 1
        } else{
          expandedTable[(i + pos), j] = NA
        }
      }
    }
  }
  row.names(expandedTable) = NULL
  
  
  for (i in 1:nrow(expandedTable)) {
    for (j in 3:ncol(expandedTable)) {
      if (!is.na(expandedTable[i, j])) {
        expandedTable[i, j] <- expandedTable[i, j] / (factorial(j - 2))
      }
    }
  }
  
  
  degree = nrow(expandedTable) - 1
  x = xs
  dividedDifferences = expandedTable
  
  for (i in 1:degree){
    result = rep(0, nrow(expandedTable))
    
    for (j in 1:(nrow(expandedTable) - i)){
      currentCol = i + 1
      
      if ((currentCol + 1) > ncol(dividedDifferences) || is.na(dividedDifferences[j, (currentCol + 1)])){
        currentDifference = dividedDifferences[(j + 1), currentCol]
        previousDifference = dividedDifferences[j, currentCol]
        currentX = dividedDifferences[(j + i), 1]
        previousX = dividedDifferences[j, 1]
        
        result[j] = (currentDifference - previousDifference) / (currentX - previousX)
      } else{
        result[j] = dividedDifferences[j, (currentCol + 1)]
      }
    }
    
    if (currentCol < ncol(expandedTable)){
      dividedDifferences[, (currentCol + 1)] = result
    } else{
      dividedDifferences = cbind(dividedDifferences, result)
    }
    
    colnames(dividedDifferences)[i+2] = paste("DD", i)
  }
  
  if (showTable){
    print("Divided differences: ")
    print(dividedDifferences)
    print("------")
  }
  
  expr = as.character(dividedDifferences[1, 2])
  matrix = matrix(c(0, dividedDifferences[1, 2], dividedDifferences[1, 2], dividedDifferences[1, 2]), nrow = nrow(expandedTable), ncol = 4, byrow = TRUE)
  colnames(matrix) = list("Order", "Coef", "Coef value", "Aprox")
  for (i in 1:degree) {
    term = "1"
    
    for (j in 1:i) {
      term = paste(term, "*", paste("(x - ", dividedDifferences[j, 1], ")", sep = ""))
    }
    
    expr = paste(expr, "+", as.character(dividedDifferences[1, i + 2]), "*", term)
    aprox = eval(parse(text = expr))
    coefValue = eval(parse(text = paste(as.character(dividedDifferences[1, i + 2]), "*", term)))
    matrix[i+1,] = c(i, dividedDifferences[1, (i + 2)], coefValue, aprox)
  }
  
  if (showExpr){
    cat("P(x) = ", expr, "\n")
    print("------")
  }
  
  # Graphs
  polynomial_function = function(x) {
    eval(parse(text = expr))
  }
  
  curve = curve(polynomial_function, from = min(expandedTable[,1]), to = max(expandedTable[,1]))
  
  plot(expandedTable[,1], expandedTable[,2], pch = 21, cex = 1.5, lwd = 0.5, bg = "red", main = "Polinomio de Newton", xlab = "X", ylab = "Fx", ylim = c(min(curve$y), max(curve$y)))
  curve(polynomial_function, from = min(expandedTable[,1]), to = max(expandedTable[,1]), col = "black", lwd = 2, add = TRUE)
  points(xs, polynomial_function(xs), pch = 21, cex = 1.8, lwd = 0.9, bg = "yellow")
  
  if (showMatrix){
    print("Matrix: ")
    print(matrix)
    print("------")
  }
  
  cat("Aprox: ")
  
  return(polynomial_function(xs))
}


# Ejemplo
x=c(3,2,5,6,9)
fx=c(27,6,135,234,783)
dx=c(33,NA,85,NA,NA)
dxx=c(20,NA,NA,15,NA)
A=cbind(x,fx, dx, dxx) ; A

intH(A, 4.5, T, T, T)

# Ejemplo PPT
x = c(-1, 1, 2)
fx = c(3, 3, 6)
dx = c(1,NA,-1)
dxx = c(NA, NA, 10)
A = cbind(x,fx,dx, dxx) ; A

intH(A,1.5,T,T)
