intN = function(A, xs, showTable = FALSE, showExpr = FALSE, showMatrix = FALSE){
  # Faltaría showGraph=F
# y en vez de FALSE pondría solo F
  degree = nrow(A) - 1
  x = xs
  dividedDifferences = A
  # Cambiaría "dividedDifferences" por "divDiff"
  
  for (i in 1:degree){
    result = rep(0, nrow(A))
    # Cambiaría "currentDifference" por "currDiff",  "currentX" por "currX"
    # y "previousX" por "prevX"
    for (j in 1:(nrow(A) - i)){
      currentDifference = dividedDifferences[j + 1, ncol(dividedDifferences)]
      previousDifference = dividedDifferences[j, ncol(dividedDifferences)]
      currentX = dividedDifferences[j + i, 1]
      previousX = dividedDifferences[j, 1]
      
      result[j] = (currentDifference - previousDifference) / (currentX - previousX)
    }
    
    dividedDifferences = cbind(dividedDifferences, result)
 
    colnames(dividedDifferences)[i+2] = paste("DD", i)
  }
  
  if (showTable){
    print("Divided differences: ")
    # mejor "Divided differences Table :  # 
    print(dividedDifferences)
    print("------")
    # Quizás un poco mas larga la linea (uniformar el largo)
  }
  
  expr = as.character(dividedDifferences[1, 2])
  matrix = matrix(c(0, dividedDifferences[1, 2], dividedDifferences[1, 2]), nrow = nrow(A), ncol = 3)
  # Dividir las lineas muy grandes :
  # matrix = matrix(c(0, dividedDifferences[1, 2], dividedDifferences[1, 2]), 
  # nrow = nrow(A), ncol = 3)
  colnames(matrix) = list("Order", "Coef", "Aprox")

  for (i in 1:degree) {
    term = "1"
    # Fijate que luego este 1 te aparece en todos los términos de la expresión
    for (j in 1:i) {
      term = paste(term, "*", paste("(x - ", dividedDifferences[j, 1], ")", sep = ""))
    }
    
    expr = paste(expr, "+", as.character(dividedDifferences[1, i + 2]), "*", term)
    aprox = eval(parse(text = expr))
    matrix[i+1,] = c(i, dividedDifferences[1, i + 2], aprox)
  }
  
  if (showExpr){
    cat("P(x) = ", expr, "\n")
    print("------")
  }
  
  # Graphs
  polynomial_function = function(x) {
    # me parece mejor "polyFunc" en vez de "polynomial_function"
    eval(parse(text = expr))
  }
  
  curve = curve(polynomial_function, from = min(A[,1]), to = max(A[,1]))
  # ¿ Es mejor curve que lines? ¿ Esta linea está de más ? Porque se repite más abajo
  plot(A[,1], A[,2], pch = 21, cex = 1.5, lwd = 0.5, bg = "red", main = "Polinomio de Newton", xlab = "X", ylab = "Fx", ylim = c(min(curve$y), max(curve$y)))
  # Partir la linea
  # plot(A[,1], A[,2], pch = 21, cex = 1.5, lwd = 0.5, bg = "red", 
  # main = "Polinomio de Newton", xlab = "X", ylab = "Fx", 
  # ylim = c(min(curve$y), max(curve$y)))
  # mejor "Newton Polynomial" y P(x) en lugar de Fx en los títulos
  curve(polynomial_function, from = min(A[,1]), to = max(A[,1]), col = "black", lwd = 2, add = TRUE)
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
x<-c(1,2,3,4,5,6,7)
fx<-c(144,56,35,22,78,3,17)
A=cbind(x,fx) ; A

intN(A, 5.5, T, T, T)
