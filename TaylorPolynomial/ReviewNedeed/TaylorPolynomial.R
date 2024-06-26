TaylorP = function(expr, x0=0, iterations=2, xs, stepByStep = F){
  if (iterations <= 0){
    print("Iterations must be greater than 0")
    return(xs)
  } 
  
  x = x0
  terms = eval(expr, x0)
  matrix = matrix(c(0, as.character(expr), terms, terms, sum(terms)), ncol = 5)
  colnames(matrix) = list("Degree", "Dfunction", "F(x0)", "Terms", "P(xs)")
  
  for (i in 1:iterations){
    expr = D(expr, "x")
    eval_expr = eval(expr)
    terms = c(terms, (eval_expr*(xs-x0)^i)/(factorial(i)))
    matrix = rbind(matrix, c(i, expr, eval_expr, terms[length(terms)], sum(terms)))
  }
  
  if (stepByStep){
    print(terms)
    return(matrix)
  } else{
    return(terms)
  }
}

# Example

fn<-expression(sin(x)*cos(x))
result = TaylorP(fn, x0 = pi/3, iterations = 5, xs = 0.3*pi, stepByStep = T)
View(result)

# "Use result[,2] to display the complete Dfunctions if stepByStep == TRUE"
#result[,2]