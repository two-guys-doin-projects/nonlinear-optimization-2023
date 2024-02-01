library(Deriv)

points <- c(
  -79,  58, -21, -55, -64,  -7, -82,  49,  81,  11,
  -46, -28,  70, -31, -83, -1,  52, -14, -29,  57,
  94, -67, -28,  61, -83,  41,   5,  -4, -69, 100,
  -47,  43,  79,  26,  41,  51, -77,  -8, -42,  77,
  100, -44, -71, -46, -14, -51,  -5, -41, -20, -32,
  42, -47,  62,  -1, -74,  73, -32, -28,  -2, 6,
  34, -94,  21, -48, -60, -13, -18, 5, 77,  74,
  -78, -57, -77, -28,  82, -83, -70, -32, 6,  96,
  81,  46,  48, 73,  55,   8,  55,   4,  14,  59,
  0,  43, -65, -78, -15, -80, -53,  78,  85,  71)

armijoCondition <- function(func, x0, d, alpha){
  if(0 > alpha || alpha > 1){
    stop("Alfa nie jest z przedzialu [0,1]")
  }
  derivative <- Deriv(func)
  function_calls <<- function_calls + 2
  return(
    func(x0 + d) <= func(x0) + (d * alpha * derivative(x0))
  )
}

determineStepSize <- function(funcDeriv, x){
  return(
    -1 * (funcDeriv[1][[1]](x)/funcDeriv[2][[1]](x))
  )
}

tuneStepSize <- function(func, x0, d, alpha, rho){
  while (armijoCondition(func, x0, d, alpha) == FALSE) {
    d <- d * rho
  }
  return(d)
}

getDerivs <- function(func){
  firstDerivative <- Deriv(func)
  secondDerivative <- Deriv(firstDerivative)
  return(c(
    firstDerivative, secondDerivative
  ))
}

functionValueDecreaseDirection <- function(functionDeriv, x){
  #print(str_interp("Wartość pierwszej pochodnej ${functionDeriv[1][[1]](x)}"))
  if(functionDeriv[1][[1]](x) < 0){
    return(-1)
  }
  return(1)
}
function_calls <- 0
newtonArmijo <- function(func, x, alpha, rho, tolerance){
  
  funcDerivative <- getDerivs(func)
  x.new <- x
  repeat{
    #print(str_interp("Druga pochodna: ${funcDerivative[2][[1]](x)}"))
    if(funcDerivative[2][[1]](x) >= 0){
      d <- determineStepSize(funcDerivative, x)
    }
    else{
      d <- 1 * functionValueDecreaseDirection(funcDerivative, x)
    }
    d <- tuneStepSize(func, x, d, alpha, rho)
    x.new <- x + d
    if(abs(x.new - x) < tolerance){
      return(c(x.new, func(x.new), function_calls))
    }
    if(x.new < -80 || x.new > 100){
      cat("x = ", x, "\n")
      return(c(NA, NA, function_calls))
    }
    x <- x.new
    y <- func(x)
  }
}

funkcja_celu <- function(x) {
  sin(x/10)*exp(-(x/10+pi)^2) - cos(x/10)*exp(-(x/10-2*pi)^2) + 0.003*(x/10)^2
}






newtonArmijo(funkcja_celu, 50, 0.15, 0.01, 0.01)
funkcja_celu(62)


results <- data.frame(x = numeric(), y = numeric(), num_calls = numeric())
colnames(results) <- c("x*", "y*", "liczba wywołań funkcji celu")

for(point in points){
  print(str_interp("Punkt ${point}"))
  function_calls <- 0
  results <- rbind(results, newtonArmijo(funkcja_celu, point, 0.15, 0.01, 0.01))
}
