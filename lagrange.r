lagrange_interpolation <- function(f, a, c, b) {
  return(
    1/2 *(
        f(a)*(c^2-b^2) + 
        f(c)*(b^2-a^2) + 
        f(b)*(a^2-c^2))
            /
        (f(a)*(c-b) + f(c)*(b-a) + f(b)*(a-c)))
}

cut_solution_interval <- function(f, a, c, b, d) {
  if(a < d & d < c & f(d) < f(c)){
    return(c(a, d, c))
  }
  if(a < d & d < c & f(d) >= f(c)){
    return(c(d, c, b))
  }
  if(c < d & d < b & f(d) < f(c)){
    return(c(c, d, b))
  }
  if(c< d & d < b & f(d) >= f(c)){
    return(c(a, c, d))
  }
}

lagrange_min <- function(f, start_interval, epsilon) {
  a <- start_interval[1]
  c <- (start_interval[1] + start_interval[2]) / 2
  b <- start_interval[2]
  d <- 0
  next_d <- 0
  gamma <- epsilon/100
  iteracje <- 0
  wywolania <- 0
  if (a >= c || c >= b || a >= b) {
    return('Źle podany przedział')
  }
  while (b - a > epsilon || b - a == 0) {
    d <- lagrange_interpolation(f, a, c, b)

    if (d > a & d < b & d != c) {
      new_interval <- cut_solution_interval(f, a, c, b, d)
      wywolania <- wywolania + 2
      a <- new_interval[1]
      c <- new_interval[2]
      b <- new_interval[3]
      iteracje <- iteracje + 1
    } else {
      return(c('Nie jest zbieżny', NA, iteracje, NA))
    }
    next_d = lagrange_interpolation(f, a, c, b)
    if(abs(next_d - d) < gamma){
      return(c(d, f(d), iteracje, wywolania)) 
    }
  }
  return(c(d, f(d), iteracje, wywolania))  
}
