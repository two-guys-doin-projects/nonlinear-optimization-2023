BDS_wykres <- function(f, x0, step){
  iterator <- 1
  przedzialy <- list()
  found_a <- FALSE
  found_b <- FALSE
  a <- 0
  b <- 0
  while(TRUE){
    next_x_a <- x0 - 2 ** iterator * step
    next_x_b <- x0 + 2 ** iterator * step
    if(f(next_x_a) >= f(x0) & found_a == FALSE){
      a <- next_x_a
      found_a <- TRUE
    }
    if(f(next_x_b) >= f(x0) & found_b == FALSE){
      b <- next_x_b
      found_b <- TRUE
    }
    przedzialy[[iterator]] <- c(next_x_a, next_x_b) 
    if(found_a & found_b){
      return(przedzialy)
    }
    iterator <- iterator + 1
  }
}

BDS <- function(f, x0, step){
  iterator <- 1
  przedzialy <- list()
  found_a <- FALSE
  found_b <- FALSE
  a <- 0
  b <- 0
  while(TRUE){
    next_x_a <- x0 - 2 ** iterator * step
    next_x_b <- x0 + 2 ** iterator * step
    #cat('i: ', iterator, ' || found_a: ', found_a, ' || found_b: ', found_b, ' || f(x): ', f(x0), ' || f(next_x): ', f(next_x_a), '\n')
    if((f(next_x_a) >= f(x0) || f(next_x_a) == -Inf || f(next_x_a) == Inf) & found_a == FALSE){
      a <- next_x_a
      found_a <- TRUE
    }
    if((f(next_x_b) >= f(x0) || f(next_x_b) == -Inf || f(next_x_b) == Inf) & found_b == FALSE){
      b <- next_x_b
      found_b <- TRUE
    }
    przedzialy[[iterator]] <- c(next_x_a, next_x_b)
    iterator <- iterator + 2
    if(found_a & found_b){
      return(c(a, b, iterator))
    }
  }
}
