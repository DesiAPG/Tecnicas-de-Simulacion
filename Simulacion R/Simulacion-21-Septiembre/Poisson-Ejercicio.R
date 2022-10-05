library(lattice)
lambda = function(x)(5+5*x)*I(x>=3&&x<=5)+20*I(x>=3&&x<=5)+(20-2*(x-5))*I(x>=5&x<=9)

procpois <- function(T){
  S <- vector()
  t <- 0
  I <- 0
  repeat{
    u <- runif(1)
    t <- t - (1 / 20)*log(u)
    if(t > T){
      break
    }
  
    if(t<=T){
      u2 <- runif(1)
      if(u2<=((lambda(t)) / 20)){
        I <- I + 1
        S[I] <-  t
      }
    }
    
  }
  return(S)
}

procpois(8)
