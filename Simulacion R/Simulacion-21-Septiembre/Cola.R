procpois <- function(T,lambda){
  S <- vector()
  t <- 0
  I <- 0
  repeat{
    u <- runif(1)
    t <- t-(1/lambda)*log(u)
    if(t > T){
      break
    }else{
      I <- I + 1
      S[I] <- t
    }
  }
  return(S)
}

procpois(12,8)

