aleatorio = runif(n=1000, min = 0, max = 1)


if (aleatorio < (1/6)) {
  print("1")
}else if(aleatorio > (1/6) && aleatorio < (2/6)) {
  print("2")
}else if(aleatorio > (2/6) && aleatorio < (3/6)) {
  print("3")
}else if(aleatorio > (3/6) && aleatorio < (4/6)) {
  print("4")
}else if(aleatorio > (4/6) && aleatorio < (5/6)) {
  print("5")
}else if(aleatorio > (5/6) && aleatorio < (6/6)) {
  print("6")
}

function1 <- function(x,y){
  value1 <- (2*x)+(3*y)
  return(value1)
}
function1(2,5)
replics = 1000
value = vector()
for (i in 1:replics) {
  aleatorio = runif(1)
  if (aleatorio < (1/6)) {
    value[i]=1
  }else if(aleatorio > (1/6) && aleatorio < (2/6)) {
    value[i]=2
  }else if(aleatorio > (2/6) && aleatorio < (3/6)) {
    value[i]=3
  }else if(aleatorio > (3/6) && aleatorio < (4/6)) {
    value[i]=4
  }else if(aleatorio > (4/6) && aleatorio < (5/6)) {
    value[i]=5
  }else if(aleatorio > (5/6) && aleatorio < (6/6)) {
    value[i]=6
  }
}

table(value)
summary(value)
barplot(table(value))
