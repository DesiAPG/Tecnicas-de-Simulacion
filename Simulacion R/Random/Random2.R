aleatorio = rnorm(1000,mean = 0,sd=1)
summary(aleatorio)
runif(1,min = 1,max = 6)
aleatorio = sample((1:6),100,replace = TRUE,prob = c(1,1,1,4,1))
table(aleatorio)
plot(table(aleatorio))
aleatorio = sample((1:6),1,replace = TRUE)
aleatorio
if (aleatorio<3) {
  print("Rojo")
}else{
  print("Negra")
}