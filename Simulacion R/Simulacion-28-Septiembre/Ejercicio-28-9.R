
#Generar llegadas
procpois = function(T,lambda){
  S = vector()
  t = 0
  I = 0
  repeat{
    u = runif(1)
    t = t-(1/lambda)*log(u)
    if(t > T){
      break
    }else{
      I = I + 1
      S[I] = t
    }
  }
  return(S)
}

LL = procpois(T,lambda)
LL[length(LL)] = Inf

#Generar tiempos de servicio
tserv = function(){
  t = ((-1/20)*log(runif(1)))
  return(t)
}

tserv()

#Evolucion del sistema
unserv = function(LL, cierre){
  t = 0
  NLL = 0
  NS = 0
  n = 0
  y = vector() #Tiempo de atencion
  sal = vector() #Salidas
  c = vector() #Clientes
  
  #Sucesos
  tLL = LL[1]
  tS = Inf
  tm = vector()
  i = 1
  j = 1
  
  #caso 1
  if((tLL <= tS) && (tLL <= cierre)){
    t = tLL
    NLL = NLL+1
    i = i+1
    tLL = LL[i]
    n = n+1
    c[length(c)+1] = n
    if (n == 1) {
      y[j] = tserv()
      tS = t+y[j]
      j = j+1
    }
  }
}





