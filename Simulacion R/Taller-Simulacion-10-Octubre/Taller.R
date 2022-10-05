#Generación de llegadas
procpois.H = function (T, lambda )
{
  S = vector ()
  t =0
  I =0
  repeat
  {
    u1 =runif (1); t =t -(1/ lambda )*log(u1)
    if (t>T) break else {I =I+1;S[I] =t}
  }
  return (S)
}
procpois.H(9 ,5)
LL = procpois.H(9 ,5) # Con esta llamada a la función procpois(5) hacemos una
# asignación en el vector LL a las llegadas de los clientes
LL[ length (LL)+1] =Inf
# Generación de tiempo de servicio
tserv = function () {
  t =(-1/20)*log( runif (1))
  return (t)
}
tserv ()
# Evolución del sistema
unserv = function (LL , cierre )
{ #Bucle principal de la aplicación
  t =0 # variable tiempo
  NLL =0 # contador de llegadas
  NS =0 # contador de salidas
  n =0 # SS: clientes en el sistema
  Y =vector () # tiempos de atención
  Sal =vector () # output instantes de salida
  c =vector () # clientes en el sistema
  Tp =0 # tiempo después de cierre
  #Lista de sucesos
  tLL =LL [1] # instante de llegada del cliente 1
  tS =Inf # instante salida cliente
  tm = vector () # tiempo cliente en el sistema
  i =1
  j =1
  repeat
  {
    # Caso 1
    if (( tLL <= tS) & (tLL <= cierre ))
    {
      t =tLL # t al instante de llegada
      NLL =NLL +1 # contador número de llegadas
      i =i+1
      tLL =LL[i] #leemos las llegadas
      n =n+1 # SS número de clientes en el sistema
      c[ length (c)+1] =n # recogemos el número de clientes en el sistema
      if (n ==1) # en el caso de que exista un único cliente en el sistema
      {
        Y[j] =tserv () # tiempos de atención
        tS = t+Y[j] #asignamos un tiempo de atención al único
        #cliente
        j =j+1
      }
    }
    
    # Caso 2
    #...
    
    
    
    
    
    
    
    
    #...
    
    # después de la hora de cierre
    # Caso 3
    if(min(tLL ,tS)>cierre ) # hay clientes en el sistema
    {
      if (n >0) # si hay clientes generar tiempo de sevicio
      {
        t =tS
        NS =NS + 1
        n =n -1
        c[ length (c)+1] =n
        Sal[NS] = t
        tm[NS] =Sal[NS]-LL[NS]
        if (n > 0)
        {
          Y[j] =tserv ()
          tS =t+Y[j]
          j =j+1
        }
      }
      # Caso 4
      if (n == 0) # si no quedan clientes
      {
        Tp =max(t-cierre ,0)
        break
      }
    }
  }
  if(tS == Inf)tS =t
  resultados =list (' tiempos en el sistema ',tm ,' clientes en el
sistema ',c,
                    ' tiempo medio cliente en el sistema ',
                    mean (tm ,na.rm= TRUE ),
                    ' número medio clientes en el sistema ',mean (c),
                    ' tiempo después cierre ',Tp ,' tiempos atención ',Y,
                    ' última salida ',tS)
  return ( resultados )
}
unserv (LL ,9)
plot ( unserv (LL ,9) [[4]] , type ='l')
# Tiempos después de cierre para n jornadas
tcierre = function (n) #En esta función n es el numero de replicas
{
  tc = vector ( length =n)
  for(i in 1 :n)
  {
    tc[i] = unserv (LL ,9) [[10]]
  }
  return (tc)
}
plot ( tcierre (100) ,type ='l')

