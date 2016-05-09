# Basic Pair-Structured Population Model

## Parameters and Variables

### N domination
w <- 3  # P,P
x <- 4  # N,P
y <- 1  # P,N
z <- 2  # N,N

### P domination
w <- 4  # P,P
x <- 3  # N,P
y <- 2  # P,N
z <- 1  # N,N

### Coexistence
w <- 3  # P,P
x <- 4  # N,P
y <- 2  # P,N
z <- 1  # N,N
  
### Bistability
w <- 4  # P,P
x <- 3  # N,P
y <- 1  # P,N
z <- 2  # N,N

## Equations
  
Wp <- vector() 
Wp[1] <- 0.1

p <- vector()
p[1] <- 0.9

Wn <- vector() 
Wn[1] <- 0.1 
 
#plot(p, ylim = c(0,2), xlab = "pasos", type='n')

steps <- c(1:61)

sim_data <- data.frame(steps)  

for (a in c(0.1,0.2,0.4,0.5,0.6,0.8,0.9)){
p[1] <- a 
  for (t in 1:60){ 
    Wp[t+1] <- w*p[t] + y*(1-p[t])
    Wn[t+1] <- x*p[t] + z*(1-p[t])
    p[t+1] <- ((p[t]*Wp[t])/(p[t]*Wp[t]+((1-p[t])*Wn[t])))
  }
  
  sim_data[[paste0("p_",a)]] <- p
  

}

for (w in 1:5){

  for (t in 1:60){ 
  
  Wp[t+1] <- w*p[t] + y*(1-p[t])
  Wn[t+1] <- x*p[t] + z*(1-p[t])
  p[t+1] <- ((p[t]*Wp[t])/(p[t]*Wp[t]+((1-p[t])*Wn[t])))
  }

sim_data[[paste0("p_",w)]] <- p

#lines(p, type="l")
}

for (x in 1:5){
  
  for (t in 1:50){ 
    
    Wp[t+1] <- w*p[t] + y*(1-p[t])
    Wn[t+1] <- x*p[t] + z*(1-p[t])
    p[t+1] <- ((p[t]*Wp[t])/(p[t]*Wp[t]+((1-p[t])*Wn[t])))
  }
  
  sim_data[[paste0("p_",x)]] <- p
  
  #lines(p, type="l")
}

for (y in 1:5){
  
  for (t in 1:30){ 
    
    Wp[t+1] <- w*p[t] + y*(1-p[t])
    Wn[t+1] <- x*p[t] + z*(1-p[t])
    p[t+1] <- ((p[t]*Wp[t])/(p[t]*Wp[t]+((1-p[t])*Wn[t])))
  }
  
  sim_data[[paste0("p_",y)]] <- p
  
  #lines(p, type="l")
}

for (z in 1:5){
  
  for (t in 1:30){ 
    
    Wp[t+1] <- w*p[t] + y*(1-p[t])
    Wn[t+1] <- x*p[t] + z*(1-p[t])
    p[t+1] <- ((p[t]*Wp[t])/(p[t]*Wp[t]+((1-p[t])*Wn[t])))
  }
  
  sim_data[[paste0("p_",z)]] <- p
  
  #lines(p, type="l")
}

ggplot() + 
  geom_line(data = sim_data, aes(steps, p_0.1, color="w=1")) + 
  geom_line(data = sim_data, aes(steps, p_0.4, color="w=2")) +
  geom_line(data = sim_data, aes(steps, p_0.6, color="w=3")) +
  geom_line(data = sim_data, aes(steps, p_0.8, color="w=4")) +
  geom_line(data = sim_data, aes(steps, p_0.9, color="w=5")) +
  ggtitle('Evolución de la proporción de Productores con diferentes valores de w ') +
  labs(color="Valores de w cuando x = 4, y = 1 y z = 2")


ggplot() + 
  geom_line(data = sim_data, aes(steps, p_0.1, color="p=0.1")) + 
  geom_line(data = sim_data, aes(steps, p_0.2, color="p=0.2")) +
  geom_line(data = sim_data, aes(steps, p_0.4, color="p=0.4")) +
  geom_line(data = sim_data, aes(steps, p_0.5, color="p=0.5")) +
  geom_line(data = sim_data, aes(steps, p_0.6, color="p=0.6")) +
  geom_line(data = sim_data, aes(steps, p_0.8, color="p=0.8")) +
  geom_line(data = sim_data, aes(steps, p_0.9, color="p=0.9")) +
  ggtitle('Evolución de la proporción de Productores con diferentes valores de p ') +
  labs(color="Valores de coexistencia")






