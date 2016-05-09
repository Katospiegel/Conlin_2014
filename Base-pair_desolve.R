library(deSolve)
## Chaos in the atmosphere
Lorenz <- function(t, state, parameters) {
  with(as.list(c(state, parameters)), {
    dX <- a * X + Y * Z
    dY <- b * (Y - Z)
    dZ <- -X * Y + c * Y - Z
    list(c(dX, dY, dZ))
  })
}
parameters <- c(a = -8/3, b = -10, c = 28)
state <- c(X = 1, Y = 1, Z = 1)
times <- seq(0, 100, by = 0.01)
out <- ode(y = state, times = times, func = Lorenz, parms = parameters)
plot(out)

}

BasePair <- function(t, state, parameters){
  with(as.list(c(state,parameters)), {
    dWp <- w*p + y*(1-p)
    dWn <- x*p + z*(1-p)
    dp <- ((p*Wp)/(p*Wp+((1-p)*Wn)))
    list(c(dWp, dWn, dp))
  })
}
parameters <- c(w = 3, x = 4, y = 2, z = 1)
state <- c(Wp = 1, Wn = 1, p = 0.5)
times <- seq(0, 100, by = 0.001)
out <- ode(y = state, times = times, func = BasePair, parms = parameters, method="rk4")
plot(out)

