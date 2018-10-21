
# generates m brownian paths along t
brownian = function(t, m) {
  dt = diff(t)
  entries = rep(sqrt(dt), each = m) * rnorm(length(dt) * m)
  dB = matrix(entries, byrow = TRUE, nrow = length(dt))
  dB
}

# generates m integral paths of dX = fdt + gdW along t
# using euler-maruyama method
eulerMaruyama = function(f, g, t, x0, m) {
  dt = diff(t)
  dB = brownian(t, m)
  X = rbind(x0, dB)
  
  for (i in 1:length(dt)) {
    X[i+1,] = X[i,] + f(t[i], X[i,])*dt[i] + g(t[i], X[i,])*dB[i,] 
  }
  
  X
}

# generates m integral paths of dX = fdt + gdW along t
milstein = function(f, g, gx, t, x0, m) {
  dt = diff(t)
  dB = brownian(t, m)
  X = rbind(x0, dB)
  
  for (i in 1:length(dt)) {
    X[i+1,] = X[i] + f(t[i], X[i,])*dt[i] + g(t[i], X[i,])*dB[i,] +
      .5*g(t[i], X[i,])*gx(t[i], X[i,])*(dB[i,]*dB[i,] - dt)
  }
  
  X
}

# generates m integral paths of dX = fdt + gdW along t
# using strong order 1 runge-kutta
stochasticRK = function(f, g, t, x0, m) {
  dt = diff(t)
  dB = brownian(t, m)
  X = rbind(x0, dB)
  
  for (i in 1:length(dt)) {
    gi = g(t[i], X[i,])
    X[i+1,] = X[i,] + f(t[i], X[i,])*dt[i] + gi*dB[i,] +
      .5*(g(t[i], X[i,] + gi*sqrt(dt[i])) - gi)*(dB[i,]^2 - dt[i]) / sqrt(dt[i])
  }
  
  X
}

lambda = 2
mu = 1

f = function(t, X) lambda*X
g = function(t, X) mu*X
X_real = function(t, B, x0) x0 * exp((lambda - mu*mu/2)*t + mu*B)