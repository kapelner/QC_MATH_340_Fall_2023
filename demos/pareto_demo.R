pacman::p_load(ggplot2, EnvStats)


k = 1
lambda = log(5) / log(4)
q = 0.8
a = k * (1 - q)^(-1 / lambda)

xmin = k
xmax = 20

dpareto_times_x = function(x, location, shape){
  dpareto(x, location, shape) * x
}

limitRange = function(fun, args, min, max) {
  function(x) {
    args$x = x
    f = do.call(fun, args)
    f[x < min  |  x > max] <- NA
    f
  }
}
alpha = 0.5
ggplot() +
  ylab("density of pareto") + 
  xlab("amount of land") +
  xlim(xmin, xmax) +
  stat_function(fun = limitRange(dpareto, args = list(location = k, shape = lambda), 1, a), geom = "area", fill = "blue",
                alpha = alpha, n = 1e3) +
  stat_function(fun = limitRange(dpareto, args = list(location = k, shape = lambda), a, xmax), geom = "area", fill = "red",
                alpha = alpha, n = 1e3) +
  stat_function(fun = limitRange(dpareto, args = list(location = k, shape = lambda), 1, xmax))

integrate(dpareto, k, a, location = k, shape = lambda)
integrate(dpareto, a, 1e5, location = k, shape = lambda)

ggplot() +
  ylab("density of pareto times amount of land") + 
  xlab("total amount of land owned") +
  xlim(xmin, xmax) +
  stat_function(fun = limitRange(dpareto_times_x, args = list(location = k, shape = lambda), 1, a), geom = "area", fill = "blue",
                alpha = alpha, n = 1e3) +
  stat_function(fun = limitRange(dpareto_times_x, args = list(location = k, shape = lambda), a, xmax), geom = "area", fill = "red",
                alpha = alpha, n = 1e3) +
  stat_function(fun = limitRange(dpareto_times_x, args = list(location = k, shape = lambda), 1, xmax))

L_of_a = function(a){
  k^lambda * lambda / (lambda - 1) * (k^(-lambda + 1) - a^(-lambda + 1))
}
L_of_a(a)
L_of_a(.Machine$double.xmax)
pi_of_a = function(a){
  L_of_a(a) / L_of_a(.Machine$double.xmax)
}
pi_of_a(a)

