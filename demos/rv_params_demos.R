install.packages("pacman")
pacman::p_load(ggplot2, data.table, VGAM)

#we proceed in reverse chronological order
# T_1 = Cauchy / T_d / T_infty = Normal

xmin = 2
xmax = 10
ggplot() +
  xlim(xmin, xmax) +
  geom_function(fun = dcauchy, colour = "red") +
  geom_function(fun = dt, args = list(df = 2), colour = "blue") +
  geom_function(fun = dt, args = list(df = 20), colour = "purple") + 
  geom_function(fun = dnorm, colour = "orange")

#LLN for all three
set.seed(1984)
n = 20000
x_cauchy = rcauchy(n)
x_student_t_2 = rt(n, 2)
x_student_t_20 = rt(n, 20)
x_norm = rnorm(n)
cauchy_means = array(NA, n)
student_t_2_means = array(NA, n)
student_t_20_means = array(NA, n)
normal_means = array(NA, n)
for (i in 1 : n){
  cauchy_means[i] = mean(x_cauchy[1 : i])
  student_t_2_means[i] = mean(x_student_t_2[1 : i])
  student_t_20_means[i] = mean(x_student_t_20[1 : i])
  normal_means[i] = mean(x_norm[1 : i])
}
lln_sim_data = data.table(
  i = 1 : n, 
  # cauchy = cauchy_means,
  student_t_2 = student_t_2_means,
  student_t_20 = student_t_20_means,
  normal = normal_means
)
ggplot(melt(lln_sim_data, id.vars = "i")) +
  ylab("x-bar value") + 
  geom_line(aes(x = i, y = value, color = variable))

###convergence in probability of s (sample standard deviation)

cauchy_s = array(NA, n)
student_t_2_s = array(NA, n)
student_t_20_s = array(NA, n)
normal_s = array(NA, n)
for (i in 1 : n){
  cauchy_s[i] = sd(x_cauchy[1 : i])
  student_t_2_s[i] = sd(x_student_t_2[1 : i])
  student_t_20_s[i] = sd(x_student_t_20[1 : i])
  normal_s[i] = sd(x_norm[1 : i])
}
lln_sim_data = data.table(
  i = 1 : n, 
  # cauchy = cauchy_s,
  # student_t_2 = student_t_2_s,
  student_t_20 = student_t_20_s, #sd = sqrt(20/18) = 1.05
  normal = normal_s
)
ggplot(melt(lln_sim_data, id.vars = "i")) +
  ylab("x-bar value") + 
  geom_line(aes(x = i, y = value, color = variable))




###CMT
set.seed(42)
x_exp = rexp(n)
avgs = array(NA, n)
inv_avgs = array(NA, n)
cubed_avgs = array(NA, n)
for (i in 1 : n){
  avgs[i] = mean(x_exp[1 : i])
  inv_avgs[i] = 1 / avgs[i]
  cubed_avgs[i] = avgs[i]^3
}
cmt_sim_data = data.table(
  i = 1 : n, 
  avgs = avgs,
  inv_avgs = inv_avgs,
  cubed_avgs = cubed_avgs
)
ggplot(melt(cmt_sim_data, id.vars = "i")) +
  ylab("x-bar value") + 
  xlim(0, 5000) + 
  ylim(0, 2) + 
  geom_line(aes(x = i, y = value, color = variable))


### gamma / erlang / exponential

xmin = 0
xmax = 5
ggplot() +
  xlim(xmin, xmax) +
  geom_function(fun = dgamma, args = list(shape = 1, rate = 1), colour = "red") +
  geom_function(fun = dgamma, args = list(shape = 1, rate = 2), colour = "blue")


xmin = 0
xmax = 10
ggplot() +
  xlim(xmin, xmax) +
  geom_function(fun = dgamma, args = list(shape = 1, rate = 1), colour = "red") +
  geom_function(fun = dgamma, args = list(shape = 2, rate = 1), colour = "blue") +
  geom_function(fun = dgamma, args = list(shape = 5, rate = 1), colour = "purple")

xmin = 0
xmax = 6
ggplot() +
  xlim(xmin, xmax) +
  geom_function(fun = dgamma, args = list(shape = 1.1, rate = 1), colour = "red") +
  geom_function(fun = dgamma, args = list(shape = 1.3, rate = 1), colour = "blue") +
  geom_function(fun = dgamma, args = list(shape = 1.5, rate = 1), colour = "purple")


### more error distributions: normal vs logistic vs laplace

xmin = -4
xmax = 4
ggplot() +
  xlim(xmin, xmax) +
  geom_function(fun = dnorm, colour = "red") +
  geom_function(fun = dlogis, colour = "blue") +
  geom_function(fun = dlaplace, colour = "purple")

### ParetoI

xmin = 0
xmax = 5
k = 2 #"shape" is our lambda
ggplot() +
  xlim(xmin, xmax) +
  geom_function(fun = dpareto, args = list(shape = 1, scale = k), colour = "red") +
  geom_function(fun = dpareto, args = list(shape = 2, scale = k), colour = "blue") +
  geom_function(fun = dpareto, args = list(shape = 5, scale = k), colour = "purple")


### weibull

xmin = 0
xmax = 10
lambda = 1 #"scale" is our k
ggplot() +
  xlim(xmin, xmax) +
  geom_function(fun = dweibull, args = list(shape = lambda, scale = 1), colour = "red") +
  geom_function(fun = dweibull, args = list(shape = lambda, scale = 2), colour = "blue") +
  geom_function(fun = dweibull, args = list(shape = lambda, scale = 5), colour = "purple")

### CLT vs T-stat convergence
set.seed(42)
n = 500
Nsim = 1000
zs = matrix(NA, nrow = n, ncol = Nsim)
ts = array(NA, nrow = n, ncol = Nsim)
for (nsim in 1 : Nsim){
  x_exp = rexp(n)
  zs[i, nsim] = 
}

cmt_sim_data = data.table(
  i = 1 : n, 
  avgs = avgs,
  inv_avgs = inv_avgs,
  cubed_avgs = cubed_avgs
)
ggplot(melt(cmt_sim_data, id.vars = "i")) +
  ylab("x-bar value") + 
  xlim(0, 5000) + 
  ylim(0, 2) + 
  geom_line(aes(x = i, y = value, color = variable))

