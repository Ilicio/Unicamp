
########################################
############### OPCIONAL ###############
########################################

# Instalacao do pacote
install.packages("tidyverse")

# Carregamento do pacote
library(tidyverse)

# Grafico usando lambda = 1
ggplot() +
  xlim(c(0,8)) +
  geom_function(fun = dexp, args = list(rate = 1)) +
  labs(x = "Waiting time", y = "Density")

#######################################
############# EXPONENCIAL #############
#######################################

# Valores de 1 a 8 com intervalos de 0.1
x <- seq(0, 8, 0.1)

# Grafico com lambda = 2
plot(x, dexp(x, 2), type = "l", ylab = "lambda", lwd = 1)

# lambda = 1
lines(x, dexp(x, rate = 1), col = "blue", lty = 1, lwd = 2)

# Adding a legend
legend("topright", c(expression(paste(, lambda)), "2", "1"),
       lty = c(0, 1, 1), col = c("blue", "red"), box.lty = 0, lwd = 2)

#######################################
################ GAMMA ################
#######################################

# Valores
alpha = 3
beta = 12.4
avrg = alpha*beta
std.dv = sqrt(alpha*beta^2)
x = 50

# Gráfico
range = seq(0,avrg + 5*std.dv,0.01)
y = dgamma(range,alpha,rate = 1/beta)
plot(range,y,type = 'l', ylim = c(0,max(y) + 0.01))


#######################################
############### POISSON ###############
#######################################

# Probabilidade
aprovados = dpois(1:140, lambda = 2.3333333)
plot(aprovados, type = "l", xlab = "chamadas", ylab = "probabilidade")

# Probabilidade acumulada
aprovados1 = ppois(19,lambda = 2.3333333, lower.tail = F)
aprovados2 = ppois(19,lambda = 2.3333333, lower.tail = F) + ppois(19,lambda = 2.3333333)

plot(ppois(1:40,2.3333333), type = "s", xlab = "chamadas", ylab = "probabilidade")

