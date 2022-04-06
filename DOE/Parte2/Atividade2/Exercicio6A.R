# Fatorial fracionário

# Fatores:
# A - Aperture        [Small and large]
# B - Exposure time   [-20% and +20%]
# C - Develop time    [30 sec and 40 sec]
# D - Mask dimension  [Small and Large]
# E - Etch time       [14.5 min and 15.5 min]

# Carregando biblioteca necessária
library(FrF2)

##########################################
############## PLANEJAMENTO ##############
##########################################

# planejamento de meia fração - 2^(5-1) 
design1 = FrF2(nruns = 16,
               nfactors = 5,
               factor.names = c("A","B","C","D", "E"),
               randomize = FALSE,
               alias.info = 3)

summary(design1)

# resposta
resposta1 = c(8,9,34,52,16,22,45,60,6,10,30,50,15,21,44,63)

# adicionando resposta ao planejamento
design1$y = resposta1

###########################################
################# ANALISE #################
###########################################

# modelo "completo"
lm1 = lm(y ~ .^5, data = design1)
summary(lm1)

# estrutura de confundimento do modelo lm1
aliases(lm1)

#modelo de segunda ordem (para ver um modelo sem confundimento)
lm2 = lm(y ~ .^2, data = design1)
summary(lm2)

# confundimento de lm2 (foi cortado o confundimento)
aliases(lm2)

# modelo sem interações BD e BE (maneira manual hierarquica)
lm3 = lm(y ~ A+B+C+D+E + A*B + A*C + A*D + A*E + B*C + C*D + C*E + D*E, data = design1)
summary(lm3)

# modelo retirando mais uma interações A*C + A*D + A*E + B*C + C*D + C*E + D*E
lm4 = lm(y ~ A+B+C+D+E + A*B, data = design1)
summary(lm4)

anova = aov(lm4)
summary(anova)

# validação da normalidade dos dados
shapiro.test(lm4$residuals)
qqnorm(lm4$residuals)
qqline(lm4$residuals)

##########################################
################ GRAFICOS ################
##########################################

# gráficos de efeitos principais e de interação
MEPlot(lm2)
IAPlot(lm2)

# gráfico de cubo (observar ponto ótimo)
cubePlot(lm2, eff1 = "A", eff2 = "B", eff3 = "C", main = "")

############################################
############ EFEITOS - OPCIONAL ############
############################################

# via pseudo erro padrao de Lenth

library(unrepx)

# design matrix
X = model.matrix( ~ A*B*C*D*E, data = design1[,-6])
X

# efeitos
efeitos = crossprod(X,resposta1)/(16/2)
efeitos = efeitos[2:16]
names(efeitos) = c("A","B","C","D","E",
                   "AB","AC","BC","AD","BD","CD","AE","BE","CE","DE")
efeitos

# graficos
par(mfrow = c(1,2))
hnplot(efeitos, method = "Lenth", ID = ME(efeitos))
parplot(efeitos, method = "Lenth")

##########################################
########## SUPERCIE DE RESPOSTA ##########
##########################################

# planejamento
design2 = expand.grid(A = c(-1,1),
                      B = c(-1,1),
                      C = c(-1,1),
                      D = c(-1,1),
                      E = c(-1.1))

# resposta
resposta2 = c(8,9,34,52,16,22,45,60,6,10,30,50,15,21,44,63)

# adicionando resposta ao planejamento
design2$y = resposta2

# pacote rsm
library(rsm)

# analise
res.fat2k = rsm(y ~ SO(A,B,C,D,E), data = design2)
summary(res.fat2k)

# graficos de contorno
par(mfrow = c(2,2))
contour(res.fat2k, ~ A + B, image = TRUE)

# graficos de superfície
persp(res.fat2k, ~ A + B, zlab = "y", col = rainbow(50), contours = ("colors"))



