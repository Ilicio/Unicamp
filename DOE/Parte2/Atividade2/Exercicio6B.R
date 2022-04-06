# Fatorial 2^3 com duas replicas

# Fatores:
# A - Aperture        [Small and large]
# B - Exposure time   [-20% and +20%]
# C - Develop time    [30 sec and 40 sec]
# D - Mask dimension  [Small and Large]
# E - Etch time       [14.5 min and 15.5 min]

library(FrF2)

##########################################
############## PLANEJAMENTO ##############
##########################################

# planejamento
design1 = FrF2(nruns = 16,
               nfactors = 3,
               factor.names = c("A","B","C"),
               randomize = F)

summary(design1)

# resposta
resposta1 = c(8,9,34,52,16,22,45,60,6,10,30,50,15,21,44,63)

# adicionando resposta ao planejamento
design1$y = resposta1

###########################################
################# EFEITOS #################
###########################################

# Calculando efeitos
X = model.matrix( ~ A*B*C, data = design1[,-4:-5])
X

N = dim(X)[1]

efeitos = crossprod(X,resposta1)/(N/2)
efeitos

############################################
############ EFEITOS - OPCIONAL ############
############################################

# trabalhando com pacote unrepx para fatorial não replicado
# pacote de autoria do professor Russel V. Lenth

install.packages("unrepx", dependencies = TRUE)
library(unrepx)

# efeitos
efeitos1 = yates(resposta1)
efeitos1

# half normal plot
hnplot(efeitos1,half = TRUE, method = "Lenth", ID = ME(efeitos1))

# pareto PSE plot (variáveis abaixo do ME não são significativas)
parplot(efeitos1, method = "Lenth")

# análise da significância dos efeitos
# teste t via pseudo erro padrão de Lenth
testesig = eff.test(efeitos1,method = "Lenth")
testesig

###########################################
################# ANALISE #################
###########################################

# modelo de regressão completo (nao conseguimos ver a significancia)
lm.completo = lm(y ~ A*B*C, data = design1)
summary(lm.completo)

# reduzindo modelo
lm.reduzido = step(lm.completo, direction = "backward", trace = FALSE)
summary(lm.reduzido)

# análise de variância
anova = aov(lm.completo)
summary(anova)

# validação da normalidade dos dados
shapiro.test(lm.completo$residuals)
qqnorm(lm.completo$residuals)
qqline(lm.completo$residuals)

##########################################
################ GRAFICOS ################
##########################################

# gráfico de efeitos
MEPlot(lm.completo)
IAPlot(lm.completo)

# gráfico de cubo (observar ponto ótimo)
cubePlot(lm.completo, eff1 = "A", eff2 = "B", eff3 = "C", main = "")

##########################################
########## SUPERCIE DE RESPOSTA ##########
##########################################

# planejamento
design2 = expand.grid(A = c(-1,1),
                      B = c(-1,1),
                      C = c(-1,1))

design2 = rbind(design2,design2)

# resposta
resposta2 = c(8,9,34,52,16,22,45,60,6,10,30,50,15,21,44,63)

# adicionando resposta ao planejamento
design2$y = resposta2

# pacote rsm
library(rsm)

# analise
res.fat2k = rsm(y ~ SO(A,B,C), data = design2)
summary(res.fat2k)

# graficos de contorno
par(mfrow = c(2,2))
contour(res.fat2k, ~A + B, image = TRUE)

# graficos de superfície
persp(res.fat2k, ~ A + B, zlab = "y", col = rainbow(50), contours = ("colors"))

