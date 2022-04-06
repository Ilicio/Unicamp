# Fatorial 2^4 nao replicado

# Fatores:
# A - temperatura
# B - tempo
# C - pressão
# D - fluxo de gás

# resposta: espessura do óxido

library(FrF2)

##########################################
############## PLANEJAMENTO ##############
##########################################

# planejamento
design1 = FrF2(nruns = 16,
               nfactors = 4,
               factor.names = c("A","B","C","D"),
               randomize = F)

summary(design1)

# resposta
resposta1 = c(378,416,381,448,372,390,385,430,380,415,371,446,378,392,376,429)

# adicionando resposta ao planejamento
design1$y = resposta1

###########################################
################# EFEITOS #################
###########################################

# Calculando efeitos
X = model.matrix( ~ A*B*C*D, data = design1[,-5])
X

N = dim(X)[1]

efeitos = crossprod(X,resposta1)/(N/2)
efeitos

############################################
############ EFEITOS - OPCIONAL ############
############################################

# trabalhando com pacote unrepx para fatorial não replicado
# pacote de autoria do professor Russel V. Lenth

library(unrepx)

# efeitos
efeitos2 = yates(resposta1)
efeitos2

# half normal plot
par(mfrow = c(1,2))
hnplot(efeitos2,half = TRUE, method = "Lenth", ID = ME(efeitos2))

# pareto PSE plot (variáveis abaixo do ME não são significativas)
parplot(efeitos2, method = "Lenth")

# análise da significância dos efeitos
# teste t via pseudo erro padrão de Lenth
testesig = eff.test(efeitos2,method = "Lenth")
testesig

###########################################
################# ANALISE #################
###########################################

# modelo de regressão completo (nao conseguimos ver a significancia)
lm.completo = lm(y ~ A*B*C*D, data = design1)
summary(lm.completo)

# terceira ordem
lm1 = lm(y ~ .^3, data = design1)
# ou: lm1 = lm(y ~ (A*B*C*D)^3, data = design1)
summary(lm1)

# reduzindo modelo
lm2 = step(lm1, direction = "backward", trace = FALSE)
summary(lm2)

# reduzindo modelo de forma hierarquica
lm3 = lm(y ~ A+B+C+D + A*B + A*C + A*D + B*C + B*D + C*D, data = design1)
summary(lm3)

# análise de variância
anova = aov(lm1)
summary(anova)

# validação da normalidade dos dados
shapiro.test(lm1$residuals)
qqnorm(lm1$residuals)
qqline(lm1$residuals)

##########################################
################ GRAFICOS ################
##########################################

# gráfico de efeitos
MEPlot(lm1)
IAPlot(lm1)

# gráfico de cubo (observar ponto ótimo)
par(mfrow = c(1,1))
# size (tamanho da bolinha), cex.clab (tamanho da fonte nas bolinhas)
cubePlot(lm1, eff1 = "A", eff2 = "B", eff3 = "C", main = "", size = 0.3, cex.clab = 1.2)

##########################################
########## SUPERCIE DE RESPOSTA ##########
##########################################

# planejamento
design2 = expand.grid(A = c(-1,1),
                      B = c(-1,1),
                      C = c(-1,1),
                      D = c(-1,1))

# resposta
resposta2 = c(378,416,381,448,372,390,385,430,380,415,371,446,378,392,376,429)

# adicionando resposta ao planejamento
design2$y = resposta2

# pacote rsm
library(rsm)

# analise
res.fat2k = rsm(y ~ SO(A,B,C,D), data = design2)
summary(res.fat2k)

# graficos de contorno
par(mfrow = c(2,2))
contour(res.fat2k, ~ A + B, image = TRUE)
contour(res.fat2k, ~ A + C, image = TRUE)

# graficos de superfície
persp(res.fat2k, ~ A + B, zlab = "y", col = rainbow(50), contours = ("colors"))
persp(res.fat2k, ~ A + C, zlab = "y", col = rainbow(50), contours = ("colors"))
