# Fatorial 2^4 nao replicado

# Fatores:
# A - colher
# B - recipiente
# C - operador
# D - cor da colher

# resposta: RGB (quantidade por cores dos chocolates M&M)

##########################################
############## PLANEJAMENTO ##############
##########################################

# planejamento

library(FrF2)

design1 = FrF2(nruns = 16,
               nfactors = 4,
               factor.names = c("A","B","C","D"),
               randomize = F)

summary(design1)

# resposta - taxa de filtração em (gal/h)
resposta1 = c(5,12,24,61,10,19,28,61,7,11,34,54,8,14,28,61)

# adicionando resposta ao planejamento
design1$y = resposta1

###########################################
################# EFEITOS #################
###########################################

# Calculando efeitos
X = model.matrix(~A*B*C*D, data = design1[,-5])
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
efeitos2 = yates(resposta1)
efeitos2

# half normal plot
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
lm1.reduzido = step(lm1, direction = "backward", trace = FALSE)
summary(lm1.reduzido)

# análise de variância
anova = aov(lm1.reduzido)
summary(anova)

# validação da normalidade dos dados
shapiro.test(lm4$residuals)
qqnorm(lm4$residuals)
qqline(lm4$residuals)

##########################################
################ GRAFICOS ################
##########################################

# gráfico de efeitos
MEPlot(lm1.reduzido)
IAPlot(lm1.reduzido)

# gráfico de cubo (observar ponto ótimo)
cubePlot(lm1, eff1 = "A", eff2 = "B", eff3 = "C", main = "")

##########################################
########## SUPERCIE DE RESPOSTA ##########
##########################################

# planejamento
design2 = expand.grid(A = c(-1,1),
                      B = c(-1,1),
                      C = c(-1,1),
                      D = c(-1,1))

# resposta - taxa de filtração em (gal/h)
resposta2 = c(5,12,24,61,10,19,28,61,7,11,34,54,8,14,28,61)

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

# graficos de superfície
# theta (-145) e phi (35) muda o ponto de vista em graus
# shade muda brilho
persp(res.fat2k, ~ A + B, zlab = "y", col = rainbow(50), contours = ("colors"))

