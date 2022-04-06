# Fatorial 2^2 com duas replicas

##########################################
############## PLANEJAMENTO ##############
##########################################

# planejamento
design1 = FrF2(nruns = 4,
               nfactors = 2,
               factor.names = c("A","B"),
               randomize = FALSE)

summary(design1)

# resposta
resposta1 = c(20,40,30,52)

# adicionando resposta ao planejamento
design1$y = resposta1

###########################################
################# EFEITOS #################
###########################################

# Calculando efeitos
X = model.matrix( ~ A*B, data = design1[,-3])
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
lm.completo = lm(y ~ A*B, data = design1)
summary(lm.completo)

# modelo retirando a interação A*B
lm.reduzido = lm(y ~ A+B, data = design1)
summary(lm4)

# análise de variância
anova = aov(lm.reduzido)
summary(anova)

# validação da normalidade dos dados
shapiro.test(lm.reduzido$residuals)
qqnorm(lm.reduzido$residuals)
qqline(lm.reduzido$residuals)

##########################################
################ GRAFICOS ################
##########################################

# gráfico de efeitos 
MEPlot(lm.reduzido)
IAPlot(lm.reduzido) # nesse caso nao possui efeitos das interações
IAPlot(lm.completo)

# gráfico de cubo (observar ponto ótimo)
cubePlot(lm.reduzido, eff1 = "A", eff2 = "B", main = "")

##########################################
########## SUPERCIE DE RESPOSTA ##########
##########################################

# carregando biblioteca
library(rsm)

# planejamento
design2 = expand.grid(A = c(-1,1),
                      B = c(-1,1))

design2 = rbind(design2)

# resposta
resposta2 = c(20,40,30,52)

# adicionando resposta ao planejamento
design2$y = resposta2

# analise
res.fat2k = rsm(y ~ SO(A,B), data = design2)
summary(res.fat2k)

# graficos de contorno
par(mfrow = c(2,2))
contour(res.fat2k, ~ A + B, image = TRUE)

# graficos de superfície
persp(res.fat2k, ~ A + B, zlab = "y", col = rainbow(50), contours = ("colors"))

##################################################################

# editando coeficientes, acrescentando interacao AB com valor 8
res.fat2k$coefficients = c(35.5,10.5,5.5,8,NA,NA)
summary(res.fat2k)

# graficos de contorno com interacao sifnificativa
par(mfrow = c(2,2))
contour(res.fat2k, ~ A + B, image = TRUE)

# graficos de superfície com interacao sifnificativa
persp(res.fat2k, ~ A + B, zlab = "y", col = rainbow(50), contours = ("colors"))
