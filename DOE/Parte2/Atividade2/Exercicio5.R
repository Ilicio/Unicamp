# Fatorial fracion�rio

# Fatores:
# tA - temperatura (A)
# pB - press�o (B)
# cC - concentra��o de formalde�do (C)
# vD - velocidade de agita��o (D)

# resposta: taxa de filtra��o do produto

# Carregando biblioteca necess�ria
library(FrF2)

##########################################
############## PLANEJAMENTO ##############
##########################################

# planejamento de meia fra��o - 2^(4-1) 
design1 = FrF2(nruns = 8,
            nfactors = 4,
            factor.names = c("tA","pB","cC","vD"),
            randomize = FALSE,
            alias.info = 3)

summary(design1)

# resposta - taxa de filtra��o em (L/h)
filtracao = c(45,100,45,65,75,60,80,96)

# adicionando resposta ao planejamento
design1$y = filtracao

# modelo "completo"
lm1 = lm(y ~ .^4, data = design1)
summary(lm1)

# estrutura de confundimento do modelo lm1
aliases(lm1)

#modelo de segunda ordem (para ver um modelo sem confundimento)
lm2 = lm(y ~ .^2, data = design1)
summary(lm2)

# confundimento de lm2
aliases(lm2)

############################################
############ EFEITOS - OPCIONAL ############
############################################

# via pseudo erro padrao de Lenth

library(unrepx)

# design matrix
X = model.matrix(~tA*cC*vD, data = design1[,-5])
X

# efeitos
efeitos = crossprod(X,filtracao)/(16/2)
efeitos = efeitos[2:8]
names(efeitos) = c("tA","cC","vD","tAcC","tAvD","cCvD","tAcCvD")
efeitos

# graficos
par(mfrow = c(1,2))
hnplot(efeitos, method = "Lenth", ID = ME(efeitos))
parplot(efeitos, method = "Lenth")

###########################################
################# ANALISE #################
###########################################

# modelo sem intera��es tA*pB, pB*cC, pB*vD (maneira manual hierarquica)
lm3 = lm(y ~ tA+cC+vD + tA*cC + tA*vD + cC*vD, data = design1)
summary(lm3)

# modelo retirando mais uma intera��o cC*vD
lm4 = lm(y ~ tA+cC+vD + tA*cC + tA*vD, data = design1)
summary(lm4)

anova = aov(lm4)
summary(anova)

# valida��o da normalidade dos dados
shapiro.test(lm4$residuals)
qqnorm(lm4$residuals)
qqline(lm4$residuals)

##########################################
################ GRAFICOS ################
##########################################

# gr�ficos de efeitos principais e de intera��o
MEPlot(lm2)
IAPlot(lm2)

# gr�fico de cubo (observar ponto �timo)
cubePlot(lm2, eff1 = "tA", eff2 = "cC", eff3 = "vD", main = "")


