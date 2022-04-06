# Fatorial fracionário

# Fatores:
# tA - temperatura (A)
# pB - pressão (B)
# cC - concentração de formaldeído (C)
# vD - velocidade de agitação (D)

# resposta: taxa de filtração do produto

# Carregando biblioteca necessária
library(FrF2)

##########################################
############## PLANEJAMENTO ##############
##########################################

# planejamento de meia fração - 2^(4-1) 
design1 = FrF2(nruns = 8,
            nfactors = 4,
            factor.names = c("tA","pB","cC","vD"),
            randomize = FALSE,
            alias.info = 3)

summary(design1)

# resposta - taxa de filtração em (L/h)
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

# modelo sem interações tA*pB, pB*cC, pB*vD (maneira manual hierarquica)
lm3 = lm(y ~ tA+cC+vD + tA*cC + tA*vD + cC*vD, data = design1)
summary(lm3)

# modelo retirando mais uma interação cC*vD
lm4 = lm(y ~ tA+cC+vD + tA*cC + tA*vD, data = design1)
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
cubePlot(lm2, eff1 = "tA", eff2 = "cC", eff3 = "vD", main = "")


