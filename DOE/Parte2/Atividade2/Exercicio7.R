# Fatorial 2^k com pontos centrais

# Fatores:
# tA - temperatura (A)
# pB - press�o (B)
# cC - concentra��o de formalde�do (C)
# vD - velocidade de agita��o (D)

# resposta: taxa de filtra��o do produto

##########################################
############## PLANEJAMENTO ##############
##########################################

# planejamento

library(FrF2)

design1 = FrF2(nruns = 16,
               nfactors = 4,
               ncenter = 4,
               factor.names = c("tA","pB","cC","vD"),
               randomize = F)

summary(design1)

# resposta - taxa de filtra��o em (gal/h)
filtracao = c(45,71,48,65,68,60,80,65,43,100,45,104,75,86,70,96,73,75,66,69)

# adicionando resposta ao planejamento
design1$y = filtracao

summary(design1)

###########################################
################# ANALISE #################
###########################################

# o termo para avaliar curvatura liscube(plan.ctpt)
lm1 = lm(y ~ tA*pB + tA*cC + tA*vD + pB*cC + pB*vD + cC*vD + !iscube(design1), data = design1)
summary(lm1)

# valida��o da normalidade dos dados
shapiro.test(lm1$residuals)
qqnorm(lm1$residuals)
qqline(lm1$residuals)

#################################################
# Identificando se h� curvatura de outra forma

# adicionando coluna com pontos centrais ao planejamento
design1$curvatura = c(rep(0,16), rep(1,4))

lm2 = lm(y ~ tA*pB + tA*cC + tA*vD + pB*cC + pB*vD + cC*vD + curvatura, data = design1)
summary(lm2)

anova = aov(lm2)
summary(anova)

# valida��o da normalidade dos dados
shapiro.test(lm1$residuals)
qqnorm(lm2$residuals)
qqline(lm2$residuals)


############################################
################# GR�FICOS #################
############################################

# carregando biblioteca
library(ggpubr)

# tabela de cores caso precise
#get_palette(palette = "default", 100)

# criando gr�ficos para cada vari�vel significativa A,C e D
p1 = ggline(design1,
            x = "tA",
            y = "y",
            add = c("mean"),
            color = "blue") + theme_bw()
p3 = ggline(design1,
            x = "cC",
            y = "y",
            add = c("mean"),
            color = "green3") + theme_bw()
p4 = ggline(design1,
            x = "vD",
            y = "y",
            add = c("mean"),
            color = "red") + theme_bw()

# transformando em fatorial para melhorar o gr�fico de intera��es
design1.plot = design1
design1.plot$tA = as.factor(design1$tA)
design1.plot$cC = as.factor(design1$cC)
design1.plot$vD = as.factor(design1$vD)

# verificando se dados est�o mesmo em fatorial
library(dplyr)
glimpse(design1.plot)

# criando gr�ficos para cada intera��o significativa AC e AD
p13 = ggline(design1.plot,
            x = "tA",
            y = "y",
            add = c("mean", "point"),
            color = "cC") + theme_bw()
p14 = ggline(design1.plot,
             x = "tA",
             y = "y",
             add = c("mean", "point"),
             color = "vD") + theme_bw()

# plotando gr�ficos em conjunto
ggarrange(p1,p3,p4,p13,p14)

