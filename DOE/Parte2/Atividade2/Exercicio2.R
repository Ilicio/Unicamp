# Fatorial 2^2 com duas replicas


# carregar biblioteca
library(FrF2)

##########################################
############## PLANEJAMENTO ##############
##########################################

# planejamento
design1 = FrF2(nruns = 4,
                    nfactors = 2,
                    replications = 3,
                    factor.names = c("C","T"),
                    randomize = FALSE)
                    
                    #factor.names = list(reagente = c(10,20),
                                        #temperatura = c(80,90)))

summary(design1)

# resposta
resposta1 = c(26.6,40.9,11.8,34,22,36.4,15.9,29,22.8,36.7,14.3,33.6)

# adicionando resposta ao planejamento
design1$y = resposta1

summary(design1)

###########################################
################# EFEITOS #################
###########################################

# Calculando efeitos
X = model.matrix( ~ C*T, data = design1[,-3])
X

N = dim(X)[1]

efeitos = crossprod(X,resposta1)/(N/2)
efeitos

############################################
############ EFEITOS - OPCIONAL ############
############################################

# via pseudo erro padrao de Lenth

library(unrepx)

# design matrix
X = model.matrix( ~ C*T, data = design1[,-3:-4])
X

# efeitos
efeitos = crossprod(X,resposta1)/(12/2)
efeitos

# graficos
par(mfrow = c(1,2))
hnplot(efeitos, method = "Lenth", ID = ME(efeitos))
parplot(efeitos, method = "Lenth")

###########################################
################# ANALISE #################
###########################################

# modelo de regressão completo (nao conseguimos ver a significancia)
lm1 = lm(y ~ C*T, data = design1)
summary(lm.completo)

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

# do pacote FrF2
par(mfrow = c(1,2))
MEPlot(lm1)
IAPlot(lm1)

# do pacote ggpubr
library(ggpubr)

# efeitos principais
efeitos_principais1 = ggline(data = planejamento.tempo,
                            x = "reagente",
                            y = "tempo",
                            ylab = "tempo",
                            xlab = "Concentração do reagente",
                            add = c("mean_se", "jitter"),
                            color = "blue",
                            ylim = c(10,45))

efeitos_principais2 = ggline(data = planejamento.tempo,
                             x = "temperatura",
                             y = "tempo",
                             ylab = "tempo",
                             xlab = "Temperatura",
                             add = c("mean_se", "jitter"),
                             color = "red",
                             ylim = c(10,45))

ggarrange(efeitos_principais1, efeitos_principais2)

# efeitos principais - sem detalhamento
efeitos_principais3 = ggline(data = planejamento.tempo,
                             x = "reagente",
                             y = "tempo",
                             ylab = "tempo",
                             xlab = "Concentração do reagente",
                             add = "mean",
                             color = "blue",
                             ylim = c(10,45))

efeitos_principais4 = ggline(data = planejamento.tempo,
                             x = "temperatura",
                             y = "tempo",
                             ylab = "tempo",
                             xlab = "Temperatura",
                             add = "mean",
                             color = "red",
                             ylim = c(10,45))

ggarrange(efeitos_principais3, efeitos_principais4)

# interação
efeitos_interacao = ggline(data = planejamento.tempo,
                           x = "reagente",
                           y = "tempo",
                           color = "temperatura",
                           linetype = "temperatura",
                           shape = "temperatura",
                           palette = c("blue", "red"),
                           ylab = "tempo",
                           xlab = "concentração do reagente",
                           add = c("mean_se", "jitter"))

ggarrange(efeitos_principais1, efeitos_principais2, efeitos_interacao,
          ncol = 2,
          nrow = 2)

##########################################
########## SUPERCIE DE RESPOSTA ##########
##########################################

# carregando biblioteca
library(rsm)

# planejamento
design2 = expand.grid(C = c(-1,1),
                      T = c(-1,1))

design2 = rbind(design2,design2,design2)

# resposta
resposta2 = c(26.6,40.9,11.8,34,22,36.4,15.9,29,22.8,36.7,14.3,33.6)

# adicionando resposta ao planejamento
design2$y = resposta2

summary(design1)

# analise
res.fat2k = rsm(y ~ SO(C,T), data = design2)
summary(res.fat2k)

# graficos de contorno
par(mfrow = c(2,2))
contour(res.fat2k, ~ C + T, image = TRUE)

# graficos de superfície
# theta (-145) e phi (35) muda o ponto de vista em graus
# shade muda brilho
persp(res.fat2k, ~ C + T, zlab = "y", col = rainbow(50), contours = ("colors"),
      theta = -145)


