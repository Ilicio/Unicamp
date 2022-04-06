# Fatorial 2^4 nao replicado

# Fatores:
# A - temperatura
# B - pressão
# C - concentração de formaldeído
# D - velocidade de agitação

# resposta: taxa de filtração do produto em (gal/h)

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

# resposta
resposta1 = c(45,71,48,65,68,60,80,65,43,100,45,104,75,86,70,96)

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
lm3 = lm(y ~ A+C+D + A*C + A*D, data = design1)
summary(lm3)

# análise de variância
anova = aov(lm3)
summary(anova)

# validação da normalidade dos dados
shapiro.test(lm3$residuals)
par(mfrow = c(1,1))
qqnorm(lm3$residuals)
qqline(lm3$residuals)
hist(lm3$residuals)

# validação da homocedasticidade dos dados
fligner.test(y ~ A, data = design1)
fligner.test(y ~ C, data = design1)
fligner.test(y ~ D, data = design1)

##########################################
################ GRAFICOS ################
##########################################

# gráfico de efeitos
MEPlot(lm1)
IAPlot(lm1)

# gráfico de cubo (observar ponto ótimo)
par(mfrow = c(1,1))
cubePlot(lm1, eff1 = "A", eff2 = "C", eff3 = "D", main = "")

##########################################
########## GRAFICOS - OPCIONAL ###########
##########################################

# gráfico de efeitos - ggpubr
library(ggpubr)
p1 = ggline(data = design1,
            x = "A", y = "y",
            add = c("mean_se","jitter"),
            color = "blue") + theme_bw()
p1

p2 = ggline(data = design1,
            x = "C", y = "y",
            add = c("mean_se","jitter"),
            color = "green2") + theme_bw()
p2

p3 = ggline(data = design1,
            x = "D", y = "y",
            add = c("mean_se","jitter"),
            color = "red") + theme_bw()
p3

p13 = ggline(data = design1,
            x = "A", y = "y",
            add = c("mean_se","jitter"),
            color = "C") + theme_bw()
p13

p14 = ggline(data = design1,
             x = "A", y = "y",
             add = c("mean_se","jitter"),
             color = "D") + theme_bw()
p14

##########################################
########## SUPERCIE DE RESPOSTA ##########
##########################################

# planejamento
design2 = expand.grid(A = c(-1,1),
                     B = c(-1,1),
                     C = c(-1,1),
                     D = c(-1,1))

# resposta
resposta2 = c(45,71,48,65,68,60,80,65,43,100,45,104,75,86,70,96)

# adicionando resposta ao planejamento
design2$y = resposta2

# pacote rsm
library(rsm)

# analise
res.fat2k = rsm(y ~ SO(A,B,C,D), data = design2)
summary(res.fat2k)

# graficos de contorno
par(mfrow = c(2,2))
contour(res.fat2k, ~ A + C, image = TRUE)
contour(res.fat2k, ~ A + D, image = TRUE)

# graficos de superfície
persp(res.fat2k, ~ A + C, zlab = "y", col = rainbow(50), contours = ("colors"))
persp(res.fat2k, ~ A + D, zlab = "y", col = rainbow(50), contours = ("colors"))

