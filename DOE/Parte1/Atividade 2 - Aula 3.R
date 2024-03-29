#####################################
#####################################
####### ATIVIDADE 2 - TESTE T #######
#####################################
#####################################

# Cria��o dos vetores
M1 = c(8.1,8.3,8.0,8.1,8.1,8.2)
M2 = c(8.4,8.4,8.3,8.4,8.3,8.4)
M3 = c(8.8,8.9,8.8,8.8,8.8,8.8)
M4 = c(8.3,8.2,8.2,8.3,8.3,8.3)

##############################
###### MAQUINAS M1 ~ M2 ######
##############################

# Cria��o do dataframe
M1M2 = data.frame(M1,M2)

# Empilhamento dos dados
M1M2.stack = stack(M1M2)

# Renomea��o das colunas
colnames(M1M2.stack) = c("valores","grupos")

# Configura��o de graficos em 1 linha e duas colunas
par(mfrow=c(1,2))

# Cria��o do gr�fico de dispers�o
qqnorm(M1)
qqline(M1)
qqnorm(M2)
qqline(M2)

# Cria��o dos histogramas
hist(M1, col = "green", freq = F, xlim = c(7.9,8.4),ylim = c(0,8))
lines(density(M1))
hist(M2, col = "green", freq = F, xlim = c(8.25,8.45),ylim = c(0,35))
lines(density(M2))

# Cria��o do Gr�fico de boxplot
boxplot(M1,M2, col = "green")

# Verifica��o de normalidade - Teste de Shapiro-wilk
shapiro.test(M1)
shapiro.test(M2)

# Verifica��o de homocedasticidade - Teste de Bartlett
bartlett.test(M1M2)

# Teste T de M1 em fun��o de M2
t.test(valores ~ grupos, data=M1M2.stack, mu = 0, var.equal = TRUE, conf.level = 0.95, alternative = c("two.sided"))

##############################
###### MAQUINAS M1 ~ M3 ######
##############################

# Cria��o do dataframe
M1M3 = data.frame(M1,M3)

# Empilhamento dos dados e renomea��o das colunas
M1M3.stack = stack(M1M3)
colnames(M1M3.stack) = c("valores","grupos")

# Configura��o de graficos em 1 linha e duas colunas
par(mfrow=c(1,2))

# Cria��o do gr�fico de dispers�o
qqnorm(M1)
qqline(M1)
qqnorm(M3)
qqline(M3)

# Cria��o dos histogramas
hist(M1, col = "green", freq = F, xlim = c(7.9,8.4),ylim = c(0,8))
lines(density(M1))
hist(M3, col = "green", freq = F, xlim = c(8.75,8.95),ylim = c(0,40))
lines(density(M3))

# Cria��o do Gr�fico de boxplot
boxplot(M1,M3, col = "green")

# Verifica��o de normalidade - Teste de Shapiro-wilk
shapiro.test(M1)
shapiro.test(M3)

# Verifica��o de homocedasticidade - Teste de Bartlett
bartlett.test(M1M3)

# Teste T de M1 em fun��o de M3
t.test(valores ~ grupos, data=M1M3.stack, mu = 0, var.equal = TRUE, conf.level = 0.95, alternative = c("two.sided"))

##############################
###### MAQUINAS M1 ~ M4 ######
##############################

# Cria��o do dataframe
M1M4 = data.frame(M1,M4)

# Empilhamento dos dados
M1M4.stack = stack(M1M4)

# Renomea��o das colunas
colnames(M1M4.stack) = c("valores","grupos")

# Configura��o de graficos em 1 linha e duas colunas
par(mfrow=c(1,2))

# Cria��o do gr�fico de dispers�o
qqnorm(M1)
qqline(M1)
qqnorm(M4)
qqline(M4)

# Cria��o dos histogramas
hist(M1, col = "green", freq = F, xlim = c(7.9,8.4),ylim = c(0,8))
lines(density(M1))
hist(M4, col = "green", freq = F, xlim = c(8.15,8.35),ylim = c(0,40))
lines(density(M4))

# Cria��o do Gr�fico de boxplot
boxplot(M1,M4, col = "green")

# Verifica��o de normalidade - Teste de Shapiro-wilk
shapiro.test(M1)
shapiro.test(M4)

# Verifica��o de homocedasticidade - Teste de Bartlett
bartlett.test(M1M4)

# Teste T de M1 em fun��o de M4
t.test(valores ~ grupos, data=M1M4.stack, mu = 0, var.equal = TRUE, conf.level = 0.95, alternative = c("two.sided"))

##############################
###### MAQUINAS M2 ~ M3 ######
##############################

# Cria��o do dataframe
M2M3 = data.frame(M2,M3)

# Empilhamento dos dados e renomea��o das colunas
M2M3.stack = stack(M2M3)
colnames(M2M3.stack) = c("valores","grupos")

# Configura��o de graficos em 1 linha e duas colunas
par(mfrow=c(1,2))

# Cria��o do gr�fico de dispers�o
qqnorm(M2)
qqline(M2)
qqnorm(M3)
qqline(M3)

# Cria��o dos histogramas
hist(M2, col = "green", freq = F, xlim = c(8.25,8.45),ylim = c(0,35))
lines(density(M2))
hist(M3, col = "green", freq = F, xlim = c(8.75,8.95),ylim = c(0,40))
lines(density(M3))

# Cria��o do Gr�fico de boxplot
boxplot(M2,M3, col = "green")

# Verifica��o de normalidade - Teste de Shapiro-wilk
shapiro.test(M2)
shapiro.test(M3)

# Verifica��o de homocedasticidade - Teste de Bartlett
bartlett.test(M2M3)

# Teste T de M2 em fun��o de M3
t.test(valores ~ grupos, data=M2M3.stack, mu = 0, var.equal = TRUE, conf.level = 0.95, alternative = c("two.sided"))

##############################
###### MAQUINAS M2 ~ M4 ######
##############################

# Cria��o do dataframe
M2M4 = data.frame(M2,M4)

# Empilhamento dos dados e renomea��o das colunas
M2M4.stack = stack(M2M4)
colnames(M2M4.stack) = c("valores","grupos")

# Configura��o de graficos em 1 linha e duas colunas
par(mfrow=c(1,2))

# Cria��o do gr�fico de dispers�o
qqnorm(M2)
qqline(M2)
qqnorm(M4)
qqline(M4)

# Cria��o dos histogramas
hist(M2, col = "green", freq = F, xlim = c(8.25,8.45),ylim = c(0,35))
lines(density(M2))
hist(M4, col = "green", freq = F, xlim = c(8.15,8.35),ylim = c(0,40))
lines(density(M4))

# Cria��o do Gr�fico de boxplot
boxplot(M2,M4, col = "green")

# Verifica��o de normalidade - Teste de Shapiro-wilk
shapiro.test(M2)
shapiro.test(M4)

# Verifica��o de homocedasticidade - Teste de Bartlett
bartlett.test(M2M4)

# Teste T de M2 em fun��o de M4
t.test(valores ~ grupos, data=M2M4.stack, mu = 0, var.equal = TRUE, conf.level = 0.95, alternative = c("two.sided"))

##############################
###### MAQUINAS M3 ~ M4 ######
##############################

# Cria��o do dataframe
M3M4 = data.frame(M3,M4)

# Empilhamento dos dados e renomea��o das colunas
M3M4.stack = stack(M3M4)
colnames(M3M4.stack) = c("valores","grupos")

# Configura��o de graficos em 1 linha e duas colunas
par(mfrow=c(1,2))

# Cria��o do gr�fico de dispers�o
qqnorm(M3)
qqline(M3)
qqnorm(M4)
qqline(M4)

# Cria��o dos histogramas
hist(M3, col = "green", freq = F, xlim = c(8.75,8.95),ylim = c(0,40))
lines(density(M3))
hist(M4, col = "green", freq = F, xlim = c(8.15,8.35),ylim = c(0,40))
lines(density(M4))

# Cria��o do Gr�fico de boxplot
boxplot(M3,M4, col = "green")

# Verifica��o de normalidade - Teste de Shapiro-wilk
shapiro.test(M3)
shapiro.test(M4)

# Verifica��o de homocedasticidade - Teste de Bartlett
bartlett.test(M3M4)

# Teste T de M3 em fun��o de M4
t.test(valores ~ grupos, data=M3M4.stack, mu = 0, var.equal = TRUE, conf.level = 0.95, alternative = c("two.sided"))

####################################
####### RESULTADOS AGREGADOS #######
####################################

# Cria��o de objetos para armazenar cada teste t
testeT.M1M2 = t.test(valores ~ grupos, data=M1M2.stack, mu = 0, var.equal = TRUE, conf.level = 0.95, alternative = c("two.sided"))
testeT.M1M3 = t.test(valores ~ grupos, data=M1M3.stack, mu = 0, var.equal = TRUE, conf.level = 0.95, alternative = c("two.sided"))
testeT.M1M4 = t.test(valores ~ grupos, data=M1M4.stack, mu = 0, var.equal = TRUE, conf.level = 0.95, alternative = c("two.sided"))
testeT.M2M3 = t.test(valores ~ grupos, data=M2M3.stack, mu = 0, var.equal = TRUE, conf.level = 0.95, alternative = c("two.sided"))
testeT.M2M4 = t.test(valores ~ grupos, data=M2M4.stack, mu = 0, var.equal = TRUE, conf.level = 0.95, alternative = c("two.sided"))
testeT.M3M4 = t.test(valores ~ grupos, data=M3M4.stack, mu = 0, var.equal = TRUE, conf.level = 0.95, alternative = c("two.sided"))

# Cria��o de dataframe com todos os testes t
testeT = data.frame (Valor_P = 
                       c(testeT.M1M2$p.value,testeT.M1M3$p.value,testeT.M1M4$p.value,
                         testeT.M2M3$p.value,testeT.M2M4$p.value,
                         testeT.M3M4$p.value))

# Altera��o dos nomes das linhas
rownames(testeT) = c("M1-M2","M1-M3","M1-M4","M2-M3","M2-M4","M3-M4")

# Cria��o de colunas com diferen�a significativas e hip�tese nula
testeT["Dif_Significativa"] = ifelse(testeT$Valor_P < 0.05,"SIM","N�O")
testeT["Hipotese_Nula"] = ifelse(testeT$Valor_P < 0.05,"Rejeita","N�o Rejeita")

# Observa��o dos resultados
testeT

#########################################
#########################################
####### ATIVIDADE 2 - TESTE TUKEY #######
#########################################
#########################################

# Cria��o dos vetores
M1 = c(8.1,8.3,8.0,8.1,8.1,8.2)
M2 = c(8.4,8.4,8.3,8.4,8.3,8.4)
M3 = c(8.8,8.9,8.8,8.8,8.8,8.8)
M4 = c(8.3,8.2,8.2,8.3,8.3,8.3)

# Cria��o do dataframe
maquinas = data.frame(M1,M2,M3,M4)

# Empilhamento dos dados e renomea��o das colunas
maquinas.stack = stack(maquinas)
colnames(maquinas.stack) = c("valores","grupos")

# Cria��o do modelo para an�lise de vari�ncia (ANOVA)
modelo = aov(maquinas.stack$valores ~ maquinas.stack$grupos)

# Demonstra��o da tabela ANOVA
summary(modelo)

# Verifica��o de normalidade - Teste de Shapiro-wilk
shapiro.test(M1)
shapiro.test(M2)
shapiro.test(M3)
shapiro.test(M4)

# Verifica��o de homocedasticidade - Teste de Bartlett
bartlett.test(valores ~ grupos, data = maquinas.stack)

# Cria��o do gr�fico de caixa
boxplot(maquinas$M1,maquinas$M2,maquinas$M3,maquinas$M4, col = "green", main = "Gr�fico de caixa",names = c("M1","M2","M3","M4"),ylab = "Tempo",xlab = "M�quinas")

# Teste de Tukey
testeTukey = TukeyHSD(modelo)
testeTukey

# Gr�fico para identidica��o das diferen�as entre as m�dias
plot(testeTukey)
