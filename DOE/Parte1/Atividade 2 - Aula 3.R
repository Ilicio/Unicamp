#####################################
#####################################
####### ATIVIDADE 2 - TESTE T #######
#####################################
#####################################

# Criação dos vetores
M1 = c(8.1,8.3,8.0,8.1,8.1,8.2)
M2 = c(8.4,8.4,8.3,8.4,8.3,8.4)
M3 = c(8.8,8.9,8.8,8.8,8.8,8.8)
M4 = c(8.3,8.2,8.2,8.3,8.3,8.3)

##############################
###### MAQUINAS M1 ~ M2 ######
##############################

# Criação do dataframe
M1M2 = data.frame(M1,M2)

# Empilhamento dos dados
M1M2.stack = stack(M1M2)

# Renomeação das colunas
colnames(M1M2.stack) = c("valores","grupos")

# Configuração de graficos em 1 linha e duas colunas
par(mfrow=c(1,2))

# Criação do gráfico de dispersão
qqnorm(M1)
qqline(M1)
qqnorm(M2)
qqline(M2)

# Criação dos histogramas
hist(M1, col = "green", freq = F, xlim = c(7.9,8.4),ylim = c(0,8))
lines(density(M1))
hist(M2, col = "green", freq = F, xlim = c(8.25,8.45),ylim = c(0,35))
lines(density(M2))

# Criação do Gráfico de boxplot
boxplot(M1,M2, col = "green")

# Verificação de normalidade - Teste de Shapiro-wilk
shapiro.test(M1)
shapiro.test(M2)

# Verificação de homocedasticidade - Teste de Bartlett
bartlett.test(M1M2)

# Teste T de M1 em função de M2
t.test(valores ~ grupos, data=M1M2.stack, mu = 0, var.equal = TRUE, conf.level = 0.95, alternative = c("two.sided"))

##############################
###### MAQUINAS M1 ~ M3 ######
##############################

# Criação do dataframe
M1M3 = data.frame(M1,M3)

# Empilhamento dos dados e renomeação das colunas
M1M3.stack = stack(M1M3)
colnames(M1M3.stack) = c("valores","grupos")

# Configuração de graficos em 1 linha e duas colunas
par(mfrow=c(1,2))

# Criação do gráfico de dispersão
qqnorm(M1)
qqline(M1)
qqnorm(M3)
qqline(M3)

# Criação dos histogramas
hist(M1, col = "green", freq = F, xlim = c(7.9,8.4),ylim = c(0,8))
lines(density(M1))
hist(M3, col = "green", freq = F, xlim = c(8.75,8.95),ylim = c(0,40))
lines(density(M3))

# Criação do Gráfico de boxplot
boxplot(M1,M3, col = "green")

# Verificação de normalidade - Teste de Shapiro-wilk
shapiro.test(M1)
shapiro.test(M3)

# Verificação de homocedasticidade - Teste de Bartlett
bartlett.test(M1M3)

# Teste T de M1 em função de M3
t.test(valores ~ grupos, data=M1M3.stack, mu = 0, var.equal = TRUE, conf.level = 0.95, alternative = c("two.sided"))

##############################
###### MAQUINAS M1 ~ M4 ######
##############################

# Criação do dataframe
M1M4 = data.frame(M1,M4)

# Empilhamento dos dados
M1M4.stack = stack(M1M4)

# Renomeação das colunas
colnames(M1M4.stack) = c("valores","grupos")

# Configuração de graficos em 1 linha e duas colunas
par(mfrow=c(1,2))

# Criação do gráfico de dispersão
qqnorm(M1)
qqline(M1)
qqnorm(M4)
qqline(M4)

# Criação dos histogramas
hist(M1, col = "green", freq = F, xlim = c(7.9,8.4),ylim = c(0,8))
lines(density(M1))
hist(M4, col = "green", freq = F, xlim = c(8.15,8.35),ylim = c(0,40))
lines(density(M4))

# Criação do Gráfico de boxplot
boxplot(M1,M4, col = "green")

# Verificação de normalidade - Teste de Shapiro-wilk
shapiro.test(M1)
shapiro.test(M4)

# Verificação de homocedasticidade - Teste de Bartlett
bartlett.test(M1M4)

# Teste T de M1 em função de M4
t.test(valores ~ grupos, data=M1M4.stack, mu = 0, var.equal = TRUE, conf.level = 0.95, alternative = c("two.sided"))

##############################
###### MAQUINAS M2 ~ M3 ######
##############################

# Criação do dataframe
M2M3 = data.frame(M2,M3)

# Empilhamento dos dados e renomeação das colunas
M2M3.stack = stack(M2M3)
colnames(M2M3.stack) = c("valores","grupos")

# Configuração de graficos em 1 linha e duas colunas
par(mfrow=c(1,2))

# Criação do gráfico de dispersão
qqnorm(M2)
qqline(M2)
qqnorm(M3)
qqline(M3)

# Criação dos histogramas
hist(M2, col = "green", freq = F, xlim = c(8.25,8.45),ylim = c(0,35))
lines(density(M2))
hist(M3, col = "green", freq = F, xlim = c(8.75,8.95),ylim = c(0,40))
lines(density(M3))

# Criação do Gráfico de boxplot
boxplot(M2,M3, col = "green")

# Verificação de normalidade - Teste de Shapiro-wilk
shapiro.test(M2)
shapiro.test(M3)

# Verificação de homocedasticidade - Teste de Bartlett
bartlett.test(M2M3)

# Teste T de M2 em função de M3
t.test(valores ~ grupos, data=M2M3.stack, mu = 0, var.equal = TRUE, conf.level = 0.95, alternative = c("two.sided"))

##############################
###### MAQUINAS M2 ~ M4 ######
##############################

# Criação do dataframe
M2M4 = data.frame(M2,M4)

# Empilhamento dos dados e renomeação das colunas
M2M4.stack = stack(M2M4)
colnames(M2M4.stack) = c("valores","grupos")

# Configuração de graficos em 1 linha e duas colunas
par(mfrow=c(1,2))

# Criação do gráfico de dispersão
qqnorm(M2)
qqline(M2)
qqnorm(M4)
qqline(M4)

# Criação dos histogramas
hist(M2, col = "green", freq = F, xlim = c(8.25,8.45),ylim = c(0,35))
lines(density(M2))
hist(M4, col = "green", freq = F, xlim = c(8.15,8.35),ylim = c(0,40))
lines(density(M4))

# Criação do Gráfico de boxplot
boxplot(M2,M4, col = "green")

# Verificação de normalidade - Teste de Shapiro-wilk
shapiro.test(M2)
shapiro.test(M4)

# Verificação de homocedasticidade - Teste de Bartlett
bartlett.test(M2M4)

# Teste T de M2 em função de M4
t.test(valores ~ grupos, data=M2M4.stack, mu = 0, var.equal = TRUE, conf.level = 0.95, alternative = c("two.sided"))

##############################
###### MAQUINAS M3 ~ M4 ######
##############################

# Criação do dataframe
M3M4 = data.frame(M3,M4)

# Empilhamento dos dados e renomeação das colunas
M3M4.stack = stack(M3M4)
colnames(M3M4.stack) = c("valores","grupos")

# Configuração de graficos em 1 linha e duas colunas
par(mfrow=c(1,2))

# Criação do gráfico de dispersão
qqnorm(M3)
qqline(M3)
qqnorm(M4)
qqline(M4)

# Criação dos histogramas
hist(M3, col = "green", freq = F, xlim = c(8.75,8.95),ylim = c(0,40))
lines(density(M3))
hist(M4, col = "green", freq = F, xlim = c(8.15,8.35),ylim = c(0,40))
lines(density(M4))

# Criação do Gráfico de boxplot
boxplot(M3,M4, col = "green")

# Verificação de normalidade - Teste de Shapiro-wilk
shapiro.test(M3)
shapiro.test(M4)

# Verificação de homocedasticidade - Teste de Bartlett
bartlett.test(M3M4)

# Teste T de M3 em função de M4
t.test(valores ~ grupos, data=M3M4.stack, mu = 0, var.equal = TRUE, conf.level = 0.95, alternative = c("two.sided"))

####################################
####### RESULTADOS AGREGADOS #######
####################################

# Criação de objetos para armazenar cada teste t
testeT.M1M2 = t.test(valores ~ grupos, data=M1M2.stack, mu = 0, var.equal = TRUE, conf.level = 0.95, alternative = c("two.sided"))
testeT.M1M3 = t.test(valores ~ grupos, data=M1M3.stack, mu = 0, var.equal = TRUE, conf.level = 0.95, alternative = c("two.sided"))
testeT.M1M4 = t.test(valores ~ grupos, data=M1M4.stack, mu = 0, var.equal = TRUE, conf.level = 0.95, alternative = c("two.sided"))
testeT.M2M3 = t.test(valores ~ grupos, data=M2M3.stack, mu = 0, var.equal = TRUE, conf.level = 0.95, alternative = c("two.sided"))
testeT.M2M4 = t.test(valores ~ grupos, data=M2M4.stack, mu = 0, var.equal = TRUE, conf.level = 0.95, alternative = c("two.sided"))
testeT.M3M4 = t.test(valores ~ grupos, data=M3M4.stack, mu = 0, var.equal = TRUE, conf.level = 0.95, alternative = c("two.sided"))

# Criação de dataframe com todos os testes t
testeT = data.frame (Valor_P = 
                       c(testeT.M1M2$p.value,testeT.M1M3$p.value,testeT.M1M4$p.value,
                         testeT.M2M3$p.value,testeT.M2M4$p.value,
                         testeT.M3M4$p.value))

# Alteração dos nomes das linhas
rownames(testeT) = c("M1-M2","M1-M3","M1-M4","M2-M3","M2-M4","M3-M4")

# Criação de colunas com diferença significativas e hipótese nula
testeT["Dif_Significativa"] = ifelse(testeT$Valor_P < 0.05,"SIM","NÃO")
testeT["Hipotese_Nula"] = ifelse(testeT$Valor_P < 0.05,"Rejeita","Não Rejeita")

# Observação dos resultados
testeT

#########################################
#########################################
####### ATIVIDADE 2 - TESTE TUKEY #######
#########################################
#########################################

# Criação dos vetores
M1 = c(8.1,8.3,8.0,8.1,8.1,8.2)
M2 = c(8.4,8.4,8.3,8.4,8.3,8.4)
M3 = c(8.8,8.9,8.8,8.8,8.8,8.8)
M4 = c(8.3,8.2,8.2,8.3,8.3,8.3)

# Criação do dataframe
maquinas = data.frame(M1,M2,M3,M4)

# Empilhamento dos dados e renomeação das colunas
maquinas.stack = stack(maquinas)
colnames(maquinas.stack) = c("valores","grupos")

# Criação do modelo para análise de variância (ANOVA)
modelo = aov(maquinas.stack$valores ~ maquinas.stack$grupos)

# Demonstração da tabela ANOVA
summary(modelo)

# Verificação de normalidade - Teste de Shapiro-wilk
shapiro.test(M1)
shapiro.test(M2)
shapiro.test(M3)
shapiro.test(M4)

# Verificação de homocedasticidade - Teste de Bartlett
bartlett.test(valores ~ grupos, data = maquinas.stack)

# Criação do gráfico de caixa
boxplot(maquinas$M1,maquinas$M2,maquinas$M3,maquinas$M4, col = "green", main = "Gráfico de caixa",names = c("M1","M2","M3","M4"),ylab = "Tempo",xlab = "Máquinas")

# Teste de Tukey
testeTukey = TukeyHSD(modelo)
testeTukey

# Gráfico para identidicação das diferenças entre as médias
plot(testeTukey)
