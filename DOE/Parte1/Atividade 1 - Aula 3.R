#####################################
#####################################
####### ATIVIDADE 1 - TESTE T #######
#####################################
#####################################

# Criação dos vetores
A = c(25,17,27,21,15)
B = c(10,-2,12,4,16)
C = c(18,8,4,14,6)
D = c(23,29,25,3,33)
E = c(11,23,5,17,9)
controle = c(8,-6,6,0,2)

##############################
###### TRATAMENTO A ~ B ######
##############################

# Criação do dataframe
AB = data.frame(A,B)

# Empilhamento dos dados e renomeação das colunas
AB.stack = stack(AB)
colnames(AB.stack) = c("valores","grupos")

# Configuração de graficos em 1 linha e duas colunas
par(mfrow=c(1,2))

# Criação do gráfico de dispersão
qqnorm(A)
qqline(A)
qqnorm(B)
qqline(B)

# Criação dos histogramas
hist(A, col = "green", freq = F, xlim = c(5,40),ylim = c(0,0.08))
lines(density(A))
hist(B, col = "green", freq = F, xlim = c(-16,30),ylim = c(0,0.08))
lines(density(B))

# Criação do Gráfico de boxplot
boxplot(A,B, col = "green")

# Verificação de normalidade - Teste de Shapiro-wilk
shapiro.test(A)
shapiro.test(B)

# Verificação de homocedasticidade - Teste de Bartlett
bartlett.test(AB)

# Teste T de A em função de B
t.test(valores ~ grupos, data=AB.stack, mu = 0, var.equal = TRUE, conf.level = 0.95, alternative = c("two.sided"))

##############################
###### TRATAMENTO A ~ C ######
##############################

# Criação do dataframe
AC = data.frame(A,C)

# Empilhamento dos dados e renomeação das colunas
AC.stack = stack(AC)
colnames(AC.stack) = c("valores","grupos")

# Configuração de graficos em 1 linha e duas colunas
par(mfrow=c(1,2))

# Criação do gráfico de dispersão
qqnorm(A)
qqline(A)
qqnorm(C)
qqline(C)

# Criação dos histogramas
hist(A, col = "green", freq = F, xlim = c(5,40),ylim = c(0,0.08))
lines(density(A))
hist(C, col = "green", freq = F, xlim = c(-16,30),ylim = c(0,0.08))
lines(density(C))

# Criação do gráfico de boxplot
boxplot(A,C, col = "green")

# Verificação de normalidade - Teste de Shapiro-wilk
shapiro.test(A)
shapiro.test(C)

# Verificação de homocedasticidade - Teste de Bartlett
bartlett.test(AC)

# Teste T de A em função de C
t.test(valores ~ grupos, data=AC.stack, mu = 0, var.equal = TRUE, conf.level = 0.95, alternative = c("two.sided"))

##############################
###### TRATAMENTO A ~ D ######
##############################

# Criação do dataframe
AD = data.frame(A,D)

# Empilhamento dos dados e renomeação das colunas
AD.stack = stack(AD)
colnames(AD.stack) = c("valores","grupos")

# Configuração de graficos em 1 linha e duas colunas
par(mfrow=c(1,2))

# Criação do gráfico de dispersão
qqnorm(A)
qqline(A)
qqnorm(D)
qqline(D)

# Criação dos histogramas
hist(A, col = "green", freq = F, xlim = c(5,40),ylim = c(0,0.08))
lines(density(A))
hist(D, col = "green", freq = F, xlim = c(-16,30),ylim = c(0,0.08))
lines(density(D))

# Criação do gráfico de boxplot
boxplot(A,D, col = "green")

# Verificação de normalidade - Teste de Shapiro-wilk
shapiro.test(A)
shapiro.test(D)

# Verificação de homocedasticidade - Teste de Bartlett
bartlett.test(AD)

# Teste T de A em função de D
t.test(valores ~ grupos, data=AD.stack, mu = 0, var.equal = TRUE, conf.level = 0.95, alternative = c("two.sided"))

##############################
###### TRATAMENTO A ~ E ######
##############################

# Criação do dataframe
AE = data.frame(A,E)

# Empilhamento dos dados e renomeação das colunas
AE.stack = stack(AE)
colnames(AE.stack) = c("valores","grupos")

# Configuração de graficos em 1 linha e duas colunas
par(mfrow=c(1,2))

# Criação do gráfico de dispersão
qqnorm(A)
qqline(A)
qqnorm(E)
qqline(E)

# Criação dos histogramas
hist(A, col = "green", freq = F, xlim = c(5,40),ylim = c(0,0.08))
lines(density(A))
hist(E, col = "green", freq = F, xlim = c(-16,30),ylim = c(0,0.08))
lines(density(E))

# Criação do gráfico de boxplot
boxplot(A,E, col = "green")

# Verificação de normalidade - Teste de Shapiro-wilk
shapiro.test(A)
shapiro.test(E)

# Verificação de homocedasticidade - Teste de Bartlett
bartlett.test(AE)

# Teste T de A em função de E
t.test(valores ~ grupos, data=AE.stack, mu = 0, var.equal = TRUE, conf.level = 0.95, alternative = c("two.sided"))

#####################################
###### TRATAMENTO A ~ controle ######
#####################################

# Criação do dataframe
Acontrole = data.frame(A,controle)

# Empilhamento dos dados e renomeação das colunas
Acontrole.stack = stack(Acontrole)
colnames(Acontrole.stack) = c("valores","grupos")

# Configuração de graficos em 1 linha e duas colunas
par(mfrow=c(1,2))

# Criação do gráfico de dispersão
qqnorm(A)
qqline(A)
qqnorm(controle)
qqline(controle)

# Criação dos histogramas
hist(A, col = "green", freq = F, xlim = c(5,40),ylim = c(0,0.08))
lines(density(A))
hist(controle, col = "green", freq = F, xlim = c(-16,30),ylim = c(0,0.08))
lines(density(controle))

# Criação do gráfico de boxplot
boxplot(A,controle, col = "green")

# Verificação de normalidade - Teste de Shapiro-wilk
shapiro.test(A)
shapiro.test(controle)

# Verificação de homocedasticidade - Teste de Bartlett
bartlett.test(Acontrole)

# Teste T de A em função de controle
t.test(valores ~ grupos, data=Acontrole.stack, mu = 0, var.equal = TRUE, conf.level = 0.95, alternative = c("two.sided"))

##############################
###### TRATAMENTO B ~ C ######
##############################

# Criação do dataframe
BC = data.frame(B,C)

# Empilhamento dos dados e renomeação das colunas
BC.stack = stack(BC)
colnames(BC.stack) = c("valores","grupos")

# Configuração de graficos em 1 linha e duas colunas
par(mfrow=c(1,2))

# Criação do gráfico de dispersão
qqnorm(B)
qqline(B)
qqnorm(C)
qqline(C)

# Criação dos histogramas
hist(B, col = "green", freq = F, xlim = c(-12,30),ylim = c(0,0.08))
lines(density(B))
hist(C, col = "green", freq = F, xlim = c(-16,30),ylim = c(0,0.08))
lines(density(C))

# Criação do gráfico de boxplot
boxplot(B,C, col = "green")

# Verificação de normalidade - Teste de Shapiro-wilk
shapiro.test(B)
shapiro.test(C)

# Verificação de homocedasticidade - Teste de Bartlett
bartlett.test(BC)

# Teste T de B em função de C
t.test(valores ~ grupos, data=BC.stack, mu = 0, var.equal = TRUE, conf.level = 0.95, alternative = c("two.sided"))

##############################
###### TRATAMENTO B ~ D ######
##############################

# Criação do dataframe
BD = data.frame(B,D)

# Empilhamento dos dados e renomeação das colunas
BD.stack = stack(BD)
colnames(BD.stack) = c("valores","grupos")

# Configuração de graficos em 1 linha e duas colunas
par(mfrow=c(1,2))

# Criação do gráfico de dispersão
qqnorm(B)
qqline(B)
qqnorm(D)
qqline(D)

# Criação dos histogramas
hist(B, col = "green", freq = F, xlim = c(-12,30),ylim = c(0,0.08))
lines(density(B))
hist(D, col = "green", freq = F, xlim = c(-16,30),ylim = c(0,0.08))
lines(density(D))

# Criação do gráfico de boxplot
boxplot(B,D, col = "green")

# Verificação de normalidade - Teste de Shapiro-wilk
shapiro.test(B)
shapiro.test(D)

# Verificação de homocedasticidade - Teste de Bartlett
bartlett.test(BD)

# Teste T de B em função de D
t.test(valores ~ grupos, data=BD.stack, mu = 0, var.equal = TRUE, conf.level = 0.95, alternative = c("two.sided"))

##############################
###### TRATAMENTO B ~ E ######
##############################

# Criação do dataframe
BE = data.frame(B,E)

# Empilhamento dos dados e renomeação das colunas
BE.stack = stack(BE)
colnames(BE.stack) = c("valores","grupos")

# Configuração de graficos em 1 linha e duas colunas
par(mfrow=c(1,2))

# Criação do gráfico de dispersão
qqnorm(B)
qqline(B)
qqnorm(E)
qqline(E)

# Criação dos histogramas
hist(B, col = "green", freq = F, xlim = c(-12,30),ylim = c(0,0.08))
lines(density(B))
hist(E, col = "green", freq = F, xlim = c(-16,30),ylim = c(0,0.08))
lines(density(E))

# Criação do gráfico de boxplot
boxplot(B,E, col = "green")

# Verificação de normalidade - Teste de Shapiro-wilk
shapiro.test(B)
shapiro.test(E)

# Verificação de homocedasticidade - Teste de Bartlett
bartlett.test(BE)

# Teste T de B em função de E
t.test(valores ~ grupos, data=BE.stack, mu = 0, var.equal = TRUE, conf.level = 0.95, alternative = c("two.sided"))

#####################################
###### TRATAMENTO B ~ Controle ######
#####################################

# Criação do dataframe
Bcontrole = data.frame(B,controle)

# Empilhamento dos dados e renomeação das colunas
Bcontrole.stack = stack(Bcontrole)
colnames(Bcontrole.stack) = c("valores","grupos")

# Configuração de graficos em 1 linha e duas colunas
par(mfrow=c(1,2))

# Criação do gráfico de dispersão
qqnorm(B)
qqline(B)
qqnorm(controle)
qqline(controle)

# Criação dos histogramas
hist(B, col = "green", freq = F, xlim = c(-12,30),ylim = c(0,0.08))
lines(density(B))
hist(controle, col = "green", freq = F, xlim = c(-16,30),ylim = c(0,0.08))
lines(density(controle))

# Criação do gráfico de boxplot
boxplot(B,controle, col = "green")

# Verificação de normalidade - Teste de Shapiro-wilk
shapiro.test(B)
shapiro.test(controle)

# Verificação de homocedasticidade - Teste de Bartlett
bartlett.test(Bcontrole)

# Teste T de B em função de controle
t.test(valores ~ grupos, data=Bcontrole.stack, mu = 0, var.equal = TRUE, conf.level = 0.95, alternative = c("two.sided"))

##############################
###### TRATAMENTO C ~ D ######
##############################

# Criação do dataframe
CD = data.frame(C,D)

# Empilhamento dos dados e renomeação das colunas
CD.stack = stack(CD)
colnames(CD.stack) = c("valores","grupos")

# Configuração de graficos em 1 linha e duas colunas
par(mfrow=c(1,2))

# Criação do gráfico de dispersão
qqnorm(C)
qqline(C)
qqnorm(D)
qqline(D)

# Criação dos histogramas
hist(C, col = "green", freq = F, xlim = c(-12,30),ylim = c(0,0.08))
lines(density(C))
hist(D, col = "green", freq = F, xlim = c(-16,30),ylim = c(0,0.08))
lines(density(D))

# Criação do gráfico de boxplot
boxplot(C,D, col = "green")

# Verificação de normalidade - Teste de Shapiro-wilk
shapiro.test(C)
shapiro.test(D)

# Verificação de homocedasticidade - Teste de Bartlett
bartlett.test(CD)

# Teste T de C em função de D
t.test(valores ~ grupos, data=CD.stack, mu = 0, var.equal = TRUE, conf.level = 0.95, alternative = c("two.sided"))

##############################
###### TRATAMENTO C ~ E ######
##############################

# Criação do dataframe
CE = data.frame(C,E)

# Empilhamento dos dados e renomeação das colunas
CE.stack = stack(CE)
colnames(CE.stack) = c("valores","grupos")

# Configuração de graficos em 1 linha e duas colunas
par(mfrow=c(1,2))

# Criação do gráfico de dispersão
qqnorm(C)
qqline(C)
qqnorm(E)
qqline(E)

# Criação dos histogramas
hist(C, col = "green", freq = F, xlim = c(-12,30),ylim = c(0,0.08))
lines(density(C))
hist(E, col = "green", freq = F, xlim = c(-16,30),ylim = c(0,0.08))
lines(density(E))

# Criação do gráfico de boxplot
boxplot(C,E, col = "green")

# Verificação de normalidade - Teste de Shapiro-wilk
shapiro.test(C)
shapiro.test(E)

# Verificação de homocedasticidade - Teste de Bartlett
bartlett.test(CE)

# Teste T de C em função de E
t.test(valores ~ grupos, data=CE.stack, mu = 0, var.equal = TRUE, conf.level = 0.95, alternative = c("two.sided"))

#####################################
###### TRATAMENTO C ~ Controle ######
#####################################

# Criação do dataframe
Ccontrole = data.frame(C,controle)

# Empilhamento dos dados e renomeação das colunas
Ccontrole.stack = stack(Ccontrole)
colnames(Ccontrole.stack) = c("valores","grupos")

# Configuração de graficos em 1 linha e duas colunas
par(mfrow=c(1,2))

# Criação do gráfico de dispersão
qqnorm(C)
qqline(C)
qqnorm(controle)
qqline(controle)

# Criação dos histogramas
hist(C, col = "green", freq = F, xlim = c(-12,30),ylim = c(0,0.08))
lines(density(C))
hist(controle, col = "green", freq = F, xlim = c(-16,30),ylim = c(0,0.08))
lines(density(controle))

# Criação do gráfico de boxplot
boxplot(C,controle, col = "green")

# Verificação de normalidade - Teste de Shapiro-wilk
shapiro.test(C)
shapiro.test(controle)

# Verificação de homocedasticidade - Teste de Bartlett
bartlett.test(Ccontrole)

# Teste T de C em função de controle
t.test(valores ~ grupos, data=Ccontrole.stack, mu = 0, var.equal = TRUE, conf.level = 0.95, alternative = c("two.sided"))

##############################
###### TRATAMENTO D ~ E ######
##############################

# Criação do dataframe
DE = data.frame(D,E)

# Empilhamento dos dados e renomeação das colunas
DE.stack = stack(DE)
colnames(DE.stack) = c("valores","grupos")

# Configuração de graficos em 1 linha e duas colunas
par(mfrow=c(1,2))

# Criação do gráfico de dispersão
qqnorm(D)
qqline(D)
qqnorm(E)
qqline(E)

# Criação dos histogramas
hist(D, col = "green", freq = F, xlim = c(-12,30),ylim = c(0,0.08))
lines(density(D))
hist(E, col = "green", freq = F, xlim = c(-16,30),ylim = c(0,0.08))
lines(density(E))

# Criação do gráfico de boxplot
boxplot(D,E, col = "green")

# Verificação de normalidade - Teste de Shapiro-wilk
shapiro.test(D)
shapiro.test(E)

# Verificação de homocedasticidade - Teste de Bartlett
bartlett.test(DE)

# Teste T de D em função de E
t.test(valores ~ grupos, data=DE.stack, mu = 0, var.equal = TRUE, conf.level = 0.95, alternative = c("two.sided"))

#####################################
###### TRATAMENTO D ~ Controle ######
#####################################

# Criação do dataframe
Dcontrole = data.frame(D,controle)

# Empilhamento dos dados e renomeação das colunas
Dcontrole.stack = stack(Dcontrole)
colnames(Dcontrole.stack) = c("valores","grupos")

# Configuração de graficos em 1 linha e duas colunas
par(mfrow=c(1,2))

# Criação do gráfico de dispersão
qqnorm(D)
qqline(D)
qqnorm(controle)
qqline(controle)

# Criação dos histogramas
hist(D, col = "green", freq = F, xlim = c(-12,30),ylim = c(0,0.08))
lines(density(D))
hist(controle, col = "green", freq = F, xlim = c(-16,30),ylim = c(0,0.08))
lines(density(controle))

# Criação do gráfico de boxplot
boxplot(D,controle, col = "green")

# Verificação de normalidade - Teste de Shapiro-wilk
shapiro.test(D)
shapiro.test(controle)

# Verificação de homocedasticidade - Teste de Bartlett
bartlett.test(Dcontrole)

# Teste T de D em função de controle
t.test(valores ~ grupos, data=Dcontrole.stack, mu = 0, var.equal = TRUE, conf.level = 0.95, alternative = c("two.sided"))

#####################################
###### TRATAMENTO E ~ Controle ######
#####################################

# Criação do dataframe
Econtrole = data.frame(E,controle)

# Empilhamento dos dados e renomeação das colunas
Econtrole.stack = stack(Econtrole)
colnames(Econtrole.stack) = c("valores","grupos")

# Configuração de graficos em 1 linha e duas colunas
par(mfrow=c(1,2))

# Criação do gráfico de dispersão
qqnorm(E)
qqline(E)
qqnorm(controle)
qqline(controle)

# Criação dos histogramas
hist(E, col = "green", freq = F, xlim = c(-12,30),ylim = c(0,0.08))
lines(density(E))
hist(controle, col = "green", freq = F, xlim = c(-16,30),ylim = c(0,0.08))
lines(density(controle))

# Criação do gráfico de boxplot
boxplot(E,controle, col = "green")

# Verificação de normalidade - Teste de Shapiro-wilk
shapiro.test(E)
shapiro.test(controle)

# Verificação de homocedasticidade - Teste de Bartlett
bartlett.test(Econtrole)

# Teste T de E em função de controle
t.test(valores ~ grupos, data=Econtrole.stack, mu = 0, var.equal = TRUE, conf.level = 0.95, alternative = c("two.sided"))

####################################
####### RESULTADOS AGREGADOS #######
####################################

# Criação de objetos para armazenar cada teste t
testeT.AB = t.test(valores ~ grupos, data=AB.stack, mu = 0, var.equal = TRUE, conf.level = 0.95, alternative = c("two.sided"))
testeT.AC = t.test(valores ~ grupos, data=AC.stack, mu = 0, var.equal = TRUE, conf.level = 0.95, alternative = c("two.sided"))
testeT.AD = t.test(valores ~ grupos, data=AD.stack, mu = 0, var.equal = TRUE, conf.level = 0.95, alternative = c("two.sided"))
testeT.AE = t.test(valores ~ grupos, data=AE.stack, mu = 0, var.equal = TRUE, conf.level = 0.95, alternative = c("two.sided"))
testeT.Acontrole = t.test(valores ~ grupos, data=Acontrole.stack, mu = 0, var.equal = TRUE, conf.level = 0.95, alternative = c("two.sided"))
testeT.BC = t.test(valores ~ grupos, data=BC.stack, mu = 0, var.equal = TRUE, conf.level = 0.95, alternative = c("two.sided"))
testeT.BD = t.test(valores ~ grupos, data=BD.stack, mu = 0, var.equal = TRUE, conf.level = 0.95, alternative = c("two.sided"))
testeT.BE = t.test(valores ~ grupos, data=BE.stack, mu = 0, var.equal = TRUE, conf.level = 0.95, alternative = c("two.sided"))
testeT.Bcontrole = t.test(valores ~ grupos, data=Bcontrole.stack, mu = 0, var.equal = TRUE, conf.level = 0.95, alternative = c("two.sided"))
testeT.CD = t.test(valores ~ grupos, data=CD.stack, mu = 0, var.equal = TRUE, conf.level = 0.95, alternative = c("two.sided"))
testeT.CE = t.test(valores ~ grupos, data=CE.stack, mu = 0, var.equal = TRUE, conf.level = 0.95, alternative = c("two.sided"))
testeT.Ccontrole = t.test(valores ~ grupos, data=Ccontrole.stack, mu = 0, var.equal = TRUE, conf.level = 0.95, alternative = c("two.sided"))
testeT.DE = t.test(valores ~ grupos, data=DE.stack, mu = 0, var.equal = TRUE, conf.level = 0.95, alternative = c("two.sided"))
testeT.Dcontrole = t.test(valores ~ grupos, data=Dcontrole.stack, mu = 0, var.equal = TRUE, conf.level = 0.95, alternative = c("two.sided"))
testeT.Econtrole = t.test(valores ~ grupos, data=Econtrole.stack, mu = 0, var.equal = TRUE, conf.level = 0.95, alternative = c("two.sided"))

# Criação de dataframe com todos os testes t
testeT = data.frame (Valor_P = 
                       c(testeT.AB$p.value,testeT.AC$p.value,testeT.AD$p.value,testeT.AE$p.value,testeT.Acontrole$p.value,
                         testeT.BC$p.value,testeT.BD$p.value,testeT.BE$p.value,testeT.Bcontrole$p.value,
                         testeT.CD$p.value,testeT.CE$p.value,testeT.Ccontrole$p.value,
                         testeT.DE$p.value,testeT.Dcontrole$p.value,
                         testeT.Econtrole$p.value))

# Alteração dos nomes das linhas
rownames(testeT) = c("A-B","A-C","A-D","A-E","A-controle","B-C","B-D","B-E","B-controle","C-D","C-E","C-controle","D-E","D-controle","E-controle")

# Criação de colunas com diferença significativas e hipótese nula
testeT["Dif_Significativa"] = ifelse(testeT$Valor_P < 0.05,"SIM","NÃO")
testeT["Hipotese_Nula"] = ifelse(testeT$Valor_P < 0.05,"Rejeita","Não Rejeita")

# Observação dos resultados
testeT
