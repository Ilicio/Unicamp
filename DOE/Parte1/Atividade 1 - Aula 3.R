#####################################
#####################################
####### ATIVIDADE 1 - TESTE T #######
#####################################
#####################################

# Cria��o dos vetores
A = c(25,17,27,21,15)
B = c(10,-2,12,4,16)
C = c(18,8,4,14,6)
D = c(23,29,25,3,33)
E = c(11,23,5,17,9)
controle = c(8,-6,6,0,2)

##############################
###### TRATAMENTO A ~ B ######
##############################

# Cria��o do dataframe
AB = data.frame(A,B)

# Empilhamento dos dados e renomea��o das colunas
AB.stack = stack(AB)
colnames(AB.stack) = c("valores","grupos")

# Configura��o de graficos em 1 linha e duas colunas
par(mfrow=c(1,2))

# Cria��o do gr�fico de dispers�o
qqnorm(A)
qqline(A)
qqnorm(B)
qqline(B)

# Cria��o dos histogramas
hist(A, col = "green", freq = F, xlim = c(5,40),ylim = c(0,0.08))
lines(density(A))
hist(B, col = "green", freq = F, xlim = c(-16,30),ylim = c(0,0.08))
lines(density(B))

# Cria��o do Gr�fico de boxplot
boxplot(A,B, col = "green")

# Verifica��o de normalidade - Teste de Shapiro-wilk
shapiro.test(A)
shapiro.test(B)

# Verifica��o de homocedasticidade - Teste de Bartlett
bartlett.test(AB)

# Teste T de A em fun��o de B
t.test(valores ~ grupos, data=AB.stack, mu = 0, var.equal = TRUE, conf.level = 0.95, alternative = c("two.sided"))

##############################
###### TRATAMENTO A ~ C ######
##############################

# Cria��o do dataframe
AC = data.frame(A,C)

# Empilhamento dos dados e renomea��o das colunas
AC.stack = stack(AC)
colnames(AC.stack) = c("valores","grupos")

# Configura��o de graficos em 1 linha e duas colunas
par(mfrow=c(1,2))

# Cria��o do gr�fico de dispers�o
qqnorm(A)
qqline(A)
qqnorm(C)
qqline(C)

# Cria��o dos histogramas
hist(A, col = "green", freq = F, xlim = c(5,40),ylim = c(0,0.08))
lines(density(A))
hist(C, col = "green", freq = F, xlim = c(-16,30),ylim = c(0,0.08))
lines(density(C))

# Cria��o do gr�fico de boxplot
boxplot(A,C, col = "green")

# Verifica��o de normalidade - Teste de Shapiro-wilk
shapiro.test(A)
shapiro.test(C)

# Verifica��o de homocedasticidade - Teste de Bartlett
bartlett.test(AC)

# Teste T de A em fun��o de C
t.test(valores ~ grupos, data=AC.stack, mu = 0, var.equal = TRUE, conf.level = 0.95, alternative = c("two.sided"))

##############################
###### TRATAMENTO A ~ D ######
##############################

# Cria��o do dataframe
AD = data.frame(A,D)

# Empilhamento dos dados e renomea��o das colunas
AD.stack = stack(AD)
colnames(AD.stack) = c("valores","grupos")

# Configura��o de graficos em 1 linha e duas colunas
par(mfrow=c(1,2))

# Cria��o do gr�fico de dispers�o
qqnorm(A)
qqline(A)
qqnorm(D)
qqline(D)

# Cria��o dos histogramas
hist(A, col = "green", freq = F, xlim = c(5,40),ylim = c(0,0.08))
lines(density(A))
hist(D, col = "green", freq = F, xlim = c(-16,30),ylim = c(0,0.08))
lines(density(D))

# Cria��o do gr�fico de boxplot
boxplot(A,D, col = "green")

# Verifica��o de normalidade - Teste de Shapiro-wilk
shapiro.test(A)
shapiro.test(D)

# Verifica��o de homocedasticidade - Teste de Bartlett
bartlett.test(AD)

# Teste T de A em fun��o de D
t.test(valores ~ grupos, data=AD.stack, mu = 0, var.equal = TRUE, conf.level = 0.95, alternative = c("two.sided"))

##############################
###### TRATAMENTO A ~ E ######
##############################

# Cria��o do dataframe
AE = data.frame(A,E)

# Empilhamento dos dados e renomea��o das colunas
AE.stack = stack(AE)
colnames(AE.stack) = c("valores","grupos")

# Configura��o de graficos em 1 linha e duas colunas
par(mfrow=c(1,2))

# Cria��o do gr�fico de dispers�o
qqnorm(A)
qqline(A)
qqnorm(E)
qqline(E)

# Cria��o dos histogramas
hist(A, col = "green", freq = F, xlim = c(5,40),ylim = c(0,0.08))
lines(density(A))
hist(E, col = "green", freq = F, xlim = c(-16,30),ylim = c(0,0.08))
lines(density(E))

# Cria��o do gr�fico de boxplot
boxplot(A,E, col = "green")

# Verifica��o de normalidade - Teste de Shapiro-wilk
shapiro.test(A)
shapiro.test(E)

# Verifica��o de homocedasticidade - Teste de Bartlett
bartlett.test(AE)

# Teste T de A em fun��o de E
t.test(valores ~ grupos, data=AE.stack, mu = 0, var.equal = TRUE, conf.level = 0.95, alternative = c("two.sided"))

#####################################
###### TRATAMENTO A ~ controle ######
#####################################

# Cria��o do dataframe
Acontrole = data.frame(A,controle)

# Empilhamento dos dados e renomea��o das colunas
Acontrole.stack = stack(Acontrole)
colnames(Acontrole.stack) = c("valores","grupos")

# Configura��o de graficos em 1 linha e duas colunas
par(mfrow=c(1,2))

# Cria��o do gr�fico de dispers�o
qqnorm(A)
qqline(A)
qqnorm(controle)
qqline(controle)

# Cria��o dos histogramas
hist(A, col = "green", freq = F, xlim = c(5,40),ylim = c(0,0.08))
lines(density(A))
hist(controle, col = "green", freq = F, xlim = c(-16,30),ylim = c(0,0.08))
lines(density(controle))

# Cria��o do gr�fico de boxplot
boxplot(A,controle, col = "green")

# Verifica��o de normalidade - Teste de Shapiro-wilk
shapiro.test(A)
shapiro.test(controle)

# Verifica��o de homocedasticidade - Teste de Bartlett
bartlett.test(Acontrole)

# Teste T de A em fun��o de controle
t.test(valores ~ grupos, data=Acontrole.stack, mu = 0, var.equal = TRUE, conf.level = 0.95, alternative = c("two.sided"))

##############################
###### TRATAMENTO B ~ C ######
##############################

# Cria��o do dataframe
BC = data.frame(B,C)

# Empilhamento dos dados e renomea��o das colunas
BC.stack = stack(BC)
colnames(BC.stack) = c("valores","grupos")

# Configura��o de graficos em 1 linha e duas colunas
par(mfrow=c(1,2))

# Cria��o do gr�fico de dispers�o
qqnorm(B)
qqline(B)
qqnorm(C)
qqline(C)

# Cria��o dos histogramas
hist(B, col = "green", freq = F, xlim = c(-12,30),ylim = c(0,0.08))
lines(density(B))
hist(C, col = "green", freq = F, xlim = c(-16,30),ylim = c(0,0.08))
lines(density(C))

# Cria��o do gr�fico de boxplot
boxplot(B,C, col = "green")

# Verifica��o de normalidade - Teste de Shapiro-wilk
shapiro.test(B)
shapiro.test(C)

# Verifica��o de homocedasticidade - Teste de Bartlett
bartlett.test(BC)

# Teste T de B em fun��o de C
t.test(valores ~ grupos, data=BC.stack, mu = 0, var.equal = TRUE, conf.level = 0.95, alternative = c("two.sided"))

##############################
###### TRATAMENTO B ~ D ######
##############################

# Cria��o do dataframe
BD = data.frame(B,D)

# Empilhamento dos dados e renomea��o das colunas
BD.stack = stack(BD)
colnames(BD.stack) = c("valores","grupos")

# Configura��o de graficos em 1 linha e duas colunas
par(mfrow=c(1,2))

# Cria��o do gr�fico de dispers�o
qqnorm(B)
qqline(B)
qqnorm(D)
qqline(D)

# Cria��o dos histogramas
hist(B, col = "green", freq = F, xlim = c(-12,30),ylim = c(0,0.08))
lines(density(B))
hist(D, col = "green", freq = F, xlim = c(-16,30),ylim = c(0,0.08))
lines(density(D))

# Cria��o do gr�fico de boxplot
boxplot(B,D, col = "green")

# Verifica��o de normalidade - Teste de Shapiro-wilk
shapiro.test(B)
shapiro.test(D)

# Verifica��o de homocedasticidade - Teste de Bartlett
bartlett.test(BD)

# Teste T de B em fun��o de D
t.test(valores ~ grupos, data=BD.stack, mu = 0, var.equal = TRUE, conf.level = 0.95, alternative = c("two.sided"))

##############################
###### TRATAMENTO B ~ E ######
##############################

# Cria��o do dataframe
BE = data.frame(B,E)

# Empilhamento dos dados e renomea��o das colunas
BE.stack = stack(BE)
colnames(BE.stack) = c("valores","grupos")

# Configura��o de graficos em 1 linha e duas colunas
par(mfrow=c(1,2))

# Cria��o do gr�fico de dispers�o
qqnorm(B)
qqline(B)
qqnorm(E)
qqline(E)

# Cria��o dos histogramas
hist(B, col = "green", freq = F, xlim = c(-12,30),ylim = c(0,0.08))
lines(density(B))
hist(E, col = "green", freq = F, xlim = c(-16,30),ylim = c(0,0.08))
lines(density(E))

# Cria��o do gr�fico de boxplot
boxplot(B,E, col = "green")

# Verifica��o de normalidade - Teste de Shapiro-wilk
shapiro.test(B)
shapiro.test(E)

# Verifica��o de homocedasticidade - Teste de Bartlett
bartlett.test(BE)

# Teste T de B em fun��o de E
t.test(valores ~ grupos, data=BE.stack, mu = 0, var.equal = TRUE, conf.level = 0.95, alternative = c("two.sided"))

#####################################
###### TRATAMENTO B ~ Controle ######
#####################################

# Cria��o do dataframe
Bcontrole = data.frame(B,controle)

# Empilhamento dos dados e renomea��o das colunas
Bcontrole.stack = stack(Bcontrole)
colnames(Bcontrole.stack) = c("valores","grupos")

# Configura��o de graficos em 1 linha e duas colunas
par(mfrow=c(1,2))

# Cria��o do gr�fico de dispers�o
qqnorm(B)
qqline(B)
qqnorm(controle)
qqline(controle)

# Cria��o dos histogramas
hist(B, col = "green", freq = F, xlim = c(-12,30),ylim = c(0,0.08))
lines(density(B))
hist(controle, col = "green", freq = F, xlim = c(-16,30),ylim = c(0,0.08))
lines(density(controle))

# Cria��o do gr�fico de boxplot
boxplot(B,controle, col = "green")

# Verifica��o de normalidade - Teste de Shapiro-wilk
shapiro.test(B)
shapiro.test(controle)

# Verifica��o de homocedasticidade - Teste de Bartlett
bartlett.test(Bcontrole)

# Teste T de B em fun��o de controle
t.test(valores ~ grupos, data=Bcontrole.stack, mu = 0, var.equal = TRUE, conf.level = 0.95, alternative = c("two.sided"))

##############################
###### TRATAMENTO C ~ D ######
##############################

# Cria��o do dataframe
CD = data.frame(C,D)

# Empilhamento dos dados e renomea��o das colunas
CD.stack = stack(CD)
colnames(CD.stack) = c("valores","grupos")

# Configura��o de graficos em 1 linha e duas colunas
par(mfrow=c(1,2))

# Cria��o do gr�fico de dispers�o
qqnorm(C)
qqline(C)
qqnorm(D)
qqline(D)

# Cria��o dos histogramas
hist(C, col = "green", freq = F, xlim = c(-12,30),ylim = c(0,0.08))
lines(density(C))
hist(D, col = "green", freq = F, xlim = c(-16,30),ylim = c(0,0.08))
lines(density(D))

# Cria��o do gr�fico de boxplot
boxplot(C,D, col = "green")

# Verifica��o de normalidade - Teste de Shapiro-wilk
shapiro.test(C)
shapiro.test(D)

# Verifica��o de homocedasticidade - Teste de Bartlett
bartlett.test(CD)

# Teste T de C em fun��o de D
t.test(valores ~ grupos, data=CD.stack, mu = 0, var.equal = TRUE, conf.level = 0.95, alternative = c("two.sided"))

##############################
###### TRATAMENTO C ~ E ######
##############################

# Cria��o do dataframe
CE = data.frame(C,E)

# Empilhamento dos dados e renomea��o das colunas
CE.stack = stack(CE)
colnames(CE.stack) = c("valores","grupos")

# Configura��o de graficos em 1 linha e duas colunas
par(mfrow=c(1,2))

# Cria��o do gr�fico de dispers�o
qqnorm(C)
qqline(C)
qqnorm(E)
qqline(E)

# Cria��o dos histogramas
hist(C, col = "green", freq = F, xlim = c(-12,30),ylim = c(0,0.08))
lines(density(C))
hist(E, col = "green", freq = F, xlim = c(-16,30),ylim = c(0,0.08))
lines(density(E))

# Cria��o do gr�fico de boxplot
boxplot(C,E, col = "green")

# Verifica��o de normalidade - Teste de Shapiro-wilk
shapiro.test(C)
shapiro.test(E)

# Verifica��o de homocedasticidade - Teste de Bartlett
bartlett.test(CE)

# Teste T de C em fun��o de E
t.test(valores ~ grupos, data=CE.stack, mu = 0, var.equal = TRUE, conf.level = 0.95, alternative = c("two.sided"))

#####################################
###### TRATAMENTO C ~ Controle ######
#####################################

# Cria��o do dataframe
Ccontrole = data.frame(C,controle)

# Empilhamento dos dados e renomea��o das colunas
Ccontrole.stack = stack(Ccontrole)
colnames(Ccontrole.stack) = c("valores","grupos")

# Configura��o de graficos em 1 linha e duas colunas
par(mfrow=c(1,2))

# Cria��o do gr�fico de dispers�o
qqnorm(C)
qqline(C)
qqnorm(controle)
qqline(controle)

# Cria��o dos histogramas
hist(C, col = "green", freq = F, xlim = c(-12,30),ylim = c(0,0.08))
lines(density(C))
hist(controle, col = "green", freq = F, xlim = c(-16,30),ylim = c(0,0.08))
lines(density(controle))

# Cria��o do gr�fico de boxplot
boxplot(C,controle, col = "green")

# Verifica��o de normalidade - Teste de Shapiro-wilk
shapiro.test(C)
shapiro.test(controle)

# Verifica��o de homocedasticidade - Teste de Bartlett
bartlett.test(Ccontrole)

# Teste T de C em fun��o de controle
t.test(valores ~ grupos, data=Ccontrole.stack, mu = 0, var.equal = TRUE, conf.level = 0.95, alternative = c("two.sided"))

##############################
###### TRATAMENTO D ~ E ######
##############################

# Cria��o do dataframe
DE = data.frame(D,E)

# Empilhamento dos dados e renomea��o das colunas
DE.stack = stack(DE)
colnames(DE.stack) = c("valores","grupos")

# Configura��o de graficos em 1 linha e duas colunas
par(mfrow=c(1,2))

# Cria��o do gr�fico de dispers�o
qqnorm(D)
qqline(D)
qqnorm(E)
qqline(E)

# Cria��o dos histogramas
hist(D, col = "green", freq = F, xlim = c(-12,30),ylim = c(0,0.08))
lines(density(D))
hist(E, col = "green", freq = F, xlim = c(-16,30),ylim = c(0,0.08))
lines(density(E))

# Cria��o do gr�fico de boxplot
boxplot(D,E, col = "green")

# Verifica��o de normalidade - Teste de Shapiro-wilk
shapiro.test(D)
shapiro.test(E)

# Verifica��o de homocedasticidade - Teste de Bartlett
bartlett.test(DE)

# Teste T de D em fun��o de E
t.test(valores ~ grupos, data=DE.stack, mu = 0, var.equal = TRUE, conf.level = 0.95, alternative = c("two.sided"))

#####################################
###### TRATAMENTO D ~ Controle ######
#####################################

# Cria��o do dataframe
Dcontrole = data.frame(D,controle)

# Empilhamento dos dados e renomea��o das colunas
Dcontrole.stack = stack(Dcontrole)
colnames(Dcontrole.stack) = c("valores","grupos")

# Configura��o de graficos em 1 linha e duas colunas
par(mfrow=c(1,2))

# Cria��o do gr�fico de dispers�o
qqnorm(D)
qqline(D)
qqnorm(controle)
qqline(controle)

# Cria��o dos histogramas
hist(D, col = "green", freq = F, xlim = c(-12,30),ylim = c(0,0.08))
lines(density(D))
hist(controle, col = "green", freq = F, xlim = c(-16,30),ylim = c(0,0.08))
lines(density(controle))

# Cria��o do gr�fico de boxplot
boxplot(D,controle, col = "green")

# Verifica��o de normalidade - Teste de Shapiro-wilk
shapiro.test(D)
shapiro.test(controle)

# Verifica��o de homocedasticidade - Teste de Bartlett
bartlett.test(Dcontrole)

# Teste T de D em fun��o de controle
t.test(valores ~ grupos, data=Dcontrole.stack, mu = 0, var.equal = TRUE, conf.level = 0.95, alternative = c("two.sided"))

#####################################
###### TRATAMENTO E ~ Controle ######
#####################################

# Cria��o do dataframe
Econtrole = data.frame(E,controle)

# Empilhamento dos dados e renomea��o das colunas
Econtrole.stack = stack(Econtrole)
colnames(Econtrole.stack) = c("valores","grupos")

# Configura��o de graficos em 1 linha e duas colunas
par(mfrow=c(1,2))

# Cria��o do gr�fico de dispers�o
qqnorm(E)
qqline(E)
qqnorm(controle)
qqline(controle)

# Cria��o dos histogramas
hist(E, col = "green", freq = F, xlim = c(-12,30),ylim = c(0,0.08))
lines(density(E))
hist(controle, col = "green", freq = F, xlim = c(-16,30),ylim = c(0,0.08))
lines(density(controle))

# Cria��o do gr�fico de boxplot
boxplot(E,controle, col = "green")

# Verifica��o de normalidade - Teste de Shapiro-wilk
shapiro.test(E)
shapiro.test(controle)

# Verifica��o de homocedasticidade - Teste de Bartlett
bartlett.test(Econtrole)

# Teste T de E em fun��o de controle
t.test(valores ~ grupos, data=Econtrole.stack, mu = 0, var.equal = TRUE, conf.level = 0.95, alternative = c("two.sided"))

####################################
####### RESULTADOS AGREGADOS #######
####################################

# Cria��o de objetos para armazenar cada teste t
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

# Cria��o de dataframe com todos os testes t
testeT = data.frame (Valor_P = 
                       c(testeT.AB$p.value,testeT.AC$p.value,testeT.AD$p.value,testeT.AE$p.value,testeT.Acontrole$p.value,
                         testeT.BC$p.value,testeT.BD$p.value,testeT.BE$p.value,testeT.Bcontrole$p.value,
                         testeT.CD$p.value,testeT.CE$p.value,testeT.Ccontrole$p.value,
                         testeT.DE$p.value,testeT.Dcontrole$p.value,
                         testeT.Econtrole$p.value))

# Altera��o dos nomes das linhas
rownames(testeT) = c("A-B","A-C","A-D","A-E","A-controle","B-C","B-D","B-E","B-controle","C-D","C-E","C-controle","D-E","D-controle","E-controle")

# Cria��o de colunas com diferen�a significativas e hip�tese nula
testeT["Dif_Significativa"] = ifelse(testeT$Valor_P < 0.05,"SIM","N�O")
testeT["Hipotese_Nula"] = ifelse(testeT$Valor_P < 0.05,"Rejeita","N�o Rejeita")

# Observa��o dos resultados
testeT
