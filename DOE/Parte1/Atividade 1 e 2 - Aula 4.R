
############################################
######### REGRESS�O LINEAR SIMPLES #########
############################################

# Cria��o vetores
treinamento = c(13,15,18,20,19,19,21,16,17,17)
finalizacao = c(5.2,5.1,4.9,4.6,4.7,4.8,4.6,4.9,5.0,5.0)

# Cria��o do dataframe unificando os vetores
dados = data.frame(treinamento,finalizacao)

# Configura��o de graficos em 1 linha e duas colunas
par(mfrow=c(1,2))

# Plotagem dos dados amostrais em diagrama de dispers�o
plot(treinamento,finalizacao, main = "Treinamento x Finaliza��o",xlab = "Treinamento (h)",ylab = "Finaliza��o (h)")

# Cria��o de gr�fico de caixa
boxplot(dados, col = "green")

# Regress�o linear simples - dilata��o � a vari�vel dependente
regressao = lm(finalizacao ~ treinamento)
regressao

# Cria��o do preditor - demonstra os pontos na linha
preditor = predict(regressao)
valores.preditos = data.frame(preditor)
valores.preditos
preditor

# Plotagem dos dados amostrais e reta de regress�o ajustada
plot(treinamento,finalizacao, main = "Finalizacao x Treinamento",xlab = "Treinamento (h)",ylab = "Finaliza��o (h)")
abline(regressao)

# Fun��o para mostrar segmentos calculado vs observado
for (i in 1:(length(treinamento)))
{lines(c(treinamento[i],treinamento[i]),c(finalizacao[i],preditor[i]))}

# An�lise de vari�ncia (ANOVA)
anova = anova(regressao)  
summary(regressao)  

# Correla��o - Coeficiente de Pearson
cor(treinamento,finalizacao)

#############################################
##### TESTE DE KRUSCAL WILLIS E NEMENYI #####
#############################################

# Insta��o de pacotes necess�rios
install.packages("PMCMR", dependencies = TRUE)

# Carregando pacotes necess�rios
library(PMCMR)

# Cria��o dos vetores
tipo1 = c(5,6.7,7,6.8)
tipo2 = c(8.3,9.3,8.6,9)
tipo3 = c(9.2,8.7,7.3,8.2)

# Cria��o do dataframe
vinhos = data.frame(tipo1,tipo2,tipo3)

# Empilhamento dos dados
vinhos.stack = stack(vinhos)

# Renomea��o das colunas
colnames(vinhos.stack) = c("valores","grupos")

# Teste de Kruskal Wallis
kruskal.test(valores ~ grupos,data = vinhos.stack)

# Teste de Nemenyi - Verificando diferen�a
posthoc.kruskal.nemenyi.test(valores ~ grupos,data = vinhos.stack)

# Cria��o de gr�fico de caixa
boxplot(vinhos, col = "green", main = "Qualidade do vinho", ylab = "Notas dos avaliadores", xlab = "Tipos de vinhos")

# Configura��o de graficos em 1 linha e duas colunas
par(mfrow=c(1,3))

# Cria��o dos histogramas
hist(tipo1, col = "green")
lines(density(tipo1))
hist(tipo2, col = "green")
lines(density(tipo2))
hist(tipo3, col = "green")
lines(density(tipo3))

