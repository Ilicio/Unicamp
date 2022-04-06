
############################################
######### REGRESSÃO LINEAR SIMPLES #########
############################################

# Criação vetores
treinamento = c(13,15,18,20,19,19,21,16,17,17)
finalizacao = c(5.2,5.1,4.9,4.6,4.7,4.8,4.6,4.9,5.0,5.0)

# Criação do dataframe unificando os vetores
dados = data.frame(treinamento,finalizacao)

# Configuração de graficos em 1 linha e duas colunas
par(mfrow=c(1,2))

# Plotagem dos dados amostrais em diagrama de dispersão
plot(treinamento,finalizacao, main = "Treinamento x Finalização",xlab = "Treinamento (h)",ylab = "Finalização (h)")

# Criação de gráfico de caixa
boxplot(dados, col = "green")

# Regressão linear simples - dilatação é a variável dependente
regressao = lm(finalizacao ~ treinamento)
regressao

# Criação do preditor - demonstra os pontos na linha
preditor = predict(regressao)
valores.preditos = data.frame(preditor)
valores.preditos
preditor

# Plotagem dos dados amostrais e reta de regressão ajustada
plot(treinamento,finalizacao, main = "Finalizacao x Treinamento",xlab = "Treinamento (h)",ylab = "Finalização (h)")
abline(regressao)

# Função para mostrar segmentos calculado vs observado
for (i in 1:(length(treinamento)))
{lines(c(treinamento[i],treinamento[i]),c(finalizacao[i],preditor[i]))}

# Análise de variância (ANOVA)
anova = anova(regressao)  
summary(regressao)  

# Correlação - Coeficiente de Pearson
cor(treinamento,finalizacao)

#############################################
##### TESTE DE KRUSCAL WILLIS E NEMENYI #####
#############################################

# Instação de pacotes necessários
install.packages("PMCMR", dependencies = TRUE)

# Carregando pacotes necessários
library(PMCMR)

# Criação dos vetores
tipo1 = c(5,6.7,7,6.8)
tipo2 = c(8.3,9.3,8.6,9)
tipo3 = c(9.2,8.7,7.3,8.2)

# Criação do dataframe
vinhos = data.frame(tipo1,tipo2,tipo3)

# Empilhamento dos dados
vinhos.stack = stack(vinhos)

# Renomeação das colunas
colnames(vinhos.stack) = c("valores","grupos")

# Teste de Kruskal Wallis
kruskal.test(valores ~ grupos,data = vinhos.stack)

# Teste de Nemenyi - Verificando diferença
posthoc.kruskal.nemenyi.test(valores ~ grupos,data = vinhos.stack)

# Criação de gráfico de caixa
boxplot(vinhos, col = "green", main = "Qualidade do vinho", ylab = "Notas dos avaliadores", xlab = "Tipos de vinhos")

# Configuração de graficos em 1 linha e duas colunas
par(mfrow=c(1,3))

# Criação dos histogramas
hist(tipo1, col = "green")
lines(density(tipo1))
hist(tipo2, col = "green")
lines(density(tipo2))
hist(tipo3, col = "green")
lines(density(tipo3))

