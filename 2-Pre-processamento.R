#========================
# PRE-PROCESSAMENTO
#========================

# Definindo diretorio de trabalho
setwd("F:/Backup/Estudos/Mestrado/Especial/1 semestre de 2020/Projeto/Projeto-final/Dataset")

# carregando dataset em UTF-8
dados_brutos = read.csv("houses_to_rent_v2.csv",fileEncoding = "UTF-8")

# criando atributos, pois esse dataset nao possui em portugues
atributos = c("cidade","area","quartos","banheiros","vagas","andar","animal","mobilhada","condominio","aluguel","impostos","seguro","total")

# atribuindo colunas no dataset
colnames(dados_brutos) = atributos

# criando novo dataset com os atributos
dados = dados_brutos

# substituindo valores "-" de andar por "0"
dados$andar = replace(dados$andar,dados$andar=="-",0)

# mostrando classes dos atributos atuais
sapply(dados, class)

# mudando atributo "andar" para inteiro
dados$andar = as.integer(as.character(dados$andar))

# substituindo atributos categóricos em númericos (cidade)
dados$cidade = factor(dados$cidade,
                      levels = c("São Paulo","Belo Horizonte","Rio de Janeiro","Porto Alegre","Campinas"),
                      labels = c(0,1,2,3,4))

# substituindo atributos categóricos em númericos (mobilhada)
dados$animal = factor(dados$animal,
                      levels = c("not acept","acept"),
                      labels = c(0,1))

# substituindo atributos categóricos em númericos (mobilhada)
dados$mobilhada = factor(dados$mobilhada,
                         levels = c("not furnished","furnished"),
                         labels = c(0,1))

# permitindo graficos em linhas e colunas.
par(mfrow=c(3,3))

# para aumentar texto do eixo X
par(cex.lab=2.5) 

# para aumentar texto do eixo Y
par(cex.axis=2.5)

# encontrando outliers via boxplot
boxplot(dados$area,xlab = "area")
boxplot(dados$quartos,xlab = "quartos")
boxplot(dados$banheiros,xlab = "banheiros")
boxplot(dados$vaga,xlab = "vagas")
boxplot(dados$andar,xlab = "andar")
boxplot(dados$condominio,xlab = "condominio")
boxplot(dados$aluguel,xlab = "aluguel")
boxplot(dados$impostos,xlab = "impostos")
boxplot(dados$seguro,xlab = "seguro")

# adicionando titulo do conjunto de boxplot
mtext("Outliers dos atributos", side = 3, line = -2, outer = TRUE, col = "blue")

# mostrando quartis dos atributos
summary(dados$area)
quantile(dados$area)
summary(dados$quartos)
quantile(dados$quartos)
summary(dados$banheiros)
quantile(dados$banheiros)
summary(dados$vagas)
quantile(dados$vagas)
summary(dados$andar)
quantile(dados$andar)
summary(dados$condominio)
quantile(dados$condominio)
summary(dados$aluguel)
quantile(dados$aluguel)
summary(dados$impostos)
quantile(dados$impostos)
summary(dados$seguro)
quantile(dados$seguro)

# criando novo conjunto de dados para retirada de outliers
dadoslimpos = dados

# criando limite superior dos atributos
limitesuperior_area = 182 + 1.5*IQR(dadoslimpos$area)
dadoslimpos$area[dadoslimpos$area > 371]
limitesuperior_andar = 8 + 1.5*IQR(dadoslimpos$andar)
dadoslimpos$andar[dadoslimpos$andar > 18.5]
limitesuperior_condominio = 1238 + 1.5*IQR(dadoslimpos$condominio)
dadoslimpos$condominio[dadoslimpos$condominio > 3000]
limitesuperior_impostos = 375 + 1.5*IQR(dadoslimpos$impostos)
dadoslimpos$impostos[dadoslimpos$impostos > 880]

# Retirando outliers de todos os atributos em um novo conjunto de dados limpos e mostrando as amostras atuais
dadoslimpos = subset(dados,dados$area < 2000 & andar < 40 & condominio < 15000 & impostos < 22000)
dim(dadoslimpos)

# permitindo graficos em linhas e colunas.
par(mfrow=c(2,2))

# para aumentar texto do eixo X
par(cex.lab=2.5) 

# para aumentar texto do eixo Y
par(cex.axis=2.5)

# encontrando outliers via boxplot novamente
boxplot(dadoslimpos$area,xlab = "area")
boxplot(dadoslimpos$andar,xlab = "andar")
boxplot(dadoslimpos$condominio,xlab = "condominio")
boxplot(dadoslimpos$impostos,xlab = "impostos")

# adicionando titulo do conjunto de boxplot
mtext("Outliers dos atributos - limpos", side = 3, line = -2, outer = TRUE, col = "blue")

# removendo coluna total, pois não será utilizada
dadoslimpos = subset(dadoslimpos, select = -c(total))

# criando conjunto de dados específicos para cada algoritmo (RL=regressão linear, AD=árvores de decisão)
dados_RL = dadoslimpos
dados_AD = dadoslimpos
