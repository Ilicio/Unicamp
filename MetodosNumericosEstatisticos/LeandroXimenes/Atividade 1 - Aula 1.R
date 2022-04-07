
# Bibliotecas necessárias
library(openxlsx)
library(corrplot)
library(RColorBrewer)
library(dplyr)

# Atribuir diretorio de trabalho
setwd("F:/Backup/Estudos/Mestrado/Especial/2 semestre de 2020/Métodos Numéricos e Estatísticos - P_FT094A_2020S2/Leandro Ronchini Ximenes/Tarefas")

###############
## Questao 1 ##
###############

# carregar dados da pasta atribuída
dados = read.xlsx("atividade1.xlsx", sheet = 1)

# Mostrar 3 gráficos em uma linha
par(mfrow=c(1,3))

# Histograma
hist(dados$temp_avg, breaks = 20, main = "Histograma da Temperatura")
hist(dados$light_avg, breaks = 20, main = "Histograma da Luminosidade")
hist(dados$humidity_avg, breaks = 20, main = "Histograma da Umidade")

# Verificar normalidade dos dados usando teste de Shapiro-Wilk
shapiroT = shapiro.test(dados$temp_avg)
shapiroL = shapiro.test(dados$light_avg)
shapiroU = shapiro.test(dados$humidity_avg)
shapiro.final = data.frame (Valor_P = c(shapiroT$p.value,shapiroL$p.value,shapiroU$p.value))

# Alteração dos nomes das linhas
rownames(shapiro.final) = c("Temperatura","Luminosidade","Umidade")

# Verificar se existem outliers
boxplot(dados$temp_avg)
boxplot(dados$light_avg)
boxplot(dados$humidity_avg)

# Aplicar correlação (Pearson)
correlacao.TL = cor(dados$temp_avg,dados$light_avg, method = "pearson")
correlacao.TU = cor(dados$temp_avg,dados$humidity_avg, method = "pearson")
correlacao.LU = cor(dados$light_avg,dados$humidity_avg, method = "pearson")

# Criar matriz com correlações
matriz = round(cor(dados[2:4],method = "pearson"),2)

# Visualizar gráficos de correlação entre as 3 variáveis
corrplot(matriz, type="upper", order="hclust",col=brewer.pal(n=8, name="RdYlBu"), tl.col = "black", addCoef.col = "black")

# Criar regressão linear dos dados
regressao.LT = lm(light_avg ~ temp_avg, data=dados)
regressao.UT = lm(humidity_avg ~ temp_avg, data=dados)
regressao.UL = lm(humidity_avg ~ light_avg, data=dados)

# Verificar reta ajustada aos dados
plot(dados$temp_avg,dados$light_avg, xlab = "Temperatura", ylab = "Luminosidade")
abline(regressao.LT, col = "red", lty = 1, lwd = 2)
plot(dados$temp_avg,dados$humidity_avg, xlab = "Temperatura", ylab = "Umidade")
abline(regressao.UT, col = "red", lty = 1, lwd = 2)
plot(dados$light_avg,dados$humidity_avg, xlab = "Luminosidade", ylab = "Umidade")
abline(regressao.UL, col = "red", lty = 1, lwd = 2)

###############
## Questao 2 ##
###############

# carregar dados da pasta atribuída
dados2 = read.xlsx("atividade1.xlsx", sheet = 2) %>%
  select(Type,Sale_Year,Median_Price,Transaction_Count)

# Criar outro dataframe somente com Type = ResidentialApartment
dados3 = dados2 %>%
  select(Type,Sale_Year,Median_Price,Transaction_Count) %>%
  filter(Type=='ResidentialApartment')

# Mostrar 3 gráficos em uma linha
par(mfrow=c(1,3))

# Histograma
hist(dados2$Sale_Year, breaks = 20, main = "Histograma do Ano de Venda")
hist(dados2$Median_Price, breaks = 20, main = "Histograma do Preço Médio")
hist(dados3$Transaction_Count, breaks = 20, main = "Histograma das Transações")

# Verificar normalidade dos dados usando teste de Shapiro-Wilk
shapiroA = shapiro.test(dados2$Sale_Year)
shapiroP = shapiro.test(dados2$Median_Price)
shapiroTr = shapiro.test(dados3$Transaction_Count)
shapiro.final2 = data.frame (Valor_P = c(shapiroA$p.value,shapiroP$p.value,shapiroTr$p.value))

# Alteração dos nomes das linhas
rownames(shapiro.final2) = c("Ano","Preço","Transação")

# Aplicar correlação (Pearson)
correlacao.AV1 = cor(dados2$Sale_Year,dados2$Median_Price, method = "pearson")
correlacao.AV2 = cor(dados3$Sale_Year,dados3$Median_Price, method = "pearson")
correlacao.AT = cor(dados2$Sale_Year,dados2$Transaction_Count, method = "pearson")

# Criar matriz com correlações
matriz2 = round(cor(dados2[2:4],method = "pearson"),2)
matriz3 = round(cor(dados3[2:3],method = "pearson"),2)

# Visualizar gráficos de correlação entre as 3 variáveis
corrplot(matriz2, type="upper", order="hclust",col=brewer.pal(n=8, name="RdYlBu"), tl.col = "black", addCoef.col = "black")
corrplot(matriz3, type="upper", order="hclust",col=brewer.pal(n=8, name="RdYlBu"), tl.col = "black", addCoef.col = "black")

# Criar regressão linear dos dados
regressao.AV1 = lm(Median_Price ~ Sale_Year, data=dados2)
regressao.AV2 = lm(Median_Price ~ Sale_Year, data=dados3)
regressao.AT = lm(Transaction_Count ~ Sale_Year, data=dados2)

# Verificar reta ajustada aos dados
plot(dados2$Sale_Year,dados2$Median_Price, xlab = "Ano", ylab = "Preço médio")
abline(regressao.AV1, col = "red", lty = 1, lwd = 2)
plot(dados3$Sale_Year,dados3$Median_Price, xlab = "Ano", ylab = "Preço médio")
abline(regressao.AV2, col = "red", lty = 1, lwd = 2)
plot(dados2$Sale_Year,dados2$Transaction_Count, xlab = "Ano", ylab = "Número de transações")
abline(regressao.AT, col = "red", lty = 1, lwd = 2)

