
# Bibliotecas necessárias
library(openxlsx)
library(dplyr)

# Atribuir diretorio de trabalho
setwd("F:/Backup/Estudos/Mestrado/Especial/2 semestre de 2020/Métodos Numéricos e Estatísticos - P_FT094A_2020S2/Leandro Ronchini Ximenes/Tarefas")

# carregar dados da pasta atribuída
dados = read.xlsx("Dados_listaTeste de Hipotese-rev1.xlsx", sheet = 1)

# Verificar estrutura das variáveis
glimpse(dados)

# removendo a primeira coluna, pois não será utilizada
dados = subset(dados, select = -c(X1))

# removendo a primeira linha, pois não será utilizada
dados = dados[-1, ]

# Renomeação das colunas
colnames(dados) = c("temperatura-0h","temperatura-1h","temperatura-2h","temperatura-3h","temperatura-4h","temperatura-5h",
                    "temperatura-6h","temperatura-7h","temperatura-8h","temperatura-9h","temperatura-10h","temperatura-11h",
                    "temperatura-12h","temperatura-13h","temperatura-14h","temperatura-15h","temperatura-16h","temperatura-17h",
                    "temperatura-18h","temperatura-19h","temperatura-20h","temperatura-21h","temperatura-22h","temperatura-23h",
                    "umidade-0h","umidade-1h","umidade-2h","umidade-3h","umidade-4h","umidade-5h",
                    "umidade-6h","umidade-7h","umidade-8h","umidade-9h","umidade-10h","umidade-11h",
                    "umidade-12h","umidade-13h","umidade-14h","umidade-15h","umidade-16h","umidade-17h",
                    "umidade-18h","umidade-19h","umidade-20h","umidade-21h","umidade-22h","umidade-23h")

# Mostrar 3 gráficos em uma linha
par(mfrow=c(3,4))

# Histograma
hist(dados$`temperatura-0h`, breaks = 20, main = "Histograma da Temperatura às 00:00h",xlab = "Temperatura em graus", ylab = "Frequência")
hist(dados$`temperatura-1h`, breaks = 20, main = "Histograma da Temperatura às 01:00h",xlab = "Temperatura em graus", ylab = "Frequência")
hist(dados$`temperatura-2h`, breaks = 20, main = "Histograma da Temperatura às 02:00h",xlab = "Temperatura em graus", ylab = "Frequência")
hist(dados$`temperatura-3h`, breaks = 20, main = "Histograma da Temperatura às 03:00h",xlab = "Temperatura em graus", ylab = "Frequência")
hist(dados$`temperatura-4h`, breaks = 20, main = "Histograma da Temperatura às 04:00h",xlab = "Temperatura em graus", ylab = "Frequência")
hist(dados$`temperatura-5h`, breaks = 20, main = "Histograma da Temperatura às 05:00h",xlab = "Temperatura em graus", ylab = "Frequência")
hist(dados$`temperatura-6h`, breaks = 20, main = "Histograma da Temperatura às 06:00h",xlab = "Temperatura em graus", ylab = "Frequência")
hist(dados$`temperatura-7h`, breaks = 20, main = "Histograma da Temperatura às 07:00h",xlab = "Temperatura em graus", ylab = "Frequência")
hist(dados$`temperatura-8h`, breaks = 20, main = "Histograma da Temperatura às 08:00h",xlab = "Temperatura em graus", ylab = "Frequência")
hist(dados$`temperatura-9h`, breaks = 20, main = "Histograma da Temperatura às 09:00h",xlab = "Temperatura em graus", ylab = "Frequência")
hist(dados$`temperatura-10h`, breaks = 20, main = "Histograma da Temperatura às 10:00h",xlab = "Temperatura em graus", ylab = "Frequência")
hist(dados$`temperatura-11h`, breaks = 20, main = "Histograma da Temperatura às 11:00h",xlab = "Temperatura em graus", ylab = "Frequência")
hist(dados$`temperatura-12h`, breaks = 20, main = "Histograma da Temperatura às 12:00h",xlab = "Temperatura em graus", ylab = "Frequência")
hist(dados$`temperatura-13h`, breaks = 20, main = "Histograma da Temperatura às 13:00h",xlab = "Temperatura em graus", ylab = "Frequência")
hist(dados$`temperatura-14h`, breaks = 20, main = "Histograma da Temperatura às 14:00h",xlab = "Temperatura em graus", ylab = "Frequência")
hist(dados$`temperatura-15h`, breaks = 20, main = "Histograma da Temperatura às 15:00h",xlab = "Temperatura em graus", ylab = "Frequência")
hist(dados$`temperatura-16h`, breaks = 20, main = "Histograma da Temperatura às 16:00h",xlab = "Temperatura em graus", ylab = "Frequência")
hist(dados$`temperatura-17h`, breaks = 20, main = "Histograma da Temperatura às 17:00h",xlab = "Temperatura em graus", ylab = "Frequência")
hist(dados$`temperatura-18h`, breaks = 20, main = "Histograma da Temperatura às 18:00h",xlab = "Temperatura em graus", ylab = "Frequência")
hist(dados$`temperatura-19h`, breaks = 20, main = "Histograma da Temperatura às 19:00h",xlab = "Temperatura em graus", ylab = "Frequência")
hist(dados$`temperatura-20h`, breaks = 20, main = "Histograma da Temperatura às 20:00h",xlab = "Temperatura em graus", ylab = "Frequência")
hist(dados$`temperatura-21h`, breaks = 20, main = "Histograma da Temperatura às 21:00h",xlab = "Temperatura em graus", ylab = "Frequência")
hist(dados$`temperatura-22h`, breaks = 20, main = "Histograma da Temperatura às 22:00h",xlab = "Temperatura em graus", ylab = "Frequência")
hist(dados$`temperatura-23h`, breaks = 20, main = "Histograma da Temperatura às 23:00h",xlab = "Temperatura em graus", ylab = "Frequência")

# Verificar normalidade dos dados usando teste de Shapiro-Wilk
shapiro0 = shapiro.test(dados$`temperatura-0h`)
shapiro1 = shapiro.test(dados$`temperatura-1h`)
shapiro2 = shapiro.test(dados$`temperatura-2h`)
shapiro3 = shapiro.test(dados$`temperatura-3h`)
shapiro4 = shapiro.test(dados$`temperatura-4h`)
shapiro5 = shapiro.test(dados$`temperatura-5h`)
shapiro6 = shapiro.test(dados$`temperatura-6h`)
shapiro7 = shapiro.test(dados$`temperatura-7h`)
shapiro8 = shapiro.test(dados$`temperatura-8h`)
shapiro9 = shapiro.test(dados$`temperatura-9h`)
shapiro10 = shapiro.test(dados$`temperatura-10h`)
shapiro11 = shapiro.test(dados$`temperatura-11h`)
shapiro12 = shapiro.test(dados$`temperatura-12h`)
shapiro13 = shapiro.test(dados$`temperatura-13h`)
shapiro14 = shapiro.test(dados$`temperatura-14h`)
shapiro15 = shapiro.test(dados$`temperatura-15h`)
shapiro16 = shapiro.test(dados$`temperatura-16h`)
shapiro17 = shapiro.test(dados$`temperatura-17h`)
shapiro18 = shapiro.test(dados$`temperatura-18h`)
shapiro19 = shapiro.test(dados$`temperatura-19h`)
shapiro20 = shapiro.test(dados$`temperatura-20h`)
shapiro21 = shapiro.test(dados$`temperatura-21h`)
shapiro22 = shapiro.test(dados$`temperatura-22h`)
shapiro23 = shapiro.test(dados$`temperatura-23h`)

shapiro.final = data.frame (Valor_P = c(shapiro0$p.value,shapiro1$p.value,shapiro2$p.value,shapiro3$p.value,
                                        shapiro4$p.value,shapiro5$p.value,shapiro6$p.value,shapiro7$p.value,
                                        shapiro8$p.value,shapiro9$p.value,shapiro10$p.value,shapiro11$p.value,
                                        shapiro12$p.value,shapiro13$p.value,shapiro14$p.value,shapiro15$p.value,
                                        shapiro16$p.value,shapiro17$p.value,shapiro18$p.value,shapiro19$p.value,
                                        shapiro20$p.value,shapiro21$p.value,shapiro22$p.value,shapiro23$p.value))

# Alteração dos nomes das linhas
rownames(shapiro.final) = c("0h","1h","2h","3h","4h","5h","6h","7h","8h","9h","10h","11h",
                     "12h","13h","14h","15h","16h","17h","18h","19h","20h","21h","22h","23h")

# Criação de colunas com diferença significativas e hipótese nula
shapiro.final["Normalidade"] = ifelse(shapiro.final$Valor_P > 0.05,"SIM","NÃO")

# Criação das médias das temperaturas
media0 = mean(dados$`temperatura-0h`)
media1 = mean(dados$`temperatura-1h`)
media2 = mean(dados$`temperatura-2h`)
media3 = mean(dados$`temperatura-3h`)
media4 = mean(dados$`temperatura-4h`)
media5 = mean(dados$`temperatura-5h`)
media6 = mean(dados$`temperatura-6h`)
media7 = mean(dados$`temperatura-7h`)
media8 = mean(dados$`temperatura-8h`)
media9 = mean(dados$`temperatura-9h`)
media10 = mean(dados$`temperatura-10h`)
media11 = mean(dados$`temperatura-11h`)
media12 = mean(dados$`temperatura-12h`)
media13 = mean(dados$`temperatura-13h`)
media14 = mean(dados$`temperatura-14h`)
media15 = mean(dados$`temperatura-15h`)
media16 = mean(dados$`temperatura-16h`)
media17 = mean(dados$`temperatura-17h`)
media18 = mean(dados$`temperatura-18h`)
media19 = mean(dados$`temperatura-19h`)
media20 = mean(dados$`temperatura-20h`)
media21 = mean(dados$`temperatura-21h`)
media22 = mean(dados$`temperatura-22h`)
media23 = mean(dados$`temperatura-23h`)

# Junção das médias em um único dataframe
medias = data.frame (media_temperatura = c(media0,media1,media2,media3,media4,media5,media6,media7,media8,media9,media10,media11,
                                           media12,media13,media14,media15,media16,media17,media18,media19,media20,media21,media22,media23))

# Média geral
mediageral = mean(medias$media_temperatura)

# Criação dos desvios padrão das temperaturas
desvio0 = sd(dados$`temperatura-0h`)
desvio1 = sd(dados$`temperatura-1h`)
desvio2 = sd(dados$`temperatura-2h`)
desvio3 = sd(dados$`temperatura-3h`)
desvio4 = sd(dados$`temperatura-4h`)
desvio5 = sd(dados$`temperatura-5h`)
desvio6 = sd(dados$`temperatura-6h`)
desvio7 = sd(dados$`temperatura-7h`)
desvio8 = sd(dados$`temperatura-8h`)
desvio9 = sd(dados$`temperatura-9h`)
desvio10 = sd(dados$`temperatura-10h`)
desvio11 = sd(dados$`temperatura-11h`)
desvio12 = sd(dados$`temperatura-12h`)
desvio13 = sd(dados$`temperatura-13h`)
desvio14 = sd(dados$`temperatura-14h`)
desvio15 = sd(dados$`temperatura-15h`)
desvio16 = sd(dados$`temperatura-16h`)
desvio17 = sd(dados$`temperatura-17h`)
desvio18 = sd(dados$`temperatura-18h`)
desvio19 = sd(dados$`temperatura-19h`)
desvio20 = sd(dados$`temperatura-20h`)
desvio21 = sd(dados$`temperatura-21h`)
desvio22 = sd(dados$`temperatura-22h`)
desvio23 = sd(dados$`temperatura-23h`)

# Junção dos desvios padrão em um único dataframe
desvios = data.frame (desvio_temperatura = c(desvio0,desvio1,desvio2,desvio3,desvio4,desvio5,desvio6,desvio7,desvio8,desvio9,desvio10,desvio11,
                                             desvio12,desvio13,desvio14,desvio15,desvio16,desvio17,desvio18,desvio19,desvio20,desvio21,desvio22,desvio23))

# Desvio padrão geral
desviogeral = mean(desvios$desvio_temperatura)







