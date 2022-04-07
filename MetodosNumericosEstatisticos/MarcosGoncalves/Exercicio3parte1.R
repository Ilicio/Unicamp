################################
###### Ajuste de uma reta ######
################################ 

# criação do vetor com dados de X
x = c(1,2,3,4,5,6,7,8)

# criação do vetor com dados de Y
y = c(0.5,0.6,0.9,0.8,1.2,1.5,1.7,2)

# regressão linear simples de Y em função de X
regressao1 = lm(y ~ x)

# visualização dos coeficientes linear e angular, e nível de ajuste da reta
summary(regressao1)

# gráfico de ajuste da reta aos dados experimentais
plot(x,y)
abline(regressao1, lwd = 1, col = "blue")

##############################
### ajuste de uma parabola ###
##############################

# criação do vetor com dados de X
x = c(1,2,3,4,5,6,7,8)

# criação do vetor com dados de Y
y = c(0.5,0.6,0.9,0.8,1.2,1.5,1.7,2)

# criação de dataframe X e Y
dados = data.frame(x,y)

# quantidade de experimentos
tamanho = dim(dados)[1]

# organização dos dados em sequencia
dados.novo = seq(min(x), max(x), length.out = tamanho)

# criação do modelo polinomial de grau 2
regressao2 = lm(y ~ poly(x, degree = 2, raw = TRUE))
summary(regressao2)

# criação do modelo polinomial de grau 3
regressao3 = lm(y ~ poly(x, degree = 3, raw = TRUE))
summary(regressao3)

# grafico do ajuste do modelo polinomial de grau 2
plot(x,y)
lines(dados.novo, predict(regressao2, data.frame(x = dados.novo)), col = "green")

# grafico do ajuste do modelo polinomial de grau 3
plot(x,y)
lines(dados.novo, predict(regressao3, data.frame(x = dados.novo)), col = "red")

########################
###### Comparação ######
########################

# Mostrar 3 gráficos em uma linha
par(mfrow=c(1,2))

plot(x,y)
abline(regressao1, lwd = 1, col = "blue")

plot(x,y)
lines(dados.novo, predict(regressao2, data.frame(x = dados.novo)), col = "green")
