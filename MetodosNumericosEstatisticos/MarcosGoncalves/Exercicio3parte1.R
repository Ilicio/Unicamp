################################
###### Ajuste de uma reta ######
################################ 

# cria��o do vetor com dados de X
x = c(1,2,3,4,5,6,7,8)

# cria��o do vetor com dados de Y
y = c(0.5,0.6,0.9,0.8,1.2,1.5,1.7,2)

# regress�o linear simples de Y em fun��o de X
regressao1 = lm(y ~ x)

# visualiza��o dos coeficientes linear e angular, e n�vel de ajuste da reta
summary(regressao1)

# gr�fico de ajuste da reta aos dados experimentais
plot(x,y)
abline(regressao1, lwd = 1, col = "blue")

##############################
### ajuste de uma parabola ###
##############################

# cria��o do vetor com dados de X
x = c(1,2,3,4,5,6,7,8)

# cria��o do vetor com dados de Y
y = c(0.5,0.6,0.9,0.8,1.2,1.5,1.7,2)

# cria��o de dataframe X e Y
dados = data.frame(x,y)

# quantidade de experimentos
tamanho = dim(dados)[1]

# organiza��o dos dados em sequencia
dados.novo = seq(min(x), max(x), length.out = tamanho)

# cria��o do modelo polinomial de grau 2
regressao2 = lm(y ~ poly(x, degree = 2, raw = TRUE))
summary(regressao2)

# cria��o do modelo polinomial de grau 3
regressao3 = lm(y ~ poly(x, degree = 3, raw = TRUE))
summary(regressao3)

# grafico do ajuste do modelo polinomial de grau 2
plot(x,y)
lines(dados.novo, predict(regressao2, data.frame(x = dados.novo)), col = "green")

# grafico do ajuste do modelo polinomial de grau 3
plot(x,y)
lines(dados.novo, predict(regressao3, data.frame(x = dados.novo)), col = "red")

########################
###### Compara��o ######
########################

# Mostrar 3 gr�ficos em uma linha
par(mfrow=c(1,2))

plot(x,y)
abline(regressao1, lwd = 1, col = "blue")

plot(x,y)
lines(dados.novo, predict(regressao2, data.frame(x = dados.novo)), col = "green")
