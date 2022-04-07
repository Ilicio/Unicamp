################################
###### Ajuste de uma reta ######
################################ 

# criação do vetor com dados de X
x = c(1,1.05,1.1,1.15,1.2,1.25,1.3,1.35)

# criação do vetor com dados de Y
y = c(1,1.01,1.02,1.04,1.05,1.06,1.065,1.08)

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
x = c(1,1.05,1.1,1.15,1.2,1.25,1.3,1.35)

# criação do vetor com dados de Y
y = c(1,1.01,1.02,1.04,1.05,1.06,1.065,1.08)

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

########################
## Grafico no GGPlot2 ##
########################

install.packages("ggpmisc", dependencies = TRUE)
library(ggplot2)
library(ggpmisc)

ggplot(dados,aes(x=x,y=y))+
  geom_point(size=2.5,pch=21,col="blue4",fill="cornflowerblue")+
  geom_smooth(method = "lm",formula = y ~ poly(x, degree = 2, raw = TRUE),
              se=FALSE,col="brown4")+
  stat_poly_eq(formula = y ~ poly(x, degree = 2, raw = TRUE),parse=TRUE,
               aes(label=paste(..eq.label..,..rr.label..,sep = "~~~")),
               label.x = 0.3,
               label.y = 0.1)+
  theme_light()+
  labs(y="Y",x="X")
  #labs(y="Consumo [Kwh/m]",x=expression(paste("Area [",m^2,"]")))

ggsave(filename = "regressao_polinomial.png")

