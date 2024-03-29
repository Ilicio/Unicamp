# Instala��o da biblioteca
install.packages("queueing")

# Carregamento da biblioteca
library(queueing)

# Fun��o Engset onde k = usu�rios, c = servidores e r = lamdba/mu
Engset(k=10, c=5, r=0.5)

# Armazenando valores de lambda, mu, servidores (c) e usu�rios (k) em M|M|c|k
dados = NewInput.MMCK(lambda = 0.5, mu = 1, c = 5, k = 10)

# Cria��o do modelo 
modelo = QueueingModel(dados)

# Visualizando resultados
summary(modelo)
