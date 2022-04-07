#######################################
############## DESCRIÇÃO ##############
#######################################

# alpha: fluxo de mensagens por usuário [mens/seg]
# M: número de usuários
# N: número de posições no buffer de mensagens, incluindo o serviço
# A: fluxo total de mensagens destinadas ao servidor [mens/seg]
# C: capacidade de escoamento do servidor [bit/seg]
# X: Tamanho da mensagem [bit]
# E(X): tamanho médioo da mensagem [bit]
# rho: tráfego destinado ao nó, mais comum utilizar rho = lambda/mu
# Pbl: probabilidade de bloqueio
# K: número de mensagens no buffer
# P(K): probabilidade do nó conter k mensagens
# E(K): número médio de mensagens no buffer
# T: tempo de espera de uma mensagem no nó [seg]
# E(T): tempo médio de espera de uma mensagem no nó [seg]
# Kg: número de mensagens na fila do sistema
# E(Kg): número médio de mensagens na fila do sistema
# Tq: ttempo de espera de uma mensagem na fila [seg]
# Ks: número de mensagens no servidor
# E(Ks): número médio de mensagens no servidor
# Tr: tempo de serviço

#######################################
############# BIBLIOTECAS #############
#######################################

# carregamento da biblioteca
library(queueing)
library(ggplot2)

#######################################
############## EXERCICIO ##############
#######################################

# fluxo de mensagens por usuário [mens/seg]
# utilizar valores entre 0.1 a 0.7
alpha = 0.2

# número de posições no buffer (14) incluindo serviço (1)
# capacidade do sistema
N = 15

# tamanho do enlace [bits/s]
enlace = 64000

# tamanho médio da mensagem [bit]
EX = 5600

# fluxo total de mensagens destinadas ao nó (mu)[mens/seg]
A = enlace/EX

# taxa de chegada [mens/seg]
# lambda = alpha * M

# criação do dataframe principal
dataframe = data.frame("M", "alpha", "EX", "N" , "rho", "Lq", "Wq", "Throughput", "L", "W", stringsAsFactors=FALSE)

# Armazenando sequencia de usuarios de 1-200
# criação da fila MM1k e do modelo
for (M in seq(1, 200, by = 1)) {
        # Criando modelo de fila
        modelo = QueueingModel(NewInput.MM1K(lambda = alpha * M, mu = A, k = N))
        # obter o relatório do modelo de fila
        dataframe = rbind(dataframe, (list(M, alpha, EX, N, modelo$RO,modelo$Lq,modelo$Wq, modelo$Throughput, modelo$L, modelo$W
    )))}

# limpando dataframe
dataframe = dataframe[-1,]
names(dataframe) = c("M", "alpha", "EX", "N" , "Rho", "clientes_fila", "tempo_fila", "Throughput", "clientes_sistema", "tempo_sistema")

# separação de filas abaixo de 0.1s
dataframe1 = subset(dataframe, dataframe$tempo_fila < 0.1)    

# visualização de gráficos
plot(dataframe$M,dataframe$tempo_fila, xlab = "Número de usuários", ylab = "Tempo na fila (s)", main = "Tempo gasto na fila por usuário", type = "p", pch = 22, col = "blue")

# customizando bordas do gráfico
axis(side = 1, at = seq(0,200,10), labels = seq(0,200,10), cex.axis = 1)
axis(side = 2, at = seq(0,1.2,0.1), labels = seq(0,1.2,0.1), cex.axis = 1)

######### AJUSTANDO USUARIOS #########

# número de usuários (arbitrário)
M = 30

# criação da fila MM1k
entrada1 = NewInput.MM1K(lambda = alpha * M, mu = A, k=N)

# criação do modelo
modelo2 = QueueingModel(entrada1)

# relatório do modelo de fila
Report(modelo2)
