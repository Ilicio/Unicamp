#######################################
############## DESCRI��O ##############
#######################################

# alpha: fluxo de mensagens por usu�rio [mens/seg]
# M: n�mero de usu�rios
# N: n�mero de posi��es no buffer de mensagens, incluindo o servi�o
# A: fluxo total de mensagens destinadas ao servidor [mens/seg]
# C: capacidade de escoamento do servidor [bit/seg]
# X: Tamanho da mensagem [bit]
# E(X): tamanho m�dioo da mensagem [bit]
# rho: tr�fego destinado ao n�, mais comum utilizar rho = lambda/mu
# Pbl: probabilidade de bloqueio
# K: n�mero de mensagens no buffer
# P(K): probabilidade do n� conter k mensagens
# E(K): n�mero m�dio de mensagens no buffer
# T: tempo de espera de uma mensagem no n� [seg]
# E(T): tempo m�dio de espera de uma mensagem no n� [seg]
# Kg: n�mero de mensagens na fila do sistema
# E(Kg): n�mero m�dio de mensagens na fila do sistema
# Tq: ttempo de espera de uma mensagem na fila [seg]
# Ks: n�mero de mensagens no servidor
# E(Ks): n�mero m�dio de mensagens no servidor
# Tr: tempo de servi�o

#######################################
############# BIBLIOTECAS #############
#######################################

# carregamento da biblioteca
library(queueing)
library(ggplot2)

#######################################
############## EXERCICIO ##############
#######################################

# fluxo de mensagens por usu�rio [mens/seg]
# utilizar valores entre 0.1 a 0.7
alpha = 0.2

# n�mero de posi��es no buffer (14) incluindo servi�o (1)
# capacidade do sistema
N = 15

# tamanho do enlace [bits/s]
enlace = 64000

# tamanho m�dio da mensagem [bit]
EX = 5600

# fluxo total de mensagens destinadas ao n� (mu)[mens/seg]
A = enlace/EX

# taxa de chegada [mens/seg]
# lambda = alpha * M

# cria��o do dataframe principal
dataframe = data.frame("M", "alpha", "EX", "N" , "rho", "Lq", "Wq", "Throughput", "L", "W", stringsAsFactors=FALSE)

# Armazenando sequencia de usuarios de 1-200
# cria��o da fila MM1k e do modelo
for (M in seq(1, 200, by = 1)) {
        # Criando modelo de fila
        modelo = QueueingModel(NewInput.MM1K(lambda = alpha * M, mu = A, k = N))
        # obter o relat�rio do modelo de fila
        dataframe = rbind(dataframe, (list(M, alpha, EX, N, modelo$RO,modelo$Lq,modelo$Wq, modelo$Throughput, modelo$L, modelo$W
    )))}

# limpando dataframe
dataframe = dataframe[-1,]
names(dataframe) = c("M", "alpha", "EX", "N" , "Rho", "clientes_fila", "tempo_fila", "Throughput", "clientes_sistema", "tempo_sistema")

# separa��o de filas abaixo de 0.1s
dataframe1 = subset(dataframe, dataframe$tempo_fila < 0.1)    

# visualiza��o de gr�ficos
plot(dataframe$M,dataframe$tempo_fila, xlab = "N�mero de usu�rios", ylab = "Tempo na fila (s)", main = "Tempo gasto na fila por usu�rio", type = "p", pch = 22, col = "blue")

# customizando bordas do gr�fico
axis(side = 1, at = seq(0,200,10), labels = seq(0,200,10), cex.axis = 1)
axis(side = 2, at = seq(0,1.2,0.1), labels = seq(0,1.2,0.1), cex.axis = 1)

######### AJUSTANDO USUARIOS #########

# n�mero de usu�rios (arbitr�rio)
M = 30

# cria��o da fila MM1k
entrada1 = NewInput.MM1K(lambda = alpha * M, mu = A, k=N)

# cria��o do modelo
modelo2 = QueueingModel(entrada1)

# relat�rio do modelo de fila
Report(modelo2)
