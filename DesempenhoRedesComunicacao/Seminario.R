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

#######################################
############## EXERCICIO ##############
#######################################

# fluxo de mensagens por usu�rio [mens/seg]
# Utilizar valores entre 0.1 a 0.7
alpha = 0.2

# n�mero de usu�rios (arbitr�rio)
M = 30

# n�mero de posi��es no buffer (14) incluindo servi�o (1)
# capacidade do sistema
N = 15

# Enlace de 64kbits/s
enlace = 64000

# tamanho m�dio da mensagem [bit]
EX = 5600

# fluxo total de mensagens destinadas ao n� (mu)[mens/seg]
A = enlace/EX

# taxa de chegada [mens/seg]
lambda = alpha * M

#Criar fila MM1
entrada_dados_MM1 = NewInput.MM1K(lambda = lambda, mu = A, k=N)

#Criar um objeto de classe fila
saida_dados_MM1 = QueueingModel(entrada_dados_MM1)

#obter o relat�rio do modelo de fila
Report(saida_dados_MM1)

teste = saida_dados_MM1$Wq















































#######################################

inicializar biblioteca e Dataframe
library(queueing)
df = data.frame("M", "alpha", "ex", "k" , "fila", "saida", stringsAsFactors=FALSE)

# Realizaer c�lculos
for (M in seq(1, 30, by = 1)) {
  for (alpha in seq(0.05, 0.45, by = 0.2)){
    for (ex in seq(500, 17000, by = 3300)){
      for (k in 1:15) {
        #Criar um objeto de classe fila
        output_MM1 = QueueingModel(NewInput.MM1KK(lambda = alpha * M, mu = 64000/ex, k = k, method = 0))
        # obter o relat�rio do modelo de fila
        df = rbind(df, (li
                        df = rbind(df, (list(M, alpha, ex, k,  output_MM1$Wq, output_MM1$Throughput)))
      }
    }
  }
}

df = df[-1,]
names(df) = c("M", "alpha", "ex", "k" , "fila", "saida")

#Pr�-processamento dados
df1$M = as.numeric(df1$M)
df1$alpha = as.numeric(df1$alpha)
df1$ex = as.numeric(df1$ex)
df1$fila = as.numeric(df1$fila)
df1$saida = as.numeric(df1$saida)

df1 = subset(df, df$fila < 0.1)
