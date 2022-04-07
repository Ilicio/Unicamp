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

#######################################
############## EXERCICIO ##############
#######################################

# fluxo de mensagens por usuário [mens/seg]
# Utilizar valores entre 0.1 a 0.7
alpha = 0.2

# número de usuários (arbitrário)
M = 30

# número de posições no buffer (14) incluindo serviço (1)
# capacidade do sistema
N = 15

# Enlace de 64kbits/s
enlace = 64000

# tamanho médio da mensagem [bit]
EX = 5600

# fluxo total de mensagens destinadas ao nó (mu)[mens/seg]
A = enlace/EX

# taxa de chegada [mens/seg]
lambda = alpha * M

#Criar fila MM1
entrada_dados_MM1 = NewInput.MM1K(lambda = lambda, mu = A, k=N)

#Criar um objeto de classe fila
saida_dados_MM1 = QueueingModel(entrada_dados_MM1)

#obter o relatório do modelo de fila
Report(saida_dados_MM1)

teste = saida_dados_MM1$Wq















































#######################################

inicializar biblioteca e Dataframe
library(queueing)
df = data.frame("M", "alpha", "ex", "k" , "fila", "saida", stringsAsFactors=FALSE)

# Realizaer cálculos
for (M in seq(1, 30, by = 1)) {
  for (alpha in seq(0.05, 0.45, by = 0.2)){
    for (ex in seq(500, 17000, by = 3300)){
      for (k in 1:15) {
        #Criar um objeto de classe fila
        output_MM1 = QueueingModel(NewInput.MM1KK(lambda = alpha * M, mu = 64000/ex, k = k, method = 0))
        # obter o relatório do modelo de fila
        df = rbind(df, (li
                        df = rbind(df, (list(M, alpha, ex, k,  output_MM1$Wq, output_MM1$Throughput)))
      }
    }
  }
}

df = df[-1,]
names(df) = c("M", "alpha", "ex", "k" , "fila", "saida")

#Pré-processamento dados
df1$M = as.numeric(df1$M)
df1$alpha = as.numeric(df1$alpha)
df1$ex = as.numeric(df1$ex)
df1$fila = as.numeric(df1$fila)
df1$saida = as.numeric(df1$saida)

df1 = subset(df, df$fila < 0.1)
