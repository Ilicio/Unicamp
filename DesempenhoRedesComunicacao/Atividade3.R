############################
######## BIBLIOTECAS########
############################

# Instalação e carregamento
install.packages("queueing")
library(queueing)


###########################
######## EXERICIO 1########
###########################

######## Exercicio 1-A ########

# Exercicio 1-A para ATENDIMENTO
# Calculando erland-B para 4 servidores e 3.9 erlangs
prob_bloqueio_atendimento = queueing::B_erlang(c = 4, u = 3.9)

queueing::B_erlang(c = 4, u = 3.9)

# Criando fila M/M/4/4 (Atendimento)
# lambda atendimento = 3.9/1.5 = 2.6
# mu = 1/1.5 = 0.6666
fila_atendimento = NewInput.MMCK(lambda = 2.6, mu = 1/1.5, c = 4, k = 4)

# Criando modelo de fila
modelo_atendimento = QueueingModel(fila_atendimento)

# Imprimindo relatório do modelo de fila
Report(modelo_atendimento)

######## Exercicio 1-B e C ########
# Dos 2.6 erlangs, temos 30% de perda, logo teremos 70% de entrada, que equivale a 1.82 erlangs.
# Desses 1.82 erlangs, 30% será para pagamentos, 50% para consultas e 20% para aplicações.

# Criando fila M/M/3 (Pagamentos)
# mu = 1/1.25 = 0.8
# lambda pagamento = 1.82*0.3 = 0.546
fila_pagamento = NewInput.MMC(lambda = 0.546, mu = 0.8, n = 0, c = 3)

# Criando modelo de fila
modelo_pagamento = QueueingModel(fila_pagamento)

# Imprimindo relatório do modelo de fila
Report(modelo_pagamento)

# Criando fila M/M/1 (Consultas)
# mu = 1/0,67 = 1.49
# lambda consultas = 1.82*0.5 = 0.91
fila_consultas = NewInput.MM1(lambda = 0.91, mu = 1.49, n = 0)

# Criando modelo de fila
modelo_consultas = QueueingModel(fila_consultas)

# Imprimindo relatório do modelo de fila
Report(modelo_consultas)

# Criando fila M/M/1 (Aplicações)
# mu = 1/1.5 = 0.6666
# lambda aplicacoes = 1.82*0.2 = 0.364
fila_aplicacoes = NewInput.MM1(lambda = 0.364, mu = 0.6666, n = 0)

# Criando modelo de fila
modelo_aplicacoes = QueueingModel(fila_aplicacoes)

# Imprimindo relatório do modelo de fila
Report(modelo_aplicacoes)

#############################
######## EXERICIO 2 #########
#############################

# Criando fila M/M/1 (Pagamento)
# mu = 1/3 = 0.3333
# lambda pagamento = 1/6 = 0.167
fila_pagamento = NewInput.MM1(lambda = 0.167, mu = 0.3333, n = 0)

# Criando modelo de fila
modelo_pagamento = QueueingModel(fila_pagamento)

# Imprimindo relatório do modelo de fila
Report(modelo_pagamento)

# Criando fila M/M/2 (Aspiração)
# mu = 1/5 = 0.2
# lambda aspiracao = 1/6(0.2) = 0.0333
fila_aspiracao = NewInput.MMC(lambda = 0.0333, mu = 0.2, n = 0, c = 2)

# Criando modelo de fila
modelo_aspiracao = QueueingModel(fila_aspiracao)

# Imprimindo relatório do modelo de fila
Report(modelo_aspiracao)

# Criando fila M/M/1 (Lavagem)
# mu = 1/3 = 0.3333
# lambda lavagem = 1/6 = 0.167
fila_lavagem = NewInput.MM1(lambda = 0.167, mu = 0.3333, n = 0)

# Criando modelo de fila
modelo_lavagem = QueueingModel(fila_lavagem)

# Imprimindo relatório do modelo de fila
Report(modelo_lavagem)

# Criando fila M/M/1 (Secagem)
# mu = 1/1 = 1
# lambda secagem = 1/6 = 0.167
fila_secagem = NewInput.MM1(lambda = 0.167, mu = 1, n = 0)

# Criando modelo de fila
modelo_secagem = QueueingModel(fila_secagem)

# Imprimindo relatório do modelo de fila
Report(modelo_secagem)

