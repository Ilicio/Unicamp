########################
###### 5 AMOSTRAS ######
########################

# Definindo a variável que armazenará os dados a serem analisados para 5 amostras
raizes = c(22.7,23.1,20.9,21.6,22.1)

# Média
media = mean(raizes)

# Mediana
mediana = median(raizes)

# Moda (nesse exemplo todas as amostras se repetem apenas uma vez, não temos moda)
moda = table(raizes)

# Aplitude
amplitude = max(raizes)-min(raizes)

# Variância
variancia = var(raizes)

# Desvio padrão
desviopadrao = sd(raizes)

# Erro padrão - antes descobrimos o valor de "n" (número de amostras)
n = length(raizes)
erropadrao = desviopadrao/sqrt(n)

# Coeficiente de variação
coeficiente = desviopadrao/media*100

#########################
###### 10 AMOSTRAS ######
#########################

# Definindo a variável que armazenará os dados a serem analisados para 10 amostras
raizes_completa = c(20.6,21.1,22.7,22.2,23.1,20.9,21.6,21.8,22.1,22.9)

# Média
media_completa = mean(raizes_completa)

# Moda (nesse exemplo todas as amostras se repetem apenas uma vez, não temos moda)
moda_completa = table(raizes_completa)

# Mediana
mediana_completa = median(raizes_completa)

# Aplitude
amplitude_completa = max(raizes_completa)-min(raizes_completa)

# Variância
variancia_completa = var(raizes_completa)

# Desvio padrão
desviopadrao_completa = sd(raizes_completa)

# Erro padrão - antes descobrimos o valor de "n" (número de amostras)
n_completa = length(raizes_completa)
erropadrao_completa = desviopadrao_completa/sqrt(n)

# Coeficiente de variação
coeficiente_completa = desviopadrao_completa/media_completa*100

