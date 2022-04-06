###########################
### PRODUÇÃO DE MADEIRA ###
###########################

# Criação dos vetores
madeira = c(35,19,31,15,20,30,40,35,18,33,
            31,26,39,27,20,29,45,30,28,43)

# Organização dos grupos EV1 e EV2
tipos = factor(rep(1:2, each = 10), label = c("EV1", "EV2"))

# Análise de variância (ANOVA)
modelo1 = aov(madeira~tipos)

# Demonstração da ANOVA
summary(modelo1)

###########################
### TEMPOS DAS MÁQUINAS ###
###########################

# Criação dos vetores
tempo = c(8.1,8.3,8,8.1,8.1,8.2,
          8.4,8.4,8.3,8.4,8.3,8.4,
          8.8,8.9,8.8,8.8,8.8,8.8,
          8.3,8.2,8.2,8.3,8.3,8.3)

# Organização dos grupos M1, M2, M3 e M4
maquinas = factor(rep(1:4, each = 6), label = c("M1","M2","M3","M4"))

# Análise de variância (ANOVA)
modelo2 = aov(tempo ~ maquinas)

# Demonstração da ANOVA
summary(modelo2)
