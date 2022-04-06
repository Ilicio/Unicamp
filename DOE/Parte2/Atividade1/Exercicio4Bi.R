# PLANEJAMENTO

# niveis codificados
niveis = c(-1,1)

# planejamento
planejamento = expand.grid(niveis,niveis,niveis,niveis)

# nome das colunas
colnames(planejamento) = c("A", "B", "C","D")

# resposta
y = c(378,416,381,448,372,390,385,430,380,415,371,446,378,392,376,429)


# adicionando resposta ao planejamento
planejamento$y = y

######################################################

# anova usando função aov
anova = aov(y ~ A*B*C, planejamento)
anova
anova(anova)

######################################################

# regressão usando função lm
regressao = lm(y ~ A*B*C, planejamento)
regressao
summary(regressao)


