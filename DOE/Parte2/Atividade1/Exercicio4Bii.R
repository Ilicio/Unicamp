# PLANEJAMENTO

# niveis codificados
niveis = c(-1,1)

# planejamento
planejamento = expand.grid(niveis,niveis,niveis,niveis)

# replicando o planejamento
planejamento = rbind(planejamento,planejamento,planejamento,planejamento)

# nome das colunas
colnames(planejamento) = c("A", "B", "C", "D")

# resposta
y = c(378,415,380,450,375,391,384,426,381,416,371,445,377,391,375,430,
      376,416,379,446,371,390,385,433,381,420,372,448,377,391,376,430,
      379,416,382,449,373,388,386,430,375,412,371,443,379,386,376,428,
      379,417,383,447,369,391,385,431,383,412,370,448,379,400,377,428)


# adicionando resposta ao planejamento
planejamento$y = y

######################################################

# anova usando função aov
anova = aov(y ~ A*B*C*D, planejamento)
anova
anova(anova)

######################################################

# regressão usando função lm
regressao = lm(y ~ A*B*C*D, planejamento)
regressao
summary(regressao)


