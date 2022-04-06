###################
#### EXEMPLO 1 ####
###################

# Criação do objeto contendo os dados amostrais
dados = c(99,95,94,99,99,
          92,99,205,102,111,
          93,113,101,102,96,
          101,106,101,98,97,
          97,105,95,104,104)


# Criação dos blocos para tratamentos
tratamentos = rep(paste(LETTERS[1:5]),5)
colunas = rep(LETTERS[1:5],5)
linhas = rep(LETTERS[1:5],each=5)

tratamentos = as.factor(tratamentos)
colunas = as.factor(colunas)
linhas = as.factor(linhas)

# Empilhamento dos dados
dados.stack = data.frame(linhas,colunas,tratamentos,dados)

str(dados.stack)

anova = aov(dados ~ colunas + tratamentos + linhas,dados.stack)
regressao = lm(dados ~ colunas + tratamentos+linhas,dados.stack)


anova(regressao)
anova(anova)
summary(anova)
summary(regressao)



# Criação da tabela com todos os dados reunidos
tabela1 = data.frame(blocos=blocos1, tratamentos=factor(tratamentos1), dados=dados1)

# Criação da análise de variança de dados em função dos tratamentos + blocos
resultado1 = aov(dados1 ~ tratamentos1 + blocos1,tabela1)

# Demonstração do resultado da ANOVA (dois comandos diferentes)
summary(resultado1)

# Teste de Tukey para identificar onde existem diferenças
TukeyHSD(resultado1)

# Configuração de graficos em 1 linha e duas colunas
par(mfrow=c(1,2))

# Demonstração do gráfico de Tukey
plot(TukeyHSD(resultado1))

# Criando um data.frame para boxplot para os produtos
df.produtos = data.frame(Produtos = c("T1", "T2", "T3", "T4", "T5"),
                         P1 = c(1.3, 1.6, 0.5, 1.2, 1.1),
                         P2 = c(5.7, 2.2, 2.4, 0.4, 2.0),
                         P3 = c(1.8, 1.7, 0.6, 1.5, 1.3),        
                         P4 = c(3.9, 4.4, 2.0, 4.1, 3.4))
boxplot(df.produtos$P1, df.produtos$P2, df.produtos$P3, df.produtos$P4, main ="Efeito de Produtos Químicos", 
        names = c("P1", "P2", "P3", "P4"), xlab = "Produtos", ylab = "Efeitos")

# Criando um data.frame para boxplot para os tecidos
df.tecidos = data.frame(Produtos = c("P1", "P2", "P3", "P4"),
                        T1 = c(1.3, 5.7, 1.8, 3.9),
                        T2 = c(1.6, 2.2, 1.7, 4.4),
                        T3 = c(0.5, 2.4, 0.6, 2.0),        
                        T4 = c(1.2, 0.4, 1.5, 4.1),
                        T5 = c(1.1, 2.0, 1.3, 3.4))
boxplot(df.tecidos$T1, df.tecidos$T2, df.tecidos$T3, df.tecidos$T4, df.tecidos$T5, main ="Efeito de Produtos Químicos", 
        names = c("T1", "T2", "T3", "T4", "T5"), xlab = "Tecidos", ylab = "Efeitos")
