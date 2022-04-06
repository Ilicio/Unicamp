###################
#### EXEMPLO 1 ####
###################

# Criação do objeto contendo os dados amostrais
dados1  =  c(1.3,1.6,0.5,1.2,1.1,
            2.2,2.4,0.4,2,1.8,
            1.8,1.7,0.6,1.5,1.3,
            3.9,4.4,2,4.1,3.4)

# Criação dos blocos para produtos
blocos1  = gl(4,5,label=c(paste("produtos",1:4)))

# Criação dos blocos para tratamentos
tratamentos1 = rep(paste("tecido",LETTERS[1:5]),4)

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


###################
#### EXEMPLO 2 ####
###################

# Criação do objeto contendo os dados amostrais
dados2  =  c(2,5,2,5,
            3,7,4,3,
            2,6,5,4,
            4,5,1,3,
            2,5,4,4)

# Criação dos blocos para pesos
blocos2  = gl(5,4,label=c(paste("peso",LETTERS[1:5])))

# Criação dos blocos para tratamentos
tratamentos2 = rep(paste("dieta",1:4),5)

# Criação da tabela com todos os dados reunidos
tabela2 = data.frame(blocos=blocos2, tratamentos=factor(tratamentos2), dados=dados2)

# Criação da análise de variança de dados em função dos tratamentos + blocos
resultado2 = aov(dados2 ~ tratamentos2 + blocos2,tabela2)

# Demonstração do resultado da ANOVA (dois comandos diferentes)
summary(resultado2)

# Teste de Tukey para identificar onde existem diferenças
TukeyHSD(resultado2)

# Configuração de graficos em 1 linha e duas colunas
par(mfrow=c(1,2))

# Demonstração do gráfico de Tukey
plot(TukeyHSD(resultado2))

# Criando um data.frame para boxplot para os pesos
df.pesos = data.frame(Pesos = c("D1", "D2", "D3", "D4"),
                         PesoA = c(2,5,2,5),
                         PesoB = c(3,7,4,30),
                         PesoC = c(2,6,5,40),     
                         PesoD = c(4,5,1,30),        
                         PesoE = c(2,5,4,4))
boxplot(df.pesos$PesoA, df.pesos$PesoB, df.pesos$PesoC, df.pesos$PesoD, df.pesos$PesoE, main ="Kilos perdidos", 
        names = c("Peso A", "Peso B", "Peso C", "Peso D", "Peso E"), xlab = "Pesos", ylab = "Kilos")

# Criando um data.frame para boxplot para as dietas
df.dietas = data.frame(Produtos = c("PesoA", "PesoB", "PesoC", "PesoD", "PesoE"),
                        Dieta1 = c(2,3,2,4,2),
                        Dieta2 = c(5,7,6,5,5),
                        Dieta3 = c(2,4,5,1,4),
                        Dieta4 = c(5,30,40,30,4))
boxplot(df.dietas$Dieta1, df.dietas$Dieta2, df.dietas$Dieta3, df.dietas$Dieta4, main ="Kilos perdidos", 
        names = c("Dieta 1", "Dieta 2", "Dieta 3", "Dieta 4"), xlab = "Dietas", ylab = "Kilos")
