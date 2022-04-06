# PLANEJAMENTO

# niveis codificados
niveis = c(-1,1)

# planejamento
planejamento = expand.grid(niveis,niveis)

# replicando o planejamento
planejamento = rbind(planejamento,planejamento,planejamento)

# nome das colunas
colnames(planejamento) = c("A", "B")

# resposta
y = c(26.6,40.9,11.8,34,
      22,36.4,15.9,29,
      22.8,36.7,14.3,33.6)


# adicionando resposta ao planejamento
planejamento$y = y

######################################################

# ANALISE PASSO A PASSO

# matriz de planejamento
X = model.matrix(~X1*X2, data = planejamento[,-3])

# efeitos
efeitos = crossprod(X,y)/(3*2^2/2)

# coeficientes
coeficientes = efeitos/2

# valores ajustados
fitted = X%*%coeficientes

# residuo
residuos = y - fitted

# número de ensaios (N) e de termos no modelo (r)
N = dim(X)[1]
r = dim(X)[2]

# soma dos quadrados dos residuos
SSE = sum(residuos^2)

# soma dos quadrados total
SST = sum(y^2)-sum(y)^2/N

# graus de liberdade dos erros
DFE = N-r

# graus de liberdade total
DFT = N-1

# media dos quadrados dos erros
MSE = SSE/DFE

# media dos quadrados total
MST = SST/DFT

# t calculado
t0 = coeficientes/sqrt(MSE/N)

# t critico
t_critico = qt(0.025, df = DFE, lower.tail = F)

# p value
pvalue = 2*pt(abs(t0), df = DFE, lower.tail = F)

######################################################

# regressão usando função lm
regressao = lm(y ~ A*B, planejamento)
regressao
summary(regressao)

######################################################

# anova usando função aov
anova = aov(y ~ A*B, planejamento)
anova
anova(anova)
