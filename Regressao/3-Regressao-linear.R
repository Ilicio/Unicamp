#========================================================================
# Transformação dos dados
#========================================================================

# transformando atributo 'animal' e 'mobilhada' para inteiro
dados_RL$animal = as.integer(as.factor(dados_RL$animal))
dados_RL$mobilhada = as.integer(as.factor(dados_RL$mobilhada))

# visualizando conjunto de dados transformado
sapply(dados_RL, class)
head(dados_RL)

# transformando atributo 'cidade' em binario
dados_RL = fastDummies::dummy_cols(dados_RL, select_columns = "cidade")
head(dados_RL)

# removendo coluna cidade, pois não será utilizada
dados_RL = subset(dados_RL, select = -c(cidade))

# realocar atributo aluguel para a última posição
colnames(dados_RL)
dados_RL = dados_RL[,c(1,2,3,4,5,6,7,8,10,11,12,13,14,15,16,9)]

# Renomeando nomes das cidades nos atributos 'cidade_0' até 'cidade_4'
atributos_novo = c("area","quartos","banheiros","vagas","andar","animal","mobilhada","condominio","impostos","seguro","sao_paulo","belo_horizonte","rio_de_janeiro","porto_alegre","campinas","aluguel")

# atribuindo colunas no dataset
colnames(dados_RL) = atributos_novo

#visualizando dataset transformado
sapply(dados_RL, class)
head(dados_RL)
dim(dados_RL)

#========================================================================
#  NORMALIZANDO ATRIBUTOS CONTINUOS 
#========================================================================

# normalizando atributos 
#rentalHouses_RL <- transform(rentalHouses_RL, area = (area - min(area)) / (max(area) - min(area)))
#rentalHouses_RL <- transform(rentalHouses_RL, rooms = (rooms - min(rooms)) / (max(rooms) - min(rooms)))
#rentalHouses_RL <- transform(rentalHouses_RL, bathroom = (bathroom - min(bathroom)) / (max(bathroom) - min(bathroom)))
#rentalHouses_RL <- transform(rentalHouses_RL, parking.spaces = (parking.spaces - min(parking.spaces)) / (max(parking.spaces) - min(parking.spaces)))
#rentalHouses_RL <- transform(rentalHouses_RL, floor = (floor - min(floor)) / (max(floor) - min(floor)))
#rentalHouses_RL <- transform(rentalHouses_RL, animal = (animal - min(animal)) / (max(animal) - min(animal)))
#rentalHouses_RL <- transform(rentalHouses_RL, furniture = (furniture - min(furniture)) / (max(furniture) - min(furniture)))
#rentalHouses_RL <- transform(rentalHouses_RL, hoa..R.. = (hoa..R.. - min(hoa..R..)) / (max(hoa..R..) - min(hoa..R..)))
#rentalHouses_RL <- transform(rentalHouses_RL, property.tax..R.. = (property.tax..R.. - min(property.tax..R..)) / (max(property.tax..R..) - min(property.tax..R..)))
#rentalHouses_RL <- transform(rentalHouses_RL, fire.insurance..R.. = (fire.insurance..R.. - min(fire.insurance..R..)) / (max(fire.insurance..R..) - min(fire.insurance..R..)))

#visualizando dataset transformado
#sapply(rentalHouses_RL, class)
#head(rentalHouses_RL)
#dim(rentalHouses_RL)

# correlação entre os dados
cor(dados_RL)
chart.Correlation(dados_RL, histogram=TRUE, pch=19)
correlacao_dados_RL = cor(dados_RL)

corrplot(correlacao_dados_RL,col=brewer.pal(n=8, name="RdYlBu"))
corrplot(correlacao_dados_RL)
corrplot(correlacao_dados_RL, type="upper", order="hclust",col=brewer.pal(n=8, name="RdYlBu"))

#========================================================================
# Criando conjunto de treino e teste
#========================================================================

# definindo semente para particoes
set.seed(2020)

# criando particões treino e teste com 80% e 20%
TREINO = sample(seq_len(nrow(dados_RL)), size = nrow(dados_RL)*0.8)
TREINO_RL = dados_RL[TREINO, ]
TESTE_RL = dados_RL[-TREINO, ]

# mostando tamanho de treinamento e teste
dim(TREINO_RL)
dim(TESTE_RL)

#========================================================================
# Aplicando R^2 ajustado, Cp e BIC no conjunto de TREINO_RL
#========================================================================

# Forcando até 15 variaveis, target é atributo 'aluguel'
regfit.NORMAL=regsubsets(aluguel~.,data=TREINO_RL,nvmax = 15, method = "exhaustive")
regfit.NORMAL=regsubsets(aluguel~area+quartos+banheiros+vagas+andar+animal+mobilhada+condominio+impostos+seguro+sao_paulo+belo_horizonte+rio_de_janeiro+porto_alegre,data = TREINO_RL, nvmax = 14)

summary(regfit.NORMAL)

# Criando sumário
regfit.NORMAL.summary=summary(regfit.NORMAL)

# obtendo melhor conjunto de variaveirs para cada estatística
max_adjr2.NORMAL = which.max(regfit.NORMAL.summary$adjr2)
min_cp.NORMAL = which.min(regfit.NORMAL.summary$cp)
min_bic.NORMAL = which.min( regfit.NORMAL.summary$bic)

# mostrando melhor conjunto de variaveirs para cada estatística
max_adjr2.NORMAL
min_cp.NORMAL
min_bic.NORMAL

# mostrando estatísticas para todos atributos
regfit.NORMAL.summary$adjr2
regfit.NORMAL.summary$cp
regfit.NORMAL.summary$bic

#========================================================================
# Gráficos das estatísticas R^2 ajustado, Cp e BIC
#========================================================================

# criando regiao para graficos
par(mfrow=c(2,3))
par(mfrow=c(1,2))

# plotando R2 ajustado e destadanco o R2 ajustado maximo para o melhor conjunto de variáveis
plot(regfit.NORMAL.summary$adjr2, xlab=" Número de Variáveis", ylab="R^2 ajustado", type="l")
points(max_adjr2.NORMAL, regfit.NORMAL.summary$adjr2[max_adjr2.NORMAL], col="red", cex=2, pch=20)

# plotando Cp e destadanco o Cp minimo para o melhor conjunto de variáveis
plot(regfit.NORMAL.summary$cp, xlab=" Numero Variaveis", ylab="Cp", type="l")
points(min_cp.NORMAL, regfit.NORMAL.summary$cp[min_cp.NORMAL], col="red", cex=2, pch=20)

# plotando BIC e destadanco o BIC minimo para o melhor conjunto de variáveis
plot(regfit.NORMAL.summary$bic, xlab=" Numero Variaveis", ylab="BIC", type="l")
points(min_bic.NORMAL, regfit.NORMAL.summary$bic[min_bic.NORMAL], col="red", cex=2, pch=20)

# mostrando grafico tipo plot  
plot(regfit.NORMAL, scale ="adjr2")
plot(regfit.NORMAL, scale ="Cp")
plot(regfit.NORMAL, scale ="bic")

# mostrando coeficientes do modelo para cada uma das estatisticas
coef(regfit.NORMAL, max_adjr2.NORMAL)
coef(regfit.NORMAL, min_cp.NORMAL)
coef(regfit.NORMAL, min_bic.NORMAL)

#========================================================================
# Seleção progressiva - TREINO_RL
#========================================================================

# usando regsubsets() no conjunto de treino e forcando até 15 variaveis, target é atributo 'aluguel'
regfit.PROG=regsubsets(aluguel~.,TREINO_RL,nvmax=15, method="forward")
regfit.PROG=regsubsets(aluguel~area+quartos+banheiros+vagas+andar+animal+mobilhada+condominio+impostos+seguro+sao_paulo+belo_horizonte+rio_de_janeiro+porto_alegre,data = TREINO_RL, nvmax = 14, method = "forward")
summary(regfit.PROG)

# criando sumário 
regfit.PROG.summary=summary(regfit.PROG)

# obtendo melhor conjunto de variaveirs para cada estatística 
max_adjr2.PROG <- which.max(regfit.PROG.summary$adjr2)
min_cp.PROG <- which.min(regfit.PROG.summary$cp)
min_bic.PROG <- which.min( regfit.PROG.summary$bic)

# mostrando melhor conjunto de variaveis para cada estatística
max_adjr2.PROG
min_cp.PROG
min_bic.PROG

# mostrando estatísticas para todos atributos
regfit.PROG.summary$adjr2
regfit.PROG.summary$cp
regfit.PROG.summary$bic

#========================================================================
# Seleção progressiva - Gráficos das estatísticas R^2 ajustado, Bic e CP
#========================================================================

# criando regiao para graficos
par(mfrow=c(2,3))

# plotando R^2 ajustado e destacando o R2 ajustado maximo para o melhor conjunto de variáveis
plot(regfit.PROG.summary$adjr2, xlab="Número de variáveis", ylab="R^2 ajustado", type="l")
points(max_adjr2.PROG, regfit.PROG.summary$adjr2[max_adjr2.PROG], col="red", cex=2, pch=20)

# plotando Cp e destacando o Cp minimo para o melhor conjunto de variáveis
plot(regfit.PROG.summary$cp, xlab="Número de variáveis", ylab="Cp", type="l")
points(min_cp.PROG, regfit.PROG.summary$cp[min_cp.PROG], col="red", cex=2, pch=20)

# plotando BIC e destacando o BIC minimo para o melhor conjunto de variáveis
plot(regfit.PROG.summary$bic, xlab="Número de variáveis", ylab="BIC", type="l")
points(min_bic.PROG, regfit.PROG.summary$bic[min_bic.PROG], col="red", cex=2, pch=20)

# mostrando grafico tipo plot  
plot(regfit.PROG, scale ="adjr2")
plot(regfit.PROG, scale ="Cp")
plot(regfit.PROG, scale ="bic")

# mostrando coeficientes do modelo para cada uma das estatisticas
coef(regfit.PROG, max_adjr2.PROG)
coef(regfit.PROG, min_cp.PROG)
coef(regfit.PROG, min_bic.PROG)

#========================================================================
# Seleção regressiva - TREINO_RL
#========================================================================

# usando regsubsets() no conjunto de treino e forcando até 15 variaveis, target é atributo 'aluguel'
regfit.REGR=regsubsets(aluguel~.,data=TREINO_RL,nvmax=15, method="backward")
regfit.REGR = regsubsets(aluguel~area+quartos+banheiros+vagas+andar+animal+mobilhada+condominio+impostos+seguro+sao_paulo+belo_horizonte+rio_de_janeiro+porto_alegre,data = TREINO_RL, nvmax = 14, method = "backward")
summary(regfit.REGR)

# criando sumário
regfit.REGR.summary=summary(regfit.REGR)

# obtendo melhor conjunto de variaveirs para cada estatística
max_adjr2.REGR <- which.max(regfit.REGR.summary$adjr2)
min_cp.REGR <- which.min(regfit.REGR.summary$cp)
min_bic.REGR <- which.min( regfit.REGR.summary$bic)

# mostrando melhor conjunto de variaveis para cada estatística
max_adjr2.REGR
min_cp.REGR
min_bic.REGR

# mostrando estatísticas para todos atributos
regfit.REGR.summary$adjr2
regfit.REGR.summary$cp
regfit.REGR.summary$bic

#========================================================================
# Seleção regressiva - Gráficos das estatísticas R^2 ajustado, Bic e CP
#========================================================================

# criando regiao para graficos
par(mfrow=c(2,3))

# plotando R^2 ajustado e destacando o R2 ajustado maximo para o melhor conjunto de variáveis
plot(regfit.REGR.summary$adjr2, xlab=" Numero Variaveis", ylab="R^2 ajustado", type="l")
points(max_adjr2.REGR, regfit.REGR.summary$adjr2[max_adjr2.REGR], col="red", cex=2, pch=20)

# plotando Cp e destacando o Cp minimo para o melhor conjunto de variáveis
plot(regfit.REGR.summary$cp, xlab=" Numero Variaveis", ylab="Cp", type="l")
points(min_cp.REGR, regfit.REGR.summary$cp[min_cp.REGR], col="red", cex=2, pch=20)

# plotando BIC e destacando o BIC minimo para o melhor conjunto de variáveis
plot(regfit.REGR.summary$bic, xlab=" Numero Variaveis", ylab="BIC", type="l")
points(min_bic.REGR, regfit.REGR.summary$bic[min_bic.REGR], col="red", cex=2, pch=20)

# mostrando grafico tipo plot  
plot(regfit.REGR, scale ="adjr2")
plot(regfit.REGR, scale ="Cp")
plot(regfit.REGR, scale ="bic")

# mostrando coeficientes do modelo para cada uma das estatisticas
coef(regfit.REGR, max_adjr2.REGR)
coef(regfit.REGR, min_cp.REGR)
coef(regfit.REGR, min_bic.REGR)

#========================================================================
# Validação cruzada - TREINO_RL
#========================================================================

# criando funcao predict para ser utilizada na validacao cruzada
predict.regsubsets=function(object, newdata, id, ...){
  form=as.formula(object$call[[2]])
  mat=model.matrix(form, newdata)
  coefi = coef(object, id=id)
  xvars=names(coefi)
  mat[,xvars]%*%coefi
}

# gerando k=10 subconjuntos para validacao cruzada 
k=10

# criando indexadores para k conjuntos da validação cruzada
folds=sample(1:k,nrow(TREINO_RL),replace=TRUE)

# criando matriz para armazenar MSE de k conjunto de dados 
VCRZ.errors=matrix(NA,k,14, dimnames=list(NULL, paste(1:14)))

# calculando erro MSE (mean square error) dos subconjuntos da validacao cruzada
# o laço for percorre a lista de atributos variando de 1 a 14 obtidos pelo regsubsets
# os coeficientes dos atributos são armazenados na matriz 'coefi'
# os coeficientes de 'coefi' são multiplicados pelos valores doa atributos da matriz 'train_test.mat"
# os valores preditos são armazenados na matriz 'pred'
# o MSE de cada conjunto de variareis é armazenado no vetor 'validacao.erros' 
# gerando matriz 10 x 14 para os MSEs dos conjuntos de validação cruzada 
for(j in 1:k) {
  best.fit=regsubsets(aluguel~area+quartos+banheiros+vagas+andar+animal+mobilhada+condominio+impostos+seguro+sao_paulo+belo_horizonte+rio_de_janeiro+porto_alegre,data=TREINO_RL[folds!=j,],nvmax=14)
  for(i in 1:14) {
    pred=predict(best.fit,TREINO_RL[folds==j,], id=i)
    VCRZ.errors[j,i]=mean((TREINO_RL$aluguel[folds==j]-pred)^2)
  }
}

# gerando a média dos 14 conjuntos de atributos para as 10 subconjuntos de validacao  
mean.VCRZ.errors=apply(VCRZ.errors, 2, mean)

# obtendo menor erro 
min_MSE_VCRZ=which.min(mean.VCRZ.errors)

# plotando média de valores
par(mfrow=c(1,3))

plot(mean.VCRZ.errors, xlab="Número de variáveis", ylab="MSE Validacao Cruzada", type='l')
points(min_MSE_VCRZ, mean.VCRZ.errors[min_MSE_VCRZ], col="red", cex=2, pch=20)

# selecionando o subconjunto de variaveis no conjunto completo
regfit.VCRZ=regsubsets(aluguel~.,data=TREINO_RL, nvmax=14)
regfit.VCRZ=regsubsets(aluguel~area+quartos+banheiros+vagas+andar+animal+mobilhada+condominio+impostos+seguro+sao_paulo+belo_horizonte+rio_de_janeiro+porto_alegre,data=TREINO_RL, nvmax=14)

# mostrando coeficientes do melhor conjunto de variaveis pela validação cruzada
coef(regfit.VCRZ,min_MSE_VCRZ)

#========================================================================
# Erro nos 10 modelos - TESTE_RL
#========================================================================

# criando uma matriz a partir do modelo de dados de TESTE_RL 
TESTE_RL.mat=model.matrix(aluguel~.,data=TESTE_RL)
TESTE_RL.mat=model.matrix(aluguel~area+quartos+banheiros+vagas+andar+animal+mobilhada+condominio+impostos+seguro+sao_paulo+belo_horizonte+rio_de_janeiro+porto_alegre,data=TESTE_RL)



# calculando MSE do modelo de conjunto de variaveis "NORMAL" com max_adjr2
coefi=coef(regfit.NORMAL, max_adjr2.NORMAL)
pred=TESTE_RL.mat[,names(coefi)]%*% coefi
MSE_NORMAL.max_adjr2 = mean((TESTE_RL$aluguel-pred)^2)

# calculando MSE do modelo de conjunto de variaveis "NORMAL" com min_cp
coefi=coef(regfit.NORMAL, min_cp.NORMAL)
pred=TESTE_RL.mat[,names(coefi)]%*% coefi
MSE_NORMAL.min_cp = mean((TESTE_RL$aluguel-pred)^2)

# calculando MSE do modelo de conjunto de variaveis "NORMAL" com min_bic
coefi=coef(regfit.NORMAL, min_bic.NORMAL)
pred=TESTE_RL.mat[,names(coefi)]%*% coefi
MSE_NORMAL.min_bic = mean((TESTE_RL$aluguel-pred)^2)

# calculando MSE do modelo de conjunto de variaveis "PROGRESSIVO" com max_adjr2
coefi=coef(regfit.PROG, max_adjr2.PROG)
pred=TESTE_RL.mat[,names(coefi)]%*% coefi
MSE_PROG.max_adjr2 = mean((TESTE_RL$aluguel-pred)^2)

# calculando MSE do modelo de conjunto de variaveis "PROGRESSIVO" com min_cp
coefi=coef(regfit.PROG, min_cp.PROG)
pred=TESTE_RL.mat[,names(coefi)]%*% coefi
MSE_PROG.min_cp = mean((TESTE_RL$aluguel-pred)^2)

# calculando MSE do modelo de conjunto de variaveis "PROGRESSIVO" com min_bic
coefi=coef(regfit.PROG, min_bic.PROG)
pred=TESTE_RL.mat[,names(coefi)]%*% coefi
MSE_PROG.min_bic = mean((TESTE_RL$aluguel-pred)^2)

# calculando MSE do modelo de conjunto de variaveis "REGRESSIVO" com max_adjr2
coefi=coef(regfit.REGR, max_adjr2.REGR)
pred=TESTE_RL.mat[,names(coefi)]%*% coefi
MSE_REGR.max_adjr2 = mean((TESTE_RL$aluguel-pred)^2)

# calculando MSE do modelo de conjunto de variaveis "REGRESSIVO" com min_cp
coefi=coef(regfit.REGR, min_cp.REGR)
pred=TESTE_RL.mat[,names(coefi)]%*% coefi
MSE_REGR.min_cp = mean((TESTE_RL$aluguel-pred)^2)

# calculando MSE do modelo de conjunto de variaveis "REGRESSIVO" com min_bic
coefi=coef(regfit.REGR, min_bic.REGR)
pred=TESTE_RL.mat[,names(coefi)]%*% coefi
MSE_REGR.min_bic = mean((TESTE_RL$aluguel-pred)^2)

# calculando MSE do modelo de conjunto de variaveis "VALIDACAO CRUZADA"
coefi=coef(regfit.VCRZ, min_MSE_VCRZ)
pred=TESTE_RL.mat[,names(coefi)]%*% coefi
MSE_VCRZ = mean((TESTE_RL$aluguel-pred)^2)

# exibindo valores dos erros para cada um dos 10 modelos
MSE_NORMAL.max_adjr2
MSE_NORMAL.min_cp
MSE_NORMAL.min_bic

MSE_PROG.max_adjr2
MSE_PROG.min_cp
MSE_PROG.min_bic

MSE_REGR.max_adjr2
MSE_REGR.min_cp
MSE_REGR.min_bic

MSE_VCRZ

#========================================================================
# Criação do modelo usando atributos da validação cruzada
#========================================================================
  
#Criando modelo no TREINO_RL com os 10 atributos da validação cruzada
modelo_VC = lm(aluguel ~ area + quartos + banheiros + vagas + andar + mobilhada + condominio + seguro + sao_paulo + belo_horizonte + porto_alegre,data = TREINO_RL)
summary(modelo_VC)

# criando preditor no TESTE_RL
preditor = predict(modelo_VC,TESTE_RL)
preditor

# gerando gráficos de previsões e comparando com atuais
plot(preditor,type = "l",lty=1.8,col="blue")
plot(TESTE_RL$aluguel,type = "l",lty = 1.8,col = "red",xlab="Predição de Aluguel", ylab="Dados de Teste")
lines(preditor,type = "l",col = "blue")

# Prevendo aluguel no conjunto de dados TREINO_RL
dados_predicao_TREINO_RL = TREINO_RL %>% 
  mutate(predicao_treino=predict(modelo_VC))
head(dados_predicao_TREINO_RL)

# Prevendo aluguel no conjunto de dados TESTE_RL
dados_predicao_TESTE_RL = data.frame(cbind(Aluguel=TESTE_RL$aluguel,predicao_teste=preditor))
head(dados_predicao_TESTE_RL)

# gerando correlação entre os atributos
correlation_accuracy = cor(dados_predicao_TESTE_RL)

# encontrando acurácia
min_max_accuracy = mean(apply(dados_predicao_TESTE_RL,1,min) / apply(dados_predicao_TESTE_RL, 1, max))
min_max_accuracy
