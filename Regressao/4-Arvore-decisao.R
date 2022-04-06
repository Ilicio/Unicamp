#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#************************************************************************
#========================================================================
#  INICIO ARVORES DECISAO 
#========================================================================
#************************************************************************
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#========================================================================
#  CRIANDO PARTICOES TESTE E VALIDACAO PARA ARVORES DE DECISAO
#========================================================================

# definindo semente para particoes
set.seed(2020)

# criando particões treino e teste - 80% para treino
treino_index = sample(seq_len(nrow(dados_AD)), size = nrow(dados_AD)*0.8)
TREINO_AD <- dados_AD[treino_index, ]
VALIDACAO_AD <- dados_AD[-treino_index, ]

# mostando tamanho de treinamento e teste
dim (TREINO_AD)
dim (VALIDACAO_AD)


#========================================================================
#  CRIANDO ARVORE
#========================================================================

# ajuste uma arvore de regressão ao conjunto de treinamento
tree.RENTALHOUSES = tree(aluguel~., TREINO_AD)
summary(tree.RENTALHOUSES)

# plotando a arvore
plot(tree.RENTALHOUSES)
text(tree.RENTALHOUSES, pretty=0)

# usando a arvore não podada para fazer previsoes no conjunto de testes
yhat=predict(tree.RENTALHOUSES ,newdata = VALIDACAO_AD)
RENTALHOUSES.test=VALIDACAO_AD[, "aluguel"]
plot(yhat, RENTALHOUSES.test)
abline(0,1)

# Prevendo aluguel no conjunto de dados TESTE_RL
dados_predicao_TESTE_AD = data.frame(cbind(Aluguel=VALIDACAO_AD$aluguel,predicao_teste=yhat))
head(dados_predicao_TESTE_AD)

# gerando gráficos de previsões e comparando com atuais
plot(yhat,type = "l",lty=1.8,col="blue")
plot(VALIDACAO_AD$aluguel,type = "l",lty = 1.8,col = "red",xlab="Predição de Aluguel", ylab="Dados de Teste")
lines(yhat,type = "l",col = "blue")

# encontrando acurácia
min_max_accuracy = mean(apply(dados_predicao_TESTE_AD,1,min) / apply(dados_predicao_TESTE_AD, 1, max))
min_max_accuracy

## calculando erro no conjunto de testes 
MSE_TREE_sem_poda=mean((yhat-RENTALHOUSES.test)^2)


#========================================================================
#  REALIZANDO CROSS VALIDATION
#========================================================================

# realizando cross validation 
vcrz.RENTALHOUSES=cv.tree(tree.RENTALHOUSES, K=20)

# plotando desvio de acordo com tamanho da arvore 
plot(vcrz.RENTALHOUSES$size, vcrz.RENTALHOUSES$dev, type='b') 


#========================================================================
#  REALIZANDO PODA
#========================================================================

# podando a arvore
prune.RENTALHOUSES=prune.tree(tree.RENTALHOUSES, best=8)

# plotando a arvore
plot(prune.RENTALHOUSES)
text(prune.RENTALHOUSES, pretty=0)

# usando a arvore podada para fazer previsoes no conjunto de testes
yhat=predict (prune.RENTALHOUSES,newdata = VALIDACAO_AD)
RENTALHOUSES.test=VALIDACAO_AD[, "aluguel"]
plot(yhat, RENTALHOUSES.test)
abline(0,1)

# Prevendo aluguel no conjunto de dados TESTE_RL
dados_predicao_TESTE_AD_podada = data.frame(cbind(Aluguel=VALIDACAO_AD$aluguel,predicao_teste=yhat))
head(dados_predicao_TESTE_AD_podada)

# gerando gráficos de previsões e comparando com atuais
plot(yhat,type = "l",lty=1.8,col="blue")
plot(VALIDACAO_AD$aluguel,type = "l",lty = 1.8,col = "red",xlab="Predição de Aluguel", ylab="Dados de Teste")
lines(yhat,type = "l",col = "blue")

# encontrando acurácia
min_max_accuracy = mean(apply(dados_predicao_TESTE_AD_podada,1,min) / apply(dados_predicao_TESTE_AD_podada, 1, max))
min_max_accuracy

# calculando erro da arvore com poda no conjunto de testes 
MSE_TREE_com_poda=mean((yhat-RENTALHOUSES.test)^2)


#========================================================================
#  EXECUTANDO BAGGING
#========================================================================

## executando bagging
bag.RENTALHOUSES=randomForest(aluguel~.,data=TREINO_AD, mtry = 11, importance=TRUE)
bag.RENTALHOUSES

# plotando a arvore
plot(bag.RENTALHOUSES)

# usando a arvore para fazer previsoes no conjunto de testes
yhat.bag=predict (bag.RENTALHOUSES ,newdata = VALIDACAO_AD)
plot(yhat.bag, RENTALHOUSES.test)
abline(0,1)

# Prevendo aluguel no conjunto de dados TESTE_AD
dados_predicao_TESTE_AD_bagging = data.frame(cbind(Aluguel=VALIDACAO_AD$aluguel,predicao_teste=yhat.bag))
head(dados_predicao_TESTE_AD_bagging)

# gerando gráficos de previsões e comparando com atuais
plot(yhat,type = "l",lty=1.8,col="blue")
plot(VALIDACAO_AD$aluguel,type = "l",lty = 1.8,col = "red",xlab="Predição de Aluguel", ylab="Dados de Teste")
lines(yhat.bag,type = "l",col = "blue")

# encontrando acurácia
min_max_accuracy = mean(apply(dados_predicao_TESTE_AD_bagging,1,min) / apply(dados_predicao_TESTE_AD_bagging, 1, max))
min_max_accuracy

# calculando erro da arvore com poda no conjunto de testes 
MSE_TREE_com_bagging=mean((yhat.bag-RENTALHOUSES.test)^2)

## observando a importancia de cada variavel e plotando
importance(bag.RENTALHOUSES)
varImpPlot(bag.RENTALHOUSES)

#========================================================================
#  EXECUTANDO RANDOM FOREST
#========================================================================

## executando o randomForest com o argumento mtry = 3 ( randomForest() utiliza m = p/3 por padrão, mas faremos com mais)
rf.RENTALHOUSES=randomForest(aluguel~.,data=TREINO_AD, importance=TRUE)
rf.RENTALHOUSES

## usando a arvore para fazer previsoes no conjunto de testes
yhat.rf=predict (rf.RENTALHOUSES ,newdata = VALIDACAO_AD)
plot(yhat.rf, RENTALHOUSES.test)
abline(0,1)

# Prevendo aluguel no conjunto de dados TESTE_AD
dados_predicao_TESTE_AD_RF = data.frame(cbind(Aluguel=VALIDACAO_AD$aluguel,predicao_teste=yhat.rf))
head(dados_predicao_TESTE_AD_RF)

# gerando gráficos de previsões e comparando com atuais
plot(yhat,type = "l",lty=1.8,col="blue")
plot(VALIDACAO_AD$aluguel,type = "l",lty = 1.8,col = "red",xlab="Predição de Aluguel", ylab="Dados de Teste")
lines(yhat.rf,type = "l",col = "blue")

# encontrando acurácia
min_max_accuracy = mean(apply(dados_predicao_TESTE_AD_RF,1,min) / apply(dados_predicao_TESTE_AD_RF, 1, max))
min_max_accuracy

# calculando erro da arvore com poda no conjunto de testes 
MSE_TREE_com_randomForest=mean((yhat.rf-RENTALHOUSES.test)^2)

## observando a importancia de cada variavel e plotando
importance(rf.RENTALHOUSES)
varImpPlot(rf.RENTALHOUSES)

#************************************************************************
#========================================================================
#  FIM ARVORES DE DECISAO
#========================================================================
#************************************************************************


#========================================================================
#  APRESENTANDO MSE DE TODOS MODELOS 
#========================================================================

# exibindo valores dos erros para cada modelo 
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
MSE_TREE_sem_poda
MSE_TREE_com_poda
MSE_TREE_com_bagging
MSE_TREE_com_randomForest
