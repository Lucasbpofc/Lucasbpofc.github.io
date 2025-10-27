##
##  Testes de Hipoteses em MLGs
##
##  Testes: Wald, TRV, Escore 
##
##  PROGRAMMER:  Sebastiao M. Filho
##
## ====================================================================
#
#
rm(list=ls())  
ls()
#
#
#install.packages("openxlsx")
require(openxlsx)
dados<-read.xlsx("3Dados_testes.xlsx")
dados
attach(dados)
#
#=============================
### Ajustando o MLG  ###
#=============================
#
fit1 <- glm(y ~ x1 + x2,family = binomial(link = "logit"),data = dados)
summary(fit1)
lk1<-as.numeric(logLik(fit1));lk1
vcov(fit1) #matriz de variancias e covariancias das estimativas dos parametros
#summary(fit1)$cov.scaled
#
#Retirar X2 que é igual a testar Beta2=0
fit2 <- glm(y ~ x1,family = binomial(link = "logit"),data = dados)
summary(fit2)
lk2<-as.numeric(logLik(fit2));lk2
vcov(fit2)
#
#Retirar X1 e X2 que é igual a testar Beta1=Beta2=0
fit3 <- glm(y ~ 1,family = binomial(link = "logit"),data = dados)
summary(fit3)
lk0<-as.numeric(logLik(fit3));lk0
vcov(fit3)
#
#========================
#TESTES DE HIPOTESES
#========================
#
#==============
# Teste Wald 
#==============
#
### Para um unico parametro (Ho: Bk = 0)
#
# O resultado do teste de Wald, para cada parametro, ja e apresentado 
# no summary do modelo.
#
### Teste Wald - para mais de um parametro (Ho: B1=B2=0)
#
B00 <- matrix(coef(fit1)[c(2,3)]); B00
CovB00 <- vcov(fit1)[c(2,3),c(2,3)]; CovB00
W <- t(B00) %*% solve(CovB00) %*% B00;W
#
### Como estamos testando dois parâmetro, a distribuição de referência
### para o teste é a qui-quadrado com 2 graus de liberdade. Vamos obter o 
### valor crítico para um nível de significância de 5%:
#
X2tab<-qchisq(0.95, df = 2, lower.tail = T);X2tab  # W > X2tab
#
#Vamos calcular o p-valor do teste.
#
pchisq(W, df = 2, lower.tail = FALSE) 
#
# OU
require(lmtest)
waldtest(fit3,fit1, test = 'Chisq')
#
#
#=============
#Teste TRV
#=============
#
#Hipotese: beta2=0
LRT<- -2*(lk2-lk1);LRT
#qchisq(p, df, ncp = 0, lower.tail = TRUE, log.p = FALSE)# valor tabelado
qchisq(0.95,1,lower.tail = TRUE)
#pchisq(q, df, ncp = 0, lower.tail = TRUE, log.p = FALSE)# valor p
pchisq(LRT,1,lower.tail = FALSE)
#ou
anova(fit2,fit1,test="LRT")# “Rao”, “LRT”, “Chisq”, “F”, “Cp”
#
### usando a função drop #retira uma variavel por vez do modelo
drop1(fit1, test = 'Chisq')
# none - nenhuma variavel foi retirada do modelo
# x1 - retira x1 e ajusta o modelo com as demais variaveis,calcula deviance e AIC.
#      Depois faz um teste TRV p/ verif a import da var. retirada. 
# x2 - idem
#==================
#Teste Escore
#==================
#
#Teste H0: Beta1=0
SR<-anova(fit3,fit2, test="Rao");SR
#
#Teste H0: Beta2=0
SR<-anova(fit2,fit1, test="Rao");SR
#
#Teste H0: Beta1=Beta2=0
SR<-anova(fit3,fit1, test="Rao");SR
#
#
# Usar o pacote statmod (sem precisar estimar o modelo irrestito)
# (testa cada parametro separadamente)
#
# Inicia a partir do modelo nulo
library(statmod) # Provides glm.scoretest
fit3 <- glm(y ~ 1,family = binomial(link="logit"),data=dados)# Mod NULO
#
#Acresentar x1 ao modelo nulo (Ho: Beta1=0) 
z1 <- glm.scoretest(fit3, dados$x1)
pval1 <- 2 * pnorm( abs(z1), lower.tail=FALSE)
round( c(score.stat=z1, P=pval1), 4)
z1^2 #resultado em Qui-quadrado
#
#Acresentar x2 ao modelo com x1 (Ho: Beta2=0)
fit2 <- glm(y ~ x1,family = binomial(link = "logit"),data = dados)
z2 <- glm.scoretest(fit2, dados$x2)
pval2 <- 2 * pnorm( abs(z2), lower.tail=FALSE)
round( c(score.stat=z2, P=pval2), 4)
z2^2 #resultado em Qui-quadrado
#
#========================
#INTERVALOS DE CONFIANCA
#========================
#
#============================
# IC-Baseado no teste de Wald
#============================
#
fit1 <- glm(y ~ x1 + x2,family = binomial(link = "logit"),data = dados)
summary(fit1)
lk1<-as.numeric(logLik(fit1));lk1
vcov(fit1)
# 
beta1<-coef(fit1)[2];beta1
Vbeta1<-vcov(fit1)[2,2];Vbeta1
IC_beta1<-c(beta1-1.96*sqrt(Vbeta1),beta1+1.96*sqrt(Vbeta1));IC_beta1
#
#OU
confint.default(fit1, parm="x1",level=0.95)
confint.default(fit1,level=0.95)#para todos os betas
#
#Se quisermos um IC(95%) para o exp(beta1),basta exponenciar os limites do IC_beta1
exp(IC_beta1)
#
#==========================================
# IC-Baseado na Verossimilhança Perfilada
#==========================================
#
# IC para x1
# Precisamos obter o valor da (log) verossimilhança maximizada
# para um grid de valores
Bx1_grid<-seq(0,8,length.out=20)#baseado na estimativa de Bx1=3.0144
Bx1_grid
#
### Agora, para cada valor de Bx1 em Bx1_grid vamos ajustar novamente
### o glm, mas fixando Bx1. Para cada ajuste vamos extrair o valor da 
### log-verossimilhança maximizada. Antes, vamos aplicar esse procedimento
### a um particular valor de Bx1, para fins ilustrativos. Tomemos
### Bx1=3.0144.
#
ajuste<-glm(y~offset(3.0144*x1)+x2, family = binomial(link = "logit"),
            data = dados)
logLik(ajuste) # log-verossimilhança maximizada para Bx1=3.0144.
#
#Agora, vamos aplicar igual procedimento para o grid de valores de Bx1.
#
vet_logLik <- numeric()
for(i in 1:length(Bx1_grid)){
  Bx1 <- Bx1_grid[i]
  ajuste<-glm(y~offset(Bx1*x1)+x2, family = binomial(link = "logit"),
              data = dados)
  vet_logLik[i] <- logLik(ajuste)
}
#
vet_logLik
max(vet_logLik)
#
### O argumento offset() acrescenta o termo correspondente ao preditor 
### do modelo sem um parâmetro a ser estimado. É a forma usada para fixar
### o valor de beta em cada componente de Bx1_grid.
#
### Agora, vamos calcular o IC baseado na estatistica do teste da razão 
### de verossimilhanças para cada valor de Bx1_grid.
#
#Gráfico do perfil da log-verossimilhança.
#
par(cex = 1.4, las = 1)
plot(Bx1_grid, vet_logLik, type = 'b', pch = 20, xlab = 'Bx1_grid',xaxt="n", 
     ylab = 'Log-verossimilhança Perfilada')
axis(1, at = seq(0, 8, by = 1))
#
# O intervalo de confiança 95% baseado no perfil da verossimilhança irá
# conter todos os valores de Bx10 tais que -2*[l(Bx10)-l(Bx1_chap)]< 3.84,
# em que l(Bx10) é a log-verossimilhança maximizada fixando Bx1 em Bx10,  
# l(Bx1_chap) é a log-verossimilhança maximizada não fixando qualquer 
# particular valor para Bx1, e 3.84 é o quantil 0.95 da distribuição 
# qui-quadrado com um grau de liberdade.
#
abline(h = logLik(fit1) - 3.84/2)
locator()    # para achar as coordenadas do ponto, use apenas o x.
abline(v = c(1.07,6.09), lty = 2, col = 'red')
#
#OU
confint(fit1)
#
#
#==========================================
# IC-Para Média
#==========================================
#
#Perfil: x1=1 e x2=0.55650584
x0 <- matrix(c(1, 1, 0.55650584)) ### Vetor de covariáveis.
### Intercepto, x1, x2.  

betaChap <- coef(fit1);betaChap
### Vetor de estimativas dos parâmetros de regressão.

etaChap <- crossprod(x0, betaChap);etaChap 
### Predição na escala do preditor.

varChap <- vcov(fit1);varChap 
### Matriz de variâncias e covariâncias estimada para os estimadores dos
### parâmetros de regressão.

VarEtaChap <- t(x0) %*% varChap %*% x0;VarEtaChap 
sqrt(VarEtaChap)
### Erro padrão de etaChap.

c(etaChap -1.96 * sqrt(VarEtaChap), etaChap +1.96 * sqrt(VarEtaChap))
### IC 95% (na escala do preditor)

exp(c(etaChap -1.96 * sqrt(VarEtaChap), etaChap +1.96 * sqrt(VarEtaChap))) 
### IC 95% (na escala da resposta)
#
#
### Agora, usando a função predict.
#
pred_link <- predict(fit1, newdata = data.frame(x1 = 1, x2=0.55650584), se.fit = TRUE)
### Declarando se.fit = TRUE, a função retorna também o erro padrão de 
### predição.
pred_link
#
pred_link$fit + c(-1.96, 1.96) * pred_link$se.fit
### IC 95% (na escala do preditor)
#
exp(pred_link$fit + c(-1.96, 1.96) * pred_link$se.fit)
### IC 95% (na escala da resposta)
#
#
















