################################################################################
#############      Modelagem da volatilidade dos preços da soja     ############
#############      autor: Lucas Pereira Belo                        ############              
################################################################################

#
rm(list=ls())
ls()

#Bibliotecas utilizadas

library(readxl)      # Importação dos dados
library(tseries)     # Para o teste PP
library(readxl)      # Para carregar o arquivo em xlsx 
library(fracdiff)    # Para estimar o valor de d
library(forecast)
library(descomponer) # Função periodograma    
library(TSA)         # Função perodogram
library(lmtest)
library(arfima)
library(moments)

soja_semanal <- read_excel("soja_semanal.xlsx")
soja<-soja_semanal
attach(soja); tail(soja)

###### Série e a decomposição de suas componentes

y<-dolar
rm(y)

y = ts(soja$dolar, start = c(2011,1), end = c(2023,12), freq = 52)
plot(y, xlab= c("Tempo"),ylab = c("Preço (R$)"))

##### Série de Log retornos

y1 = ts(diff(log(y)), start = c(2011,1), end = c(2023,12), freq = 52) # r = ln[y_t/y_(t-1)]
plot(y1, xlab= c("Tempo"),ylab = c("Log retornos"), col = "blue")
title("Série de log retornos dos preços da soja")

par(mfrow = c(2,1))
plot.ts(y)
plot.ts(y1)

###### Verificando ACF e PACF

n = length(y) #tamanho da amostra
par(mfrow = c(2,2))
acf(y, n)
acf(y1, n)
pacf(y, n)
pacf(y1, n)

###### Teste de Cox-Stuart

library("randtests") # Função cox.stuart.test
cox.stuart.test(y1)

############## Verificando se há Tendência Determinística na série

library("randtests")
cox.stuart.test(y1)
model_t = tslm(y1 ~ trend); summary(model_t)

#### ESTACIONARIDADE

### Teste Phillips-Perron (PP) ###
teste_pp <- pp.test(y1)
cat("Resultado do Teste Phillips-Perron (PP):\n")
print(teste_pp)

############## Teste de Raíz Unitária de Dickey-Fuller e Phillips-Perron

library("tseries")
library("urca")
adf.test(y1)
pp.test(y1)
summary(ur.df(y1, type = "trend", selectlags = "AIC"))
summary(ur.pp(y1, type = "Z-tau", model = "trend", lags = "short"))

############## Teste de Dependência Longa
install.packages("pracma")
install.packages("fracdiff")
library("pracma")
library("fracdiff")

hurst = hurstexp(y1)
(d = hurst[[1]] - 0.5)
fracdiff(y1)
y2 = ts(diffseries(y1, d = 0.01173613), start = c(2008,1), end = c(2023,12), frequency = 12)

par(mfrow = c(3,3))
plot.ts(y)
plot.ts(y1)
plot.ts(y2)
acf(y, n)
acf(y1, n)
acf(y2, n)
pacf(y, n)
pacf(y1, n)
pacf(y2, n)

############## Teste de Raíz Unitária Sazonal

library("uroot")

hegy = hegy.test(y1, deterministic = c(1,1,1), lag.method = c("AIC"))
summary(hegy)

library(uroot)
# O argumento 'type' pode ser "trig" (trigonométrico) ou "dummy"
teste_ch <- ch.test(y1, type = "trig")

summary(teste_ch)


############## Verificando se há Ciclos na série 

dt = 1
n = length(y1)
k = seq(0, n - 1, 1)
fk = (k/(n*dt)) # frequência de fourrier de interesse (simetricas)
TDF = fft(y1) # Transformada Discreta de Fourrier
MAG2 = Mod(TDF)^2 # Magnitude ao quadrado
periodograma = cbind(fk[1:((n-1)/2)],MAG2[1:((n-1)/2)])
plot(periodograma, type = "h", ylab = "Magnitude da Frequencia", xlab = "Frequencia de Fourrier")

(max_fk = periodograma[,1][periodograma[,2] == max(periodograma[,2])])
(wk = (2*pi*max_fk))
(pk = 2*pi/wk) # pk = 1/fk ou pk = 2pi/wk

###### Teste G de Fisher
library("ptest") # Teste G

(t_fisher = ptestg(y2, method = "Fisher")) # p > alpha => periodicidade não significativa
(wjj = 2*pi*t_fisher$freq)

t = seq(1,n)
sen = sin(wjj*t)
cos = cos(wjj*t)
Xs = cbind(sen)
library(forecast)
arima = Arima(y1, order = c(1,0,1),include.mean = F); summary(arima); coeftest(arima)

y_hat = arima$fitted
plot(y1, col = "black", type = "l")
lines(y_hat, col = "red", add = T)

auto.arima(y1)
# Certifique-se que sua série y1 é um objeto 'ts' com a frequência correta
# Ex: y1 <- ts(seus_dados, frequency = 12, start = c(2008, 1))

# Ajusta o modelo SARIMA(1,0,1)(0,1,1) com frequência 52
sarima <- Arima(y1,
                  order = c(1, 0, 1),        # Ordem não-sazonal (p,d,q)
                  seasonal = c(0, 1, 1),     # Ordem sazonal (P,D,Q)
                  include.mean = FALSE)


y_hat = sarima$fitted
plot(y1, col = "black", type = "l")
lines(y_hat, col = "red", add = T)

# Agora sim, você analisa o modelo SARIMA completo
summary(sarima)
coeftest(sarima)

# Iniciando a Análise Residual 
rm = residuals(sarima)
hist(rm)

shapiro.test(rm) # Não rejeitar H0 indica normalidade
Box.test(rm, 15, type = c("Box-Pierce")) # Não Rejeitar H0 indica ausência de correlação
Box.test(rm^2, 15, type = c("Box-Pierce")) # Não Rejeitar H0 indica variância Constante

par(mfrow = c(2,1))
acf(rm, n); pacf(rm, n)
acf(rm^2, n); pacf(rm^2, n)
qqnorm(rm); qqline(rm, col = "red"); hist(rm, prob = F)

# Modelo ARCH
# Carregar o pacote fGarch
library(fGarch)

model_arch = garchFit(~garch(1, 0), include.mean = TRUE, data = rm, trace = FALSE, cond.dist = c("sstd"))
summary(model_arch)
# O modelo ARCH não conseguiu capturar a heterocedasticidade no quadrado dos resíduos
# Testei outras distribuições, porém nenhuma delas contribuiu.

# Modelo GARCH
library(fGarch)
arch = garchFit(~ garch(1,1), include.mean = F, data = rm, trace = F, cond.dist = c("snorm"))
summary(arch)
plot(arch) # 10, 11, 1337.903.873 26.594.653

# norm -> AIC = -4.281948  ,BIC = -4.253927  log Likelihood = 1365.659
#snorm -> AIC = -4.292517  ,BIC = -4.257492  log Likelihood = 1370.021 
# std  -> AIC = -4.288165  ,BIC = -4.253140  log Likelihood = 1368.636 
#sstd  -> AIC = -4.293187  ,BIC = -4.251157  log Likelihood = 1371.233
#ged   -> AIC = -4.289182  ,BIC = -4.254157  log Likelihood = 1368.96
#sged  -> AIC = -4.294868  ,BIC = -4.252837  log Likelihood = 1371.768
#snig  -> AIC = -4.298018  ,BIC = -4.255988  log Likelihood = 1372.77
#QMLE  -> AIC = -4.281948  ,BIC = -4.253927  log Likelihood = 1365.659

#cond.dist = c("norm", "snorm", "ged", "sged",
#"std", "sstd", "snig", "QMLE"),
# O melhor modelo foi o com a distribuição snig, porém, ainda apresentou leves indícios
# de heterocedasticidade, com um valor p muito próximo de 5%. 

cbind(1,1)
prev = predict(arch, h = 3, plot = T, trace = T)
prev

#Como alternativa, eu ajustei outros modelos 


##### Modelo EGARCH
library(rugarch)

spec_egarch <- ugarchspec(
  variance.model = list(model = "eGARCH", garchOrder = c(1,1)),
  mean.model = list(armaOrder = c(1,1), include.mean = TRUE),
  distribution.model = "snorm" # Distribuição assimétrica com caudas pesadas (skewed Student's t)
)
fit_egarch <- ugarchfit(spec = spec_egarch, data = rm)   
fit_egarch

# Ajustar o modelo EGARCH aos dados
fit_egarch <- ugarchfit(spec = spec_egarch, data = rm) 

# Exibir o resumo do modelo ajustado
print(fit_egarch)

## TESTANDO O "apARCH" (Asymmetric Power ARCH)

spec <- ugarchspec(variance.model = list(model = "apARCH", garchOrder = c(1, 1)), 
                   mean.model = list(armaOrder = c(1, 1), include.mean = TRUE),
                   distribution.model = "snorm")

fit_ugarchspec<-ugarchfit(spec = spec, data = rm)
fit_ugarchspec
plot(fit_ugarchspec)
# O Modelo apARCH se mostrou mais eficiente para solucionar o problema de
# heterocedasticidade.  

# norm -> AIC = -4.2836  ,BIC = -4.2276  log Likelihood = 1370.187 
#snorm -> AIC = -4.2921  ,BIC = -4.2290  log Likelihood = 1373.873 
# std  -> AIC = -4.2873  ,BIC = -4.2243  log Likelihood = 1372.373 
#sstd  -> AIC = -4.2928  ,BIC = -4.2228  log Likelihood = 1375.111 
#ged   -> AIC = -4.2894  ,BIC = -4.2264  log Likelihood = 1373.039 
#sged  -> AIC = -4.2940  ,BIC = -4.2239  log Likelihood = 1375.491

# O modelo com a distribuição sged apresentou o maior valor para o 
# AIC e para o log Likelihood. Por isso foi o modelo escolhido.


