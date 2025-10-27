################################################################################
##################    Modelagem de eventos extromos          ###################
################################################################################

# Banco de dados utilizados
PRECIP <- c(98.7, 79.7, 46.6, 85.4, 70.0, 75.6, 68.0, 76.8, 66.0, 107.2, 
            148.4, 108.0, 53.4, 95.0, 72.4, 54.8, 63.6, 88.6, 91.2, 102.0, 
            50.6, 88.6, 118.0, 74.0, 118.6, 165.8, 63.0, 63.0, 85.6, 52.0, 
            110.6, 134.8, 50.0, 95.0, 50.4, 80.0, 53.4, 63.6, 90.2, 86.8, 
            83.0, 60.8, 58.1, 77.4, 47.2, 85.4, 58.0, 89.3, 60.0, 52.0, 
            75.8, 155.8, 90.6, 73.0, 52.8, 76.2, 65.6, 90.0, 64.6, 116.8, 
            90.6, 59.0, 101.4, 73.8, 87.4)

# Sequência do período observado
year=seq(1961, 2025, 1)

# Criando a série temporal
precip_ts <- ts(PRECIP, start = min(year), frequency = 1)  # frequência = 1 (dados anuais)

# Plotando a série temporal
plot(precip_ts, type = "o", col = "blue", 
     ylab = "Precipitação", xlab = "Ano", 
     main = "Série Temporal de Precipitação")

# Verifica a presença de autocorrelação
Box.test(precip_ts, type = "Ljung-Box")

# Esse teste verifica a existencia de autocorrelaçao no 10lag
# Para verificar em outros lags basta substituir 10 pelo valor desejado
# Um complemento visual a esteteste é a inclusão dos gráficos ACF  e PACF
# Os gráficos darão um retorno visual corroborando com o teste.

# Gráficos de ACF e PACF
acf(precip_ts)
pacf(precip_ts)

# teste de tendência
library(randtests)
cox.stuart.test(precip_ts)
# H0: A série não apresenta tendência 

# Teste de estacionariedade
library(tseries)
adf.test(precip_ts)
# H0: A série é não estacionária

# Histograma dos dados
hist(PRECIP, breaks = 12, col = "lightblue", 
     main = "Histograma da Precipitação", xlab = "Precipitação")

# Teste de normalidade 

# Kolmogorov-Smirnov
ks.test(precip_ts, "pnorm", mean = mean(precip_ts), sd = sd(precip_ts))

# Estimando os parâmetros da distribuição 

library(ismev)
gev.fit(precip_ts)

# ξ=0, a GEV vira uma distribuição de Gumbel.
# ξ>0, a GEV vira uma distribuição de Fréchet
# ξ<0, a GEV vira uma distribuição de Weibull

B=gev.fit(precip_ts)

gev.diag(B)

# Deve-se ajustar esses valores até encontrar os máximos 

library(extRemes)
fit1 <- fevd(PRECIP, units = "mm")
par(mfrow=c(2,2))
b1<-ci(fit1, method = "proflik", xrange = c(100, 140), verbose = TRUE,, return.period = c(10))
b2<-ci(fit1, method = "proflik", xrange = c(115, 190), verbose = TRUE,, return.period = c(25))
b3<-ci(fit1, method = "proflik", xrange = c(125, 240), verbose = TRUE,, return.period = c(50))
b4<-ci(fit1, method = "proflik", xrange = c(140, 310), verbose = TRUE,, return.period = c(100))