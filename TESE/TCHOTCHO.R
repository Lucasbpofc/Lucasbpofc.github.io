
# Criando o vetor de precipitações máximas
PRECIP <- c(
  63.9, 70.2, 75.8, 108.1, 86.4, 68.9, 46.2, 54.4, 62.5, 82.9, 66.8, 61.4,
  62.2, 73.1, 150.3, 69.0, 59.8, 54.1, 83.0, 58.6, 102.7, 59.9, 92.8, 70.9,
  79.2, 68.1, 95.4, 47.6, 62.0, 155.9, 66.0, 89.4, 72.6, 68.6, 60.9, 78.0,
  68.7, 57.4, 56.0, 89.6, 80.4, 246.4, 64.0, 120.7, 91.0, 102.1, 49.2, 96.5,
  104.4, 72.0, 69.8, 145.0, 82.6, 82.2, 75.8, 137.6, 70.2, 58.0, 64.2, 93.6,
  75.6, 96.4, 44.8, 91.4, 67.2
)

# Verificando o tamanho e a estrutura do vetor
length(PRECIP)
summary(PRECIP)

year=seq(1961, 2025, 1)

# Criando a série temporal
precip_ts <- ts(PRECIP, start = min(year), frequency = 1)  # frequência = 1 (dados anuais)

# Plotando a série temporal
plot(precip_ts, type = "o", col = "blue", 
     ylab = "Precipitacao", xlab = "Ano", 
     main = "Serie Temporal de Precipitacao")

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

# teste de tendência mais robusto
library(Kendall)
MannKendall(precip_ts)

# Teste de estacionariedade
library(tseries)
adf.test(precip_ts)
# H0: A série é não estacionária

# Histograma dos dados
hist(PRECIP, breaks = 12, col = "lightblue", 
     main = "Histograma da Precipitacao", xlab = "Precipitacao")

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

# ==============================================================================
# TESTE DE ADEQUAÇÃO (GOF) - DADOS vs DISTRIBUIÇÃO GEV
# ==============================================================================

library(extRemes)

library(goftest)

# EXTRATINDO OS PARÂMETROS DO MODELO
loc_est   <- fit1$results$par["location"]
scale_est <- fit1$results$par["scale"]
shape_est <- fit1$results$par["shape"]

teste_ks <- ks.test(precip_ts, "pevd", 
                    loc = loc_est, 
                    scale = scale_est, 
                    shape = shape_est)

teste_ad <- ad.test(precip_ts, "pevd", 
                    loc = loc_est, 
                    scale = scale_est, 
                    shape = shape_est)

print(teste_ks)
print(teste_ad)

# CÁLCULO DOS NÍVEIS DE RETORNO 

library(extRemes)

#  NÍVEIS DE RETORNO 

df_precip <- data.frame(Chuva = precip_ts)

# 2. Ajustamos o modelo M0 "avisando" o pacote de onde vêm os dados
# Note que agora usamos a fórmula (Chuva) e o argumento data (df_precip)
fit_gev <- fevd(Chuva, data = df_precip, type = "GEV")

# 3. Agora o cálculo dos Níveis de Retorno vai rodar perfeitamente!
niveis_retorno_m0 <- return.level(fit_gev, 
                                  return.period = c(10, 50, 100), 
                                  do.ci = TRUE)

print(niveis_retorno_m0)

# Gráfico sem forçar os nomes dos eixos para evitar o conflito
plot(fit_gev, type = "rl", 
     main = "Curva de Nivel de Retorno - GEV Estacionario",
     pch = 19, col = "black")
