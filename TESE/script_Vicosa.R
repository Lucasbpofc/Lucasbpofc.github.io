################################################################################
############      Lucas Pareira Belo - lucas.p.belo@ufv.br       ###############
################################################################################

# Pacotes utilizados
library(readxl)
library(dplyr)
library(tseries)   
library(trend)     
library(randtests) 
library(goftest)   
library(extRemes)  
library(ismev)     
library(urca)      
library(DT)
library(kableExtra)

# Leitura e Defini√ß√µes
EXTREMOS <- read_excel("EXTREMOS.xlsx")
CHUVA <- EXTREMOS$PRECIP
year <- seq(1968, 2022, 1)

# 1. Encontrando os √≠ndices (posi√ß√µes) dos extremos
idx_max <- which.max(CHUVA) # Posi√ß√£o da maior chuva
idx_min <- which.min(CHUVA) # Posi√ß√£o da menor chuva

# 3. Resumo Estat√≠stico Padr√£o
summary(EXTREMOS$PRECIP)

plot(CHUVA ~ year, type="b", main="S√©rie Hist√≥rica: Precipita√ß√£o (Jan)", 
     ylab="Precipita√ß√£o (mm)", xlab="Ano", pch=19, col="#2980b9", lwd=2)
grid()

hist(CHUVA, main="Distribui√ß√£o de Frequ√™ncias", 
     xlab="Precipita√ß√£o (mm)", col="#3498db", border="white", prob=TRUE)
lines(density(CHUVA), col="#c0392b", lwd=2)

# 1. An·lise Visual de DependÍncia (ACF e PACF)
par(mfrow = c(1, 2)) 

acf(CHUVA, main = "FunÁ„o de AutocorrelaÁ„o (FAC)", ylab = "ACF")
pacf(CHUVA, main = "AutocorrelaÁ„o Parcial (FACP)", ylab = "PACF")

par(mfrow = c(1, 1)) 

# 2. Teste de Estacionariedade (Augmented Dickey-Fuller)
# type = "trend" considera intercepto e tendÍncia na equaÁ„o do teste
teste_adf <- ur.df(CHUVA, type = "trend", selectlags = "AIC")
summary(teste_adf)

# Compare a estatÌstica de teste (tau3) com os valores crÌticos (Critical Values).
# H0: Existe Raiz Unit·ria (N„o estacion·ria).
# Se t-statistic < Critical Value (5%), rejeita-se H0 (SÈrie È Estacion·ria).

# 3. Teste de TendÍncia (Cox-Stuart)
# H0: N„o existe tendÍncia monotÙnica.
cox.stuart.test(CHUVA)

# 4. Teste de Aleatoriedade (Runs Test)
# H0: A sÈrie È aleatÛria.
runs.test(CHUVA)

# Vari·vel global para as funÁıes manuais
dataset <- CHUVA

gev.loglik <- function(theta){
  mu <- theta[1]
  sigma <- theta[2]
  xi <- theta[3]
  
  m <- min((1+(xi*(dataset-mu)/sigma)))
  if(m < 0.00001) return(as.double(1e6))
  if(sigma < 0.00001) return(as.double(1e6))
  
  if(xi == 0){ # Caso Gumbel
    loglik <- (-length(dataset)*log(sigma)-sum((dataset-mu)/sigma)
               -sum(exp(-((dataset-mu)/sigma))))
  } else { # Caso GEV
    loglik <- (-length(dataset)*log(sigma)
               -(1/xi+1)*sum(log(1+(xi*(dataset-mu)/sigma)))
               -sum((1+(xi*(dataset-mu)/sigma))^(-1/xi)))
  }
  return(-loglik)
}


# Chute inicial
theta_start <- c(mean(dataset), sd(dataset), 0.1)

# OtimizaÁ„o via Newton-type algorithm
A <- nlm(gev.loglik, theta_start, hessian=TRUE)
print(A$estimate) # Par‚metros Estimados (Mu, Sigma, Xi)

# Matriz de Vari‚ncia-Covari‚ncia
varcovar <- solve(A$hessian)
cat("Erros Padr„o dos Par‚metros:\n")
print(sqrt(diag(varcovar)))

# FunÁıes auxiliares CDF e Inversa
GEV.DF <- function(data, mu, sigma, xi){
  if(xi==0) { GEV=exp(-exp(-((data-mu)/sigma))) }
  else { GEV=exp(-(1+xi*((data-mu)/sigma))^(-1/xi)) }
  return(GEV)
}

GEV.INV <- function(data, mu, sigma, xi){
  if(xi==0) { INV=mu-sigma*log(-log(1-data)) } # Ajuste teÛrico para p
  else { INV=mu+(sigma/xi)*(((-log(data))^(-xi))-1) }
  return(INV)
}

# PreparaÁ„o dos dados ordenados
ordered <- sort(CHUVA)
n <- length(ordered)
empirical <- (1:n)/(n+1)

# GeraÁ„o dos valores do modelo
model_probs <- numeric(n)
model_quantile <- numeric(n)

for(i in 1:n){
  model_probs[i] <- GEV.DF(ordered[i], A$est[1], A$est[2], A$est[3])
  model_quantile[i] <- GEV.INV(empirical[i], A$est[1], A$est[2], A$est[3])
}


# 1. Instale e carregue o pacote (se ainda n„o tiver)
# install.packages("bgev")
library(bgev)

# 2. Ajuste do Modelo Bimodal (Estimativa)
fit_bimodal <- bgev.mle(dados_jan_temp)

# 3. PreparaÁ„o para o Gr·fico
# Criamos uma sequÍncia suave de temperaturas para desenhar a curva
x_seq <- seq(min(dados_jan_temp), max(dados_jan_temp), length.out = 200)

# ExtraÌmos os par‚metros do ajuste (mu, sigma, xi, delta)
p <- fit_bimodal$par

# 4. Plotagem Comparativa
hist(dados_jan_temp, freq=FALSE, main="Ajuste: Bimodal (BGEV) vs GEV Padr„o",
     xlab="Temperatura (∫C)", ylim=c(0, 0.4), # Ajuste o ylim se cortar o topo
     col="lightgray", border="white")

# Linha da BGEV (Bimodal) - CORRIGIDA
lines(x_seq, 
      dbgev(x_seq, mu = p[1], sigma = p[2], xi = p[3], delta = p[4]), 
      col = "#c0392b", lwd = 3) # Vermelho

# Linha da GEV Padr„o (para comparaÁ„o)
# Ajustamos uma GEV simples sÛ para ver a diferenÁa
fit_gev_simples <- fevd(dados_jan_temp, type="GEV")
p_gev <- fit_gev_simples$results$par
lines(x_seq, 
      devd(x_seq, loc=p_gev[1], scale=p_gev[2], shape=p_gev[3], type="GEV"), 
      col = "#2980b9", lwd = 2, lty = 2) # Azul pontilhado

legend("topright", legend=c("BGEV (Bimodal)", "GEV (Unimodal)"),
       col=c("#c0392b", "#2980b9"), lwd=c(3, 2), lty=c(1, 2), bty="n")

# DefiniÁ„o de cores Hexadecimais para Temperatura
cor_fria   <- "#2980b9"  # Azul (Belize Hole)
cor_neutra <- "#f1c40f"  # Amarelo (Sunflower)
cor_quente <- "#c0392b"  # Vermelho (Pomegranate)

# Cria uma funÁ„o que gera 'n' cores baseadas nessas 3
paleta_termica <- colorRampPalette(c(cor_fria, cor_neutra, cor_quente))

# Teste visual
barplot(rep(1, 100), col = paleta_termica(100), border = NA, main = "Sua Paleta de Temperatura")
