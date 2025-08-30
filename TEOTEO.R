#################################################################
#########           Lucas Pereira Belo           ################
#########            Email:lucas.p.belo@ufv.br   ################
########            TEOTEO - AM                 #################
#################################################################

# É uma boa prática carregar todos os pacotes necessários no início.
library(readxl)    # Para ler arquivos Excel (.xlsx)
library(dplyr)     # Para manipulação de dados (mutate, group_by, etc.)
library(lubridate) # Para funções de data (as_date, year, quarter)
library(hms)       # Para funções de tempo (as_hms)


# ===================================================================
# 2. CARREGAMENTO E LIMPEZA COMPLETA DOS DADOS
# ===================================================================

# Carrega os dados do arquivo Excel
# (Opcional: use o argumento 'skip = 9' para pular as 9 primeiras linhas se elas não forem necessárias)
dados_brutos <- read_excel("TEOTEO.xlsx")
tail(dados_brutos)
head(dados_brutos)
# Limpeza e transformação em um único passo usando o pipe (%>%)
dados_limpos <- dados_brutos %>%
  # Renomeia as colunas para nomes mais simples e sem espaços
  rename(
    DATA = 1,        # Renomeia a 1ª coluna
    HORA = 2,        # Renomeia a 2ª coluna
    PRECIP = 3,      # Renomeia a 3ª coluna
    TEMPERATURA = 4  # Renomeia a 4ª coluna
  ) %>%
  # Converte os tipos de cada coluna para o formato correto
  mutate(
    DATA = as_date(DATA),
    HORA = as_hms(HORA),
    # CORREÇÃO: Dentro do mutate, use os nomes das colunas diretamente
    PRECIP = as.numeric(PRECIP),
    TEMPERATURA = as.numeric(TEMPERATURA)
  )

# Verifique a estrutura final dos dados limpos para garantir que tudo está correto
cat("--- Estrutura dos Dados Após a Limpeza ---\n")
glimpse(dados_limpos)


# ===================================================================
# 3. ANÁLISES A PARTIR DOS DADOS LIMPOS
# ===================================================================

# --- 3.1 Análise de Máximos Anuais (Block Maxima) ---
dados_max_anuais <- dados_limpos %>%
  mutate(ano = year(DATA)) %>%
  group_by(ano) %>%
  slice(which.max(PRECIP)) %>%
  ungroup()

cat("\n--- Tabela com a Precipitação Máxima de Cada Ano ---\n")
print(dados_max_anuais)


# --- 3.2 Análise de Máximos Trimestrais ---
# CORREÇÃO: A análise deve partir dos dados já limpos ('dados_limpos')
dados_max_trimestrais <- dados_limpos %>%
  mutate(
    ano = year(DATA),
    trimestre = quarter(DATA)
  ) %>%
  group_by(ano, trimestre) %>%
  slice(which.max(PRECIP)) %>%
  ungroup()

cat("\n--- Tabela com a Precipitação Máxima de Cada Trimestre ---\n")
print(dados_max_trimestrais)


# --- 3.3 Análise da Série Temporal Diária Completa ---
# Criando o objeto de série temporal a partir dos dados limpos
ts_precip_diaria <- ts(dados_limpos$PRECIP, start = c(2006, 1), frequency = 365.25)

# Plot da série temporal completa
plot(ts_precip_diaria, 
     xlab = "Tempo", 
     ylab = "Precipitação (mm)",
     main = "Série Temporal de Precipitação Diária")

# Plot da Função de Autocorrelação (ACF)
acf(ts_precip_diaria, main = "Função de Autocorrelação para Precipitação Diária")
# Visualização

dados<-dados_max_precip_trimestral
names(dados)
plot_precip <- ggplot(dados, aes(x = as.factor(DATA), y = PRECIP)) +
  geom_bar(stat = "identity") +
  labs(title = "Maior Precipitação por Ano", x = "Ano", y = "Precipitação (mm)")
print(plot_precip)


# Cria o vetor 'PRECIP' com os dados fornecidos
PRECIP <- c(13.6, 40.2, 29.8,  8.2,  5.0, 15.2, 35.4, 15.0,  9.2, 52.0, 27.8, 44.2,
            5.4, 37.2, 31.4, 26.0,  5.2, 45.8, 16.6, 19.6,  6.0, 45.2, 13.0, 38.6,
            9.8, 54.2, 33.8, 23.8,  6.8, 22.4, 15.4, 23.4,  6.6, 21.0, 30.4, 36.0,
            3.2, 41.6, 32.6, 13.0,  5.4, 17.8, 12.0,  4.4, 19.0,  1.4,  4.0,  3.2,
            27.6, 33.2, 14.8,  4.8, 19.0, 62.2, 41.2,  8.8, 36.4, 51.8,  1.4,  2.8,
            37.6, 12.2,  5.6,  2.4, 45.2, 22.2, 25.2, 29.0, 39.0, 58.6, 11.0,  2.4,
            46.0, 18.0, 12.0)

# Imprime o vetor para confirmar
print(PRECIP)

length(PRECIP)

# 1. Seu vetor de dados (com 75 observações)
PRECIP <- c(13.6, 40.2, 29.8,  8.2,  5.0, 15.2, 35.4, 15.0,  9.2, 52.0, 27.8, 44.2,
            5.4, 37.2, 31.4, 26.0,  5.2, 45.8, 16.6, 19.6,  6.0, 45.2, 13.0, 38.6,
            9.8, 54.2, 33.8, 23.8,  6.8, 22.4, 15.4, 23.4,  6.6, 21.0, 30.4, 36.0,
            3.2, 41.6, 32.6, 13.0,  5.4, 17.8, 12.0,  4.4, 19.0,  1.4,  4.0,  3.2,
            27.6, 33.2, 14.8,  4.8, 19.0, 62.2, 41.2,  8.8, 36.4, 51.8,  1.4,  2.8,
            37.6, 12.2,  5.6,  2.4, 45.2, 22.2, 25.2, 29.0, 39.0, 58.6, 11.0,  2.4,
            46.0, 18.0, 12.0)

precip_trimestral_ts <- ts(PRECIP, start = c(2006, 4), frequency = 4)

# 3. Plote a série temporal

plot(precip_trimestral_ts, 
     main = "Precipitação Trimestral (2006-2025)",
     xlab = "Ano",
     ylab = "Precipitação (mm)")

print(precip_trimestral_ts)

acf(precip_trimestral_ts)
pacf(precip_trimestral_ts)


# Criando o gráfico
ggplot(dados_max_trimestrais, aes(x = dados_max_trimestrais$DATA, y = PRECIP)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "darkred", size = 2) +
  labs(
    title = "Série temporal de precipitação máxima trimestral",
    x = "Ano",
    y = "Precipitação (mm)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

library(ggplot2)
library(dplyr)

# Seleciona os 10 maiores valores
top10 <- dados_max_trimestrais %>%
  arrange(desc(PRECIP)) %>%
  slice(1:4)

# Gráfico com os rótulos nos 10 maiores valores
ggplot(dados_max_trimestrais, aes(x = DATA, y = PRECIP)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "darkred", size = 2) +
  # Adiciona os rótulos
  geom_text(
    data = top10,
    aes(label = round(PRECIP, 1)),
    vjust = -0.8, color = "black", size = 3.5
  ) +
  labs(
    title = "",
    x = "Ano",
    y = "Precipitação (mm)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )



# Verifica a presença de autocorrelação

Box.test(precip_trimestral_ts, type = "Ljung-Box")

# Esse teste verifica a existencia de autocorrelaçao no 10lag
# Para verificar em outros lags basta substituir 10 pelo valor desejado
# Um complemento visual a esteteste é a inclusão dos gráficos ACF  e PACF
# Os gráficos darão um retorno visual corroborando com o teste.

# Gráficos de ACF e PACF
acf(precip_trimestral_ts)
pacf(precip_trimestral_ts)

# Se ainda não estiver carregada:
# precip_ts <- ts(dados)

# Configura pacotes para formato decimal com vírgula
options(OutDec = ",")

# Define layout com 1 linha e 2 colunas
par(mfrow = c(2, 2), mar = c(4, 4, 1, 1))

# Plot ACF
acf(precip_trimestral_ts, main = "",
    ylab = "Autocorrelação", xlab = "Defasagem")

# Plot PACF
pacf(precip_trimestral_ts, main = "",
     ylab = "Autocorrelação Parcial", xlab = "Defasagem")

# Teste de aleatoriedade
library(tseries)

median_precip <- median(PRECIP)
precip_binary <- ifelse(PRECIP >= median_precip, 1, 0)

# Aplicando o runs test
resultado <- runs.test(as.factor(precip_binary))
resultado

# teste de tendência
library(randtests)
cox.stuart.test(precip_trimestral_ts)
# H0: A série não apresenta tendência 

# teste de estacionariedade com componente de tendencia
library(urca)
adf_trend <- ur.df(precip_trimestral_ts, type = "trend", selectlags = "AIC")
summary(adf_trend)

# Histograma dos dados
hist(PRECIP, breaks = 12, col = "lightblue", 
     main = "Histograma da Precipitação", xlab = "Precipitação")

# Teste de normalidade 
library(extRemes)
library(stats)

# Kolmogorov-Smirnov
ks.test(precip_trimestral_ts, mode = "GEV", mean = mean(precip_trimestral_ts), sd = sd(precip_trimestral_ts))
help(ks.test)
library(evd)
library(randtests)
library(tseries)

ks.test(precip_trimestral_ts, "pgev", loc = A$estimate[1], scale = A$estimate[2], shape = A$estimate[3])


# Função CDF da GEV
pgev_manual <- function(q, loc, scale, shape) {
  if (scale <= 0) stop("scale must be positive")
  z <- (q - loc) / scale
  if (shape == 0) {
    return(exp(-exp(-z)))  # Gumbel
  } else {
    t <- 1 + shape * z
    if (any(t <= 0)) return(0)  # fora do suporte da GEV
    return(exp(-t^(-1 / shape)))
  }
}

ks.test(precip_trimestral_ts, function(q) pgev_manual(q, 
                                           loc = A$estimate[1], 
                                           scale = A$estimate[2], 
                                           shape = A$estimate[3]))


# Estimando os parâmetros da distribuição via função

#Valores iniciais para o vetor TETA
theta=c(mean(PRECIP), sd(PRECIP), 0.1)

## Criando a função ##
gev.loglik=function(theta){
  mu=theta[1]
  sigma=theta[2]
  xi=theta[3]
  m=min((1+(xi*(dataset-mu)/sigma)))                              #Nota 1
  if(m<0.00001)return(as.double(1000000))                         #Nota 2
  if(sigma<0.00001)return(as.double(1000000))                     #Nota 3
  if(xi==0){
    loglik=(-length(dataset)*log(sigma)-sum((dataset-mu)/sigma)
            -sum(exp(-((dataset-mu)/sigma))))                           #Nota 4
  }else{
    loglik=(-length(dataset)*log(sigma)
            -(1/xi+1)*sum(log(1+(xi*(dataset-mu)/sigma)))
            -sum((1+(xi*(dataset-mu)/sigma))**(-1/xi)))}                #Nota 5
  return(-loglik)}                                                #Nota 6
#######################################################################
dataset=PRECIP
theta=c(mean(PRECIP), sd(PRECIP), 0.1)
nlm(gev.loglik,theta)
A<-nlm(gev.loglik,theta,hessian=TRUE)

varcovar=solve(A$hessian)
sqrt(diag(varcovar))

ordered=sort(PRECIP)

empirical=vector('numeric',length(ordered))
for(i in 1:length(empirical)){
  empirical[i]=i/(length(dataset)+1)
}

GEV.DF=function(data,mu,sigma,xi){
  if(xi==0){
    GEV=exp(-exp(-((data-mu)/sigma)))}
  else{
    GEV=exp(-(1+xi*((data-mu)/sigma))^(-1/xi))}
  return(GEV)}

model=vector('numeric',length(dataset))
for(i in 1:length(model)){
  model[i]=GEV.DF(ordered[i],A$est[1],A$est[2],A$est[3])}


plot(model~empirical,main='Gráfico de probabilidade')
abline(0,1)

model.quantile=vector('numeric',length(dataset))
GEV.INV=function(data,mu,sigma,xi){
  if(xi==0){
    INV=mu-sigma*log(-log(1-data))}
  else{
    INV=mu+(sigma/xi)*(((-log(data))^(-xi))-1)}
  return(INV)
}
for(i in 1:length(model.quantile)){
  model.quantile[i]=GEV.INV(empirical[i],A$est[1],A$est[2],A$est[3])
}


plot(model.quantile~ordered,main='Gráfico quantil-quantil')
abline(0,1)

y10=-log(1-(1/10))
del=matrix(ncol=1,nrow=3)
del[1,1]=1
del[2,1]=-((A$est[3])^(-1))*(1-(y10^(-A$est[3])))
del[3,1]=(((A$est[2])*((A$est[3])^(-2))*(1-((y10)^(-A$est[3]))))
          -((A$est[2])*((A$est[3])^(-1))*((y10)^(-(A$est[3])))*log(y10)))
del.transpose=t(del)

sqrt(del.transpose%*%varcovar%*%del)

yrfun=function(A,r){
  yr=-log(1-(1/(r)))
  del=matrix(ncol=1,nrow=3)
  del[1,1]=1
  del[2,1]=-((A$est[3])^(-1))*(1-(yr^(-A$est[3])))
  del[3,1]=(((A$est[2])*((A$est[3])^(-2))*(1-((yr)^(-A$est[3]))))
            -((A$est[2])*((A$est[3])^(-1))*((yr)^(-(A$est[3])))*log(yr)))
  del.transpose=t(del)
  return(c(round(nlm(gev.loglik,theta,hessian=T)$estimate[1]+ ((nlm(gev.loglik,theta,hessian=T)$estimate[2])/(nlm(gev.loglik,theta,hessian=T)$estimate[3]))*((-log(1 - (r)^(-1)))^(-(nlm(gev.loglik,theta,hessian=T)$estimate[3])) - 1),4),round(sqrt(del.transpose%*%varcovar%*%del),4)))}

yrfun(A,10)
yrfun(A,25)
yrfun(A,50)
yrfun(A,100)

library(ismev)
gev.fit(precip_trimestral_ts)
# ξ=0, a GEV vira uma distribuição de Gumbel.
# ξ>0, a GEV vira uma distribuição de Fréchet
# ξ<0, a GEV vira uma distribuição de Weibull

B=gev.fit(precip_trimestral_ts)
B$mle
B$cov
B$data
gev.diag(B)
body(gev.diag)
body(gev.rl)
gev.rl()
body(gev.rl.gradient)
gev.rl.gradient()
retorno<-{
  eps <- 1e-06
  a1 <- a
  a2 <- a
  a3 <- a
  a1[1] <- a[1] + eps
  a2[2] <- a[2] + eps
  a3[3] <- a[3] + eps
  f <- c(seq(0.01, 0.09, by = 0.01), 0.1, 0.2, 0.3, 0.4, 0.5, 
         0.6, 0.7, 0.8, 0.9, 0.95, 0.99, 0.995, 0.999)
  q <- gevq(a, 1 - f)
  d <- t(gev.rl.gradient(a = a, p = 1 - f))
  v <- apply(d, 1, q.form, m = mat)
  plot(-1/log(f), q, log = "x", type = "n", xlim = c(0.1, 1000), 
       ylim = c(min(dat, q), max(dat, q)), xlab = "Return Period", 
       ylab = "Return Level")
  title("Return Level Plot")
  lines(-1/log(f), q)
  lines(-1/log(f), q + 1.96 * sqrt(v), col = 4)
  lines(-1/log(f), q - 1.96 * sqrt(v), col = 4)
  points(-1/log((1:length(dat))/(length(dat) + 1)), sort(dat))
}
# Deve-se ajustar esses valores até encontrar os máximos 

library(extRemes)
fit1 <- fevd(PRECIP, units = "mm"); fit1
par(mfrow=c(2,2))
b1<-ci(fit1, method = "proflik", xrange = c(10, 30), verbose = TRUE,, return.period = c(2))
b2<-ci(fit1, method = "proflik", xrange = c(30, 65), verbose = TRUE,, return.period = c(8))
b3<-ci(fit1, method = "proflik", xrange = c(40, 85), verbose = TRUE,, return.period = c(16))
b4<-ci(fit1, method = "proflik", xrange = c(45, 170), verbose = TRUE,, return.period = c(32))


citation("tseries")
citation("ismev")
citation("extRemes")
citation("ggplot2")
citation("readxl")
citation("tidyverse")

# Dados
mu <- 14.5868110
sigma <- 12.3026203
PRECIP <- c(13.6, 40.2, 29.8,  8.2,  5.0, 15.2, 35.4, 15.0,  9.2, 52.0, 27.8, 44.2,
            5.4, 37.2, 31.4, 26.0,  5.2, 45.8, 16.6, 19.6,  6.0, 45.2, 13.0, 38.6,
            9.8, 54.2, 33.8, 23.8,  6.8, 22.4, 15.4, 23.4,  6.6, 21.0, 30.4, 36.0,
            3.2, 41.6, 32.6, 13.0,  5.4, 17.8, 12.0,  4.4, 19.0,  1.4,  4.0,  3.2,
            27.6, 33.2, 14.8,  4.8, 19.0, 62.2, 41.2,  8.8, 36.4, 51.8,  1.4,  2.8,
            37.6, 12.2,  5.6,  2.4, 45.2, 22.2, 25.2, 29.0, 39.0, 58.6, 11.0,  2.4,
            46.0, 18.0, 12.0)

# Função de densidade da Gumbel
dgumbel <- function(x, mu, sigma) {
  z <- (x - mu) / sigma
  (1 / sigma) * exp(-(z + exp(-z)))
}

# Sequência para a curva
x_vals <- seq(10, 80, length.out = 500)
y_vals <- dgumbel(x_vals, mu, sigma)

# Histograma com densidade
hist(PRECIP, breaks = 6, freq = FALSE, col = "lightblue",
     xlim = c(10, 80), ylim = c(0, 0.030),
     main = "Histograma e Curva da Gumbel",
     xlab = "Precipitação", ylab = "Densidade")

# Eixos personalizados
axis(side = 1, at = seq(40, 180, by = 20))
axis(side = 2)
box()

# Adicionando a curva da Gumbel
lines(x_vals, y_vals, col = "red", lwd = 2)

# Adicionando pontos (bolinhas) para os dados observados
points(PRECIP, rep(0, length(PRECIP)), pch = 16, col = "black")

# Grade opcional
grid()

library(ggplot2)
library(patchwork)
library(dplyr)
library(forecast)

# 1. Histograma com gradiente de cor
df_precip <- data.frame(PRECIP = PRECIP)
p1 <- ggplot(df_precip, aes(x = PRECIP)) +
  geom_histogram(bins = 12, aes(fill = stat(count)), color = "black") +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(title = "Histograma da Precipitação",
       x = "Precipitação (mm)", y = "Frequência", fill = "FREQ") +
  theme_minimal() +
  scale_x_continuous(labels = function(x) gsub("\\.", ",", as.character(x))) +
  scale_y_continuous(labels = function(x) gsub("\\.", ",", as.character(x)))
# 2. Gráfico das 10 maiores precipitações
top10_precip <- dados %>%
  arrange(desc(PRECIP)) %>%
  slice(1:10) %>%
  mutate(Data = as.factor(PRECIP))  # garantir que Data seja fator

p2 <- ggplot(top10_precip, aes(x = reorder(DATA, -PRECIP), y = PRECIP, fill = PRECIP)) +
  geom_bar(stat = "identity", color = "black") +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(title = "Top 10 precipitações",
       x = "Data", y = "Precipitação (mm)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(labels = function(x) gsub("\\.", ",", as.character(x))) +
  scale_x_discrete(labels = function(x) gsub("\\.", ",", as.character(x)))

# 3. ACF
p3 <- ggAcf(precip_trimestral_ts) +
  labs(title = "ACF - Série de precipitação", x = "Defasagem") +
  scale_y_continuous(labels = function(x) gsub("\\.", ",", as.character(x))) +
  theme_minimal()

# 4. PACF
p4 <- ggPacf(precip_trimestral_ts) +
  labs(title = "PACF - Série de precipitação", x = "Defasagem") +
  scale_y_continuous(labels = function(x) gsub("\\.", ",", as.character(x))) +
  theme_minimal()

# Combinar os 4 gráficos
(p1 | p2) / (p3 | p4)

