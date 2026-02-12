# ==============================================================================
# PROJETO: Análise de Extremos Climáticos - Dezembro (1968-2022)
# AUTOR: Lucas Pereira Belo
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. CARREGAMENTO DE PACOTES
# ------------------------------------------------------------------------------
library(DT)
library(readxl)
library(dplyr)
library(ggplot2)    # Gráficos avançados
library(tseries)    # Testes de séries temporais
library(trend)      # Testes não-paramétricos (MK, Cox-Stuart)
library(goftest)    # Testes de bondade de ajuste (AD)
library(extRemes)   # Modelagem GEV
library(urca)       # Testes de raiz unitária
library(kableExtra) # Tabelas formatadas
library(distillery)
library(randtests)  # Runs Test
library(gridExtra) # Sugestão para plotar lado a lado (opcional)

# ------------------------------------------------------------------------------
# 2. LEITURA E PREPARAÇÃO DOS DADOS
# ------------------------------------------------------------------------------
df_raw <- read_excel("EXTREMOS.xlsx", sheet = 12) 

# Definição do mês e preparação do DataFrame
NOME_MES <- "Dezembro"
anos <- seq(1968, 2022, 1)
df_analise <- data.frame(Ano = anos, Temp = df_raw$TEMP)
df_analise <- na.omit(df_analise)

# Visualização Exploratória Rápida
hist(df_analise$Temp, main=paste("Distribuicao de Frequencias -", NOME_MES), 
     xlab="Temperatura (C)", col="#3498db", border="white", prob=TRUE)
lines(density(df_analise$Temp), col="#c0392b", lwd=2)

# ==============================================================================
# 3. ANÁLISE EXPLORATÓRIA 
# ==============================================================================

modelo_linear <- lm(Temp ~ Ano, data = df_analise)
summary(modelo_linear)
tendencia <- coef(modelo_linear)[2]

ggplot(df_analise, aes(x = Ano, y = Temp)) +
  geom_line(color = "gray50", size = 0.8) +
  geom_point(color = "#c0392b", size = 2) +
  geom_smooth(method = "lm", se = FALSE, color = "black", linetype="dashed", size = 0.8) +
  theme_minimal() +
  labs(title = paste("Série Histórica:", NOME_MES),
       subtitle = paste0("Tendência Linear Estimada: ", round(tendencia, 3), " °C/ano"),
       y = "Temperatura Máxima (°C)", x = "Ano")

# ---  (ACF e PACF) ---

# Configuração da área de plotagem (1 linha, 2 colunas)
par(mfrow = c(1, 2), mar = c(4, 4, 3, 1))

# ACF - Autocorrelação (Diagnóstico para termos MA)
acf(df_analise$Temp, main = "Autocorrelação (ACF)", 
    col = "#2980b9", lwd = 2, bty="n")

# PACF - Autocorrelação Parcial (Diagnóstico para termos AR)
pacf(df_analise$Temp, main = "Autocorrelação Parcial (PACF)", 
     col = "#c0392b", lwd = 2, bty="n")

par(mfrow = c(1, 1))

# -------------------------------------------------------
# TESTE 1: Mann-Kendall (Tendência Monotônica)
# -------------------------------------------------------
mk.test(df_analise$Temp)
# p-valor >= 0,05: H0 (Nula): Não há tendência.
# p-valor < 0,05: H1 (Alt.): Existe tendência monotônica (crescente ou decrescente).

# -------------------------------------------------------
# TESTE 2: Cox-Stuart (Tendência em Dispersão/Localização)
# -------------------------------------------------------
cox.stuart.test(df_analise$Temp)
# p-valor >= 0,05: H0 (Nula): Não há tendência.
# p-valor < 0,05: H1 (Alt.): Existe tendência (crescente ou decrescente).

# -------------------------------------------------------
# TESTE 3: Augmented Dickey-Fuller (Estacionariedade)
# -------------------------------------------------------
adf.test(df_analise$Temp, alternative = "stationary")
# p-valor >= 0,05: H0 (Nula): A série tem raiz unitária (NÃO é estacionária).
# p-valor < 0,05: H1 (Alt.): A série é estacionária.

# -------------------------------------------------------
# TESTE 4: Runs Test / Teste de Sequências (Aleatoriedade)
# -------------------------------------------------------
runs.test(df_analise$Temp)
# p-valor >= 0,05: H0 (Nula): A série é aleatória.
# p-valor < 0,05: H1 (Alt.): A série NÃO é aleatória (indica tendência ou oscilação sistemática).

# -------------------------------------------------------
# TESTE 5: Ljung-Box (Independência / Ruído Branco)
# -------------------------------------------------------
Box.test(df_analise$Temp, type = "Ljung-Box")
# p-valor >= 0,05: H0 (Nula): Os dados são independentes (Ruído Branco).
# p-valor < 0,05: H1 (Alt.): Os dados possuem correlação serial significativa.


#### 5. MODELAGEM GEV (M0 A M4) ####

# M0: Estacionário
fit0 <- fevd(Temp, data = df_analise, type = "GEV")
plot(fit0)

library("ismev")
B=gev.fit(df_analise$Temp)
gev.diag(B)

# O gráfico de quantis não captura o ponto mais extremo (referente ao ano de 2015)
# O gráfico de retorno também não consegue capturar esse ponto.

# --- TESTE B: Anderson-Darling (AD) ---
# H0: Os dados seguem a distribuição GEV ajustada.
# H1: Os dados NÃO seguem a distribuição.

ad.test(df_analise$Temp, "pevd", 
                  loc = params["location"], 
                  scale = params["scale"], 
                  shape = params["shape"], 
                  type = "GEV")

# O modelo se adequou bem aos dados

# M1: Locação variável no tempo
fit1 <- fevd(Temp, data = df_analise, location.fun = ~Ano, type = "GEV")
plot(fit1)

# M2: Locação + Escala (Variabilidade) com Log-link
fit2 <- fevd(Temp, data = df_analise, location.fun = ~Ano, scale.fun = ~Ano, 
             use.phi = TRUE, type = "GEV")
plot(fit2)

# M3: Locação + Forma (Shape)
fit3 <- fevd(Temp, data = df_analise, location.fun = ~Ano, shape.fun = ~Ano, type = "GEV")
plot(fit3)

# M4: Modelo Completo
fit4 <- fevd(Temp, data = df_analise, location.fun = ~Ano, scale.fun = ~Ano, 
             shape.fun = ~Ano, use.phi = TRUE, type = "GEV")
plot(fit4)


#### 6. SELEÇÃO DE MODELOS E TRV ####

aic_vals <- c(summary(fit0, silent=TRUE)$AIC, 
              summary(fit1, silent=TRUE)$AIC, 
              summary(fit2, silent=TRUE)$AIC, 
              summary(fit3, silent=TRUE)$AIC, 
              summary(fit4, silent=TRUE)$AIC)

# Se quiser o BIC também:
bic_vals <- c(summary(fit0, silent=TRUE)$BIC, 
              summary(fit1, silent=TRUE)$BIC, 
              summary(fit2, silent=TRUE)$BIC, 
              summary(fit3, silent=TRUE)$BIC, 
              summary(fit4, silent=TRUE)$BIC)

nomes_modelos <- c("M0: Estacionário", 
                   "M1: Locação", 
                   "M2: Loc+Escala", 
                   "M3: Loc+Shape", 
                   "M4: Completo")

# Tabela de comparação
df_comp <- data.frame(
  Modelo = nomes_modelos, 
  AIC = round(aic_vals, 2),
  BIC = round(bic_vals, 2),
  Delta_AIC = round(aic_vals - min(aic_vals), 2)
)

print(df_comp)

# Identificar vencedor (Menor AIC)
vencedor_idx <- which.min(aic_vals)

# Selecionar o objeto do modelo vencedor
obj_vencedor <- list(fit0, fit1, fit2, fit3, fit4)[[vencedor_idx]]

# Imprimir o resultado de forma segura
print(paste(">>> O Modelo VENCEDOR (Menor AIC) eh:", nomes_modelos[vencedor_idx]))

# Tabela de comparação
df_comp <- data.frame(
  Modelo = nomes_modelos,
  AIC = round(aic_vals, 2)
)
print(df_comp)

# 7. DIAGNÓSTICO DOS RESÍDUOS (MODELO VENCEDOR: MU + XI VARIÁVEIS)

# 1. Recuperar os Coeficientes e Dados
betas <- obj_vencedor$results$par
anos  <- df_analise$Ano
n     <- length(anos)
zt    <- df_analise$Temp

# 2. Reconstrução Manual dos Parâmetros (Específico: Mu(t), Sigma fixo, Xi(t))

# --- A) LOCAÇÃO (Mu) -> Tendência Linear ---
# Parâmetros esperados: "mu0" (intercepto) e "mu1" (inclinação)
mu_t <- betas["mu0"] + betas["mu1"] * anos

# --- B) ESCALA (Sigma) -> Constante ---
# Parâmetro esperado: "scale" (pois não variou no modelo vencedor)
sigma_t <- rep(betas["scale"], n)

# --- C) FORMA (Xi/Shape) -> Tendência Linear ---
# Parâmetros esperados: "xi0" (intercepto) e "xi1" (inclinação)
xi_t <- betas["xi0"] + betas["xi1"] * anos


#### CÁLCULO DOS RESÍDUOS PADRONIZADOS (Eq. 6.6 Coles) ####


res_gumbel <- numeric(n)

for(i in 1:n) {
  # Termo dentro do logaritmo: 1 + xi * z_pad
  z_pad <- (zt[i] - mu_t[i]) / sigma_t[i]
  termo_log <- 1 + xi_t[i] * z_pad
  
  # Verificação de segurança: xi muito próximo de zero (comportamento Gumbel)
  if(abs(xi_t[i]) < 1e-6) {
    res_gumbel[i] <- z_pad
  } else {
    # Proteção: O termo do log deve ser positivo. 
    # Se o modelo extrapolar matematicamente para um valor impossível, usamos um piso.
    res_gumbel[i] <- (1/xi_t[i]) * log(max(1e-6, termo_log))
  }
}

# 4. Filtragem e Ordenação
res_validos   <- res_gumbel[is.finite(res_gumbel)]
n_val         <- length(res_validos)
res_ordenados <- sort(res_validos)

# ------------------------------------------------------------------------------
# 5. PLOTAGEM (DIAGNÓSTICO VISUAL)
# ------------------------------------------------------------------------------
par(mfrow = c(2, 2))

# A) Probability Plot
prob_pos <- (1:n_val) / (n_val + 1)
plot(exp(-exp(-res_ordenados)), prob_pos, 
     main = "Probability Plot", xlab = "Modelo (Gumbel)", ylab = "Empirico", 
     pch = 20, col = "#2980b9")
abline(0, 1, col = "red", lwd = 1.5)

# B) Quantile Plot (QQ) - O mais importante para extremos
qq_teorico <- -log(-log(prob_pos))
plot(qq_teorico, res_ordenados, 
     main = "Quantile Plot (QQ)", xlab = "Teorico", ylab = "Empirico", 
     pch = 20, col = "#2980b9")
abline(0, 1, col = "red", lwd = 1.5)

# C) Histograma
hist(res_validos, freq = FALSE, main = "Densidade Residual", 
     xlab = "Residuos", col = "lightblue", border = "white")
curve(exp(-x - exp(-x)), add = TRUE, col = "red", lwd = 2)

# D) Resíduos no Tempo
plot(anos, res_validos, main = "Residuos vs Tempo", 
     xlab = "Ano", ylab = "Residuos Padronizados", pch = 20, col = "gray50")
abline(h = 0, col = "red", lty = 2)

par(mfrow = c(1, 1))


# 6. (Kolmogorov-Smirnov e Anderson-Darling)

# Estatísticas descritivas básicas dos resíduos
cat("\n--- Estatísticas dos Resíduos ---\n")
cat("Média (Ideal ~0.577):", round(mean(res_validos), 4), "\n")
cat("Variância (Ideal ~1.645):", round(var(res_validos), 4), "\n")

# Teste de Kolmogorov-Smirnov (H0: Segue Gumbel Padrão)
ks_teste <- ks.test(res_validos, function(x) exp(-exp(-x)))
print(ks_teste)

# Teste de Anderson-Darling (Via PIT para Uniforme)
# Recalculando o PIT manualmente para garantir consistência com os parâmetros manuais
z_pit_manual <- exp(-exp(-res_validos)) 
ad_final <- ad.test(z_pit_manual, null = "punif", min = 0, max = 1)
print(ad_final)

# ------------------------------------------------------------------------------
# 9. EVOLUÇÃO DO RISCO CLIMÁTICO (NÍVEL DE RETORNO 100 ANOS)
# ------------------------------------------------------------------------------
periodo <- 100
y_p <- -log(1 - 1/periodo)

# Cálculo do Return Level variando no tempo
rl_100 <- mu_t + (sigma_t / xi_t) * ( (y_p^(-xi_t)) - 1 )

plot(df_analise$Ano, df_analise$Temp, pch=19, col="gray70",
     main = "Evoluca do Risco de 100 Anos",
     ylab = "Temperatura Maxima (C)", xlab = "Ano")
lines(df_analise$Ano, rl_100, col = "#c0392b", lwd = 3)
legend("topleft", legend = c("Observado", "Retorno 100 anos"), 
       col = c("gray70", "#c0392b"), pch = c(19, NA), lwd = c(NA, 3))



##### DIAGNÓSTICO MODELO M1: LOCAÇÃO VARIÁVEL (Mu(t)) #####

# 1. Recuperar Coeficientes
betas <- fit1$results$par
anos  <- df_analise$Ano
n     <- length(anos)
zt    <- df_analise$Temp

# 2. Reconstrução dos Parâmetros (M1)
# Mu: Varia linearmente (mu0 + mu1 * t)
# Sigma: Constante ("scale")
# Xi: Constante ("shape")

mu_t    <- betas["mu0"] + betas["mu1"] * anos
sigma_t <- rep(betas["scale"], n)
xi_t    <- rep(betas["shape"], n)

# 3. Cálculo dos Resíduos Padronizados
res_gumbel <- numeric(n)
for(i in 1:n) {
  z_pad <- (zt[i] - mu_t[i]) / sigma_t[i]
  termo_log <- 1 + xi_t[i] * z_pad
  
  if(abs(xi_t[i]) < 1e-6) {
    res_gumbel[i] <- z_pad
  } else {
    res_gumbel[i] <- (1/xi_t[i]) * log(max(1e-6, termo_log))
  }
}

# 4. Filtragem
res_validos   <- res_gumbel[is.finite(res_gumbel)]
n_val         <- length(res_validos)
res_ordenados <- sort(res_validos)

# 5. Plotagem M1
par(mfrow = c(2, 2))
prob_pos <- (1:n_val) / (n_val + 1)

# PP-Plot
plot(exp(-exp(-res_ordenados)), prob_pos, main = "PP Plot (M1 - Locacao)", 
     xlab = "Modelo", ylab = "Empirico", pch = 20, col = "#2980b9")
abline(0, 1, col = "red")

# QQ-Plot
plot(-log(-log(prob_pos)), res_ordenados, main = "QQ Plot (M1 - Locacao)", 
     xlab = "Teorico", ylab = "Empirico", pch = 20, col = "#2980b9")
abline(0, 1, col = "red")

# Histograma
hist(res_validos, freq = FALSE, main = "Densidade M1", xlab = "Residuos", col = "lightblue", border="white")
curve(exp(-x - exp(-x)), add = TRUE, col = "red", lwd=2)

# Tempo
plot(anos, res_validos, main = "Residuos vs Tempo (M1)", ylab="Residuos", pch=20, col="gray50")
abline(h=0, col="red", lty=2)
par(mfrow = c(1, 1))

# 6. Testes M1
cat("\n>>> Testes para M1 (Locacao):\n")
print(ks.test(res_validos, function(x) exp(-exp(-x))))
print(ad.test(exp(-exp(-res_validos)), null="punif", min=0, max=1))


# 9. EVOLUÇÃO DO RISCO CLIMÁTICO (MODELO M1: LOCAÇÃO VARIÁVEL)

# 1. Definição do Período de Retorno
periodo <- 100

# y_p é o negativo do log da probabilidade de não-excedência
# y_p = -ln(1 - 1/T)
y_p <- -log(1 - 1/periodo)

# 2. Recuperação dos Parâmetros do Modelo M1 (fit1)
# IMPORTANTE: Certifique-se de que 'fit1' é o objeto do seu modelo M1
betas <- fit1$results$par  
anos  <- df_analise$Ano
n     <- length(anos)

# --- A) LOCAÇÃO (Mu) -> Varia com o tempo ---
mu_t <- betas["mu0"] + betas["mu1"] * anos

# --- B) ESCALA (Sigma) -> Constante ---
sigma_t <- rep(betas["scale"], n)

# --- C) FORMA (Xi) -> Constante ---
xi_t <- rep(betas["shape"], n)

# 3. Cálculo do Nível de Retorno (Return Level)
# Fórmula GEV Inversa: RL = mu + (sigma/xi) * [ (y_p^(-xi)) - 1 ]
# Tratamento para caso Gumbel (xi ~ 0) incluído para segurança

rl_100 <- numeric(n)

for(i in 1:n) {
  if(abs(xi_t[i]) < 1e-6) {
    # Fórmula Gumbel (caso Xi seja zero)
    # RL = mu - sigma * log(y_p)
    rl_100[i] <- mu_t[i] - sigma_t[i] * log(y_p)
  } else {
    # Fórmula GEV Padrão
    rl_100[i] <- mu_t[i] + (sigma_t[i] / xi_t[i]) * ( (y_p^(-xi_t[i])) - 1 )
  }
}

# 4. Visualização
# Define limites do eixo Y para garantir que tudo caiba (dados + linha de risco)
y_min <- min(df_analise$Temp, na.rm=TRUE)
y_max <- max(c(df_analise$Temp, rl_100), na.rm=TRUE) + 2

plot(df_analise$Ano, df_analise$Temp, 
     pch = 19, col = "gray70",
     main = "Evolucao do Risco de 100 Anos (Modelo M1)",
     ylab = "Temperatura Maxima (C)", xlab = "Ano",
     ylim = c(y_min, y_max))

# Adiciona a linha de tendência do retorno de 100 anos
lines(df_analise$Ano, rl_100, col = "#c0392b", lwd = 3)

# Legenda
legend("topleft", 
       legend = c("Observado", "Retorno 100 anos"), 
       col = c("gray70", "#c0392b"), 
       pch = c(19, NA), 
       lwd = c(NA, 3), 
       bty = "n") # bty="n" remove a caixa da legenda para ficar mais limpo

#### DIAGNÓSTICO MODELO M2: LOCAÇÃO + ESCALA (Mu(t), Sigma(t)) ####

# 1. Recuperar Coeficientes
# Certifique-se de que 'fit2' é o seu modelo com scale.fun = ~Ano
betas <- fit2$results$par 
anos  <- df_analise$Ano
n     <- length(anos)
zt    <- df_analise$Temp

# 2. Reconstrução dos Parâmetros (M2)
# Mu: Varia linearmente (mu0 + mu1 * t)
# Sigma: Varia Log-linearmente (exp(phi0 + phi1 * t)) -> Garante positividade
# Xi: Constante ("shape")

mu_t    <- betas["mu0"] + betas["mu1"] * anos
sigma_t <- exp(betas["phi0"] + betas["phi1"] * anos) # APLICANDO A EXPONENCIAL
xi_t    <- rep(betas["shape"], n)

# 3. Cálculo dos Resíduos Padronizados (Transformados para Gumbel)
res_gumbel <- numeric(n)
for(i in 1:n) {
  # Padronização básica (Z-score local)
  z_pad <- (zt[i] - mu_t[i]) / sigma_t[i]
  
  # Transformação para a escala Gumbel (Resíduos do Modelo)
  if(abs(xi_t[i]) < 1e-6) {
    res_gumbel[i] <- z_pad
  } else {
    termo_log <- 1 + xi_t[i] * z_pad
    # Proteção contra log de valores negativos/zero
    termo_log <- max(1e-6, termo_log)
    res_gumbel[i] <- (1/xi_t[i]) * log(termo_log)
  }
}

# 4. Filtragem e Ordenação
res_validos   <- res_gumbel[is.finite(res_gumbel)]
n_val         <- length(res_validos)
res_ordenados <- sort(res_validos)

# 5. Plotagem Diagnóstica M2
par(mfrow = c(2, 2))
prob_pos <- (1:n_val) / (n_val + 1)

# PP-Plot
plot(exp(-exp(-res_ordenados)), prob_pos, 
     main = "PP Plot (M2 - Loc. + Escala)", 
     xlab = "Probabilidade Modelo", ylab = "Probabilidade Empirica", 
     pch = 20, col = "#8e44ad") # Cor Roxo para diferenciar
abline(0, 1, col = "red")

# QQ-Plot
plot(-log(-log(prob_pos)), res_ordenados, 
     main = "QQ Plot (M2 - Loc. + Escala)", 
     xlab = "Quantil Teorico (Gumbel)", ylab = "Quantil Empirico", 
     pch = 20, col = "#8e44ad")
abline(0, 1, col = "red")

# Histograma
hist(res_validos, freq = FALSE, 
     main = "Densidade dos Resíduos (M2)", 
     xlab = "Residuos", col = "#d2b4de", border="white")
curve(exp(-x - exp(-x)), add = TRUE, col = "red", lwd=2)

# Resíduos vs Tempo (IMPORTANTE: Verificar Homocedasticidade)
plot(anos, res_validos, 
     main = "Residuos vs Tempo (M2)", 
     ylab="Residuos Padronizados", xlab="Ano",
     pch=20, col="gray50")
abline(h=0, col="red", lty=2)
# Adiciona uma linha suave para ver se a variância estabilizou
lines(lowess(anos, res_validos), col="blue", lwd=2)
par(mfrow = c(1, 1))


# 6. Testes de Aderência M2
cat("\n>>> Testes de Aderência para M2 (Loc + Escala):\n")
# KS Test (H0: Resíduos seguem dist. Gumbel padrão)
print(ks.test(res_validos, function(x) exp(-exp(-x))))

# 9. EVOLUÇÃO DO RISCO CLIMÁTICO (MODELO M2)

# 1. Definição do Período de Retorno
periodo <- 100
y_p <- -log(1 - 1/periodo)

# 2. Recuperação dos Parâmetros (Já feito acima, mas reforçando para o bloco)
# mu_t e sigma_t já são vetores que variam com o tempo!

# 3. Cálculo do Nível de Retorno Variável (RL)
rl_100_m2 <- numeric(n)

for(i in 1:n) {
  if(abs(xi_t[i]) < 1e-6) {
    # Caso Gumbel
    rl_100_m2[i] <- mu_t[i] - sigma_t[i] * log(y_p)
  } else {
    # Caso GEV: Note que agora sigma_t[i] muda a cada ano, ampliando o cone de risco
    rl_100_m2[i] <- mu_t[i] + (sigma_t[i] / xi_t[i]) * ( (y_p^(-xi_t[i])) - 1 )
  }
}

# 4. Visualização da Evolução do Risco
y_min <- min(df_analise$Temp, na.rm=TRUE)
y_max <- max(c(df_analise$Temp, rl_100_m2), na.rm=TRUE) + 2

plot(df_analise$Ano, df_analise$Temp, 
     pch = 19, col = "gray70",
     main = "Evolucao do Risco (M2: Variância Crescente)",
     ylab = "Temperatura Maxima (C)", xlab = "Ano",
     ylim = c(y_min, y_max))

# Linha de Tendência do Retorno de 100 Anos
lines(df_analise$Ano, rl_100_m2, col = "#8e44ad", lwd = 3) # Roxo

legend("topleft", 
       legend = c("Observado", "Retorno 100 anos (M2)"), 
       col = c("gray70", "#8e44ad"), 
       pch = c(19, NA), 
       lwd = c(NA, 3), 
       bty = "n")

# 6. Testes M2
cat("\n>>> Testes para M2 (Locação + Escala):\n")
print(ks.test(res_validos, function(x) exp(-exp(-x))))
print(ad.test(exp(-exp(-res_validos)), null="punif", min=0, max=1))

##### DIAGNÓSTICO MODELO M3: LOCAÇÃO + FORMA (Mu(t), Xi(t))  ####


# 1. Recuperar Coeficientes
betas <- fit3$results$par
anos  <- df_analise$Ano
n     <- length(anos)
zt    <- df_analise$Temp

# 2. Reconstrução dos Parâmetros (M3)
# Mu: Linear (mu0 + mu1 * t)
# Sigma: Constante ("scale")
# Xi: Linear (xi0 + xi1 * t)

mu_t    <- betas["mu0"] + betas["mu1"] * anos
sigma_t <- rep(betas["scale"], n)
xi_t    <- betas["xi0"] + betas["xi1"] * anos

# 3. Cálculo dos Resíduos
res_gumbel <- numeric(n)
for(i in 1:n) {
  z_pad <- (zt[i] - mu_t[i]) / sigma_t[i]
  termo_log <- 1 + xi_t[i] * z_pad
  
  if(abs(xi_t[i]) < 1e-6) {
    res_gumbel[i] <- z_pad
  } else {
    res_gumbel[i] <- (1/xi_t[i]) * log(max(1e-6, termo_log))
  }
}

# 4. Filtragem
res_validos   <- res_gumbel[is.finite(res_gumbel)]
n_val         <- length(res_validos)
res_ordenados <- sort(res_validos)

# 5. Plotagem M3
par(mfrow = c(2, 2))
prob_pos <- (1:n_val) / (n_val + 1)

plot(exp(-exp(-res_ordenados)), prob_pos, main = "PP Plot (M3 - Loc+Shape)", 
     xlab = "Modelo", ylab = "Empirico", pch = 20, col = "#2980b9")
abline(0, 1, col = "red")

plot(-log(-log(prob_pos)), res_ordenados, main = "QQ Plot (M3 - Loc+Shape)", 
     xlab = "Teorico", ylab = "Empirico", pch = 20, col = "#2980b9")
abline(0, 1, col = "red")

hist(res_validos, freq = FALSE, main = "Densidade M3", xlab = "Residuos", col = "lightblue", border="white")
curve(exp(-x - exp(-x)), add = TRUE, col = "red", lwd=2)

plot(anos, res_validos, main = "Residuos vs Tempo (M3)", ylab="Residuos", pch=20, col="gray50")
abline(h=0, col="red", lty=2)
par(mfrow = c(1, 1))

# 6. Testes M3
cat("\n>>> Testes para M3 (Locação + Forma):\n")
print(ks.test(res_validos, function(x) exp(-exp(-x))))
print(ad.test(exp(-exp(-res_validos)), null="punif", min=0, max=1))

