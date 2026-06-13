# ==============================================================================
# PROJETO: MODELAGEM NÃO-ESTACIONÁRIA DE EXTREMOS CLIMÁTICOS - CENÁRIO 4.5
# LOCAL: Vale do Mucuri (Teófilo Otoni - MG)
# OBJETIVO: Correção de viés (Quantile Mapping) e projeção de Níveis de Retorno
#           sob cenários de Mudança Climática (RCP 4.5) no século XXI.
# ==============================================================================
# REFERÊNCIAS TEÓRICAS APLICADAS:
# 1. Jenkinson (1955) & Coles (2001): Fundamentação clássica da GEV e inferência.
# 2. Gudmundsson et al. (2012): Base teórica para a Transformação Quantílica (qmap).
# 3. DIRETRIZ METODOLÓGICA (Weather and Climate Extremes): Seleção de modelos 
#    não-estacionários (AICc, BIC) priorizando a parcimônia e a coerência física.
# ==============================================================================

library(terra)
library(qmap)
library(extRemes)

# ------------------------------------------------------------------------------
# FASE 1: GEOLOCALIZAÇÃO E EXTRAÇÃO DO MODELO ETA (TEÓFILO OTONI)
# ------------------------------------------------------------------------------
# JUSTIFICATIVA METODOLÓGICA PARA O ARTIGO:
# Para representar com fidelidade a orografia do Vale do Mucuri, os dados de 
# precipitação simulados pelo modelo climático Eta foram extraídos utilizando 
# um recorte espacial específico (bounding box). Utilizou-se o pacote 
# estatístico 'terra' para isolar a célula da grade correspondente às 
# coordenadas da estação meteorológica de Teófilo Otoni, garantindo a 
# representatividade local dos extremos hidrológicos.
# ------------------------------------------------------------------------------
cat("INICIANDO FASE 1: Geolocalização e Extração de Dados...\n")

limites_ajustados <- ext(-43.0, -40.0, -19.0, -16.5)
pontos_extracao <- data.frame(lon = -41.50, lat = -17.85)

PRECIP <- c(63.9, 70.2, 75.8, 108.1, 86.4, 68.9, 46.2, 54.4, 62.5, 82.9, 66.8, 61.4,
            62.2, 73.1, 150.3, 69.0, 59.8, 54.1, 83.0, 58.6, 102.7, 59.9, 92.8, 70.9,
            79.2, 68.1, 95.4, 47.6, 62.0, 155.9, 66.0, 89.4, 72.6, 68.6, 60.9, 78.0,
            68.7, 57.4, 56.0, 89.6, 80.4, 246.4, 64.0, 120.7, 91.0, 102.1, 49.2, 96.5,
            104.4, 72.0, 69.8, 145.0, 82.6, 82.2, 75.8, 137.6, 70.2, 58.0, 64.2, 93.6,
            75.6, 96.4, 44.8, 91.4, 67.2)

anos_reais_completos <- 1961:2025

dados_chuva_hist <- rast("pr_historical_1961_2005.nc")
chuva_focada_hist <- crop(dados_chuva_hist, limites_ajustados)

anos_simulados <- 1961:2005
vetor_simulado_bruto <- numeric(length(anos_simulados))

for (i in 1:length(anos_simulados)) {
  dia_inicial <- ((i - 1) * 360) + 1
  chuva_extraida <- extract(chuva_focada_hist[[dia_inicial:(i * 360)]], pontos_extracao)
  vetor_simulado_bruto[i] <- max(chuva_extraida[, -1], na.rm = TRUE)
}

# ------------------------------------------------------------------------------
# FASE 2: CALIBRAÇÃO E CORREÇÃO DE VIÉS (QUANTILE MAPPING)
# ------------------------------------------------------------------------------
# JUSTIFICATIVA METODOLÓGICA PARA O ARTIGO:
# Modelos climáticos regionais frequentemente apresentam vieses sistemáticos na 
# magnitude da precipitação. Para mitigar esse erro, aplicou-se a técnica de 
# Quantile Mapping empírico, utilizando a série histórica do INMET como verdade 
# terrestre. Foi estabelecido um limiar de dia chuvoso (wet day) de 0.1 mm para 
# corrigir a superestimativa de garoas contínuas comum em modelos dinâmicos, 
# calibrando a distribuição acumulada do modelo Eta à realidade observada.
# ------------------------------------------------------------------------------
cat("INICIANDO FASE 2: Calibração e Correção de Viés...\n")

vetor_real_treino <- PRECIP[anos_reais_completos >= 1961 & anos_reais_completos <= 2005]

funcao_correcao_TO <- fitQmap(obs = vetor_real_treino, 
                              mod = vetor_simulado_bruto, 
                              method = "QUANT", wet.day = 0.1)

vetor_simulado_corrigido <- doQmap(x = vetor_simulado_bruto, fobj = funcao_correcao_TO)

# ------------------------------------------------------------------------------
# FASE 3: EXTRAÇÃO DO FUTURO E CONSTRUÇÃO DA SÉRIE CONTÍNUA (RESTAURADA)
# ------------------------------------------------------------------------------
# JUSTIFICATIVA METODOLÓGICA PARA O ARTIGO:
# Para viabilizar a modelagem não-estacionária ao longo de todo o século XXI, 
# construiu-se uma série temporal contínua (1961 a 2099) focada no cenário 
# de estabilização intermediária (RCP 4.5). Os dados históricos observados 
# foram concatenados às projeções futuras, sendo aplicada a mesma função de 
# transferência empírica gerada na calibração histórica. A variável Tempo (t) 
# foi escalonada com base no ano inicial (1961 = 1) para alimentar a distribuição.
# ------------------------------------------------------------------------------
cat("INICIANDO FASE 3: Extração do Século XXI (2006-2099)...\n")

# Lista de arquivos baixados (RCP 4.5)
arquivos_futuro <- c("pr_rcp4.5_2006_2041.nc", "pr_rcp4.5_2041_2070.nc", "pr_rcp4.5_2071_2099.nc")

vetor_futuro_bruto <- c()

for (arquivo in arquivos_futuro) {
  chuva_fatia <- crop(rast(arquivo), limites_ajustados)
  n_anos <- nlyr(chuva_fatia) / 360
  temp_max <- numeric(n_anos)
  for (i in 1:n_anos) {
    dia_ini <- ((i - 1) * 360) + 1
    ext <- extract(chuva_fatia[[dia_ini:(i * 360)]], pontos_extracao)
    temp_max[i] <- max(ext[, -1], na.rm = TRUE)
  }
  vetor_futuro_bruto <- c(vetor_futuro_bruto, temp_max)
}

# Corrigindo o viés do futuro usando a mesma "chave" do histórico
vetor_futuro_corrigido <- doQmap(x = vetor_futuro_bruto, fobj = funcao_correcao_TO)

# Criando a Super Série Unificada (Passado Real + Futuro Simulado Corrigido)
anos_continuos <- 1961:2099
super_serie_TO <- data.frame(
  Ano = anos_continuos,
  Tempo = anos_continuos - 1960, # Escalonamento temporal (1961 = 1)
  Precip_Max = c(vetor_real_treino, vetor_futuro_corrigido)
)

# ------------------------------------------------------------------------------
# FASE 4: MODELAGEM GEV NÃO-ESTACIONÁRIA E SELEÇÃO DE CRITÉRIOS
# ------------------------------------------------------------------------------
# JUSTIFICATIVA METODOLÓGICA PARA O ARTIGO:
# Na etapa exploratória da modelagem, a variável Tempo (t) atuou como "proxy" 
# para capturar tendências seculares nas tempestades extremas. Oito arquiteturas 
# de modelos GEV foram ajustadas, permitindo que a média, a variância e a 
# forma da distribuição variassem ano a ano. A seleção do modelo mais adequado 
# foi pautada nos Critérios de Informação de Akaike corrigido (AICc) e Bayesiano 
# (BIC), equilibrando a aderência aos dados e o Princípio da Parcimônia.
# ------------------------------------------------------------------------------
cat("INICIANDO FASE 4: Modelagem e Comparação de Critérios...\n")

# Ajuste dos 8 modelos fundamentais
m0 <- fevd(x = Precip_Max, data = super_serie_TO, type = "GEV")

m1 <- fevd(x = Precip_Max, data = super_serie_TO, location.fun = ~ Tempo, type = "GEV")
plot(m1)
m2 <- fevd(x = Precip_Max, data = super_serie_TO, scale.fun = ~ Tempo, type = "GEV")
m3 <- fevd(x = Precip_Max, data = super_serie_TO, shape.fun = ~ Tempo, type = "GEV")
m4 <- fevd(x = Precip_Max, data = super_serie_TO, location.fun = ~ Tempo, scale.fun = ~ Tempo, type = "GEV")
plot(m4)
m5 <- fevd(x = Precip_Max, data = super_serie_TO, location.fun = ~ Tempo, shape.fun = ~ Tempo, type = "GEV")
m6 <- fevd(x = Precip_Max, data = super_serie_TO, scale.fun = ~ Tempo, shape.fun = ~ Tempo, type = "GEV")
m7 <- fevd(x = Precip_Max, data = super_serie_TO, location.fun = ~ Tempo, scale.fun = ~ Tempo, shape.fun = ~ Tempo, type = "GEV")
plot(m7)
# Função robusta de Critérios de Informação
calcular_criterios <- function(modelo) {
  nll <- modelo$results$value; k <- length(modelo$results$par); n <- modelo$n
  aic <- 2 * k + 2 * nll
  bic <- k * log(n) + 2 * nll
  fator_correcao <- (2 * k * (k + 1)) / (n - k - 1)
  aicc <- aic + fator_correcao
  bicc <- bic + fator_correcao
  return(c(AIC = round(aic, 2), AICc = round(aicc, 2), BIC = round(bic, 2), BICc = round(bicc, 2)))
}

lista_m <- list(m0, m1, m2, m3, m4, m5, m6, m7)
nomes_m <- c("M0: Estacionário", "M1: mu(t)", "M2: sigma(t)", "M3: xi(t)", 
             "M4: mu(t) + sigma(t)", "M5: mu(t) + xi(t)", "M6: sigma(t) + xi(t)", "M7: mu(t) + sigma(t) + xi(t)")

tabela_crit <- data.frame(
  Modelo = nomes_m,
  AIC = sapply(lista_m, function(x) calcular_criterios(x)["AIC"]),
  AICc = sapply(lista_m, function(x) calcular_criterios(x)["AICc"]),
  BIC = sapply(lista_m, function(x) calcular_criterios(x)["BIC"]),
  BICc = sapply(lista_m, function(x) calcular_criterios(x)["BICc"])
)

print("=== RANKING DOS MODELOS (ORDENADO PELO AICc) ===")
print(tabela_crit[order(tabela_crit$AICc), ])

# ------------------------------------------------------------------------------
# FASE 5: PROJEÇÕES E DIAGNÓSTICO VISUAL
# ------------------------------------------------------------------------------
# JUSTIFICATIVA METODOLÓGICA PARA O ARTIGO:
# Para quantificar o impacto na engenharia e planejamento urbano, os Níveis de 
# Retorno (TR) foram projetados para o final do século (2099). A evolução 
# temporal das projeções centenárias foi plotada para contrastar as extrapolações 
# geradas pelos modelos de melhor ajuste estatístico (AICc/BIC), avaliando o 
# risco de explosão matemática nas caudas (instabilidade do parâmetro de forma).
# ------------------------------------------------------------------------------
cat("INICIANDO FASE 5: Projeções e Diagnóstico Final...\n")

periodos <- c(2, 5, 10, 25, 50, 100)
rl_m1 <- return.level(m1, return.period = periodos)
rl_m4 <- return.level(m4, return.period = periodos)
rl_m0 <- return.level(m0, return.period = periodos)

tabela_2099 <- data.frame(
  TR_Anos = periodos,
  M1_2099_mm = round(as.numeric(rl_m1[nrow(rl_m1), ]), 1),
  M4_2099_mm = round(as.numeric(rl_m4[nrow(rl_m4), ]), 1),
  M7_2099_mm = round(as.numeric(rl_m7[nrow(rl_m7), ]), 1)
)
print("--- Níveis de Retorno em 2099 (mm/dia) ---")
print(tabela_2099)

# Plotagem de Diagnóstico (QQ-Plots)
par(mfrow = c(1, 3), mar = c(5, 4, 4, 2) + 0.1)
plot(m1, type = "qq", main = "QQ-Plot M1 (BIC Winner)", col = "blue", pch = 16)
plot(m4, type = "qq", main = "QQ-Plot M4 (Parcimônia)", col = "darkgreen", pch = 16)
plot(m7, type = "qq", main = "QQ-Plot M7 (AICc Winner)", col = "red", pch = 16)

# Evolução Temporal (Duelo de Modelos)
dev.off() # Reseta layout
par(mar = c(5, 5, 4, 2) + 0.1)
plot(super_serie_TO$Ano, super_serie_TO$Precip_Max, type = "h", col = "lightgray", lwd = 2,
     ylab = "Precipitação (mm/dia)", xlab = "Ano",
     main = "Evolução do TR 100 Anos (1961-2099) - Teófilo Otoni",
     ylim = c(40, max(rl_m7[, 6]) * 1.1))

lines(super_serie_TO$Ano, rl_m4[, 6], col = "darkgreen", lwd = 3, lty = 1)
lines(super_serie_TO$Ano, rl_m7[, 6], col = "red", lwd = 2, lty = 2)
lines(super_serie_TO$Ano, rl_m1[, 6], col = "blue", lwd = 3, lty = 3)

legend("topleft", 
       legend = c("Observado/Simulado", "M4 (mu(t) + sigma(t))", "M7 (mu(t) + sigma(t) + xi(t))", "M1 (mu(t))"),
       col = c("lightgray", "darkgreen", "red", "blue"), 
       lty = c(1, 1, 2, 3), lwd = c(2, 3, 2, 3), cex = 0.8, bg = "white")

cat("Execução concluída com sucesso. Scripts prontos para exportação.\n")

# ==============================================================================
# PROJETO: MODELAGEM NÃO-ESTACIONÁRIA DE EXTREMOS CLIMÁTICOS - CENÁRIO 8.5
# LOCAL: Vale do Mucuri (Teófilo Otoni - MG)
# OBJETIVO: Correção de viés (Quantile Mapping) e projeção de Níveis de Retorno
#           sob cenários de Mudança Climática (RCP 8.5) no século XXI.
# ==============================================================================

# Lista de arquivos baixados (RCP 8.5)
arquivos_futuro <- c("pr_rcp8.5_2006_2041.nc", "pr_rcp8.5_2041_2070.nc", "pr_rcp8.5_2071_2099.nc")

vetor_futuro_bruto <- c()

for (arquivo in arquivos_futuro) {
  chuva_fatia <- crop(rast(arquivo), limites_ajustados)
  n_anos <- nlyr(chuva_fatia) / 360
  temp_max <- numeric(n_anos)
  for (i in 1:n_anos) {
    dia_ini <- ((i - 1) * 360) + 1
    ext <- extract(chuva_fatia[[dia_ini:(i * 360)]], pontos_extracao)
    temp_max[i] <- max(ext[, -1], na.rm = TRUE)
  }
  vetor_futuro_bruto <- c(vetor_futuro_bruto, temp_max)
}

# Corrigindo o viés do futuro usando a mesma "chave" do histórico
vetor_futuro_corrigido <- doQmap(x = vetor_futuro_bruto, fobj = funcao_correcao_TO)

# Criando a Super Série Unificada (Passado Real + Futuro Simulado Corrigido)
anos_continuos <- 1961:2099
super_serie_TO <- data.frame(
  Ano = anos_continuos,
  Tempo = anos_continuos - 1960, # Escalonamento temporal (1961 = 1)
  Precip_Max = c(vetor_real_treino, vetor_futuro_corrigido)
)

# Carregando o pacote necessário
library(trend)

# 1. Aplicando o Teste de Mann-Kendall na série de precipitação máxima
teste_mk <- mk.test(super_serie_TO$Precip_Max)

# 2. Calculando a magnitude da tendência (Quantos mm aumenta/diminui por ano)
teste_sen <- sens.slope(super_serie_TO$Precip_Max)

# ------------------------------------------------------------------------------
# FASE 4: MODELAGEM GEV NÃO-ESTACIONÁRIA E SELEÇÃO DE CRITÉRIOS
# ------------------------------------------------------------------------------
# JUSTIFICATIVA METODOLÓGICA PARA O ARTIGO:
# Na etapa exploratória da modelagem, a variável Tempo (t) atuou como "proxy" 
# para capturar tendências seculares nas tempestades extremas. Oito arquiteturas 
# de modelos GEV foram ajustadas, permitindo que a média, a variância e a 
# forma da distribuição variassem ano a ano. A seleção do modelo mais adequado 
# foi pautada nos Critérios de Informação de Akaike corrigido (AICc) e Bayesiano 
# (BIC), equilibrando a aderência aos dados e o Princípio da Parcimônia.
# ------------------------------------------------------------------------------
cat("INICIANDO FASE 4: Modelagem e Comparação de Critérios...\n")

# Ajuste dos 8 modelos fundamentais
m0 <- fevd(x = Precip_Max, data = super_serie_TO, type = "GEV")
plot(m0)
m1 <- fevd(x = Precip_Max, data = super_serie_TO, location.fun = ~ Tempo, type = "GEV")
plot(m1)
m2 <- fevd(x = Precip_Max, data = super_serie_TO, scale.fun = ~ Tempo, type = "GEV")
plot(m2)
m3 <- fevd(x = Precip_Max, data = super_serie_TO, shape.fun = ~ Tempo, type = "GEV")
plot(m3)
m4 <- fevd(x = Precip_Max, data = super_serie_TO, location.fun = ~ Tempo, scale.fun = ~ Tempo, type = "GEV")
plot(m4)
m5 <- fevd(x = Precip_Max, data = super_serie_TO, location.fun = ~ Tempo, shape.fun = ~ Tempo, type = "GEV")
plot(m5)
m6 <- fevd(x = Precip_Max, data = super_serie_TO, scale.fun = ~ Tempo, shape.fun = ~ Tempo, type = "GEV")
plot(m6)
m7 <- fevd(x = Precip_Max, data = super_serie_TO, location.fun = ~ Tempo, scale.fun = ~ Tempo, shape.fun = ~ Tempo, type = "GEV")
plot(m7)

# Função robusta de Critérios de Informação
calcular_criterios <- function(modelo) {
  nll <- modelo$results$value; k <- length(modelo$results$par); n <- modelo$n
  aic <- 2 * k + 2 * nll
  bic <- k * log(n) + 2 * nll
  fator_correcao <- (2 * k * (k + 1)) / (n - k - 1)
  aicc <- aic + fator_correcao
  bicc <- bic + fator_correcao
  return(c(AIC = round(aic, 2), AICc = round(aicc, 2), BIC = round(bic, 2), BICc = round(bicc, 2)))
}

lista_m <- list(m0, m1, m2, m3, m4, m5, m6, m7)
nomes_m <- c("M0: Estacionário", "M1: mu(t)", "M2: sigma(t)", "M3: xi(t)", 
             "M4: mu(t) + sigma(t)", "M5: mu(t) + xi(t)", "M6: sigma(t) + xi(t)", "M7: mu(t) + sigma(t) + xi(t)")

tabela_crit <- data.frame(
  Modelo = nomes_m,
  AIC = sapply(lista_m, function(x) calcular_criterios(x)["AIC"]),
  AICc = sapply(lista_m, function(x) calcular_criterios(x)["AICc"]),
  BIC = sapply(lista_m, function(x) calcular_criterios(x)["BIC"]),
  BICc = sapply(lista_m, function(x) calcular_criterios(x)["BICc"])
)

print("=== RANKING DOS MODELOS (ORDENADO PELO AICc) ===")
print(tabela_crit[order(tabela_crit$AICc), ])

# ------------------------------------------------------------------------------
# FASE 5: PROJEÇÕES E DIAGNÓSTICO VISUAL
# ------------------------------------------------------------------------------
# JUSTIFICATIVA METODOLÓGICA PARA O ARTIGO:
# Para quantificar o impacto na engenharia e planejamento urbano, os Níveis de 
# Retorno (TR) foram projetados para o final do século (2099). A evolução 
# temporal das projeções centenárias foi plotada para contrastar as extrapolações 
# geradas pelos modelos de melhor ajuste estatístico (AICc/BIC), avaliando o 
# risco de explosão matemática nas caudas (instabilidade do parâmetro de forma).
# ------------------------------------------------------------------------------
cat("INICIANDO FASE 5: Projeções e Diagnóstico Final...\n")

periodos <- c(2, 5, 10, 25, 50, 100)
rl_m2 <- return.level(m2, return.period = periodos)
rl_m7 <- return.level(m7, return.period = periodos)
rl_m4 <- return.level(m4, return.period = periodos)

tabela_2099 <- data.frame(
  TR_Anos = periodos,
  M1_2099_mm = round(as.numeric(rl_m2[nrow(rl_m2), ]), 1),
  M4_2099_mm = round(as.numeric(rl_m7[nrow(rl_m7), ]), 1),
  M7_2099_mm = round(as.numeric(rl_m4[nrow(rl_m4), ]), 1)
)
print("--- Níveis de Retorno em 2099 (mm/dia) ---")
print(tabela_2099)

# Plotagem de Diagnóstico (QQ-Plots)
par(mfrow = c(1, 3), mar = c(5, 4, 4, 2) + 0.1)
plot(m2, type = "qq", main = "M2: sigma(t)", col = "blue", pch = 16)
plot(m7, type = "qq", main = "M7: mu(t) + sigma(t) + xi(t)", col = "darkgreen", pch = 16)
plot(m4, type = "qq", main = "M4: mu(t) + sigma(t)", col = "red", pch = 16)

# Evolução Temporal (Duelo de Modelos)
dev.off() # Reseta layout
par(mar = c(5, 5, 4, 2) + 0.1)
plot(super_serie_TO$Ano, super_serie_TO$Precip_Max, type = "h", col = "lightgray", lwd = 2,
     ylab = "Precipitação (mm/dia)", xlab = "Ano",
     main = "Evolução do TR 100 Anos (1961-2099) - Teófilo Otoni",
     ylim = c(40, max(rl_m7[, 6]) * 1.1))

lines(super_serie_TO$Ano, rl_m2[, 6], col = "darkgreen", lwd = 3, lty = 1)
lines(super_serie_TO$Ano, rl_m7[, 6], col = "red", lwd = 2, lty = 2)
lines(super_serie_TO$Ano, rl_m4[, 6], col = "blue", lwd = 3, lty = 3)

legend("topleft", 
       legend = c("Observado/Simulado", "M2: sigma(t)", "M7: mu(t) + sigma(t) + xi(t)", "M4: mu(t) + sigma(t)"),
       col = c("lightgray", "darkgreen", "red", "blue"), 
       lty = c(1, 1, 2, 3), lwd = c(2, 3, 2, 3), cex = 0.8, bg = "white")

cat("Execução concluída com sucesso. Scripts prontos para exportação.\n")

