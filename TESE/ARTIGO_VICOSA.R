# ====================================================================
# MODELO DE MISTURA (ZCAS vs Convectiva) - SCRIPT DEFINITIVO
# LOCAL: Viçosa (MG) - Cenário RCP 4.5
# ====================================================================
library(terra)
library(qmap)
library(dplyr)
library(extRemes)
library(stats)

# ====================================================================
# FASE 1: DADOS REAIS E CONFIGURAÇÃO ESPACIAL
# ====================================================================
# O Vetor Oficial do INMET de Viçosa (1968 a 2005)
PRECIP <- c(90.0, 59.2, 75.7, 75.8, 72.7, 42.2, 106.0, 96.6, 84.4, 65.8, 
            73.6, 163.9, 83.6, 91.2, 112.6, 78.4, 82.3, 100.9, 184.8, 110.8, 
            56.7, 67.3, 67.7, 74.8, 77.0, 44.3, 109.3, 69.5, 54.6, 92.2, 
            133.4, 73.9, 87.4, 108.6, 72.8, 71.6, 133.4, 80.3)

limites_ajustados <- ext(-44.0, -41.5, -22.0, -19.5)
pontos_extracao <- data.frame(lon = -42.88, lat = -20.75)


# ====================================================================
# FASE 2: TREINANDO O QUANTILE MAPPING (HISTÓRICO)
# ====================================================================
cat("\n=== 2. TREINANDO O QUANTILE MAPPING (HISTÓRICO) ===\n")

dados_chuva_hist <- rast("pr_historical_1961_2005.nc")
chuva_focada_hist <- crop(dados_chuva_hist, limites_ajustados)

vetor_simulado_bruto <- numeric(45) # 1961 a 2005
for (i in 1:45) {
  dia_inicial <- ((i - 1) * 360) + 1
  chuva_extraida <- extract(chuva_focada_hist[[dia_inicial:(i * 360)]], pontos_extracao)
  vetor_simulado_bruto[i] <- max(chuva_extraida[, -1], na.rm = TRUE)
}

# Isolando 1968-2005 e criando a "chave" de correção
vetor_simulado_treino <- vetor_simulado_bruto[8:45] 
funcao_correcao_VIC <- fitQmap(obs = PRECIP, mod = vetor_simulado_treino, method = "QUANT", wet.day = 0.1)


# ====================================================================
# FASE 3: EXTRAINDO E CORRIGINDO O SÉCULO XXI (RCP 4.5)
# ====================================================================
cat("\n=== 3. EXTRAINDO E CORRIGINDO O SÉCULO XXI (RCP 4.5) ===\n")

arquivos_futuro_45 <- c("pr_rcp4.5_2006_2041.nc", "pr_rcp4.5_2041_2070.nc", "pr_rcp4.5_2071_2099.nc")
vetor_futuro_bruto_45 <- c()

for (arquivo in arquivos_futuro_45) {
  chuva_fatia <- crop(rast(arquivo), limites_ajustados)
  n_anos <- nlyr(chuva_fatia) / 360
  temp_max <- numeric(n_anos)
  for (i in 1:n_anos) {
    dia_ini <- ((i - 1) * 360) + 1
    ext_val <- extract(chuva_fatia[[dia_ini:(i * 360)]], pontos_extracao)
    temp_max[i] <- max(ext_val[, -1], na.rm = TRUE)
  }
  vetor_futuro_bruto_45 <- c(vetor_futuro_bruto_45, temp_max)
}

# Aplicando a chave de correção no futuro
vetor_futuro_corrigido_45 <- doQmap(x = vetor_futuro_bruto_45, fobj = funcao_correcao_VIC)


# ====================================================================
# FASE 4: A GRANDE COSTURA (CRIANDO A SÉRIE DA MISTURA)
# ====================================================================
cat("\n=== 4. A GRANDE COSTURA: CRIANDO O VETOR_CHUVA ===\n")

anos_continuos <- 1968:2099

super_serie_VIC_45 <- data.frame(
  Ano = anos_continuos,
  Precip_Max = c(PRECIP, vetor_futuro_corrigido_45)
)

# Adicionando as etiquetas de origem e consolidando a base da Mistura
super_serie_mistura <- super_serie_VIC_45 %>%
  mutate(Origem = ifelse(Ano <= 2005, "Real_Observado", "RCP_4.5_Corrigido"))

# O Vetor Oficial que alimenta a estatística
vetor_chuva <- super_serie_mistura$Precip_Max
cat("Vetor de chuva resgatado com sucesso! Possui", length(vetor_chuva), "anos de dados.\n")


# ====================================================================
# FASE 5: AJUSTE DO MODELO DE MISTURA (ZCAS vs Convectiva)
# ====================================================================
cat("\n=== 5. INICIANDO O AJUSTE DO MODELO DE MISTURA (Penalidade de Leptocurtose) ===\n")

# Função de Log-Verossimilhança Negativa para a Mistura
nll_mistura <- function(pars) {
  p    <- pars[1] 
  mu1  <- pars[2]; sig1 <- pars[3]; xi1 <- pars[4] 
  mu2  <- pars[5]; sig2 <- pars[6]; xi2 <- pars[7] 
  
  if(p <= 0 || p >= 1 || sig1 <= 0 || sig2 <= 0) return(1e9)
  
  dens1 <- tryCatch(devd(vetor_chuva, loc=mu1, scale=sig1, shape=xi1, type="GEV"), error=function(e) rep(0, length(vetor_chuva)))
  dens2 <- tryCatch(devd(vetor_chuva, loc=mu2, scale=sig2, shape=xi2, type="GEV"), error=function(e) rep(0, length(vetor_chuva)))
  
  dens_mistura <- p * dens1 + (1 - p) * dens2
  dens_mistura[is.na(dens_mistura) | dens_mistura <= 0] <- 1e-10
  return(-sum(log(dens_mistura)))
}

# Chutes e Limites Estratégicos (Achatando a ZCAS)
chutes_mix <- c(
  p = 0.85,      
  mu1 = 80,  sig1 = 15, xi1 = 0.1,  
  mu2 = 190, sig2 = 40, xi2 = 0.1   
)

lim_inf <- c(p = 0.1,  mu1 = 40,  sig1 = 1, xi1 = -0.5, 
             mu2 = 150, sig2 = 10, xi2 = -0.5)

lim_sup <- c(p = 0.9,  mu1 = 120, sig1 = 60, xi1 = 1.0, 
             mu2 = 300, sig2 = 80, xi2 = 0.3)

# Otimização
ajuste_mix <- optim(par = chutes_mix, 
                    fn = nll_mistura, 
                    method = "L-BFGS-B", 
                    lower = lim_inf, 
                    upper = lim_sup, 
                    control = list(maxit = 20000))

pars_mix <- ajuste_mix$par
names(pars_mix) <- c("Peso(p)", "mu1", "sig1", "xi1", "mu2", "sig2", "xi2")

cat("\n=== PARÂMETROS FINAIS DO MODELO DE MISTURA ===\n")
print(round(pars_mix, 3))


# ====================================================================
# FASE 6: PLOTAGEM AVANÇADA (DISSECANDO A MISTURA)
# ====================================================================
cat("\n=== 6. GERANDO GRÁFICO DE DIAGNÓSTICO ===\n")

hist(vetor_chuva, breaks = 30, prob = TRUE, col = "gray90", border = "white",
     main = "Modelo de Mistura (ZCAS vs Convectiva) em Viçosa - RCP 4.5",
     xlab = "Precipitação Extrema Anual (mm/dia)", ylab = "Densidade",
     ylim = c(0, max(density(vetor_chuva)$y) * 1.3))

lines(density(vetor_chuva), lwd = 2, lty = 2, col = "black") 

eixo_x <- seq(min(vetor_chuva), max(vetor_chuva), length.out = 500)

curva1 <- devd(eixo_x, loc=pars_mix[2], scale=pars_mix[3], shape=pars_mix[4], type="GEV")
curva2 <- devd(eixo_x, loc=pars_mix[5], scale=pars_mix[6], shape=pars_mix[7], type="GEV")

curva1_ponderada <- pars_mix[1] * curva1
curva2_ponderada <- (1 - pars_mix[1]) * curva2
curva_total <- curva1_ponderada + curva2_ponderada

polygon(c(eixo_x, rev(eixo_x)), c(curva1_ponderada, rep(0, length(eixo_x))), 
        col=rgb(0, 0, 1, 0.2), border=NA) 
polygon(c(eixo_x, rev(eixo_x)), c(curva2_ponderada, rep(0, length(eixo_x))), 
        col=rgb(1, 0, 0, 0.2), border=NA) 

lines(eixo_x, curva1_ponderada, col="blue", lwd=2)      
lines(eixo_x, curva2_ponderada, col="red", lwd=2)       
lines(eixo_x, curva_total, col="darkgreen", lwd=3)      

legend("topright", 
       legend = c("Densidade Empírica", "GEV 1 (Convectiva)", "GEV 2 (ZCAS)", "Mistura Total"),
       col = c("black", "blue", "red", "darkgreen"), 
       lty = c(2, 1, 1, 1), lwd = c(2, 2, 2, 3), cex = 0.8, bg = "white")


# ====================================================================
# FASE 7: CÁLCULO DOS NÍVEIS DE RETORNO (INVERSÃO DA CDF)
# ====================================================================
cat("\n=== 7. CALCULANDO NÍVEIS DE RETORNO VIA INVERSÃO NUMÉRICA ===\n")

# Função Cumulativa (CDF) da Mistura
cdf_mistura <- function(x, pars) {
  p <- pars[1]
  cdf1 <- pevd(x, loc=pars[2], scale=pars[3], shape=pars[4], type="GEV")
  cdf2 <- pevd(x, loc=pars[5], scale=pars[6], shape=pars[7], type="GEV")
  return(p * cdf1 + (1 - p) * cdf2)
}

periodos_retorno <- c(2, 5, 10, 25, 50, 100)
niveis_projetados <- numeric(length(periodos_retorno))

for (i in 1:length(periodos_retorno)) {
  TR <- periodos_retorno[i]
  probabilidade_alvo <- 1 - (1 / TR)
  
  busca <- uniroot(function(x) cdf_mistura(x, pars_mix) - probabilidade_alvo, 
                   lower = 0, upper = 1000, extendInt = "yes")
  
  niveis_projetados[i] <- busca$root
}

tabela_TR_Mistura <- data.frame(
  TR_Anos = periodos_retorno,
  Probabilidade_Nao_Excedencia = paste0(round((1 - 1/periodos_retorno) * 100, 1), "%"),
  Precipitacao_Esperada_mm = round(niveis_projetados, 1)
)

cat("\n=== TABELA FINAL DE NÍVEIS DE RETORNO (VIÇOSA - RCP 4.5 Bimodal) ===\n")
print(tabela_TR_Mistura)

# ====================================================================
# [8] CÁLCULO DOS INTERVALOS DE CONFIANÇA (BOOTSTRAP 95%)
# ====================================================================
cat("\n=== 8. INICIANDO BOOTSTRAP PARA INTERVALOS DE CONFIANÇA (95%) ===\n")
cat("Atenção: O R vai rodar o modelo 100 vezes. Isso leva alguns minutos...\n")

# Configurações do Bootstrap
n_boot <- 100
niveis_boot <- matrix(NA, nrow = n_boot, ncol = length(periodos_retorno))

# Usamos os parâmetros vencedores da amostra original como "chute inicial"
# do bootstrap para acelerar muito a convergência do algoritmo
chutes_boot <- pars_mix 

for(b in 1:n_boot) {
  # 1. Cria uma nova história climática embaralhando os dados reais (com reposição)
  amostra_boot <- sample(vetor_chuva, replace = TRUE)
  
  # 2. Função de Verossimilhança adaptada para a amostra do universo paralelo
  nll_boot <- function(pars) {
    p <- pars[1]; mu1 <- pars[2]; sig1 <- pars[3]; xi1 <- pars[4]
    mu2 <- pars[5]; sig2 <- pars[6]; xi2 <- pars[7]
    
    if(p <= 0 || p >= 1 || sig1 <= 0 || sig2 <= 0) return(1e9)
    
    dens1 <- tryCatch(devd(amostra_boot, loc=mu1, scale=sig1, shape=xi1, type="GEV"), error=function(e) rep(0, length(amostra_boot)))
    dens2 <- tryCatch(devd(amostra_boot, loc=mu2, scale=sig2, shape=xi2, type="GEV"), error=function(e) rep(0, length(amostra_boot)))
    
    dens_m <- p * dens1 + (1 - p) * dens2
    dens_m[is.na(dens_m) | dens_m <= 0] <- 1e-10
    return(-sum(log(dens_m)))
  }
  
  # 3. Ajusta o Modelo de Mistura para essa nova amostra
  fit_boot <- tryCatch(
    optim(par = chutes_boot, fn = nll_boot, method = "L-BFGS-B", 
          lower = lim_inf, upper = lim_sup, control = list(maxit = 5000)),
    error = function(e) NULL
  )
  
  # 4. Se o modelo convergiu, calcula os Níveis de Retorno
  if(!is.null(fit_boot)) {
    for(i in 1:length(periodos_retorno)) {
      prob_alvo <- 1 - (1 / periodos_retorno[i])
      busca <- tryCatch(
        uniroot(function(x) cdf_mistura(x, fit_boot$par) - prob_alvo, 
                lower = 0, upper = 1000, extendInt = "yes"),
        error = function(e) list(root = NA)
      )
      niveis_boot[b, i] <- busca$root
    }
  }
  
  # Imprime o progresso no console
  if(b %% 10 == 0) cat(" Progresso Bootstrap: ", b, "/", n_boot, " concluídos...\n")
}

# 5. Calculando os percentis 2.5% (Limite Inferior) e 97.5% (Limite Superior)
limite_inferior <- apply(niveis_boot, 2, quantile, probs = 0.025, na.rm = TRUE)
limite_superior <- apply(niveis_boot, 2, quantile, probs = 0.975, na.rm = TRUE)

# ====================================================================
# [9] A TABELA FINAL DEFINITIVA COM MARGEM DE ERRO
# ====================================================================
tabela_TR_Definitiva <- data.frame(
  TR_Anos = periodos_retorno,
  Probabilidade = paste0(round((1 - 1/periodos_retorno) * 100, 1), "%"),
  IC_Inferior_95 = round(limite_inferior, 1),
  Precipitacao_Esperada_mm = round(niveis_projetados, 1), # A pontual que calculamos antes
  IC_Superior_95 = round(limite_superior, 1)
)

cat("\n====================================================================\n")
cat("      TABELA OFICIAL: NÍVEIS DE RETORNO COM IC 95% (MISTURA)        \n")
cat("====================================================================\n")
print(tabela_TR_Definitiva)
cat("\nNota para a dissertação: Intervalos gerados via Non-Parametric Bootstrap (B=100).\n")