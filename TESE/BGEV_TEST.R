# ====================================================================
# PREPARAÇÃO: CRIANDO A SUPER SÉRIE (1968 a 2099)
# ====================================================================
library(dplyr)

cat("Construindo a Super Série Histórica + Futuro...\n")

# 1. Pegamos a realidade (1968 a 2005) da sua planilha EXTREMOS2
passado_real <- extremos_reais_anuais %>% 
  filter(ano >= 1968 & ano <= 2005) %>%
  select(Ano = ano, Precip_Max = Precip_Max) %>%
  mutate(Origem = "Real_Observado")

# 2. Pegamos o século XXI corrigido (2006 a 2099) que costuramos antes
futuro_proj <- futuro_completo_bruto %>%
  select(Ano = Ano, Precip_Max = Simulado_Corrigido) %>%
  mutate(Origem = "RCP_4.5_Corrigido")

# 3. A união perfeita usando o pacote dplyr
super_serie <- bind_rows(passado_real, futuro_proj)

cat("Tudo pronto! A 'super_serie' agora tem", nrow(super_serie), "anos de dados contínuos.\n")

# ====================================================================
# AVALIAÇÃO EXAUSTIVA: OS 8 MODELOS DA GEV NÃO-ESTACIONÁRIA (CORRIGIDO)
# ====================================================================
library(extRemes)

cat("Ajustando todas as 8 combinações possíveis de não-estacionariedade...\n")

# M0: Estacionário Clássico (Nenhum parâmetro varia)
mod0 <- fevd(x = super_serie$Precip_Max, data = super_serie, 
             type = "GEV", units = "mm/dia")

# --------------------------------------------------------------------
# VARIAÇÕES INDIVIDUAIS (Apenas 1 parâmetro varia com o tempo)
# --------------------------------------------------------------------
# M1: Apenas Locação muda (A média desliza)
mod1 <- fevd(x = super_serie$Precip_Max, data = super_serie, 
             location.fun = ~ Ano, type = "GEV", units = "mm/dia")

# M2: Apenas Escala muda (A variância/incerteza aumenta)
mod2 <- fevd(x = super_serie$Precip_Max, data = super_serie, 
             scale.fun = ~ Ano, type = "GEV", units = "mm/dia")

# M3: Apenas Forma muda (A cauda de extremos raros fica mais pesada)
mod3 <- fevd(x = super_serie$Precip_Max, data = super_serie, 
             shape.fun = ~ Ano, type = "GEV", units = "mm/dia")

# --------------------------------------------------------------------
# VARIAÇÕES DUPLAS (2 parâmetros variam simultaneamente)
# --------------------------------------------------------------------
# M4: Locação e Escala mudam 
mod4 <- fevd(x = super_serie$Precip_Max, data = super_serie, 
             location.fun = ~ Ano, scale.fun = ~ Ano, type = "GEV", units = "mm/dia")

# M5: Locação e Forma mudam
mod5 <- fevd(x = super_serie$Precip_Max, data = super_serie, 
             location.fun = ~ Ano, shape.fun = ~ Ano, type = "GEV", units = "mm/dia")

plot(mod5)
# M6: Escala e Forma mudam
mod6 <- fevd(x = super_serie$Precip_Max, data = super_serie, 
             scale.fun = ~ Ano, shape.fun = ~ Ano, type = "GEV", units = "mm/dia")

plot(mod6)
# --------------------------------------------------------------------
# VARIAÇÃO TOTAL (O caos completo)
# --------------------------------------------------------------------
# M7: Todos os parâmetros mudam (Locação, Escala e Forma)
mod7 <- fevd(x = super_serie$Precip_Max, data = super_serie, 
             location.fun = ~ Ano, scale.fun = ~ Ano, shape.fun = ~ Ano, 
             type = "GEV", units = "mm/dia")
plot(mod7)
# ====================================================================
# O GRANDE RANKING (CRITÉRIO DE AKAIKE - AIC)
# ====================================================================
# Extraindo os valores de AIC de cada modelo
aic_tabela <- data.frame(
  Modelo = c("M0: Estacionario", 
             "M1: Apenas mu(t)", 
             "M2: Apenas sigma(t)", 
             "M3: Apenas xi(t)", 
             "M4: mu(t) + sigma(t)", 
             "M5: mu(t) + xi(t)", 
             "M6: sigma(t) + xi(t)", 
             "M7: mu(t) + sigma(t) + xi(t)"),
  AIC = c(summary(mod0)$AIC, 
          summary(mod1)$AIC, 
          summary(mod2)$AIC, 
          summary(mod3)$AIC, 
          summary(mod4)$AIC, 
          summary(mod5)$AIC, 
          summary(mod6)$AIC, 
          summary(mod7)$AIC)
)

# Ordenando a tabela do menor AIC (Melhor) para o maior AIC (Pior)
aic_tabela <- aic_tabela[order(aic_tabela$AIC), ]

print("=== RANKING DOS MODELOS (O MENOR AIC VENCE) ===")
print(aic_tabela)

# Destacando o vencedor
melhor_modelo <- as.character(aic_tabela$Modelo[1])
cat(paste("\n🏆 O Vencedor Estatístico para Viçosa é:", melhor_modelo, "\n"))

# ====================================================================
# DIAGNÓSTICO DA BIMODALIDADE: QUEM É O CULPADO?
# ====================================================================
cat("Investigando a origem das duas modas...\n")

# Separando os vetores
dados_passado <- super_serie$Precip_Max[super_serie$Origem == "Real_Observado"]
dados_futuro  <- super_serie$Precip_Max[super_serie$Origem == "RCP_4.5_Corrigido"]

# Plotando as duas densidades sobrepostas
plot(density(dados_futuro), col = "red", lwd = 2, 
     main = "Investigação da Bimodalidade em Viçosa", 
     xlab = "Precipitação Extrema Anual (mm/dia)", ylab = "Densidade",
     xlim = c(min(super_serie$Precip_Max), max(super_serie$Precip_Max)))

lines(density(dados_passado), col = "blue", lwd = 2)

legend("topright", legend = c("Passado Real (1968-2005)", "Futuro RCP 4.5 (2006-2099)"),
       col = c("blue", "red"), lwd = 2)

# ====================================================================
# AJUSTE DA DISTRIBUIÇÃO BGEV (Bimodal GEV - UnB)
# ====================================================================

# 1. Instalar o pacote oficial se for a primeira vez
# install.packages("bgev")
library(bgev)

cat("Ajustando a BGEV (4 parâmetros) para a super série (1968-2099)...\n")

# Extraindo apenas o vetor numérico de precipitação
vetor_chuva <- super_serie$Precip_Max

# 2. Criando o motor de otimização (Maximum Likelihood Estimation)
# Como o pacote nos dá a função dbgev (densidade), criamos a Log-Verossimilhança Negativa
nll_bgev <- function(pars) {
  mu_val    <- pars[1]
  sigma_val <- pars[2]
  xi_val    <- pars[3]
  delta_val <- pars[4]
  
  # Trava matemática: sigma tem que ser positivo e delta > -1
  if (sigma_val <= 0 | delta_val <= -1) return(1e9) 
  
  # Calcula a densidade para cada ponto de chuva usando o pacote da UnB
  densidade <- dbgev(vetor_chuva, mu = mu_val, sigma = sigma_val, 
                     xi = xi_val, delta = delta_val)
  
  # Evita log(0) que quebra a matemática
  densidade[densidade <= 0] <- 1e-10 
  
  # Retorna a soma do log negativo (o que o R vai tentar minimizar)
  return(-sum(log(densidade)))
}

# 3. Os chutes iniciais para a máquina começar a procurar
# Colocamos o 'mu' perto do primeiro pico (60) e o delta ativado (0.5)
chutes <- c(mu = 66, sigma = 61, xi = 0.4, delta = 0.26)

# 4. Rodando o otimizador Nelder-Mead
ajuste_bgev <- optim(par = chutes, fn = nll_bgev, 
                     method = "Nelder-Mead", 
                     control = list(maxit = 10000))

parametros_finais <- ajuste_bgev$par
nomes_parametros <- c("Locação (mu)", "Escala (sigma)", "Forma (xi)", "Bimodalidade (delta)")
names(parametros_finais) <- nomes_parametros

print("=== PARÂMETROS VENCEDORES DA BGEV ===")
print(round(parametros_finais, 3))

# ====================================================================
# PLOTAGEM DO RESULTADO (O TRIUNFO DA METODOLOGIA)
# ====================================================================
# Desenhando o Histograma de Fundo
hist(vetor_chuva, breaks = 30, prob = TRUE, col = "lightgray", border = "white",
     main = "BGEV: A Modelagem do Novo Regime Climático em Viçosa",
     xlab = "Precipitação Extrema Anual (mm/dia)", ylab = "Densidade",
     ylim = c(0, max(density(vetor_chuva)$y) * 1.2))

# Plotando a densidade empírica pontilhada (o que a natureza + modelo fizeram)
lines(density(vetor_chuva), lwd = 2, lty = 2, col = "black")

# Plotando a Curva BGEV Teórica Ajustada (A Mágica do 4º Parâmetro)
eixo_x <- seq(min(vetor_chuva), max(vetor_chuva), length.out = 500)
curva_bgev <- dbgev(eixo_x, 
                    mu = parametros_finais[1], 
                    sigma = parametros_finais[2], 
                    xi = parametros_finais[3], 
                    delta = parametros_finais[4])

# A linha verde grossa é o seu modelo matemático final!
lines(eixo_x, curva_bgev, col = "darkgreen", lwd = 3)

legend("topright", legend = c("Dados (1968-2099)", "Densidade Empírica", "Curva Teórica BGEV"),
       fill = c("lightgray", NA, NA), col = c(NA, "black", "darkgreen"), 
       lty = c(NA, 2, 1), lwd = c(NA, 2, 3), border = c("white", NA, NA), cex = 0.8)


# ====================================================================
# AJUSTE DA BGEV - MOTOR DE OTIMIZAÇÃO AVANÇADO (L-BFGS-B)
# ====================================================================

cat("Rodando o otimizador L-BFGS-B com limites rígidos...\n")

# 1. Os Chutes Calibrados (Olhando diretamente para o seu histograma)
# mu = 85 (Onde está o pico principal cinza)
# sigma = 15 (A largura aproximada do pico principal)
# xi = 0.1 (Extremos climáticos começam com caudas leves)
# delta = 0.5 (Ligando o interruptor da bimodalidade)
chutes_calibrados <- c(mu = 80, sigma = 30, xi = -0.6, delta = 0.5)

# 2. As "Paredes" (Limites inferiores e superiores para o algoritmo não surtar)
limite_inferior <- c(mu = 40,  sigma = 1,   xi = -0.5, delta = -2)
limite_superior <- c(mu = 150, sigma = 100, xi = 1.0,  delta = 5)

# 3. Rodando o novo motor
ajuste_bgev_novo <- optim(par = chutes_calibrados, 
                          fn = nll_bgev, 
                          method = "L-BFGS-B", 
                          lower = limite_inferior,
                          upper = limite_superior,
                          control = list(maxit = 10000))

parametros_finais <- ajuste_bgev_novo$par
names(parametros_finais) <- c("Locação (mu)", "Escala (sigma)", "Forma (xi)", "Bimodalidade (delta)")

print("=== NOVOS PARÂMETROS VENCEDORES DA BGEV ===")
print(round(parametros_finais, 3))

# ====================================================================
# NOVA PLOTAGEM DO RESULTADO
# ====================================================================
hist(vetor_chuva, breaks = 30, prob = TRUE, col = "lightgray", border = "white",
     main = "BGEV (Corrigida): A Modelagem do Novo Regime Climático em Viçosa",
     xlab = "Precipitação Extrema Anual (mm/dia)", ylab = "Densidade",
     ylim = c(0, max(density(vetor_chuva)$y) * 1.2))

lines(density(vetor_chuva), lwd = 2, lty = 2, col = "black")

# Desenhando a nova curva
eixo_x <- seq(min(vetor_chuva), max(vetor_chuva), length.out = 500)
curva_bgev_nova <- dbgev(eixo_x, 
                         mu = parametros_finais[1], 
                         sigma = parametros_finais[2], 
                         xi = parametros_finais[3], 
                         delta = parametros_finais[4])

# Linha verde atualizada
lines(eixo_x, curva_bgev_nova, col = "darkgreen", lwd = 3)

legend("topright", legend = c("Dados (1968-2099)", "Densidade Empírica", "Curva Teórica BGEV"),
       fill = c("lightgray", NA, NA), col = c(NA, "black", "darkgreen"), 
       lty = c(NA, 2, 1), lwd = c(NA, 2, 3), border = c("white", NA, NA), cex = 0.8)
