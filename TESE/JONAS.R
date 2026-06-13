library(terra)
library(qmap) 

# Carregar o arquivo NetCDF para a memória
arquivo_nc <- "pr_historical_1961_2005.nc"
dados_chuva <- rast(arquivo_nc)

situ45 <- "pr_rcp4.5_2006_2041.nc"
dados_4.5 <- rast(situ45)

situ85 <- "pr_rcp8.5_2006_2041.nc"
dados_8.5 <- rast(situ85)

library(terra)
library(ggplot2)
library(dplyr)
library(tidyr)
library(geobr)
library(sf)

# 1. Carregar Limites Espaciais
vicosa_sf <- read_municipality(code_muni = 3171303, year = 2020, showProgress = FALSE)
vicosa_vect <- vect(vicosa_sf)

# 2. Extrair dados dos Rasters (Carregamento em etapas seguras)

# Passo A: Carregar os rasters na memória (Se der erro aqui, o nome do arquivo pode estar errado)
r45 <- rast("pr_rcp4.5_2006_2041.nc")
r85 <- rast("pr_rcp8.5_2006_2041.nc")

# Passo B: Recortar os rasters para o contorno de Viçosa 
dados_45_crop <- terra::crop(r45, vicosa_vect)
dados_85_crop <- terra::crop(r85, vicosa_vect)

# Passo C: Criar a função de extração e anualização
extrair_anual <- function(rast_obj, nome_cenario) {
  anos <- as.numeric(format(time(rast_obj), "%Y"))
  max_bruta <- as.numeric(terra::extract(rast_obj, vicosa_vect, fun = max, na.rm = TRUE)[, -1])
  
  data.frame(Ano = anos, Prec = max_bruta) %>%
    group_by(Ano) %>%
    summarise(!!nome_cenario := max(Prec, na.rm = TRUE)) %>%
    filter(Ano <= 2022) # Mantém apenas até 2022
}

# Passo D: Aplicar a função (Agora os objetos existem com certeza)
sim_45 <- extrair_anual(dados_45_crop, "RCP_4.5")
sim_85 <- extrair_anual(dados_85_crop, "RCP_8.5")

# 3. Preparar os seus Dados Reais Observados
dados_reais <- c(90, 59.2, 75.7, 75.8, 72.7, 42.2, 106, 96.6, 84.4, 65.8, 73.6, 
                 163.9, 83.6, 91.2, 112.6, 78.4, 82.3, 100.9, 184.8, 110.8, 56.7, 
                 67.3, 67.7, 74.8, 77, 44.3, 109.3, 69.5, 54.6, 92.2, 133.4, 73.9, 
                 87.4, 108.6, 72.8, 71.6, 133.4, 80.3, 90.5, 92.7, 94.4, 80.2, 
                 64.2, 89, 89.8, 60.4, 59.6, 110.4, 80.6, 87.2, 50.6, 112.6, 126.1, 
                 51.6, 84)
year <- seq(1968, 2022, 1)

df_reais <- data.frame(Ano = year, Observado = dados_reais)

# Filtrar para a janela de validação (2006 - 2022)
obs_vicosa <- df_reais %>%
  filter(Ano >= 2006 & Ano <= 2022)

# 4. Unir os conjuntos de dados
df_completo <- obs_vicosa %>%
  left_join(sim_45, by = "Ano") %>%
  left_join(sim_85, by = "Ano")

# Colocar no formato longo para o ggplot2
df_long <- df_completo %>%
  pivot_longer(cols = c(Observado, RCP_4.5, RCP_8.5), names_to = "Cenario", values_to = "Prec_Max")

# 5. Criar o Gráfico de Comparação
ggplot(df_long, aes(x = Ano, y = Prec_Max, color = Cenario, linetype = Cenario)) +
  geom_line(aes(size = Cenario == "Observado")) +
  geom_point(aes(size = Cenario == "Observado")) +
  
  # Destacar a linha dos dados reais (linha sólida e mais grossa)
  scale_size_manual(values = c("TRUE" = 1.2, "FALSE" = 0.7), guide = "none") +
  scale_linetype_manual(values = c("Observado" = "solid", "RCP_4.5" = "dashed", "RCP_8.5" = "dotdash")) +
  scale_color_manual(values = c("Observado" = "black", "RCP_4.5" = "#2c7bb6", "RCP_8.5" = "#d7191c")) +
  
  scale_x_continuous(breaks = seq(2006, 2022, by = 2)) +
  labs(
    title = "Validação: Máximas Observadas vs. Modelos Climáticos",
    subtitle = "Município de Viçosa (2006 a 2022)",
    x = "Ano",
    y = "Precipitação Máxima Anual (mm)",
    color = "Série Histórica",
    linetype = "Série Histórica"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "bottom",
    legend.title = element_blank()
  )


# Enquadramento da Zona da Mata)
limites_ajustados <- ext(-44.2, -41.2, -22.5, -19.5)
chuva_zm_focada <- crop(dados_chuva, limites_ajustados)

# A Fronteira (Matriz com os 21 pontos estratégicos do contorno)
bordas_zm_21pts <- matrix(c(
  -41.85, -19.55, -41.65, -19.80, -41.75, -20.30, -41.80, -20.60, 
  -42.00, -20.90, -42.20, -21.30, -42.45, -21.60, -42.70, -21.85, 
  -43.00, -21.95, -43.40, -22.05, -43.80, -22.00, -43.95, -21.80, 
  -43.80, -21.45, -43.60, -21.20, -43.40, -20.95, -43.30, -20.60, 
  -43.20, -20.20, -42.90, -19.90, -42.30, -19.75, -42.00, -19.60, 
  -41.85, -19.55
), ncol = 2, byrow = TRUE)

contorno_zm <- vect(bordas_zm_21pts, type = "polygons", crs = crs(chuva_zm_focada))

# Os Pontos de Estudo (Viçosa e Juiz de Fora)
cidades_estudo <- data.frame(
  cidade = c("Viçosa", "Juiz de Fora"),
  lon = c(-42.88, -43.35),
  lat = c(-20.75, -21.76)
)

# Plotagem do Mapa
plot(chuva_zm_focada[[1]], main = "Zona da Mata: Viçosa e Juiz de Fora")
plot(contorno_zm, add = TRUE, border = "white", lwd = 2, lty = 2)
points(x = cidades_estudo$lon, y = cidades_estudo$lat, col = "red", pch = 19, cex = 1.5)
text(x = cidades_estudo$lon, y = cidades_estudo$lat, labels = cidades_estudo$cidade, 
     pos = 4, col = "black", cex = 1.1, font = 2)

# O LAÇO PARA A CHUVA MÁXIMA ANUAL (Rx1day)
print("Extraindo os extremos simulados (1961-2005)...")
anos_simulados <- 1961:2005
num_anos <- length(anos_simulados)

matriz_rx1day <- matrix(NA, nrow = num_anos, ncol = 2)
colnames(matriz_rx1day) <- cidades_estudo$cidade
rownames(matriz_rx1day) <- anos_simulados
pontos_extracao <- cidades_estudo[, c("lon", "lat")]

for (i in 1:num_anos) {
  dia_inicial <- ((i - 1) * 360) + 1
  dia_final <- i * 360
  chuva_do_ano <- chuva_zm_focada[[dia_inicial:dia_final]]
  chuva_extraida <- extract(chuva_do_ano, pontos_extracao)
  maximo_anual <- apply(chuva_extraida[, -1], 1, max, na.rm = TRUE)
  matriz_rx1day[i, ] <- maximo_anual
}

dados_finais <- as.data.frame(matriz_rx1day)

# PARTE 2: VALIDAÇÃO DO MODELO (DADOS REAIS VS SIMULADOS 1968-2005)

print("Iniciando a validação com os dados reais...")

# 1. Os dados reais (INMET/Estação de Viçosa)
anos_observados <- 1968:2005 

precip_real <- c(90.0, 59.2, 75.7, 75.8, 72.7, 42.2, 106.0, 96.6, 84.4, 65.8, 
                 73.6, 163.9, 83.6, 91.2, 112.6, 78.4, 82.3, 100.9, 184.8, 110.8, 
                 56.7, 67.3, 67.7, 74.8, 77.0, 44.3, 109.3, 69.5, 54.6, 92.2, 
                 133.4, 73.9, 87.4, 108.6, 72.8, 71.6, 133.4, 80.3)

# 2. Recortando a Simulação (Viçosa) correspondente ao período real
simulacao_vicosa <- dados_finais[as.character(anos_observados), "Viçosa"]

# Juntando tudo na Tabela Mestra
tabela_comparativa <- data.frame(
  Ano = anos_observados,
  Real = precip_real,
  Simulado_Bruto = simulacao_vicosa
)

# PARTE 3:  QUANTILE MAPPING

print("Treinando o modelo de correção de viés...")

# 1. Preparação dos Vetores diretamente da tabela
vetor_real <- as.numeric(na.omit(tabela_comparativa$Real))    
vetor_simulado <- as.numeric(na.omit(tabela_comparativa$Simulado_Bruto))

# 2. Treinando o Algoritmo (Encontrando a Função de Transferência)
funcao_correcao <- fitQmap(obs = vetor_real, 
                           mod = vetor_simulado, 
                           method = "QUANT", 
                           wet.day = 0.1)

# 3. Aplicando a Correção
print("Aplicando o downscaling aos dados do modelo...")
tabela_comparativa$Simulado_Corrigido <- doQmap(x = vetor_simulado, 
                                                fobj = funcao_correcao)

# PARTE 4: COMPROVAÇÃO VISUAL (Antes e Depois)

# Ajustando a tela para mostrar 2 gráficos lado a lado
par(mfrow = c(1, 2), mar = c(5, 4, 4, 2) + 0.1)

# --- GRÁFICO 1: O Boxplot ---
boxplot(tabela_comparativa$Real, 
        tabela_comparativa$Simulado_Bruto, 
        tabela_comparativa$Simulado_Corrigido,
        names = c("Real", "Simulado\n(Bruto)", "Simulado\n(Corrigido)"),
        col = c("lightblue", "lightgreen", "royalblue"),
        ylab = "Precipitação Extrema (mm/dia)",
        main = "Efeito do Quantile Mapping")

# --- GRÁFICO 2: A Série Temporal ---
plot(tabela_comparativa$Ano, tabela_comparativa$Real, type = "l", col = "black", lwd = 2,
     ylab = "Chuva Extrema (mm/dia)", xlab = "Ano",
     main = "Série Histórica: Evolução",
     ylim = c(0, max(tabela_comparativa$Real) * 1.1)) 

# Adicionando a linha do modelo bruto (achatada)
lines(tabela_comparativa$Ano, tabela_comparativa$Simulado_Bruto, type = "l", col = "red", lty = 2, lwd = 2)

# Adicionando a linha do modelo CORRIGIDO (A nova realidade)
lines(tabela_comparativa$Ano, tabela_comparativa$Simulado_Corrigido, type = "l", col = "blue", lty = 1, lwd = 2)

# Legenda atualizada
legend("topleft", legend = c("Real (Ponto)", "Modelo Bruto", "Modelo Corrigido"),
       col = c("black", "red", "blue"), lty = c(1, 2, 1), lwd = 2, cex = 0.8)

# Volta a tela ao normal
par(mfrow = c(1, 1))

# PARTE 3: CORREÇÃO DE VIÉS (BIAS CORRECTION - LINEAR SCALING)

print("Calculando o Fator de Correção de Viés (Linear Scaling)...")

# 1. Calculando a média climática histórica de cada série
media_real <- mean(tabela_comparativa$Real, na.rm = TRUE)
media_simulada <- mean(tabela_comparativa$Simulado_Bruto, na.rm = TRUE)

# 2. Descobrindo o Fator Multiplicativo (O "peso" da correção)
fator_correcao <- media_real / media_simulada
print(paste("O modelo subestimou a chuva. Fator de Correção:", round(fator_correcao, 3)))

# 3. Aplicando a correção matemática na nossa série do modelo
# Criamos uma nova coluna apenas com os dados ajustados à realidade
tabela_comparativa$Simulado_Linear <- tabela_comparativa$Simulado_Bruto * fator_correcao

# ====================================================================
# 4. PLOTANDO O RESULTADO FINAL CORRIGIDO
# ====================================================================
par(mfrow=c(1,2)) 

# Novo Boxplot (As caixas devem se alinhar agora!)
boxplot(tabela_comparativa$Real, tabela_comparativa$Simulado_Linear,
        names = c("Observado (Real)", "Modelo (Corrigido)"),
        col = c("lightblue", "lightgreen"),
        main = "Chuva Extrema: Viés Corrigido",
        ylab = "Precipitação Máxima Anual (mm/dia)")

# Novo Gráfico de Linhas
plot(tabela_comparativa$Ano, tabela_comparativa$Real, type = "l", lwd = 2,
     ylim = c(30, 200), xlab = "Ano", ylab = "Chuva Extrema (mm/dia)",
     main = "Histórico: Real x Simulado (Corrigido)")

# Desenhando a nova linha vermelha tracejada por cima (agora mais alta)
lines(tabela_comparativa$Ano, tabela_comparativa$Simulado_Linear, 
      col = "red", lwd = 2, lty = 2)

legend("topright", legend = c("Real", "Simulado Corrigido"), 
       col = c("black", "red"), lty = c(1, 2), lwd = 2, cex = 0.8)

par(mfrow=c(1,1))

print("--- AVALIAÇÃO NUMÉRICA DE ERRO DO MODELO ---")

# 1. Calculando as médias com os nomes exatos das colunas
media_real <- mean(tabela_comparativa$Real, na.rm = TRUE)
media_simulada_bruta <- mean(tabela_comparativa$Simulado_Bruto, na.rm = TRUE)

# 2. Descobrindo o Fator Multiplicativo
fator_correcao <- media_real / media_simulada_bruta

# 3. Aplicando a correção linear
tabela_comparativa$Simulado_Linear <- tabela_comparativa$Simulado_Bruto * fator_correcao

# 4. Erro Quadrático Médio (EQM)
eqm_antes  <- mean((tabela_comparativa$Real - tabela_comparativa$Simulado_Bruto)^2, na.rm = TRUE)
eqm_depois <- mean((tabela_comparativa$Real - tabela_comparativa$Simulado_Linear)^2, na.rm = TRUE)

# 5. Raiz do Erro Quadrático Médio (RMSE)
rmse_antes  <- sqrt(eqm_antes)
rmse_depois <- sqrt(eqm_depois)

# Exibindo os resultados
print(paste("Erro Médio (RMSE) Antes   :", round(rmse_antes, 2), "mm/dia"))
print(paste("Erro Médio (RMSE) Depois  :", round(rmse_depois, 2), "mm/dia"))

# ====================================================================
# PARTE 6: O SHOWDOWN DOS MÉTODOS (COMPARAÇÃO DE RMSE)
# ====================================================================

# 1. Calculando o erro do Quantile Mapping (Simulado_Corrigido)
eqm_qmap <- mean((tabela_comparativa$Real - tabela_comparativa$Simulado_Corrigido)^2, na.rm = TRUE)
rmse_qmap <- sqrt(eqm_qmap)

# 2. Montando a Tabela Final de Resultados
tabela_resultados_erro <- data.frame(
  Metodo_de_Correcao = c("1. Modelo Bruto (Eta 20x20km)", 
                         "2. Linear Scaling (Fator Médio)", 
                         "3. Quantile Mapping (Ajuste de Cauda)"),
  RMSE_mm_por_dia = c(round(rmse_antes, 2), 
                      round(rmse_depois, 2), 
                      round(rmse_qmap, 2))
)

# 3. Exibindo o veredito
print("=== TABELA FINAL DE AVALIAÇÃO DE DESEMPENHO ===")
print(tabela_resultados_erro)

# ====================================================================
# PREPARAÇÃO DA BASE REAL (CRIANDO ANOS E EXTRAINDO EXTREMOS)
# ====================================================================
library(readxl)
library(dplyr)
library(extRemes)
cat("Lendo os dados diários e criando a cronologia...\n")

# 1. Carregar a planilha EXTREMOS2
# Dica: Se o seu Excel estiver em português, os decimais podem estar com vírgula. 
# Se for o caso, use dec = "," dentro do read.csv.
dados_diarios <- read_excel("EXTREMOS2.xlsx")

# 2. O Truque da Virada de Ano
# ---> COLOQUE AQUI O ANO DA PRIMEIRA LINHA DA SUA PLANILHA <---
ano_inicial <- 1968 

# Identifica matematicamente onde ocorre a virada do ano (quando o mês diminui)
virada_de_ano <- c(FALSE, diff(dados_diarios$mes) < 0)

# Cria a coluna 'ano' somando 1 a cada virada detectada
dados_diarios$ano <- ano_inicial + cumsum(virada_de_ano)

# 3. Extrair a Precipitação Máxima Anual (Rx1day) até 2018
extremos_reais_anuais <- dados_diarios %>%
  filter(ano <= 2018) %>%
  group_by(ano) %>%
  summarise(Precip_Max = max(Precip, na.rm = TRUE))

# ====================================================================
# VALIDAÇÃO RÁPIDA
# ====================================================================
print("Resumo da extração dos extremos anuais reais:")
head(extremos_reais_anuais) # Confere o começo (ex: 1968, 1969...)
tail(extremos_reais_anuais) # Confere o final (ex: ..., 2017, 2018)


# ====================================================================
# A PONTE: TREINANDO O QUANTILE MAPPING COM A BASE NOVA
# ====================================================================
library(qmap)
library(extRemes)

cat("Sincronizando as séries para treinar a correção de viés...\n")

# 1. Definir a janela de anos em que o Pluviômetro e o Modelo coexistem
anos_sobreposicao <- 1968:2005

# 2. Separar apenas os dados reais de 1968 a 2005 para o treino
vetor_real_treino <- extremos_reais_anuais %>% 
  filter(ano %in% anos_sobreposicao) %>% 
  pull(Precip_Max)

# 3. Separar os dados do modelo Eta (assumindo que 'dados_finais' ainda está no R)
vetor_simulado_treino <- dados_finais[as.character(anos_sobreposicao), "Viçosa"]

# 4. Treinar o algoritmo e gerar o objeto que faltava!
funcao_correcao_nova <- fitQmap(obs = vetor_real_treino, 
                                mod = vetor_simulado_treino, 
                                method = "QUANT", wet.day = 0.1)

simulado_historico_corrigido <- doQmap(x = vetor_simulado_treino, 
                                       fobj = funcao_correcao_nova)

cat("Correção aplicada com sucesso! Variável 'simulado_historico_corrigido' criada.\n")

# ====================================================================
# O DUELO DAS GEVs (Real até 2018 vs. Simulado Corrigido 1968-2005)
# ====================================================================
cat("Ajustando as distribuições GEV...\n")

# GEV 1: O Mundo Real (com toda a força estatística até 2018)
gev_real_2018 <- fevd(x = extremos_reais_anuais$Precip_Max, 
                      type = "GEV", 
                      units = "mm/dia")

# GEV 2: O Modelo Simulado e Corrigido (O seu laboratório validado)
gev_simulado_corrigido <- fevd(x = simulado_historico_corrigido, 
                               type = "GEV", 
                               units = "mm/dia")

# --------------------------------------------------------------------
# Extraindo as Chuvas de Projeto
# --------------------------------------------------------------------
periodos_retorno <- c(2, 5, 10, 25, 50, 100)

niveis_real <- return.level(gev_real_2018, return.period = periodos_retorno)
niveis_sim <- return.level(gev_simulado_corrigido, return.period = periodos_retorno)

tabela_retornos_final <- data.frame(
  Tempo_de_Retorno_Anos = periodos_retorno,
  Real_Ate_2018_mm = round(as.numeric(niveis_real), 1),
  Simulado_Corrigido_mm = round(as.numeric(niveis_sim), 1)
)

print("=== CHUVAS DE PROJETO: REAL VS SIMULADO CORRIGIDO ===")
print(tabela_retornos_final)

# --------------------------------------------------------------------
# Plotando os Níveis de Retorno Lado a Lado
# --------------------------------------------------------------------
# --------------------------------------------------------------------
# Plotando os Níveis de Retorno Lado a Lado (CORRIGIDO)
# --------------------------------------------------------------------
par(mfrow = c(1, 2))

# Removidos xlab e ylab para evitar o conflito com o extRemes
plot(gev_real_2018, type = "rl", 
     main = "Curva de Retorno: Real (Até 2018)")

plot(gev_simulado_corrigido, type = "rl", 
     main = "Curva de Retorno: Simulado Corrigido")

par(mfrow = c(1, 1))

# ====================================================================
# PARTE 8: EXTRAÇÃO E CORREÇÃO DO CENÁRIO FUTURO (RCP 4.5: 2006 - 2041)
# ====================================================================

library(terra)
library(qmap)
library(extRemes)

# 1. Carregar o novo NetCDF do Futuro (Cenário Intermediário)
arquivo_nc_futuro <- "pr_rcp4.5_2006_2041.nc"
dados_chuva_futuro <- rast(arquivo_nc_futuro)

cat("Recortando o mapa futuro para a Zona da Mata...\n")
chuva_zm_futuro <- crop(dados_chuva_futuro, limites_ajustados)

# 2. O LAÇO PARA A CHUVA MÁXIMA ANUAL DO FUTURO (Rx1day)
anos_futuros <- 2006:2041
num_anos_fut <- length(anos_futuros)

matriz_futuro <- matrix(NA, nrow = num_anos_fut, ncol = 2)
colnames(matriz_futuro) <- cidades_estudo$cidade
rownames(matriz_futuro) <- anos_futuros

cat("Extraindo os extremos simulados do Futuro (2006-2041)...\n")
for (i in 1:num_anos_fut) {
  # Assumindo o calendário de 360 dias do modelo Eta
  dia_inicial <- ((i - 1) * 360) + 1
  dia_final <- i * 360
  
  chuva_do_ano_fut <- chuva_zm_futuro[[dia_inicial:dia_final]]
  chuva_extraida_fut <- extract(chuva_do_ano_fut, pontos_extracao)
  
  # Pegando o máximo anual
  maximo_anual_fut <- apply(chuva_extraida_fut[, -1], 1, max, na.rm = TRUE)
  matriz_futuro[i, ] <- maximo_anual_fut
}

tabela_futuro <- as.data.frame(matriz_futuro)
vetor_futuro_bruto <- tabela_futuro$Viçosa

# ====================================================================
# A MAGIA ACONTECE: CORREÇÃO DO FUTURO COM A CHAVE DO PASSADO
# ====================================================================
cat("Aplicando o Quantile Mapping no cenário futuro RCP 4.5...\n")

# Usamos a 'funcao_correcao_nova' que treinamos no passado!
vetor_futuro_corrigido <- doQmap(x = vetor_futuro_bruto, 
                                 fobj = funcao_correcao_nova)

tabela_futuro$Viçosa_Corrigida <- vetor_futuro_corrigido

# ====================================================================
# O CLÍMAX: AJUSTE DA GEV FUTURA E COMPARAÇÃO DE IMPACTO
# ====================================================================
cat("Ajustando a GEV para o Cenário Futuro...\n")

gev_futuro <- fevd(x = tabela_futuro$Viçosa_Corrigida, 
                   type = "GEV", 
                   units = "mm/dia")

# Extraindo os Níveis de Retorno para o Futuro
niveis_futuro <- return.level(gev_futuro, return.period = periodos_retorno)

# Montando a Super Tabela de Comparação Final
tabela_mudanca_climatica <- data.frame(
  Tempo_Retorno = periodos_retorno,
  Real_Passado_mm = round(as.numeric(niveis_real), 1),
  Futuro_RCP45_mm = round(as.numeric(niveis_futuro), 1)
)

# Calculando a variação percentual (O Impacto das Mudanças Climáticas)
tabela_mudanca_climatica$Aumento_Percentual <- round(
  ((tabela_mudanca_climatica$Futuro_RCP45_mm - tabela_mudanca_climatica$Real_Passado_mm) / 
     tabela_mudanca_climatica$Real_Passado_mm) * 100, 1
)

print("=== IMPACTO DAS MUDANÇAS CLIMÁTICAS: CHUVAS DE PROJETO EM VIÇOSA ===")
print(tabela_mudanca_climatica)

# ====================================================================
# PLOTAGEM DO DUELO FINAL
# ====================================================================
par(mfrow = c(1, 2))

plot(gev_real_2018, type = "rl", 
     main = "Passado (Observado até 2018)")

plot(gev_futuro, type = "rl", 
     main = "Futuro (RCP 4.5: 2006-2041)")

par(mfrow = c(1, 1))

# ====================================================================
# PARTE 10: CENÁRIO FUTURO DE MÉDIO PRAZO (RCP 4.5: 2041 - 2070)
# ====================================================================
cat("\nProcessando a fatia de Médio Prazo (2041 - 2070)...\n")

# 1. Carregar o NetCDF do Médio Prazo
# ---> ATENÇÃO: Confirme o nome do arquivo baixado <---
arquivo_nc_mid <- "pr_rcp4.5_2041_2070.nc" 
dados_chuva_mid <- rast(arquivo_nc_mid)

cat("Recortando o mapa para a Zona da Mata...\n")
chuva_zm_mid <- crop(dados_chuva_mid, limites_ajustados)

# 2. Extração da Chuva Máxima Anual
anos_mid <- 2041:2070
num_anos_mid <- length(anos_mid)

matriz_mid <- matrix(NA, nrow = num_anos_mid, ncol = 2)
colnames(matriz_mid) <- cidades_estudo$cidade
rownames(matriz_mid) <- anos_mid

for (i in 1:num_anos_mid) {
  # Assumindo o calendário de 360 dias do modelo Eta
  dia_inicial <- ((i - 1) * 360) + 1
  dia_final <- i * 360
  
  chuva_do_ano <- chuva_zm_mid[[dia_inicial:dia_final]]
  chuva_extraida <- extract(chuva_do_ano, pontos_extracao)
  matriz_mid[i, ] <- apply(chuva_extraida[, -1], 1, max, na.rm = TRUE)
}

tabela_mid <- as.data.frame(matriz_mid)
vetor_mid_bruto <- tabela_mid$Viçosa

# 3. Correção de Viés (Usando a mesma Chave Mestra!)
cat("Aplicando o Quantile Mapping para 2041-2070...\n")
tabela_mid$Viçosa_Corrigida <- doQmap(x = vetor_mid_bruto, 
                                      fobj = funcao_correcao_nova)

# 4. Ajuste da GEV para o Médio Prazo
cat("Ajustando a GEV para o Cenário 2041-2070...\n")
gev_mid <- fevd(x = tabela_mid$Viçosa_Corrigida, 
                type = "GEV", 
                units = "mm/dia")

niveis_mid <- return.level(gev_mid, return.period = periodos_retorno)

# 5. Atualizando a Super Tabela de Comparação
tabela_mudanca_climatica$Futuro_2041_2070_mm <- round(as.numeric(niveis_mid), 1)

# Calculando o Aumento Percentual em relação ao PASSADO REAL
tabela_mudanca_climatica$Aumento_2041_2070_Perc <- round(
  ((tabela_mudanca_climatica$Futuro_2041_2070_mm - tabela_mudanca_climatica$Real_Passado_mm) / 
     tabela_mudanca_climatica$Real_Passado_mm) * 100, 1
)

# Reorganizando a ordem das colunas para ficar elegante na leitura
tabela_mudanca_climatica <- tabela_mudanca_climatica[, c(
  "Tempo_Retorno", 
  "Real_Passado_mm", 
  "Futuro_RCP45_mm", "Aumento_Percentual", 
  "Futuro_2041_2070_mm", "Aumento_2041_2070_Perc"
)]

print("=== EVOLUÇÃO TEMPORAL DOS EXTREMOS (VIÇOSA) ===")
print(tabela_mudanca_climatica)

# 6. Plotagem Comparativa das 3 Fatias de Tempo
par(mfrow = c(1, 3)) # Agora dividimos a tela em 3!

plot(gev_real_2018, type = "rl", main = "Passado (Até 2018)", ylim = c(50, 450))
plot(gev_futuro, type = "rl", main = "Futuro Próximo (2006-2041)", ylim = c(50, 450))
plot(gev_mid, type = "rl", main = "Médio Prazo (2041-2070)", ylim = c(50, 450))

par(mfrow = c(1, 1))
