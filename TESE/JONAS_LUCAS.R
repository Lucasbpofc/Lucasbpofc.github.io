
# ====================================================================
# SCRIPT FINAL: O SÉCULO XXI COMPLETO (RCP 4.5: 2006 a 2099)
# ====================================================================

library(terra)
library(qmap)
library(extRemes)
library(dplyr)

cat("Carregando o motor de extração em lote...\n")

# 1. Função inteligente para extrair qualquer arquivo NetCDF do modelo Eta
extrair_extremos_eta <- function(nome_arquivo, ano_inicio, ano_fim) {
  cat(paste("Lendo e fatiando:", nome_arquivo, "...\n"))
  mapa <- rast(nome_arquivo)
  mapa_recortado <- crop(mapa, limites_ajustados)
  
  anos <- ano_inicio:ano_fim
  extremos_vicosa <- numeric(length(anos))
  
  for (i in 1:length(anos)) {
    dia_inicial <- ((i - 1) * 360) + 1
    dia_final <- i * 360
    chuva_ano <- mapa_recortado[[dia_inicial:dia_final]]
    
    # Extrai apenas para o ponto 1 (Viçosa) da nossa tabela de cidades
    chuva_pontos <- extract(chuva_ano, pontos_extracao[1, ])
    extremos_vicosa[i] <- max(chuva_pontos[, -1], na.rm = TRUE)
  }
  
  return(data.frame(Ano = anos, Simulado_Bruto = extremos_vicosa))
}

# 2. Extraindo as três fatias do futuro
fatia1 <- extrair_extremos_eta("pr_rcp4.5_2006_2041.nc", 2006, 2041)
fatia2 <- extrair_extremos_eta("pr_rcp4.5_2041_2070.nc", 2041, 2070)
fatia3 <- extrair_extremos_eta("pr_rcp4.5_2071_2099.nc", 2071, 2099)

# 3. Costurando a Linha do Tempo Contínua (2006 - 2099)
cat("Costurando os 94 anos de dados contínuos do Século XXI...\n")
futuro_completo_bruto <- bind_rows(fatia1, fatia2, fatia3) %>%
  distinct(Ano, .keep_all = TRUE) %>% # Garante que não teremos sobreposição de anos
  arrange(Ano)

# 4. A Grande Correção de Viés (Mapeamento de Quantis)
cat("Limpando o viés do Século XXI usando a Chave Mestra...\n")
futuro_completo_bruto$Simulado_Corrigido <- doQmap(x = futuro_completo_bruto$Simulado_Bruto, 
                                                   fobj = funcao_correcao_nova)

# 5. Ajuste da GEV Robusta e Estável
cat("Ajustando a GEV definitiva do Futuro...\n")
gev_futuro_seculo21 <- fevd(x = futuro_completo_bruto$Simulado_Corrigido, 
                            type = "GEV", 
                            units = "mm/dia")

# 6. Extraindo a Chuva de Projeto Definitiva
niveis_futuro_sec21 <- return.level(gev_futuro_seculo21, return.period = periodos_retorno)

# 7. Montando a Tabela Final para o seu capítulo de Resultados
tabela_tese_final <- data.frame(
  Tempo_Retorno_Anos = periodos_retorno,
  Passado_Real_1968_2018 = round(as.numeric(niveis_real), 1),
  Futuro_RCP45_2006_2099 = round(as.numeric(niveis_futuro_sec21), 1)
)

tabela_tese_final$Aumento_Percentual <- round(
  ((tabela_tese_final$Futuro_RCP45_2006_2099 - tabela_tese_final$Passado_Real_1968_2018) / 
     tabela_tese_final$Passado_Real_1968_2018) * 100, 1
)

print("=== RESULTADO DEFINITIVO DA TESE: IMPACTO NO SÉCULO XXI ===")
print(tabela_tese_final)

# 8. Plotagem do Duelo Definitivo (Passado vs Futuro)
par(mfrow = c(1, 2))

# Fixamos o ylim para a escala ficar idêntica nos dois e a visualização ser justa
plot(gev_real_2018, type = "rl", 
     main = "Passado Real (1968-2018)", ylim = c(50, 450))

plot(gev_futuro_seculo21, type = "rl", 
     main = "Futuro RCP 4.5 (2006-2099)", ylim = c(50, 450))

par(mfrow = c(1, 1))
