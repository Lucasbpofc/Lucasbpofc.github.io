library(terra)
library(ggplot2)
library(dplyr)
library(tidyr)
library(geobr)

# 1. Carregar e Concatenar os Rasters
# Supondo que os novos arquivos se chamem "pr_rcp4.5_2041_2099.nc" etc.
r45_p1 <- rast("pr_rcp4.5_2006_2041.nc")
r45_p2 <- rast("pr_rcp4.5_2041_2070.nc")
r45_p3 <- rast("pr_rcp4.5_2071_2099.nc")

dados_45_full <- c(r45_p1, r45_p2, r45_p3) # Concatena as camadas temporais

r85_p1 <- rast("pr_rcp8.5_2006_2041.nc")
r85_p2 <- rast("pr_rcp8.5_2041_2070.nc")
r85_p3 <- rast("pr_rcp8.5_2071_2099.nc")
dados_85_full <- c(r85_p1, r85_p2, r85_p3)

codigo_cidade <- 4314902 
nome_cidade <- "Porto Alegre - RS"
# 2. Definir o limite de Viçosa e Recortar
vicosa_vect <- vect(read_municipality(code_muni = 2611606, showProgress = FALSE))
d45_crop <- terra::crop(dados_45_full, vicosa_vect)
d85_crop <- terra::crop(dados_85_full, vicosa_vect)

# 3. Extração Otimizada das Máximas Anuais
extrair_vicosa_99 <- function(rast_obj, nome_cenario) {
  # Extrair todos os valores espaciais máximos de uma vez
  max_espacial <- as.numeric(terra::extract(rast_obj, vicosa_vect, fun = max, na.rm = TRUE)[, -1])
  anos <- as.numeric(format(time(rast_obj), "%Y"))
  
  data.frame(Ano = anos, Prec = max_espacial) %>%
    group_by(Ano) %>%
    summarise(!!nome_cenario := max(Prec, na.rm = TRUE))
}

sim_45_99 <- extrair_vicosa_99(d45_crop, "RCP_4.5")
sim_85_99 <- extrair_vicosa_99(d85_crop, "RCP_8.5")

# 4. Unificar e Plotar
df_99 <- left_join(sim_45_99, sim_85_99, by = "Ano") %>%
  pivot_longer(cols = -Ano, names_to = "Cenario", values_to = "Prec_Max")

ggplot(df_99, aes(x = Ano, y = Prec_Max, color = Cenario)) +
  geom_line(alpha = 0.4) +
  geom_smooth(method = "loess", span = 0.2, se = FALSE, size = 1.2) +
  scale_color_manual(values = c("RCP_4.5" = "#2c7bb6", "RCP_8.5" = "#d7191c")) +
  labs(title = "Projeção de Extremos em Viçosa (2006 - 2099)",
       subtitle = "Comparação RCP 4.5 vs RCP 8.5",
       x = "Ano", y = "Precipitação Máxima Anual (mm)") +
  theme_minimal()


# ==============================================================================
# ANÁLISE DE VARIÂNCIA E EXTREMOS (Focando no período 2050 - 2099)
# ==============================================================================

# 1. Filtrar os dados para a janela de maior divergência climática
df_futuro <- df_99 %>% 
  filter(Ano >= 2050)

# 2. Calcular as Estatísticas de Dispersão
estatisticas_dispersao <- df_futuro %>%
  group_by(Cenario) %>%
  summarise(
    Media = mean(Prec_Max, na.rm = TRUE),
    Variancia = var(Prec_Max, na.rm = TRUE),
    Desvio_Padrao = sd(Prec_Max, na.rm = TRUE),
    Maximo_Absoluto = max(Prec_Max, na.rm = TRUE),
    Amplitude = max(Prec_Max, na.rm = TRUE) - min(Prec_Max, na.rm = TRUE)
  )

# Exibir os resultados no console
print("Estatísticas de Dispersão (2050 - 2099):")
print(estatisticas_dispersao)

# 3. Visualizar a Dispersão através de um Boxplot
ggplot(df_futuro, aes(x = Cenario, y = Prec_Max, fill = Cenario)) +
  geom_boxplot(alpha = 0.7, outlier.size = 3, outlier.colour = "black") +
  scale_fill_manual(values = c("RCP_4.5" = "#2c7bb6", "RCP_8.5" = "#d7191c")) +
  labs(
    title = "Dispersão da Precipitação Máxima Anual",
    subtitle = "Município de Viçosa (Janela 2050 - 2099)",
    x = "Cenário de Emissão",
    y = "Precipitação Máxima (mm)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "none", # Esconde a legenda pois o eixo X já diz o cenário
    axis.text.x = element_text(size = 12, face = "bold")
  )
