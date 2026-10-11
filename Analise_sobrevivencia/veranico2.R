################################################################################
#############               Lucas Pereira Belo                 #################
################################################################################

library(survival)
library(stats)

# 1. Carregar a base de dados atualizada
df_veranicos <- read.csv("base_veranicos_mg_gap_times2.csv")
head(df_veranicos)

# 2. Tratamento das Variáveis Categóricas
df_veranicos$cidade <- as.factor(df_veranicos$cidade)
df_veranicos$zcas_categoria <- as.factor(df_veranicos$zcas_categoria)
df_veranicos$enso_categoria <- as.factor(df_veranicos$enso_categoria)

# Define "Inativa" como categoria de referência.
# Assim, o coeficiente vai mostrar o efeito de ter a ZCAS "Ativa"
df_veranicos$zcas_categoria <- relevel(df_veranicos$zcas_categoria, ref = "Inativa")

# Define "La Niña" como categoria de referência.
df_veranicos$enso_categoria <- relevel(df_veranicos$enso_categoria, ref = "La Niña")

# 3. Ajuste de alguns modelos de Cox com Fragilidade Gama

# Modelo 1: Efeito Isolado do El Niño
mod_cox_1 <- coxph(
  Surv(gap_time, status) ~ enso_categoria + frailty(cidade, distribution = "gamma"), 
  data = df_veranicos
)

# Modelo 2: Efeito Isolado da ZCAS (Nova covariável)
mod_cox_2 <- coxph(
  Surv(gap_time, status) ~ zcas_categoria + frailty(cidade, distribution = "gamma"), 
  data = df_veranicos
)

# Modelo 3: Efeito Aditivo (ZCAS + El Niño)
mod_cox_3 <- coxph(
  Surv(gap_time, status) ~ enso_categoria + zcas_categoria + frailty(cidade, distribution = "gamma"), 
  data = df_veranicos
)

# Modelo 4: Efeito de Interação (O efeito da ZCAS muda dependendo do El Niño?)
mod_cox_4 <- coxph(
  Surv(gap_time, status) ~ enso_categoria * zcas_categoria + frailty(cidade, distribution = "gamma"), 
  data = df_veranicos
)

# 4. Extração e Comparação dos Critérios de Informação

resultados_ic <- data.frame(
  Modelo = c(
    "1. ONI_Verao", 
    "2. ZCAS_Categoria", 
    "3. ONI + ZCAS", 
    "4. ONI * ZCAS (Interação)"
  ),
  AIC = c(AIC(mod_cox_1), AIC(mod_cox_2), AIC(mod_cox_3), AIC(mod_cox_4)),
  BIC = c(BIC(mod_cox_1), BIC(mod_cox_2), BIC(mod_cox_3), BIC(mod_cox_4))
)

# Ordena a tabela do melhor modelo (menor AIC) para o pior
resultados_ic <- resultados_ic[order(resultados_ic$AIC), ]

print("Seleção de Modelos Semiparamétricos (Cox + Fragilidade Gama):")
print(resultados_ic, row.names = FALSE)

# 1. Ajuste do modelo espelho marginal (apenas para o diagnóstico)
mod_cox_espelho <- coxph(
  Surv(gap_time, status) ~   enso_categoria+zcas_categoria + cluster(cidade), 
  data = df_veranicos
)

# 2. Teste de Schoenfeld
teste_ph <- cox.zph(mod_cox_espelho)
print("Teste de Schoenfeld (Riscos Proporcionais):")
print(teste_ph)

# 3. Plotagem dos resíduos
par(mfrow = c(2, 2))
plot(teste_ph, lwd = 2, col = "blue")
par(mfrow = c(1, 1))


# 1. Extração dos Resíduos Martingale do modelo com fragilidade
res_martingale <- residuals(mod_cox_4, type = "martingale")

# 2. Cálculo dos Resíduos de Cox-Snell
res_cox_snell <- df_veranicos$status - res_martingale

# 3. Ajuste do modelo nulo (Kaplan-Meier / Nelson-Aalen) sobre os resíduos
# Para avaliar a aderência, estimamos a sobrevivência dos próprios resíduos
ajuste_cs <- survfit(Surv(res_cox_snell, df_veranicos$status) ~ 1)

# 4. Cálculo da Função de Risco Acumulado Empírico H(t) = -log(S(t))
risco_acumulado_cs <- -log(ajuste_cs$surv)

# 5. Gráfico de Diagnóstico
plot(ajuste_cs$time, risco_acumulado_cs, type = "s", col = "blue", lwd = 2,
     xlab = "Resíduos de Cox-Snell (r)", ylab = "Risco Acumulado Estimado H(r)",
     main = "Ajuste Global: Modelo de Cox com Fragilidade Gama")

# Linha de referência de 45 graus (ajuste perfeito)
abline(0, 1, col = "red", lty = 2, lwd = 2)

legend("topleft", legend = c("Curva Empírica do Modelo", "Ajuste Perfeito (45°)"), 
       col = c("blue", "red"), lty = c(1, 2), lwd = 2, bty = "n")

# Um desvio a partir do valor x = 5 no eixo x pode ser observado, para isso, posteriormente
# investigamos a dependencia temporal das variáveis

#Fragilidade observada em cada cidade

library(ggplot2)

# 1. Extrair os efeitos aleatórios (log-frailty) do modelo
log_fragilidade <- mod_cox_3$frail

# 2. Calcular o multiplicador de risco (exp(log-frailty))
mult_risco <- exp(log_fragilidade)

# 3. Nomear corretamente as cidades usando os níveis do fator
# O coxph sempre ordena as fragilidades pelos níveis do fator (ordem alfabética)
nomes_cidades <- levels(as.factor(df_veranicos$cidade))

# 4. Criar o data.frame corrigido
df_fragilidade <- data.frame(
  Cidade = nomes_cidades,
  Log_Fragilidade = as.numeric(log_fragilidade),
  Multiplicador = as.numeric(mult_risco)
)

# 5. Ordenar do maior para o menor multiplicador de risco
df_fragilidade <- df_fragilidade[order(df_fragilidade$Multiplicador, decreasing = TRUE), ]

# Imprimir a tabela no console
print("Tabela de Fragilidade por Cidade (w):")
print(df_fragilidade, row.names = FALSE)

# 6. Gráfico para visualização regional
ggplot(df_fragilidade, aes(x = reorder(Cidade, Multiplicador), y = Multiplicador)) +
  geom_bar(stat = "identity", fill = "#2c3e50", width = 0.7) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "#e74c3c", linewidth = 1) +
  coord_flip() +
  labs(
    title = "Efeito Microclimático por Cidade (Fragilidade Gama)",
    subtitle = "Linha vermelha (1.0) = Média global de MG\n> 1: Veranicos quebram mais rápido | < 1: Veranicos persistem mais",
    x = "Microrregião / Cidade",
    y = "Multiplicador de Risco de Quebra da Seca (w)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text = element_text(size = 11)
  )

# Testando o efeito do tempo na variavel ZCAS 

# 1. Fatiar o banco de dados manualmente
# Isso quebra cada veranico em intervalos de 1 dia, criando a variável 't_atual'
df_expandido <- survSplit(
  Surv(gap_time, status) ~ ., 
  data = df_veranicos, 
  cut = 1:max(df_veranicos$gap_time), 
  episode = "t_atual"
)

# 2. Criar a interação temporal manualmente no novo banco
# Exemplo: El Niño perdendo ou ganhando força com o logaritmo do tempo
df_expandido$el_nino_log_t <- ifelse(df_expandido$enso_categoria == "El Niño", 1, 0) * log(df_expandido$t_atual)

# 3. Ajustar o Modelo de Cox com Frailty em formato Counting Process (tstart, tstop)
mod_cox_frailty_time <- coxph(
  Surv(tstart, gap_time, status) ~ zcas_categoria + enso_categoria + el_nino_log_t + frailty(cidade, distribution = "gamma"),
  data = df_expandido
)

summary(mod_cox_frailty_time)

# Criar a interação temporal para a ZCAS (considerando "Ativa" como evento de interesse)
df_expandido$zcas_log_t <- ifelse(df_expandido$zcas_categoria == "Ativa", 1, 0) * log(df_expandido$t_atual)

# Ajustar o novo modelo contemplando a interação temporal dupla (ENSO e ZCAS)
mod_cox_frailty_time_full <- coxph(
  Surv(tstart, gap_time, status) ~ zcas_categoria + zcas_log_t + enso_categoria + el_nino_log_t + frailty(cidade, distribution = "gamma"),
  data = df_expandido
)

summary(mod_cox_frailty_time_full)

# 1. Cria um ID único para cada evento antes do fatiamento
df_veranicos$id_veranico <- 1:nrow(df_veranicos)

# 2. Fatiamento com Counting Process
df_expandido <- survSplit(
  Surv(gap_time, status) ~ ., 
  data = df_veranicos, 
  cut = 1:max(df_veranicos$gap_time), 
  episode = "t_atual"
)

# 3. Recria a interação temporal
df_expandido$el_nino_log_t <- ifelse(df_expandido$enso_categoria == "El Niño", 1, 0) * log(df_expandido$t_atual)

# 4. Reajusta o modelo
mod_cox_frailty_time <- coxph(
  Surv(tstart, gap_time, status) ~ zcas_categoria + enso_categoria + el_nino_log_t + frailty(cidade, distribution = "gamma"),
  data = df_expandido
)

# 5. Extração e agregação usando o novo 'id_veranico'
df_expandido$res_mart <- residuals(mod_cox_frailty_time, type = "martingale")

df_agregado <- aggregate(
  cbind(status, res_mart) ~ id_veranico, 
  data = df_expandido, 
  FUN = sum
)

# 6. Cálculo dos Resíduos de Cox-Snell e Risco Acumulado
res_cox_snell_time <- df_agregado$status - df_agregado$res_mart
status_final <- df_agregado$status

ajuste_cs_time <- survfit(Surv(res_cox_snell_time, status_final) ~ 1)
risco_acumulado_time <- -log(ajuste_cs_time$surv)

# 7. Plotagem Final
plot(ajuste_cs_time$time, risco_acumulado_time, type = "s", col = "#8e44ad", lwd = 2,
     xlab = "Resíduos de Cox-Snell (r)", ylab = "Risco Acumulado Estimado H(r)",
     main = "Ajuste Global: Efeito Temporal + Fragilidade")
abline(0, 1, col = "red", lty = 2, lwd = 2)
legend("topleft", legend = c("Modelo Tempo-Dependente", "Ajuste Perfeito (45°)"), 
       col = c("#8e44ad", "red"), lty = c(1, 2), lwd = 2, bty = "n")


# 1. Extrair os valores do log da fragilidade
log_fragilidades <- mod_cox_frailty_time$frail

# 2. Extrair os nomes das 12 cidades em ordem alfabética (padrão do coxph)
nomes_cidades <- levels(as.factor(df_expandido$cidade))

# 3. Criar o data.frame emparelhando os nomes e convertendo para Razão de Risco
df_fragilidade <- data.frame(
  cidade = nomes_cidades,
  fragilidade = exp(log_fragilidades)
)

# 4. Ordenar das cidades com maior propensão a veranicos para a menor
df_fragilidade <- df_fragilidade[order(-df_fragilidade$fragilidade), ]

# Visualizar o resultado correto
print(df_fragilidade)


library(ggplot2)

# 1. Definir o vetor de tempo (ex: 1 a 100 dias de gap time)
tempo_dias <- 1:100

# 2. Calcular o Hazard Ratio (HR) contínuo para o El Niño
hr_el_nino <- exp(0.3873 - 0.0734 * log(tempo_dias))

# 3. Criar o data.frame para plotagem
df_plot_dinamico <- data.frame(
  Dia = tempo_dias,
  HR = hr_el_nino
)

# 4. Construir o gráfico com ggplot2
ggplot(df_plot_dinamico, aes(x = Dia, y = HR)) +
  geom_line(color = "#d35400", linewidth = 1.2) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "darkgray", linewidth = 1) +
  labs(
    title = "Efeito Tempo-Dependente do El Niño sobre os Veranicos",
    subtitle = "Decaimento da Razão de Risco (HR) ao longo do período sem chuvas",
    x = "Dias desde o início do risco (Gap Time)",
    y = "Razão de Risco (Referência: La Niña)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.title = element_text(face = "bold")
  )

library(survival)
library(ggplot2)

# 1. Ajuste do modelo de Kaplan-Meier
ajuste_km_enso <- survfit(Surv(gap_time, status) ~ enso_categoria, data = df_veranicos)

# 2. Extração manual dos dados para um data.frame amigável ao ggplot2
df_km <- data.frame(
  tempo = ajuste_km_enso$time,
  sobrevivencia = ajuste_km_enso$surv,
  # O rep() duplica o nome do estrato de acordo com a quantidade de eventos nele
  fase = rep(names(ajuste_km_enso$strata), ajuste_km_enso$strata)
)

# Limpeza dos nomes dos estratos (remove o prefixo "enso_categoria=")
df_km$fase <- gsub("enso_categoria=", "", df_km$fase)

# Adição da origem (tempo 0, sobrevivência 1) para o gráfico partir do topo
df_inicio <- data.frame(
  tempo = rep(0, length(unique(df_km$fase))),
  sobrevivencia = rep(1, length(unique(df_km$fase))),
  fase = unique(df_km$fase)
)
df_km <- rbind(df_inicio, df_km)

# 3. Plotagem com ggplot2
ggplot(df_km, aes(x = tempo, y = sobrevivencia, color = fase)) +
  geom_step(linewidth = 1.2) + # geom_step é crucial para curvas de sobrevivência
  scale_color_manual(
    name = "Fase do ENSO",
    values = c("El Niño" = "#d35400", "La Niña" = "#7f8c8d", "Neutro" = "#2980b9")
  ) +
  labs(
    title = "Curvas de Sobrevivência por Fase do ENSO",
    x = "Dias sem chuva (Gap Time)",
    y = "Probabilidade de Sobrevivência H(t)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.title = element_text(face = "bold"),
    legend.position = "bottom"
  )
