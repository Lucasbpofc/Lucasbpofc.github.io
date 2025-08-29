#######################################################################
################          Exercício método Delta        ###############

# --- 1. Definir os Dados do Problema ---

# Grupo A (Tratamento)
sucessos_A <- 80
total_A <- 100

# Grupo B (Controle)
sucessos_B <- 60
total_B <- 100

# --- 2. Calcular as Estimativas Pontuais ---

# Proporções amostrais de sucesso
p_hat_A <- sucessos_A / total_A  # 0.8
p_hat_B <- sucessos_B / total_B  # 0.6

# Odds Ratio (OR) estimado
odds_A <- p_hat_A / (1 - p_hat_A)
odds_B <- p_hat_B / (1 - p_hat_B)
or_hat <- odds_A / odds_B

# Logaritmo do Odds Ratio (vamos construir o IC para este valor)
log_or_hat <- log(or_hat)

cat("Odds Ratio Estimado:", round(or_hat, 2), "\n")
cat("Log do Odds Ratio Estimado:", round(log_or_hat, 2), "\n\n")


# --- 3. Aplicar o Método Delta (Calcular Erro Padrão) ---

# Fórmula da variância do log(OR) derivada do Método Delta
var_log_or <- (1 / (total_A * p_hat_A * (1 - p_hat_A))) + (1 / (total_B * p_hat_B * (1 - p_hat_B)))

# Erro Padrão (Standard Error) é a raiz quadrada da variância
se_log_or <- sqrt(var_log_or)

cat("Erro Padrão do Log(OR) (via Método Delta):", round(se_log_or, 2), "\n\n")


# --- 4. Construir o Intervalo de Confiança de 95% ---

# Nível de significância
alpha <- 0.05
# Valor crítico da Normal Padrão para 95% de confiança (Z_{alpha/2})
z_critico <- qnorm(1 - alpha / 2) # Aprox. 1.96

# Calcular os limites do IC na escala do logaritmo
limite_inferior_log <- log_or_hat - z_critico * se_log_or
limite_superior_log <- log_or_hat + z_critico * se_log_or

# --- 5. Transformar o IC de volta para a Escala do Odds Ratio ---

# Para obter o IC do OR, aplicamos a exponencial nos limites do log(OR)
limite_inferior_or <- exp(limite_inferior_log)
limite_superior_or <- exp(limite_superior_log)

# --- 6. Apresentar e Interpretar o Resultado Final ---
cat("Intervalo de Confiança de 95% para o Odds Ratio:\n")
cat("[", round(limite_inferior_or, 2), ",", round(limite_superior_or, 2), "]\n\n")

# Interpretação
if (limite_inferior_or > 1) {
  cat("Interpretação: Como o intervalo de confiança está totalmente acima de 1, temos evidência estatística (com 95% de confiança) de que as chances de sucesso com o Tratamento A são maiores do que com o Controle B.\n")
} else {
  cat("Interpretação: Como o intervalo de confiança contém 1, não temos evidência estatística para afirmar que há uma diferença significativa nas chances de sucesso entre os grupos.\n")
}
