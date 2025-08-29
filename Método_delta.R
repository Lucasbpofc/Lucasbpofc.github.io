# --- 1. Simular uma Amostra Aleatória ---
# Define uma semente para reprodutibilidade
set.seed(42)

# Parâmetros da nossa população Weibull
weibull_shape <- 2
weibull_scale <- 5
n <- 1000 # Tamanho da nossa amostra

# Gera uma amostra i.i.d. de uma distribuição Weibull
amostra_weibull <- rweibull(n, shape = weibull_shape, scale = weibull_scale)

# --- 2. Calcular as Estatísticas da Amostra ---
# Estas são as nossas estimativas para os parâmetros da população

# Média amostral (nossa estimativa de µ)
x_bar <- mean(amostra_weibull)

# Variância amostral (nossa estimativa de σ²)
s2 <- var(amostra_weibull)

# --- 3. Aplicar o Método Delta ---

# a) Calcule o estimador transformado: arctan(média amostral)
arctan_x_bar <- atan(x_bar)

# b) Calcule a derivada g'(µ) no ponto estimado x_bar
g_prime_hat <- 1 / (1 + x_bar^2)

# c) Estime a variância da média amostral, Var(X̄ₙ)
var_x_bar <- s2 / n

# d) Use a fórmula do Método Delta para estimar a variância do estimador transformado
var_arctan_x_bar <- (g_prime_hat^2) * var_x_bar

# e) Calcule o erro padrão (Standard Error)
se_arctan_x_bar <- sqrt(var_arctan_x_bar)

# --- 4. Construir o Intervalo de Confiança de 95% ---

# Valor crítico da Normal Padrão para 95% de confiança (Z ≈ 1.96)
z_critico <- qnorm(0.975)

# Limites do intervalo de confiança para arctan(µ)
limite_inferior <- arctan_x_bar - z_critico * se_arctan_x_bar
limite_superior <- arctan_x_bar + z_critico * se_arctan_x_bar

# --- 5. Apresentar os Resultados ---

cat("--- Resultados para uma Amostra (n=", n, ") ---\n")
cat("Média Amostral (x_bar):", round(x_bar, 4), "\n")
cat("Arco Tangente da Média Amostral (arctan(x_bar)):", round(arctan_x_bar, 4), "\n")
cat("Erro Padrão para arctan(x_bar) (via Método Delta):", round(se_arctan_x_bar, 6), "\n")
cat("Intervalo de Confiança de 95% para arctan(µ): [", round(limite_inferior, 4), ",", round(limite_superior, 4), "]\n")

# --- Verificação via Simulação ---
n_sims <- 10000 # Vamos gerar 10.000 amostras

# Vetor para guardar as médias de cada uma das 10.000 amostras
medias_amostrais_sim <- replicate(n_sims, mean(rweibull(n, shape = weibull_shape, scale = weibull_scale)))

# Aplica a transformação arctan a TODAS as médias amostrais
arctan_das_medias_sim <- atan(medias_amostrais_sim)

# A variância "real" (observada na simulação) do nosso estimador
var_simulada <- var(arctan_das_medias_sim)

# --- Comparação Final ---
cat("\n--- Verificação via Simulação ---\n")
cat("Variância estimada pelo Método Delta (em uma amostra):", format(var_arctan_x_bar, scientific = FALSE), "\n")
cat("Variância 'real' observada em", n_sims, "simulações:", format(var_simulada, scientific = FALSE), "\n")
