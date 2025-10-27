################################################################################
#############                 Lucas Pereira Belo - 94074           #############
#############                 Questão 11 e 7                       #############
################################################################################
install.packages("languageserver")
library(survival)

# Parâmetros para a simulação
true_shape <- 1.5
true_scale <- 90
n_plantas <- 400
limite_experimento <- 150 # Fim do estudo em 150 dias

set.seed(42)

# Gera os tempos de morte "verdadeiros" contínuos
tempo_contínuo <- rweibull(n = n_plantas, shape = true_shape, scale = true_scale)

# Converter para dias inteiros
tempo_diario <- ceiling(tempo_contínuo)

# Cria o dataframe
dados_soja <- data.frame(
  id_planta = 1:n_plantas,
  tempo = pmin(tempo_diario, limite_experimento),
  status = ifelse(tempo_diario < limite_experimento, 1, 0)
)
print(head(dados_soja))

# HISTOGRAMA DOS DADOS OBSERVADOS

hist(dados_soja$tempo,
     main = "Histograma dos Tempos Observados (Dias)",
     xlab = "Dias até a Infecção ou Censura",
     ylab = "Frequência",
     col = "gray", border = "black", breaks = 20)


# AJUSTE DO MODELO DE SOBREVIVÊNCIA WEIBULL

modelo_weibull <- survreg(
  Surv(tempo, status) ~ 1,
  data = dados_soja,
  dist = "weibull",
  scale = 0  
)

print(summary(modelo_weibull))

# Extração dos parâmetros estimados
est_shape <- 1 / modelo_weibull$scale
est_scale <- exp(coef(modelo_weibull)[1])


# item g: Gráfico da função de probabilidade ou densidade de probabilidade
hist(dados_soja$tempo[dados_soja$status==1],
     main = "Histograma com densidade Weibull ajustada",
     xlab = "Tempo (dias)",
     ylab = "Densidade",
     col = "lightblue", border = "black",
     prob = TRUE,
     breaks = 20)

curve(dweibull(x, shape = est_shape, scale = est_scale),
      col = "red",
      lwd = 2,
      add = TRUE)

legend("topright", legend = "FDP Weibull Ajustada", col = "red", lwd = 2)


# item h: GRÁFICO DA FUNÇÃO DISTRIBUIÇÃO ACUMULADA (FDA)
km_fit <- survfit(Surv(tempo, status) ~ 1, data = dados_soja)

plot(km_fit$time, 1 - km_fit$surv,
     type = 's',
     main = "Função Distribuição Acumulada (FDA)",
     xlab = "Tempo (dias)",
     ylab = "Probabilidade acumulada de morte P(T <= t)",
     col = "blue",
     lwd = 2,
     ylim = c(0, 1),
     xlim = c(0, limite_experimento))
grid()

curve(pweibull(x, shape = est_shape, scale = est_scale, lower.tail = TRUE),
      col = "red",
      lwd = 2,
      add = TRUE)

legend("topleft",
       legend = c("FDA Empírica (Observado)", "Modelo Weibull (Ajustado)"),
       col = c("blue", "red"),
       lwd = 2)


# i) Cálculo de probabilidade considerando a distribuição assumida
# Pergunta 1: Probabilidade de morte até os 60 dias.
P60_dias <- pweibull(60, shape = est_shape, scale = est_scale)
print(paste("Probabilidade de morte até 60 dias:", P60_dias))

# Pergunta 2: Probabilidade de sobreviver por mais de 120 dias.
prob_mais120_dias <- pweibull(120, shape = est_shape, scale = est_scale, lower.tail = FALSE)
print(paste("Probabilidade de sobreviver mais de 120 dias:", prob_mais120_dias))

# Pergunta 3: Probabilidade de infecção ENTRE os dias 80 e 100.
prob_ate_100 <- pweibull(100, shape = est_shape, scale = est_scale)
prob_ate_80 <- pweibull(80, shape = est_shape, scale = est_scale)
prob_entre_80_e_100 <- prob_ate_100 - prob_ate_80
print(paste("Probabilidade de morte entre 80 e 100 dias:", prob_entre_80_e_100))


# item j: Medidas de posição e de dispersão
#### MEDIANA ####
tempo_mediano <- qweibull(0.50, shape = est_shape, scale = est_scale)
print(paste("Tempo mediano até a morte (dias):", round(tempo_mediano)))

#### Cálculo da MÉDIA ####
tempo_medio <- est_scale * gamma(1 + 1/est_shape)
print(paste("Tempo médio até a morte (dias):", round(tempo_medio)))

#### Cálculo da MODA ####
if (est_shape > 1) {
  tempo_moda <- est_scale * ((est_shape - 1) / est_shape)^(1/est_shape)
  print(paste("Tempo modal até a morte (dias):", round(tempo_moda)))
} else {
  print("Moda é no tempo 0 (k <= 1)")
}

# item c) Questão 7. INTERVALOS DE CONFIANÇA (IC) DE 95%

# Primeiro, obtemos os ICs para os parâmetros internos do R.

ic_modelo_interno <- confint(modelo_weibull)

# Para Lambda (escala):
ic_lambda <- exp(ic_modelo_interno["(Intercept)", ])
cat(sprintf("--> IC de 95%% para o Parâmetro de Escala (n): [%.4f, %.4f]\n",
            ic_lambda[1], ic_lambda[2]))

# Item d) Questão 7
matriz_var_cov <- vcov(modelo_weibull)
print(matriz_var_cov)

info_fisher_observada <- solve(matriz_var_cov)
print(info_fisher_observada)

#-----------------------------------------------------------------------
# GRÁFICO COMPARATIVO: EFEITO DO PARÂMETRO DE ESCALA (LAMBDA)
#-----------------------------------------------------------------------
forma_k <- 1.5
lambdas_a_comparar <- c(1, 2, 4)
cores <- c("darkred", "darkblue", "darkgreen")
x_vals <- seq(0, 12, length.out = 500)

plot(x_vals, dweibull(x_vals, shape = forma_k, scale = lambdas_a_comparar[1]),
     type = "n",
     main = "Efeito do Parâmetro de Escala (λ) na FDP Weibull",
     sub = sprintf("Parâmetro de Forma (k) = %.1f mantido constante", forma_k),
     xlab = "Tempo (t)",
     ylab = "Densidade de Probabilidade f(t)",
     ylim = c(0, 0.8))
grid()

for (i in 1:length(lambdas_a_comparar)) {
  lambda_atual <- lambdas_a_comparar[i]
  densidade <- dweibull(x_vals, shape = forma_k, scale = lambda_atual)
  lines(x_vals, densidade, col = cores[i], lwd = 2)
}

legend("topright",
       title = "Escala (λ)",
       legend = lambdas_a_comparar,
       col = cores,
       lwd = 2,
       bty = "n")