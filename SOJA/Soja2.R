# Carregamento de Pacotes
library(readxl)      # Importação
library(ggplot2)     # Gráficos
library(tseries)     # Testes de Estacionariedade
library(fracdiff)    # Memória Longa
library(forecast)    # Modelagem ARIMA
library(descomponer) # Periodograma
library(TSA)         # Periodograma
library(lmtest)      # Testes de Coeficientes
library(arfima)      # ARFIMA
library(moments)     # Estatísticas descritivas
library(randtests)   # Cox-Stuart
library(urca)        # Testes de Raiz Unitária Avançados
library(pracma)      # Hurst
library(uroot)       # HEGY e CH
library(fGarch)      # Modelagem GARCH (pacote 1)
library(rugarch)     # Modelagem GARCH (pacote 2 - mais robusto)

# Importação dos Dados
soja_mensal <- read_excel("soja_mensal.xlsx")
soja <- soja_mensal

y<-soja$Reais 
rm(y)

tail(soja); head(soja)

y = ts(soja$Reais, start = c(2012,3), end = c(2024,12), freq = 12)

plot(y, 
     xlab = "Tempo", 
     ylab = "Preço (R$)",
     main = "Evolução do Preço da Soja", # Adiciona um título ao gráfico
     col = "blue",  # Define a cor da linha (opcional)
     lwd = 2)       # Define a espessura da linha (opcional)

plot(y, xlab = "Tempo", ylab = "Preço (R$)", axes = TRUE)

y1 = ts(diff(log(y)), start = c(2012,3), end = c(2024,12), freq = 12) # r = ln[y_t/y_(t-1)]
plot(y1,
     xlab= "Tempo",
     ylab = "Preço (R$)",
     main = "Evolução da série de log retornos",
     col = "darkblue",
     lwd = 2)

# Gráficos de autocorrelação e autocorrelação parcial

n = length(y) #tamanho da amostra
par(mfrow = c(2,2))
acf(y, n)
acf(y1, n)
pacf(y, n)
pacf(y1, n)

cox.stuart.test(y1)

adf.test(y1)

pegram<-periodogram(y1)

freq_dominant <- pegram$freq[which.max(pegram$spec)]
period <- 1 / freq_dominant
cat("Frequência dominante:", freq_dominant, "\n")
cat("Período dominante:", period, "\n")

library(boot) #estimativa bootstrap
# Aplicando o bootstrap
boot_result <- boot(data = y1, statistic = stat_function, R = 1000)

# Calculando o intervalo de confiança
boot_ci <- boot.ci(boot_result, type = "perc")

# Visualizando os resultados
print(boot_ci)

# Ajuste automático 

# 1. Preparação dos Dados
y1_clean <- y1[!is.na(y1) & !is.infinite(y1)]
y1_final <- ts(y1_clean, frequency = 12, start = start(y1))

# 2. Grid de Parâmetros
# Mantemos o grid otimizado para evitar problemas de convergência
grid_params <- expand.grid(
  p = 0:3, q = 0:3, 
  P = 0:1, Q = 0:1, 
  D = 0:1
)

tabela_resultados <- data.frame()
total <- nrow(grid_params)

# 3. Loop de Estimação
for(i in 1:total){
  par <- grid_params[i, ]
  tryCatch({
    # Ajuste com CSS-ML para obter métricas válidas
    fit_temp <- Arima(y1_final, 
                      order = c(par$p, 0, par$q), 
                      seasonal = list(order = c(par$P, par$D, par$Q), period = 12), 
                      include.mean = FALSE,
                      method = "CSS-ML") 
    
    if(!is.na(fit_temp$bic)){
      tabela_resultados <- rbind(tabela_resultados, data.frame(
        Modelo = paste0("SARIMA(", par$p, ",0,", par$q, ")(", par$P, ",", par$D, ",", par$Q, ")[12]"),
        p = par$p, q = par$q, P = par$P, D = par$D, Q = par$Q,
        AIC = fit_temp$aic,
        BIC = fit_temp$bic,
        AICc = fit_temp$aicc,     # Incluindo AICc
        LogLik = fit_temp$loglik, # Apenas informativo
        stringsAsFactors = FALSE
      ))
    }
  }, error = function(e) { return(NULL) })
}

# 4. Lógica de Votação (Majority Vote)
if(nrow(tabela_resultados) > 0){
  
  # Identifica o melhor valor (mínimo) para cada métrica
  min_aic <- min(tabela_resultados$AIC)
  min_bic <- min(tabela_resultados$BIC)
  min_aicc <- min(tabela_resultados$AICc)
  
  # Cria coluna de votos (inicia com 0)
  tabela_resultados$Votos <- 0
  
  # Adiciona voto se o modelo tiver o melhor AIC
  tabela_resultados$Votos <- tabela_resultados$Votos + (tabela_resultados$AIC == min_aic)
  # Adiciona voto se o modelo tiver o melhor BIC
  tabela_resultados$Votos <- tabela_resultados$Votos + (tabela_resultados$BIC == min_bic)
  # Adiciona voto se o modelo tiver o melhor AICc
  tabela_resultados$Votos <- tabela_resultados$Votos + (tabela_resultados$AICc == min_aicc)
  
  # Ordenação: 1º Por Votos (Desc), 2º Por BIC (Asc - Desempate)
  tabela_resultados <- tabela_resultados[order(-tabela_resultados$Votos, tabela_resultados$BIC), ]
  
  # Exibição
  library(knitr)
  kable(head(tabela_resultados, 10), 
        row.names = FALSE, 
        digits = 2, 
        caption = "Top 10 Modelos (Ordenados por Votos nas métricas AIC, BIC e AICc)")
  
} else {
  cat("**Aviso:** Nenhum modelo convergiu.")
}
