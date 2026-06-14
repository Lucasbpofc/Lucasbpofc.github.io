################################################################################
############                    WOME WORK END               ####################
################################################################################

# IMPORTAÇÃO DOS DADOS 
library(readxl)
dd3 <- read_excel("dd3.xlsx")
tail(dd3)
head(dd3)

# CRIANDO O CONJUNTO DE DADOS DA ATIVIDADE

library(dplyr)
dados <- dd3 %>%
  select(
    Family = Familia,
    SN     = NC,        # NC mapeado para Stalk Number (SN)
    TSH    = TCH.x,     # TCH.x mapeado para Tons of Stalks per Hectare (TSH)
    SD     = DIAMETRO,  # DIAMETRO mapeado para Stalk Diameter (SD)
    SH     = ALTURA,    # ALTURA mapeada para Stalk Height (SH)
    R      = Red,
    G      = Green,
    B      = Blue,
    VARI   = VARI,
    GLI    = GLI,
    NGRDI  = NGRDI
  )

head(dados)
tail(dados)

# ANÁLISE DESCRITIVA 

library(tidyverse)
library(corrplot)
library(psych)

dim(dados) #Dimensão dos dados
names(dados) #nomes das variáveis

# Transformar a coluna 'Family' em fator 
dados$Family <- as.factor(dados$Family)

# Resumo  dos dados (Mínimo, Máximo, Quartis e Média)
summary(dados)

# Resumo detalhado (inclui desvio padrão, assimetria e curtose)
# Usando a função describe do pacote psych, excluindo a coluna Family (que é fator)
descritiva_detalhada <- describe(dados %>% select(-Family))
print(descritiva_detalhada)

# ANÁLISE GRÁFICA (BOXPLOTS)

dados_long <- dados %>%
  select(-Family) %>% # Removemos a família para plotar apenas as numéricas
  pivot_longer(cols = everything(), names_to = "Variavel", values_to = "Valor")

grafico_boxplots <- ggplot(dados_long, aes(x = Variavel, y = Valor, fill = Variavel)) +
  geom_boxplot(color = "black", alpha = 0.7) +
  facet_wrap(~ Variavel, scales = "free") + # Cria um gráfico separado para cada variável
  theme_minimal() +
  labs(title = "Distribuição das Variáveis (Boxplots)",
       x = NULL, y = "Valores") +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, face = "bold"))

print(grafico_boxplots)

# ANÁLISE DE CORRELAÇÃO - QUESTÃO 1 DO TRABALHO

# Seleciona apenas as variáveis numéricas
dados_num <- dados %>% select(-Family)

# Calcula a matriz de correlação (Pearson)
matriz_corr <- cor(dados_num, use = "complete.obs")

# Plota a matriz de correlação de forma bem visual e profissional
corrplot(matriz_corr, 
         method = "color",       # Usa cores para representar a força
         type = "upper",         # Mostra só a metade de cima da matriz
         addCoef.col = "black",  # Adiciona os números da correlação em preto
         tl.col = "black",       # Cor do texto dos eixos
         tl.srt = 45,            # Rotaciona os nomes das variáveis em 45 graus
         diag = FALSE,           # Tira a diagonal principal (que é sempre 1)
         title = "Matriz de Correlação das Variáveis",
         mar = c(0,0,1,0))       # Ajusta a margem para o título caber direitinho

# Certifique-se de que a biblioteca tidyverse está carregada
library(tidyverse)

# ANÁLISE GRÁFICA: TSH VS VARIÁVEIS PREDITORAS (SCATTER PLOTS)

# 1. Preparar os dados empilhando as variáveis preditoras para plotagem em grade
dados_plot_bivariado <- dados %>%
  select(-Family) %>%
  pivot_longer(cols = -TSH, # Empilha tudo, EXCETO o TSH (que será nosso eixo Y)
               names_to = "Nome_Preditora",
               values_to = "Valor_Preditora")

# Criar o gráfico de grade usando ggplot2 e geom_point + geom_smooth
grafico_preditoras_vs_tsh <- ggplot(dados_plot_bivariado, aes(x = Valor_Preditora, y = TSH)) +
  geom_point(color = "black", alpha = 0.5, size = 1.5) + # Pontos mais transparentes para ver densidade
  geom_smooth(method = "lm", color = "red", fill = "pink", alpha = 0.3) + # Adiciona linha de regressão linear
  facet_wrap(~ Nome_Preditora, scales = "free_x", ncol = 3) + # Uma grade para cada preditora
  theme_minimal() +
  labs(
    title = "Relação bivariada: Preditoras vs Variável Resposta (TSH)",
    subtitle = "A linha vermelha representa a tendência linear (Regression Line)",
    x = "Valor da Variável Preditora",
    y = "Produtividade (TSH)"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11, face = "italic"),
    strip.text = element_text(face = "bold", size = 10, color = "blue") # Destaca os nomes das preditoras
  )

print(grafico_preditoras_vs_tsh)

# HISTOGRAMA DE TSH
hist(dados$TSH, col = "blue")
shapiro.test(TSH) 
# Como o p-valor > 0,05 a variável é normalmente distribuida.
names(dados)
attach(dados)
plot(SN, TSH)
plot(SD, TSH)
plot(SH, TSH)
plot(R, TSH)
plot(G, TSH)
plot(B, TSH)
plot(VARI, TSH)
plot(GLI, TSH)
plot(NGRDI, TSH)

# Linear Regression