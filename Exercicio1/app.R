# --- Carregar os pacotes necessários ---
library(shiny)

# ==============================================================================
# Interface do Usuário (UI) - A parte visual do aplicativo
# ==============================================================================
ui <- fluidPage(
  
  # Título do Aplicativo
  titlePanel("Visualização Interativa do Teorema Central do Limite"),
  
  
  
  # Layout com uma barra lateral para os controles e um painel principal para o gráfico
  sidebarLayout(
    
    # Painel da barra lateral com os controles
    sidebarPanel(
      h4("Parâmetros da Simulação"),
      
      # Controle deslizante para o tamanho da amostra 'n'
      sliderInput(inputId = "n_slider",
                  label = "Tamanho da Amostra (n):",
                  min = 5,
                  max = 5000,
                  value = 30, # Valor inicial
                  step = 10,
                  animate = animationOptions(interval = 200, loop = FALSE)),
      
      hr(), # Linha horizontal para separar
      
      p("A população original segue uma distribuição Exponencial (muito assimétrica)."),
      p("Observe como o histograma das médias amostrais padronizadas se aproxima da curva Normal padrão (vermelha) à medida que 'n' aumenta.")
    ),
    
    # Painel principal onde o gráfico será exibido
    mainPanel(
      plotOutput(outputId = "distPlot")
    )
  )
)

# ==============================================================================
# Servidor (Server) - A lógica R que gera os resultados
# ==============================================================================
server <- function(input, output) {
  
  # Esta parte do código gera o gráfico.
  # A expressão é "reativa": ela será executada novamente toda vez que um input mudar (o slider)
  output$distPlot <- renderPlot({
    
    # --- Configuração da População ---
    lambda <- 2 
    mu_pop <- 1 / lambda      
    sigma_pop <- 1 / lambda   
    
    # --- Simulação ---
    # Pega o valor ATUAL do slider para 'n'
    n <- input$n_slider 
    n_sims <- 2000 # Número de médias a serem calculadas
    
    # Gera as médias amostrais
    medias_amostrais <- replicate(n_sims, mean(rexp(n, rate = lambda)))
    
    # Padroniza as médias
    z_scores <- (medias_amostrais - mu_pop) / (sigma_pop / sqrt(n))
    
    # --- Gráfico ---
    # Histograma dos resultados
    hist(z_scores,
         probability = TRUE,
         main = paste("Distribuição das Médias para n =", n),
         xlab = "Média Amostral Padronizada (Z)",
         ylab = "Densidade",
         xlim = c(-4, 4),
         breaks = 40)
    
    # Sobrepõe a curva da Normal Padrão teórica (N(0,1))
    curve(dnorm(x, mean = 0, sd = 1), 
          from = -4, to = 4, 
          col = "red", 
          lwd = 2, 
          add = TRUE)
  })
}

# ==============================================================================
# Comando para rodar o aplicativo
# ==============================================================================
shinyApp(ui = ui, server = server)