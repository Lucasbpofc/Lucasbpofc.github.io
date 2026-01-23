# --- Carregar os pacotes necessários ---
library(shiny)

# ==============================================================================
# Interface do Usuário (UI)
# ==============================================================================
ui <- fluidPage(
  
  # Título do Aplicativo
  titlePanel("Teorema de De Moivre-Laplace: Aproximação da Binomial pela Normal"),
  
  # Layout com barra lateral para controles e painel principal para o gráfico
  sidebarLayout(
    
    # Painel da barra lateral com os controles
    sidebarPanel(
      h4("Parâmetros da Distribuição Binomial"),
      
      # Controle deslizante para o número de tentativas 'n'
      sliderInput(inputId = "n_slider",
                  label = "Número de Tentativas (n):",
                  min = 10,
                  max = 2000,
                  value = 30, # Valor inicial
                  step = 10,
                  animate = animationOptions(interval = 200, loop = FALSE)),
      
      # Controle deslizante para a probabilidade de sucesso 'p'
      sliderInput(inputId = "p_slider",
                  label = "Probabilidade de Sucesso (p):",
                  min = 0.01,
                  max = 0.99,
                  value = 0.5, # Valor inicial
                  step = 0.01),
      
      hr(), # Linha horizontal
      
      h4("Condição para Boa Aproximação:"),
      p("A aproximação da Binomial pela Normal é considerada boa quando:"),
      p(strong("n * p > 5"), " e ", strong("n * (1-p) > 5")),
      # Mostra o status atual da condição
      uiOutput("status_condicao")
    ),
    
    # Painel principal onde o gráfico será exibido
    mainPanel(
      plotOutput(outputId = "distPlot")
    )
  )
)

# ==============================================================================
# Servidor (Server) - A lógica R
# ==============================================================================
server <- function(input, output) {
  
  # Gera o gráfico de forma reativa
  output$distPlot <- renderPlot({
    
    # --- Pega os valores atuais dos sliders ---
    n <- input$n_slider
    p <- input$p_slider
    
    # --- Parâmetros para as distribuições ---
    # Define o intervalo de sucessos (eixo x)
    x_binom <- 0:n
    
    # Calcula as probabilidades da Binomial para cada x
    y_binom <- dbinom(x_binom, size = n, prob = p)
    
    # Calcula os parâmetros da Normal que aproxima a Binomial
    mu_norm <- n * p
    sigma_norm <- sqrt(n * p * (1 - p))
    
    # --- Cria o Gráfico ---
    # Gráfico de barras da distribuição Binomial (usando type='h' para linhas verticais)
    plot(x_binom, y_binom, 
         type = 'h', # 'h' para criar um histograma com linhas
         lwd = 4,    # Largura das linhas/barras
         col = "steelblue",
         main = paste("Binomial(n =", n, ", p =", p, ") vs. Aproximação Normal"),
         xlab = "Número de Sucessos (k)",
         ylab = "Probabilidade")
    
    # Sobrepõe a curva da Normal
    curve(dnorm(x, mean = mu_norm, sd = sigma_norm),
          from = 0, to = n,
          col = "red",
          lwd = 2,
          add = TRUE)
    
    # Adiciona uma legenda
    legend("topright", 
           legend = c("Probabilidade Binomial", "Aproximação Normal"), 
           col = c("steelblue", "red"), 
           lwd = c(4, 2),
           bty = "n")
  })
  
  # Gera o texto de status da condição de forma reativa
  output$status_condicao <- renderUI({
    n <- input$n_slider
    p <- input$p_slider
    
    # Verifica a condição
    check1 <- n * p > 5
    check2 <- n * (1 - p) > 5
    
    if (check1 && check2) {
      tags$p(strong("Status: Condição ATENDIDA."), style = "color:green;")
    } else {
      tags$p(strong("Status: Condição NÃO ATENDIDA."), style = "color:red;")
    }
  })
}

# ==============================================================================
# Comando para rodar o aplicativo
# ==============================================================================
shinyApp(ui = ui, server = server)