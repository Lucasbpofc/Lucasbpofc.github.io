# --- Carregar pacotes ---
library(shiny)
library(ggplot2)

# ==============================================================================
# Interface do Usuário (UI)
# ==============================================================================
ui <- fluidPage(
  
  titlePanel("Laboratório Interativo do Teorema Central do Limite (TCL)"),
  
  sidebarLayout(
    sidebarPanel(
      h4("1. Selecione a Distribuição da População"),
      
      selectInput("dist", "Distribuição:",
                  choices = c("Gama", "t-Student", "Weibull", "Binomial", "Uniforme")),
      
      hr(),
      
      # --- Controles Condicionais ---
      conditionalPanel("input.dist == 'Gama'",
                       numericInput("gama_shape", "Shape (Forma):", value = 2, min = 0.1),
                       numericInput("gama_rate", "Rate (Taxa):", value = 1, min = 0.1)),
      conditionalPanel("input.dist == 't-Student'",
                       numericInput("t_df", "Graus de Liberdade (df):", value = 3, min = 1)),
      conditionalPanel("input.dist == 'Weibull'",
                       numericInput("weibull_shape", "Shape (Forma):", value = 1.5, min = 0.1),
                       numericInput("weibull_scale", "Scale (Escala):", value = 1, min = 0.1)),
      conditionalPanel("input.dist == 'Binomial'",
                       numericInput("binom_size", "Número de Tentativas (size):", value = 10, min = 1),
                       sliderInput("binom_prob", "Probabilidade de Sucesso (prob):", min = 0.01, max = 0.99, value = 0.5)),
      
      hr(),
      h4("2. Configure a Amostragem"),
      
      sliderInput("n_slider", "Tamanho da Amostra (n):", 
                  min = 2, max = 2000, value = 30, step = 1,
                  animate = animationOptions(interval = 300, loop = FALSE)),
      
      sliderInput("n_sims", "Número de Amostras (Simulações):",
                  min = 100, max = 10000, value = 1000, step = 100)
      
      # <<<<<<< MUDANÇA: O BOTÃO FOI REMOVIDO DAQUI
      # actionButton("go_button", "Rodar Simulação", icon = icon("play"))
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Distribuição da População", 
                 plotOutput("populationPlot"),
                 p("Este gráfico mostra a forma da distribuição de onde as amostras são retiradas.")),
        tabPanel("Demonstração do TCL (Distribuição das Médias)", 
                 plotOutput("cltPlot"),
                 p("Este gráfico mostra o histograma das médias de cada amostra. Pelo TCL, ele deve convergir para uma distribuição Normal (curva vermelha) à medida que 'n' aumenta."))
      )
    )
  )
)

# ==============================================================================
# Servidor (Server) - Lógica do App
# ==============================================================================
server <- function(input, output) {
  
  # --- Simulação Reativa ---
  # <<<<<<< MUDANÇA: trocamos eventReactive por reactive
  # Agora, 'dados_simulados' será recalculado sempre que qualquer input dentro dele mudar
  dados_simulados <- reactive({
    
    n <- input$n_slider
    n_sims <- input$n_sims
    dist <- input$dist
    
    # Gera as médias amostrais baseado na distribuição selecionada
    medias_amostrais <- switch(dist,
                               "Gama"      = replicate(n_sims, mean(rgamma(n, shape = input$gama_shape, rate = input$gama_rate))),
                               "t-Student" = replicate(n_sims, mean(rt(n, df = input$t_df))),
                               "Weibull"   = replicate(n_sims, mean(rweibull(n, shape = input$weibull_shape, scale = input$weibull_scale))),
                               "Binomial"  = replicate(n_sims, mean(rbinom(n, size = input$binom_size, prob = input$binom_prob))),
                               "Uniforme"  = replicate(n_sims, mean(runif(n)))
    )
    return(medias_amostrais)
  })
  
  # --- Gráfico da População ---
  # Este gráfico só reage à mudança da distribuição e seus parâmetros
  output$populationPlot <- renderPlot({
    
    dist <- input$dist # Não precisa de isolate() aqui
    x_pop <- seq(-10, 20, length.out = 500)
    
    df_pop <- switch(dist,
                     "Gama"      = data.frame(x = x_pop, y = dgamma(x_pop, shape = input$gama_shape, rate = input$gama_rate)),
                     "t-Student" = data.frame(x = x_pop, y = dt(x_pop, df = input$t_df)),
                     "Weibull"   = data.frame(x = x_pop, y = dweibull(x_pop, shape = input$weibull_shape, scale = input$weibull_scale)),
                     "Binomial"  = {
                       x_b <- 0:input$binom_size
                       data.frame(x = x_b, y = dbinom(x_b, size = input$binom_size, prob = input$binom_prob))
                     },
                     "Uniforme"  = data.frame(x = x_pop, y = dunif(x_pop))
    )
    
    p <- ggplot(df_pop, aes(x, y)) + labs(title = paste("Forma da Distribuição de Origem:", dist), x = "", y = "Densidade / Probabilidade")
    
    if (dist == "Binomial") {
      p + geom_col(fill = "dodgerblue", alpha = 0.7)
    } else {
      p + geom_line(color = "dodgerblue", size = 1.5)
    }
  })
  
  # --- Gráfico da Demonstração do TCL ---
  # Este gráfico agora depende diretamente de 'dados_simulados', que é reativo
  output$cltPlot <- renderPlot({
    
    medias <- dados_simulados()
    
    ggplot(data.frame(medias = medias), aes(x = medias)) +
      geom_histogram(aes(y = ..density..), bins = 30, fill = "lightblue", color = "black") +
      stat_function(fun = dnorm, args = list(mean = mean(medias), sd = sd(medias)),
                    color = "red", size = 1.2) +
      labs(title = paste("Distribuição das Médias Amostrais para n =", input$n_slider), # << Usa o n atual
           x = "Média Amostral",
           y = "Densidade") +
      theme_minimal()
  })
}

# ==============================================================================
# Rodar o App
# ==============================================================================
shinyApp(ui = ui, server = server)