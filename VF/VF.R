library(shiny)
library(shinyjs) 

# --- 1. LISTA DE DESAFIOS (POSIÇÕES) ---
desafios <- list(
  list(titulo = "Papai e Mamãe", categoria = "geral", icone = "👩‍❤️‍💋‍👨", desc = "O clássico indispensável. Olho no olho e conexão total."),
  list(titulo = "De Lado (Conchinha)", categoria = "geral", icone = "🥄", desc = "Conforto e intimidade. O encaixe perfeito dos corpos deitados."),
  list(titulo = "Flor de Lótus", categoria = "geral", icone = "🧘", desc = "Sentados frente a frente, pernas entrelaçadas e abraço firme."),
  list(titulo = "A Prancha", categoria = "geral", icone = "🛹", desc = "Ele por cima apoiado nos braços, contato total pele a pele."),
  list(titulo = "Posição de Quatro", categoria = "geral", icone = "🐕", desc = "A clássica favorita. Ângulo perfeito e profundidade total."),
  list(titulo = "Frango Assado", categoria = "geral", icone = "🍗", desc = "Variação da missionária: Pernas dela bem altas, apoiadas nos ombros dele."),
  list(titulo = "Cavalgada (Ela por cima)", categoria = "geral", icone = "🤠", desc = "Ela no comando total do ritmo e da velocidade."),
  list(titulo = "Sobre a Mesa", categoria = "geral", icone = "🍽️", desc = "Ela na borda da mesa ou móvel alto, ele em pé. Altura ideal."),
  list(titulo = "O Trapézio", categoria = "geral", icone = "🎪", desc = "Ela deitada, pernas elevadas e apoiadas no peito dele."),
  list(titulo = "Borboleta Paraguaia", categoria = "geral", icone = "🦋", desc = "Inovação! Quadril elevado e pernas abertas. Vale a criatividade."),
  list(titulo = "Canguru Perneta", categoria = "geral", icone = "🦘", desc = "Uma perna dela no chão, a outra na cintura dele. Haja equilíbrio!"),
  list(titulo = "A Batedeira", categoria = "geral", icone = "🌀", desc = "Movimentos circulares de quadril enquanto penetra. A sensação é única."),
  list(titulo = "A Tesoura", categoria = "geral", icone = "✂️", desc = "Deitados de lado, pernas entrelaçadas como uma tesoura."),
  list(titulo = "Carrinho de Mão", categoria = "geral", icone = "🛒", desc = "Haja braço! Ela se apoia nas mãos, ele segura as pernas por trás."),
  list(titulo = "Posição 69", categoria = "oral", icone = "♋", desc = "Prazer simultâneo. Sincronia perfeita entre dar e receber."),
  list(titulo = "Oral Caprichado (Nela)", categoria = "oral", icone = "👅", desc = "Foco total nela. Sem pressa, explore tudo."),
  list(titulo = "Oral Caprichado (Nele)", categoria = "oral", icone = "🍭", desc = "Foco total nele. Capriche nos movimentos."),
  list(titulo = "Massagem Sensual", categoria = "massagem", icone = "💆‍♂️", desc = "Pausa para relaxar. Use óleo e percorra o corpo todo."),
  list(titulo = "Massagem nos Pés", categoria = "massagem", icone = "🦶", desc = "Comece pelos pés e vá subindo devagarinho.")
)

# --- 2. LISTA DE VERDADES (SUPER LISTA ATUALIZADA) ---
verdades <- c(
  "Qual é o lugar mais maluco onde você já transou?",
  "Qual foi a coisa que você fez na cama que nunca mais quer repetir?",
  "O que você mais curte nas preliminares?",
  "Qual é a sua posição favorita de todas?",
  "Como você se sente ao se masturbar?",
  "Qual foi o melhor orgasmo que você já teve?",
  "Qual é a primeira coisa safada que você faria se acordasse do sexo oposto?",
  "Qual é a fantasia mais louca que você já teve e que se tornou realidade?",
  "Como foi sua primeira vez no sexo?",
  "Qual é a sua história mais doida de uma noite só?",
  "Qual é o seu segredinho safado que nunca contou?",
  "Você já fez role-play (teatro) na cama?",
  "Qual é o lugar mais radical onde você gostaria de transar?",
  "Você já foi pego no flagra?",
  "Qual é a sua maneira favorita de iniciar o sexo?",
  "Você já usou brinquedos sexuais durante o sexo?",
  "Qual é a sua jogada certeira para me excitar?",
  "Você já fez um ménage ou pensou em fazer?",
  "Qual é a mensagem de texto mais safada que você já enviou?",
  "Qual foi a transa mais longa que você já teve?",
  "Você curte assistir pornô juntos?",
  "Qual é o seu role-play sexual mais inesquecível?",
  "Você já teve um amasso público que acabou em sexo?",
  "Qual é o seu tipo favorito de lingerie?",
  "Você já teve uma fantasia sobre alguém que não seja eu?",
  "Qual é o lugar mais ousado onde você já fez preliminares?",
  "Você já experimentou ou tem curiosidade sobre bondage/BDSM?",
  "Qual é a sua maneira favorita de me provocar antes do sexo?",
  "Você faria um strip tease ou lap dance pra mim agora?",
  "Qual foi a transa mais espontânea que você já teve?",
  # -- EXTRAS DA VERSÃO ANTERIOR --
  "Qual parte do meu corpo você mais gosta de beijar?",
  "O que eu faço na cama que te deixa louco(a) na hora?",
  "Descreva o que você quer fazer comigo hoje em 3 palavras.",
  "Qual a posição que a gente NUNCA fez, mas você tem curiosidade?"
)

ui <- fluidPage(
  useShinyjs(),
  title = "Verdade ou Desafio - Edição Especial",
  
  # Áudio do Alarme
  tags$audio(id = "som_alarme", src = "https://www.soundjay.com/buttons/beep-01a.mp3", type = "audio/mp3", style = "display:none;"),
  
  tags$head(
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Montserrat:wght@400;700&family=Pacifico&display=swap"),
    tags$style(HTML("
      body { background-color: #1a0505; color: #ff4d4d; font-family: 'Montserrat', sans-serif; text-align: center; }
      .main-container { display: flex; flex-direction: column; align-items: center; justify-content: center; min-height: 100vh; padding: 20px; }
      
      /* Botões de Escolha */
      .btn-choice-container { display: flex; gap: 20px; flex-wrap: wrap; justify-content: center; margin-top: 30px; }
      .btn-verdade { background: linear-gradient(45deg, #2b5876, #4e4376); color: white; border: 2px solid #fff; padding: 30px 40px; border-radius: 20px; font-size: 24px; font-weight: bold; width: 250px; transition: 0.3s; }
      .btn-verdade:hover { transform: scale(1.05); box-shadow: 0 0 20px #4e4376; color: white; }
      
      .btn-desafio { background: linear-gradient(45deg, #ff0000, #990000); color: white; border: 2px solid #fff; padding: 30px 40px; border-radius: 20px; font-size: 24px; font-weight: bold; width: 250px; transition: 0.3s; }
      .btn-desafio:hover { transform: scale(1.05); box-shadow: 0 0 20px #ff0000; color: white; }

      /* Cards */
      .card-content { background: #2b0b0b; border: 2px solid #555; border-radius: 20px; padding: 30px; margin: 20px auto; width: 90%; max-width: 500px; box-shadow: 0 0 20px rgba(0,0,0,0.8); animation: fadeIn 0.5s; }
      .card-desafio { border-color: #ff0000; box-shadow: 0 0 20px rgba(255, 0, 0, 0.3); }
      .card-verdade { border-color: #4e4376; box-shadow: 0 0 20px rgba(78, 67, 118, 0.5); }
      
      /* Tipografia */
      h1 { font-family: 'Pacifico', cursive; color: #fff; margin-bottom: 30px; font-size: 40px; }
      h2 { color: #ff4d4d; font-weight: bold; }
      .desc-intro { font-size: 18px; color: #ccc; max-width: 600px; margin: 0 auto; line-height: 1.6; }
      .emoji-grande { font-size: 80px; margin: 10px 0; display: block; filter: drop-shadow(0 0 10px rgba(255,255,255,0.2)); }
      .titulo-posicao { font-size: 28px; font-weight: bold; color: #fff; margin-bottom: 10px; }
      .desc-posicao { font-size: 18px; color: #ccc; margin-bottom: 20px; font-style: italic; }
      .texto-verdade { font-size: 24px; font-weight: bold; color: #fff; margin: 20px 0; font-style: italic; line-height: 1.4; }
      .timer-display { font-size: 50px; font-weight: 900; color: #fff; text-shadow: 0 0 10px #ff0000; margin: 15px 0; font-family: 'Courier New', monospace; }
      
      /* Botões de Ação */
      .btn-start { background: #fff; color: #ff0000; border: 2px solid #ff0000; font-size: 18px; font-weight: bold; padding: 10px 30px; border-radius: 30px; margin-top: 10px; cursor: pointer; }
      .btn-voltar { background: transparent; color: #888; border: 1px solid #555; margin-top: 30px; padding: 10px 20px; border-radius: 10px; }
      .btn-voltar:hover { background: #333; color: white; }
      
      /* Animações */
      @keyframes fadeIn { from { opacity: 0; transform: translateY(10px); } to { opacity: 1; transform: translateY(0); } }
      .btn-stop-sound { background-color: #ff0000; color: white; border: none; font-weight: bold; padding: 10px 20px; border-radius: 5px; margin-top: 10px; }
    "))
  ),
  
  tabsetPanel(
    id = "nav_principal", type = "hidden",
    
    # --- PÁGINA 1: INTRO ---
    tabPanel("intro",
             div(class = "main-container",
                 h1("Round 2"),
                 div(class = "emoji-grande", "🎲"),
                 br(),
                 p(class = "desc-intro", "Bem-vindos à fase bônus. Aqui as regras são simples:"),
                 p(class = "desc-intro", "Escolham entre revelar um segredo profundo ou encarar um desafio físico cronometrado."),
                 br(),
                 actionButton("go_escolha", "COMEÇAR O JOGO", class = "btn-start", style = "background: #ff4d4d; color: white; border: none; padding: 15px 40px;")
             )
    ),
    
    # --- PÁGINA 2: A ESCOLHA ---
    tabPanel("escolha",
             div(class = "main-container",
                 h1("Faça sua Escolha"),
                 div(class = "btn-choice-container",
                     actionButton("go_verdade", HTML("🔵<br>VERDADE"), class = "btn-verdade"),
                     actionButton("go_desafio", HTML("🔥<br>DESAFIO"), class = "btn-desafio")
                 ),
                 br(), br(),
                 p("Escolham com sabedoria...", style="color: #666;")
             )
    ),
    
    # --- PÁGINA 3A: CAMINHO DA VERDADE ---
    tabPanel("verdade_page",
             div(class = "main-container",
                 h1("Verdade"),
                 div(class = "card-content card-verdade",
                     div(class = "emoji-grande", "🤫"),
                     uiOutput("texto_pergunta"), 
                     br(),
                     actionButton("nova_verdade", "Outra Pergunta", class = "btn-start", style="border-color: #4e4376; color: #4e4376;")
                 ),
                 actionButton("back_verdade", "Voltar para Escolha", class = "btn-voltar")
             )
    ),
    
    # --- PÁGINA 3B: CAMINHO DO DESAFIO ---
    tabPanel("desafio_page",
             div(class = "main-container",
                 h1("Desafio"),
                 p("Sua posição sorteada é:"),
                 uiOutput("card_desafio"),
                 br(),
                 actionButton("novo_desafio", "Girar Novamente", class = "btn-start"),
                 br(),
                 actionButton("back_desafio", "Voltar para Escolha", class = "btn-voltar")
             )
    )
  )
)

server <- function(input, output, session) {
  
  item_atual <- reactiveVal(NULL)
  tempo_restante <- reactiveVal(0)
  cronometro_ativo <- reactiveVal(FALSE)
  pergunta_atual <- reactiveVal(NULL)
  
  js_parar_som <- "var audio = document.getElementById('som_alarme'); audio.pause(); audio.currentTime = 0;"
  
  # --- NAVEGAÇÃO ---
  observeEvent(input$go_escolha, { updateTabsetPanel(session, "nav_principal", selected = "escolha") })
  observeEvent(input$back_verdade, { updateTabsetPanel(session, "nav_principal", selected = "escolha") })
  observeEvent(input$back_desafio, { 
    runjs(js_parar_som)
    cronometro_ativo(FALSE)
    updateTabsetPanel(session, "nav_principal", selected = "escolha") 
  })
  
  # --- VERDADE ---
  sortear_verdade <- function() {
    p <- sample(verdades, 1)
    pergunta_atual(p)
  }
  
  observeEvent(input$go_verdade, {
    sortear_verdade()
    updateTabsetPanel(session, "nav_principal", selected = "verdade_page")
  })
  observeEvent(input$nova_verdade, { sortear_verdade() })
  
  output$texto_pergunta <- renderUI({ div(class="texto-verdade", pergunta_atual()) })
  
  # --- DESAFIO ---
  sortear_desafio <- function() {
    runjs(js_parar_som) 
    cronometro_ativo(FALSE) 
    
    escolhido <- sample(desafios, 1)[[1]]
    item_atual(escolhido)
    
    segundos <- 60 
    if (escolhido$categoria == "massagem") { segundos <- 180 } 
    else if (escolhido$categoria == "oral") { segundos <- 120 } 
    else { segundos <- sample(seq(60, 120, by=10), 1) }
    
    tempo_restante(segundos)
  }
  
  observeEvent(input$go_desafio, {
    sortear_desafio()
    updateTabsetPanel(session, "nav_principal", selected = "desafio_page")
  })
  observeEvent(input$novo_desafio, { sortear_desafio() })
  
  # Timer 
  observe({
    invalidateLater(1000, session)
    running <- isolate(cronometro_ativo())
    
    if (running) {
      t <- isolate(tempo_restante())
      if (t > 0) {
        tempo_restante(t - 1)
      } else {
        cronometro_ativo(FALSE)
        runjs("document.getElementById('som_alarme').play();") 
        
        showModal(modalDialog(
          title = "ACABOU O TEMPO! 🔔",
          div(style="text-align: center;",
              h2("Parem agora! 🛑", style="color: red; font-weight: bold;"),
              h4("Respirem fundo..."),
              br(),
              actionButton("btn_parar_som_modal", "🔕 Parar Som e Fechar", class = "btn-stop-sound")
          ),
          footer = NULL, easyClose = FALSE
        ))
      }
    }
  })
  
  observeEvent(input$btn_parar_som_modal, { runjs(js_parar_som); removeModal() })
  observeEvent(input$btn_start_timer, { cronometro_ativo(TRUE) })
  
  output$display_tempo <- renderText({
    s <- tempo_restante()
    sprintf("%02d:%02d", s %/% 60, s %% 60)
  })
  
  output$card_desafio <- renderUI({
    req(item_atual())
    item <- item_atual()
    ativo <- cronometro_ativo()
    tempo <- tempo_restante()
    
    btn_html <- if (!ativo && tempo > 0) {
      actionButton("btn_start_timer", "⏱️ Valendo!", class = "btn-start")
    } else if (ativo) {
      p("Contando...", style="color: #00ff00; font-weight: bold; animation: pulse 1s infinite;")
    } else {
      p("Tempo Esgotado!", style="color: red;")
    }
    
    div(class = "card-content card-desafio",
        div(class = "emoji-grande", item$icone),
        div(class = "titulo-posicao", item$titulo),
        div(class = "desc-posicao", item$desc),
        hr(style = "border-color: #555;"),
        div(class = "timer-display", textOutput("display_tempo")),
        btn_html
    )
  })
}

shinyApp(ui, server)