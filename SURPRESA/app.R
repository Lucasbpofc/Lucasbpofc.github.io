library(shiny)
library(glue)
library(lubridate) # Biblioteca para lidar melhor com datas/horas

# --- CONFIGURAÇÕES ---
# 1. AJUSTE A DATA E A HORA AQUI (Formato: AAAA-MM-DD HH:MM:SS)
data_hora_inicio <- "2022-03-05 22:30:00" 

nome_dela <- "Meu amor"
musica_arquivo <- "musica.mp3"

# --- LISTA DE FOTOS ---
lista_fotos <- c(
  "foto1.jpeg",
  "foto2.jpeg",
  "foto3.jpeg",
  "foto4.jpeg",
  "foto5.1.jpeg",
  "foto5.jpeg",
  "foto6.jpeg",
  "foto7.jpeg",
  "foto8.jpeg",
  "foto9.jpeg",
  "foto10.jpeg",
  "foto11.jpeg",
  "foto12.jpeg",
  "foto13.jpeg",
  "foto14.jpeg",
  "foto15.jpeg",
  "foto16.jpeg",
  "foto17.jpeg",
  "foto20.jpeg"
)

# --- LEGENDAS ---
# Importante: Tem que ter exatas 18 frases, na mesma ordem das fotos!
legendas <- c(
  "1. O começo de tudo...",
  "2. Nossa primeira trilha",
  "3. Mirante em São Geraldo",
  "4. Conhecendo a Serra do Brigadeiro",
  "5. Fotão retirada do pico da bandeira",
  "6. O fatídico dia do show no Falamansa",
  "7. O Hulk magrelo e sua amada",
  "8. Os padrinhos mais bonitos da festa",
  "9. Formatura do Mestrado",
  "10. Com a mão onde eu gosto kkkk",
  "11. Momento de carinho antes de furnicar",
  "12. Nossa foto mais linda",
  "13. Dia de ficar segurando xixi no spring",
  "14. Cachoeira secreta na fazenda do brigadeiro",
  "15. Dia do casarão",
  "16. Eu você e a Juliana",
  "17. De uns casamentos por ai",
  "18. Meu papel de parede",
  "19. Te amo muito!"
)
# --- 3. NOVAS LISTAS (PREENCHA AQUI!) ---
# Dica: Use emojis para ficar bonito na tela
lista_filmes <- c(
  "🎬 O milagre da cela 7",
  "🎬 A Vida é bela",
  "🎬 Jojo Rabbit",
  "🎬 Interestelar",
  "🎬 Anjos da Lei",
  "🎬 Pantera Negra",
  "🎬 A vingança está na moda",
  "🎬 A Noiva Cadáver",
  "🎬 Sexta feira muito louca"
)

# --- 3. NOVAS LISTAS (ATUALIZADO COM EMOJIS) ---

lista_comidas <- c(
  "🍕 Pizza Portuguesa",
  "🍣 Sushi",
  "🍔 Hambúrguer",
  "🍝 Aquele macarrão que você faz",
  "🍫 Chocolate (sempre)",
  "🥘 Sua lasanha",
  "🥓 Nosso trio mineiro",
  "🥖 Nosso pão de alho",
  "🥔 Nosso purê de batata"
)

lista_lugares <- c(
  "🌲 Serra do Brigadeiro",
  "🌊 Cachoeira do Boné",
  "🌊 Cachoeira Grande",
  "⛪ Ouro Preto",
  "🌊 Cachoeira da Usina",
  "🌄 Mirante em São Geraldo",
  "🍻 Quase todos os bares de Viçosa",
  "🏡 Nossa casa"
)
# --- CÁLCULOS DE TEMPO ---
inicio <- as.POSIXct(data_hora_inicio)
agora <- Sys.time()
diferenca <- as.period(interval(inicio, agora))
anos <- diferenca@year
meses <- diferenca@month
dias <- diferenca@day
horas <- diferenca@hour
minutos_totais <- as.numeric(difftime(agora, inicio, units = "mins"))

get_estacao <- function(data) {
  d <- month(data) * 100 + day(data)
  if (d >= 1221 || d <= 320) return("Verão")
  if (d >= 321 && d <= 620) return("Outono")
  if (d >= 621 && d <= 922) return("Inverno")
  return("Primavera")
}
estacao_inicio <- get_estacao(inicio)
periodo_dia <- ifelse(hour(inicio) < 12, "manhã", ifelse(hour(inicio) < 18, "tarde", "noite"))

# --- INTERFACE (UI) ---
ui <- fluidPage(
  title = "Nossa Retrospectiva",
  
  tags$head(
    tags$style(HTML("
      /* TEMA: TERRACOTA, VINHO E MARROM */
      body { background-color: #2B1E1A; color: #F2E8DC; font-family: 'Helvetica Neue', Helvetica, Arial, sans-serif; text-align: center; }
      
      .btn-spotify { 
        background-color: #5D1219; color: #F2E8DC; border-radius: 50px; border: 1px solid #75222B; 
        padding: 15px 30px; font-size: 18px; font-weight: bold; margin-top: 20px; box-shadow: 0 4px 15px rgba(0, 0, 0, 0.4); 
      }
      .btn-spotify:hover { background-color: #C85A3D; color: #FFFFFF; }
      
      h1 { font-size: 32px; font-weight: bold; margin-bottom: 20px; color: #C85A3D; }
      h2 { font-size: 24px; color: #E08E79; }
      h3 { color: #A69B95; font-size: 18px; }
      
      .destaque { font-size: 50px; font-weight: 900; color: #C85A3D; }
      
      .img-container { margin: 20px auto; max-width: 90%; max-height: 50vh; border-radius: 5px; box-shadow: 0 10px 25px rgba(0,0,0,0.6); border: 3px solid #5D1219; }
      .slide-content { padding-top: 5vh; max-width: 600px; margin: 0 auto; }
      
      /* Estilo das Listas (Filmes, Comidas...) */
      .lista-favoritos { font-size: 22px; line-height: 1.8; text-align: left; display: inline-block; margin-top: 20px; }
      
      .progress-bar-bg { width: 80%; height: 6px; background-color: #4A352F; margin: 0 auto 20px auto; border-radius: 5px; }
      .progress-bar-fill { height: 100%; background-color: #C85A3D; border-radius: 5px; transition: width 0.5s; }
    "))
  ),
  
  # Navegação
  tabsetPanel(
    id = "wizard",
    type = "hidden",
    
    # 1. CAPA
    tabPanel("intro",
             div(class = "slide-content",
                 br(), br(),
                 h1(glue("Olá, {nome_dela}!")),
                 h3("Preparei uma retrospectiva da nossa história."),
                 p("Aumente o som..."),
                 tags$audio(src = musica_arquivo, type = "audio/mp3", controls = TRUE, style="width: 80%; margin: 20px 0;"),
                 br(),
                 actionButton("go_contador", "Começar", class = "btn-spotify")
             )
    ),
    
    # 2. CONTADOR
    tabPanel("contador",
             div(class = "slide-content",
                 h3("Tudo começou naquela"),
                 h2(glue("{periodo_dia} de {estacao_inicio}")),
                 h3(format(inicio, "em %d/%m/%Y às %H:%M")),
                 br(),
                 h3("Desde então, já se passaram:"),
                 h1(class = "destaque", glue("{format(round(minutos_totais), big.mark='.')} minutos")),
                 p("Ou, para ser mais exato:"),
                 h2(glue("{anos} anos, {meses} meses, {dias} dias e {horas} horas")),
                 h3("de nós dois."),
                 br(),
                 actionButton("go_fotos", "Ver Melhores Momentos", class = "btn-spotify")
             )
    ),
    
    # 3. GALERIA DE FOTOS
    tabPanel("galeria",
             div(class = "slide-content",
                 div(class = "progress-bar-bg", uiOutput("barra_progresso")),
                 uiOutput("imagem_atual"),
                 uiOutput("legenda_atual"),
                 br(),
                 actionButton("proxima_foto", "Próximo", class = "btn-spotify")
             )
    ),
    
    # 4. SLIDE DE FILMES
    tabPanel("filmes",
             div(class = "slide-content",
                 h1("Nossos Filmes"),
                 h3("Histórias que vivemos juntos na tela:"),
                 div(class = "lista-favoritos", uiOutput("ui_filmes")),
                 br(), br(),
                 actionButton("go_comidas", "Próximo", class = "btn-spotify")
             )
    ),
    
    # 5. SLIDE DE COMIDAS
    tabPanel("comidas",
             div(class = "slide-content",
                 h1("Sabores que Amamos"),
                 h3("Porque amor também é comer bem:"),
                 div(class = "lista-favoritos", uiOutput("ui_comidas")),
                 br(), br(),
                 actionButton("go_lugares", "Próximo", class = "btn-spotify")
             )
    ),
    
    # 6. SLIDE DE LUGARES
    tabPanel("lugares",
             div(class = "slide-content",
                 h1("Lugares Especiais"),
                 h3("Onde deixamos nossas pegadas:"),
                 div(class = "lista-favoritos", uiOutput("ui_lugares")),
                 br(), br(),
                 actionButton("go_final", "Próximo", class = "btn-spotify")
             )
    ),
    
    # 7. FINAL
    tabPanel("final",
             div(class = "slide-content",
                 br(), br(),
                 h1("Te amo!"),
                 h3("Obrigado por ser minha melhor companhia em tudo isso."),
                 br(),
                 actionButton("restart", "Ver tudo de novo", class = "btn-spotify")
             )
    )
  )
)

# --- SERVIDOR (SERVER) ---
server <- function(input, output, session) {
  
  indice_foto <- reactiveVal(1)
  
  # Navegação Simples
  observeEvent(input$go_contador, { updateTabsetPanel(session, "wizard", selected = "contador") })
  observeEvent(input$go_fotos, { indice_foto(1); updateTabsetPanel(session, "wizard", selected = "galeria") })
  
  # Navegação entre as Novas Listas
  observeEvent(input$go_comidas, { updateTabsetPanel(session, "wizard", selected = "comidas") })
  observeEvent(input$go_lugares, { updateTabsetPanel(session, "wizard", selected = "lugares") })
  observeEvent(input$go_final, { updateTabsetPanel(session, "wizard", selected = "final") })
  observeEvent(input$restart, { updateTabsetPanel(session, "wizard", selected = "intro") })
  
  # Lógica da Galeria (Próximo Foto -> Se acabar, vai para Filmes)
  observeEvent(input$proxima_foto, {
    atual <- indice_foto()
    if (atual < length(lista_fotos)) {
      indice_foto(atual + 1)
    } else {
      updateTabsetPanel(session, "wizard", selected = "filmes") # <--- MUDANÇA AQUI
    }
  })
  
  # Render Outputs
  output$imagem_atual <- renderUI({ img(src = lista_fotos[indice_foto()], class = "img-container") })
  output$legenda_atual <- renderUI({ h3(legendas[indice_foto()]) })
  
  output$barra_progresso <- renderUI({
    pct <- (indice_foto() / length(lista_fotos)) * 100
    div(class = "progress-bar-fill", style = glue("width: {pct}%;"))
  })
  
  # Renderizar as Listas com quebra de linha HTML
  output$ui_filmes <- renderUI({ HTML(paste(lista_filmes, collapse = "<br>")) })
  output$ui_comidas <- renderUI({ HTML(paste(lista_comidas, collapse = "<br>")) })
  output$ui_lugares <- renderUI({ HTML(paste(lista_lugares, collapse = "<br>")) })
}

shinyApp(ui, server)