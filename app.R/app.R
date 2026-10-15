# EXTREMOS EM VICOSa — Shiny / R
# Instale uma vez no console:
# install.packages(c("shiny", "bslib", "readxl", "extRemes", "distillery", "ismev"))
# Abra este arquivo no RStudio e clique em Run App.
# Alternativa: shiny::runApp("caminho/para/extremos_shiny")
# Cada linha deve ser UM maximo de janeiro por ano, ja extraido.
# O aplicativo nao calcula maximos a partir de observacoes diarias.
# Versao inicial: GEV/Gumbel estacionarias; sem POT ou covariaveis.

pacotes <- c("shiny", "bslib", "readxl", "extRemes", "distillery", "ismev")
faltam <- pacotes[!vapply(pacotes, requireNamespace, logical(1), quietly = TRUE)]
if (length(faltam)) stop("Instale os pacotes: ", paste(faltam, collapse = ", "))
library(shiny)

options(shiny.maxRequestSize = 20 * 1024^2)
cor_a <- "#2889c7"
cor_b <- "#dc7633"

# Amostra artificial: nunca representa os dados observados de Vicosa.
set.seed(2026)
demonstracao <- data.frame(
  ANO = 1968:2022,
  PRECIP = extRemes::revd(55, loc = 65, scale = 18, shape = 0.08,
                         type = "GEV")
)

ajustar <- function(d, modelo) {
  if (nrow(d) < 15) stop("Selecione pelo menos 15 extremos. Esse limite operacional nao garante precisao.")
  if (length(unique(d$chuva)) < 5) stop("Poucos valores distintos para ajustar o modelo.")
  avisos <- character()
  fit <- withCallingHandlers(
    extRemes::fevd(d$chuva, type = modelo, method = "MLE", units = "mm"),
    warning = function(w) {
      avisos <<- c(avisos, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  if (!is.null(fit$results$convergence) && fit$results$convergence != 0)
    stop("A otimizacao nao convergiu. Reveja os dados ou a janela temporal.")
  p <- fit$results$par
  mu <- unname(p["location"])
  sigma <- unname(p["scale"])
  xi <- if (modelo == "Gumbel") 0 else unname(p["shape"])
  if (any(!is.finite(c(mu, sigma, xi))) || sigma <= 0 ||
      any(1 + xi * (d$chuva - mu) / sigma <= 0))
    stop("O ajuste retornou parametros invalidos ou violacao do suporte.")
  list(fit = fit, dados = d, modelo = modelo, mu = mu, sigma = sigma,
       xi = xi, avisos = unique(avisos))
}

# Falhas de ajuste/IC viram mensagens legiveis na interface.
seguro <- function(expr) tryCatch(expr, error = function(e) list(erro = conditionMessage(e)))
exigir <- function(x) {
  validate(need(is.null(x$erro), x$erro))
  x
}
quantil <- function(a, p) extRemes::qevd(p, loc = a$mu, scale = a$sigma,
                                        shape = a$xi, type = "GEV")
acumulada <- function(a, x) extRemes::pevd(x, loc = a$mu, scale = a$sigma,
                                          shape = a$xi, type = "GEV")
niveis <- function(a, periodos, confianca, metodo = "normal", xrange = NULL) {
  periodos <- as.numeric(periodos)
  if (!length(periodos) || any(!is.finite(periodos)) || any(periodos <= 1))
    stop("Os periodos de retorno devem ser finitos e maiores que 1.")
  if (length(confianca) != 1L || !is.finite(confianca) ||
      confianca <= 0 || confianca >= 1) stop("Confianca invalida.")

  # Uma chamada escalar por periodo: evita depender da montagem matricial
  # de varios intervalos dentro de ci.fevd e de atributos da classe ci.
  linhas <- lapply(periodos, function(T) {
    ic <- tryCatch(
      distillery::ci(a$fit, type = "return.level", return.period = T,
                     alpha = 1 - confianca, method = metodo, xrange = xrange, verbose = FALSE),
      error = function(e) stop(sprintf("IC para T = %g anos: %s", T,
                                       conditionMessage(e)), call. = FALSE)
    )
    limites <- as.numeric(ic)
    if (length(limites) != 3L || any(!is.finite(limites)))
      stop(sprintf("IC para T = %g nao retornou tres numeros finitos.", T))
    z <- as.numeric(quantil(a, 1 - 1 / T))
    if (length(z) != 1L || !is.finite(z) || limites[1] > limites[3])
      stop(sprintf("Nivel ou limites invalidos para T = %g.", T))
    data.frame(T_anos = T, nivel_mm = z, inferior_mm = limites[1],
               superior_mm = limites[3])
  })
  resultado <- do.call(rbind, linhas)
  rownames(resultado) <- NULL
  resultado
}
curva <- function(tb, cor, adicionar = FALSE) {
  if (!adicionar) {
    plot(tb$T_anos, tb$nivel_mm, type = "n", log = "x",
         ylim = range(tb[, c("inferior_mm", "superior_mm")]),
         xlab = "Periodo de retorno (anos; maximos de janeiro)",
         ylab = "Precipitacao (mm)")
  }
  polygon(c(tb$T_anos, rev(tb$T_anos)),
          c(tb$inferior_mm, rev(tb$superior_mm)),
          col = grDevices::adjustcolor(cor, alpha.f = 0.16), border = NA)
  lines(tb$T_anos, tb$nivel_mm, col = cor, lwd = 2)
}

ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "flatly"),
  titlePanel("Extremos de precipitacao | Vicosa"),
  sidebarLayout(
    sidebarPanel(width = 3,
      radioButtons("fonte", "Origem dos dados", c("Demonstracao simulada" = "demo",
                                                 "Minha planilha Excel" = "excel")),
      conditionalPanel("input.fonte == 'excel'",
        fileInput("arquivo", "Carregar EXTREMOS.xlsx", accept = c(".xlsx", ".xls")),
        uiOutput("folhas_ui"), uiOutput("colunas_ui"),
        conditionalPanel("input.ano_col == '__sequencia__'",
          numericInput("ano_inicio", "Ano da primeira linha", 1968, step = 1),
          checkboxInput("confirmar", "Confirmo uma linha por ano, sem anos ausentes e em ordem", FALSE)
        )
      ),
      textInput("rotulo", "Descricao do bloco", "Janeiro"),
      helpText("Uma linha = um maximo do mes escolhido em um ano. Unidade: mm."),
      uiOutput("janela_ui"),
      selectInput("modelo", "Distribuicao", c("GEV", "Gumbel")),
      selectInput("conf", "Confianca dos intervalos",
                  choices = c("90%" = "90", "95%" = "95", "99%" = "99"),
                  selected = "95"),
      numericInput("retorno", "Periodo de retorno T (anos)", 10, min = 1.01, max = 1000),
      hr(),
      helpText("Ajustes estacionarios por maxima verossimilhanca. Os intervalos da curva usam o metodo delta."),
      downloadButton("baixar", "Baixar niveis de retorno")
    ),
    mainPanel(width = 9,
      uiOutput("origem_ui"),
      tabsetPanel(id = "aba",
        tabPanel("Dados", value = "dados",
          verbatimTextOutput("resumo"), plotOutput("serie"),
          tableOutput("dados_tabela")
        ),
        tabPanel("Ajuste", value = "ajuste",
          verbatimTextOutput("parametros"), plotOutput("densidade"),
          p("O sinal da forma indica o comportamento da cauda estimada. Sua incerteza deve ser considerada; a estimativa isolada nao define conclusivamente a familia.")
        ),
        tabPanel("Diagnosticos", value = "diagnosticos",
          plotOutput("diagnosticos", height = "440px"),
          p("A proximidade da diagonal apoia a adequacao marginal. Estes graficos nao comprovam independencia ou estacionariedade."),
          plotOutput("dependencia", height = "260px")
        ),
        tabPanel("Niveis de retorno", value = "retorno",
          plotOutput("curva_retorno"), tableOutput("retornos"),
          verbatimTextOutput("interpretacao"),
          numericInput("critico", "Valor critico de precipitacao (mm)", 100, min = 0),
          numericInput("horizonte", "Horizonte de interesse (anos)", 30, min = 1, max = 1000),
          verbatimTextOutput("risco"),
          numericInput("perfil_low", "Limite inferior da busca do perfil (mm)", 50),
          numericInput("perfil_up", "Limite superior da busca do perfil (mm)", 250),
          helpText("Esses valores delimitam a grade de precipitacao em que o perfil e avaliado. Ajuste-os para abranger o pico e os dois cruzamentos com a linha critica."),
          actionButton("perfil", "Calcular IC por perfil (extRemes)"),
          helpText("O perfil pode demorar. Recalcule se alterar dados, modelo, T ou confianca."),
          verbatimTextOutput("resultado_perfil")
        ),
        tabPanel("Conferencia ismev", value = "ismev",
          p("Ajuste GEV independente com ismev::gev.fit(), para os mesmos dados da janela A. Esta aba sempre usa GEV, mesmo se Gumbel estiver selecionada na barra lateral."),
          tableOutput("parametros_ismev"),
          plotOutput("diag_ismev", height = "520px"),
          p("O perfil abaixo usa T e os limites de busca informados na aba Niveis de retorno. Cada pacote otimiza seu proprio ajuste; pequenas diferencas numericas sao possiveis."),
          actionButton("perfil_ismev", "Desenhar perfil GEV (ismev)"),
          plotOutput("grafico_perfil_ismev", height = "360px")
        ),
        tabPanel("Comparar janelas", value = "comparar",
          p("A janela A e a selecionada na barra lateral. Escolha abaixo a janela B. Ambas usam a mesma distribuicao e confianca."),
          uiOutput("janela_b_ui"),
          plotOutput("comparacao"), tableOutput("comparacao_tabela"),
          p("Janelas podem se sobrepor. Diferencas nas estimativas nao constituem um teste de mudanca climatica ou de tendencia.")
        ),
        tabPanel("Como interpretar", value = "ajuda",
          h4("O que esta sendo modelado?"),
          p("Maximos de um mesmo mes, com uma observacao por ano. Confirme se PRECIP e o maximo diario dentro do mes ou outra medida antes de interpretar os resultados."),
          p("Um nivel de retorno de 100 anos tem probabilidade de excedencia de 1% em cada ano, para esse mes, sob estacionariedade. Nao implica ocorrencias regulares a cada 100 anos."),
          p("As probabilidades em varios anos pressupõem independencia entre os maximos anuais e parametros constantes."),
          p("Amostras pequenas e periodos muito superiores ao tempo observado produzem extrapolacoes com elevada incerteza. Limites negativos da aproximacao normal nao tem interpretacao fisica para chuva."),
          p("O aplicativo nao verifica automaticamente todos os pressupostos. ACF e diagnosticos graficos devem ser acompanhados de conhecimento da serie, cobertura dos registros e avaliacao de tendencia e dependencia."),
          p("Versao inicial: sem POT/GPD, covariaveis ou teste KS. Dados artificiais sao apenas demonstrativos; nao representam registros observados de Vicosa."),
          p("Pacotes: shiny, bslib, readxl, extRemes, distillery e ismev. codigo de ajuste: extRemes::fevd(), method = 'MLE'.")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  confianca_atual <- reactive({
    req(input$conf)
    valor <- suppressWarnings(as.numeric(input$conf))
    validate(need(length(valor) == 1L && is.finite(valor),
                  "Selecione um nivel de confianca."))
    if (valor > 1 && valor <= 100) valor <- valor / 100
    validate(need(valor > 0 && valor < 1,
                  "A confianca deve estar entre 0 e 100%, sem incluir os extremos."))
    valor
  })
  output$folhas_ui <- renderUI({
    req(input$arquivo)
    folhas <- readxl::excel_sheets(input$arquivo$datapath)
    selectInput("folha", "Aba do Excel", folhas)
  })
  bruto <- reactive({
    if (input$fonte == "demo") return(demonstracao)
    req(input$arquivo, input$folha)
    as.data.frame(readxl::read_excel(input$arquivo$datapath, sheet = input$folha))
  })
  output$colunas_ui <- renderUI({
    n <- names(bruto())
    escolhida <- if ("PRECIP" %in% n) "PRECIP" else n[1]
    candidato <- n[toupper(n) %in% c("ANO", "YEAR")]
    tagList(selectInput("valor_col", "Coluna dos maximos (mm)", n, escolhida),
            selectInput("ano_col", "Coluna do ano",
                        c("Gerar sequencia de anos" = "__sequencia__", stats::setNames(n, n)),
                        if (length(candidato)) candidato[1] else "__sequencia__"))
  })
  dados <- reactive({
    b <- bruto()
    if (input$fonte == "demo") return(data.frame(ano = b$ANO, chuva = b$PRECIP))
    req(input$valor_col, input$ano_col)
    validate(need(input$valor_col %in% names(b), "Selecione a coluna dos maximos."))
    v <- b[[input$valor_col]]
    validate(need(is.numeric(v), "A coluna de precipitacao deve ser numerica no Excel."))
    if (input$ano_col == "__sequencia__") {
      validate(need(isTRUE(input$confirmar), "Confirme a sequencia anual ou selecione uma coluna de ano."))
      req(input$ano_inicio)
      y <- input$ano_inicio + seq_len(nrow(b)) - 1
    } else {
      validate(need(input$ano_col %in% names(b), "Selecione a coluna de ano."))
      y <- suppressWarnings(as.numeric(as.character(b[[input$ano_col]])))
    }
    validate(need(length(y) > 0 && all(is.finite(y)) && all(y == floor(y)),
                  "Todos os anos devem ser numeros inteiros, sem celulas vazias."),
             need(!anyDuplicated(y), "Existem anos duplicados: use uma linha por ano."),
             need(all(is.na(v) | (is.finite(v) & v >= 0)), "Precipitacoes devem ser finitas e nao negativas."))
    d <- data.frame(ano = y, chuva = v)
    d[order(d$ano), ]
  })
  seletor_janela <- function(id, titulo) {
    d <- dados()
    validate(need(nrow(d) >= 2 && diff(range(d$ano)) > 0, "Sao necessarios pelo menos dois anos distintos."))
    sliderInput(id, titulo, min(d$ano), max(d$ano), range(d$ano), step = 1, sep = "")
  }
  output$janela_ui <- renderUI(seletor_janela("janela", "Janela A (anos)"))
  output$janela_b_ui <- renderUI(seletor_janela("janela_b", "Janela B (anos)"))
  filtrar <- function(janela) {
    req(janela)
    d <- dados()
    d <- d[d$ano >= janela[1] & d$ano <= janela[2] & !is.na(d$chuva), ]
    validate(need(nrow(d) >= 2, "A janela possui menos de dois maximos validos."))
    d
  }
  selecionados <- reactive(filtrar(input$janela))
  ajuste <- reactive(exigir(seguro(ajustar(selecionados(), input$modelo))))
  ajuste_b <- reactive(exigir(seguro(ajustar(filtrar(input$janela_b), input$modelo))))
  t_atual <- reactive({
    req(input$retorno)
    validate(need(is.finite(input$retorno) && input$retorno > 1 && input$retorno <= 1000,
                  "T deve estar entre 1 (exclusivo) e 1000 anos."))
    input$retorno
  })
  periodos <- reactive(sort(unique(c(2, 5, 10, 20, 50, 100, t_atual()))))
  tabela <- reactive(exigir(seguro(niveis(ajuste(), periodos(), confianca_atual()))))
  grade <- reactive(exp(seq(log(1.1), log(max(100, t_atual())), length.out = 100)))
  output$origem_ui <- renderUI({
    if (input$fonte == "demo") {
      div(class = "alert alert-warning", "DEMONSTRACAO: dados simulados, sem correspondencia com as chuvas observadas de Vicosa.")
    } else {
      req(input$arquivo, input$folha)
      div(class = "alert alert-info", paste("Arquivo:", input$arquivo$name,
          "| Aba:", input$folha, "| Bloco:", input$rotulo))
    }
  })
  output$resumo <- renderPrint({
    d <- selecionados(); b <- dados()
    cat("Bloco:", input$rotulo, "\nJanela selecionada:", paste(input$janela, collapse = " a "),
        "\nMaximos validos:", nrow(d), "\nAnos com precipitacao ausente na planilha:", sum(is.na(b$chuva)),
        "\nAnos sem linha na planilha:", length(setdiff(seq(min(b$ano), max(b$ano)), b$ano)), "\n\n")
    print(summary(d$chuva))
  })
  output$dados_tabela <- renderTable(selecionados(), digits = 2)
  output$serie <- renderPlot({
    d <- selecionados()
    plot(d$ano, d$chuva, type = "b", pch = 19, col = cor_a,
         xlab = "Ano", ylab = "Maximo do mes (mm)", main = input$rotulo)
  })
  output$parametros <- renderPrint({
    a <- ajuste()
    print(summary(a$fit))
    if (length(a$avisos)) cat("\nAvisos do ajuste:\n", paste(a$avisos, collapse = "\n"))
  })
  output$densidade <- renderPlot({
    a <- ajuste()
    hist(a$dados$chuva, probability = TRUE, breaks = "FD", col = "grey85", border = "white",
         main = paste(a$modelo, "ajustada"), xlab = "Maximo do mes (mm)", ylab = "Densidade")
    xx <- seq(min(a$dados$chuva), max(a$dados$chuva), length.out = 400)
    lines(xx, extRemes::devd(xx, loc = a$mu, scale = a$sigma, shape = a$xi,
                            type = "GEV"), col = cor_a, lwd = 2)
  })
  output$diagnosticos <- renderPlot({
    a <- ajuste(); x <- sort(a$dados$chuva); p <- seq_along(x) / (length(x) + 1)
    op <- par(mfrow = c(1, 2)); on.exit(par(op))
    plot(quantil(a, p), x, pch = 19, col = cor_a, main = "Q-Q",
         xlab = "Quantis teoricos (mm)", ylab = "Quantis observados (mm)"); abline(0, 1, lty = 2)
    plot(p, acumulada(a, x), pch = 19, col = cor_a, main = "P-P",
         xlab = "Probabilidade empirica", ylab = "Probabilidade ajustada",
         xlim = c(0, 1), ylim = c(0, 1)); abline(0, 1, lty = 2)
  })
  output$dependencia <- renderPlot({
    d <- selecionados()
    validate(need(all(diff(d$ano) == 1), "ACF omitida: existem lacunas anuais na janela."))
    acf(d$chuva, main = "ACF dos maximos (defasagens em anos)")
  })
  output$curva_retorno <- renderPlot({
    tb <- exigir(seguro(niveis(ajuste(), grade(), confianca_atual())))
    curva(tb, cor_a); abline(v = t_atual(), lty = 3)
  })
  output$retornos <- renderTable(tabela(), digits = 2)
  output$interpretacao <- renderPrint({
    a <- ajuste(); T <- t_atual(); z <- quantil(a, 1 - 1 / T)
    cat(sprintf("Nivel de retorno de %.2f anos: %.2f mm.\n", T, z))
    cat(sprintf("Probabilidade de excedencia em cada %s: %.2f%% por ano.\n", input$rotulo, 100 / T))
    if (T > diff(range(a$dados$ano)) + 1)
      cat("O periodo de retorno excede a extensao temporal observada: ha extrapolacao.\n")
    cat("Interpretacao condicionada ao modelo estacionario e ao bloco informado.")
  })
  output$risco <- renderPrint({
    req(input$critico, input$horizonte)
    validate(need(input$critico >= 0 && input$horizonte >= 1 &&
                    input$horizonte == floor(input$horizonte), "Informe valor critico nao negativo e horizonte inteiro positivo."))
    p <- 1 - acumulada(ajuste(), input$critico)
    risco <- -expm1(input$horizonte * log1p(-p))
    cat(sprintf("Probabilidade anual de superar %.2f mm nesse mes: %.2f%%.\n", input$critico, 100 * p))
    cat(sprintf("Probabilidade de ao menos uma superacao em %d anos: %.2f%%.\n", as.integer(input$horizonte), 100 * risco))
    cat("Calculo sob independencia entre anos e estacionariedade.")
  })
  limites_perfil <- reactive({
    req(input$perfil_low, input$perfil_up)
    validate(need(is.finite(input$perfil_low) && is.finite(input$perfil_up) &&
                    input$perfil_low < input$perfil_up,
                  "O limite inferior deve ser menor que o superior."))
    c(input$perfil_low, input$perfil_up)
  })
  chave <- reactive(list(dados = selecionados(), modelo = input$modelo,
                         T = t_atual(), conf = confianca_atual(),
                         xrange = limites_perfil()))
  perfil <- eventReactive(input$perfil, {
    k <- chave()
    resultado <- withProgress(message = "Calculando perfil de verossimilhanca...", value = 0.5, {
      seguro(niveis(ajuste(), t_atual(), confianca_atual(), "proflik",
                    xrange = limites_perfil()))
    })
    list(chave = k, resultado = resultado)
  }, ignoreInit = TRUE)
  output$resultado_perfil <- renderPrint({
    req(perfil())
    validate(need(identical(perfil()$chave, chave()), "Configuracao alterada. Clique novamente para atualizar o perfil."))
    r <- perfil()$resultado
    if (!is.null(r$erro)) cat("Perfil indisponivel:", r$erro) else print(r, row.names = FALSE)
  })
  ajuste_ismev <- reactive({
    d <- selecionados()
    validate(need(nrow(d) >= 15, "Selecione pelo menos 15 maximos."))
    exigir(seguro({
      b <- ismev::gev.fit(d$chuva, show = FALSE)
      if (b$conv != 0 || any(!is.finite(b$mle)))
        stop("O ajuste ismev nao convergiu ou retornou parametros invalidos.")
      b
    }))
  })
  output$parametros_ismev <- renderTable({
    b <- ajuste_ismev()
    data.frame(parametro = c("Localizacao", "Escala", "Forma"),
               estimativa = as.numeric(b$mle), erro_padrao = as.numeric(b$se))
  }, digits = 4)
  output$diag_ismev <- renderPlot({
    b <- ajuste_ismev()
    op <- par(no.readonly = TRUE); on.exit(par(op))
    ismev::gev.diag(b)
  })
  pedido_ismev <- eventReactive(input$perfil_ismev, {
    list(chave = chave(), ajuste = ajuste_ismev())
  }, ignoreInit = TRUE)
  output$grafico_perfil_ismev <- renderPlot({
    req(pedido_ismev())
    pedido <- pedido_ismev()
    validate(need(identical(pedido$chave, chave()),
                  "Configuracao alterada. Clique novamente para atualizar o perfil ismev."))
    k <- pedido$chave
    op <- par(no.readonly = TRUE); on.exit(par(op))
    resultado <- seguro({
      ismev::gev.prof(pedido$ajuste, m = k$T,
                      xlow = k$xrange[1], xup = k$xrange[2],
                      conf = k$conf, nint = 100)
      list(ok = TRUE)
    })
    exigir(resultado)
  })
  output$comparacao <- renderPlot({
    aa <- exigir(seguro(niveis(ajuste(), grade(), confianca_atual())))
    bb <- exigir(seguro(niveis(ajuste_b(), grade(), confianca_atual())))
    plot(aa$T_anos, aa$nivel_mm, type = "n", log = "x",
         ylim = range(c(aa$inferior_mm, aa$superior_mm, bb$inferior_mm, bb$superior_mm)),
         xlab = "Periodo de retorno (anos)", ylab = "Maximo do mes (mm)")
    curva(aa, cor_a, TRUE); curva(bb, cor_b, TRUE)
    legend("topleft", c("Janela A", "Janela B"), col = c(cor_a, cor_b), lwd = 2, bty = "n")
  })
  output$comparacao_tabela <- renderTable({
    linha <- function(a, nome) {
      tb <- niveis(a, t_atual(), confianca_atual())
      cbind(janela = nome, inicio = min(a$dados$ano), fim = max(a$dados$ano),
            n = nrow(a$dados), tb)
    }
    exigir(seguro(rbind(linha(ajuste(), "A"), linha(ajuste_b(), "B"))))
  }, digits = 2)
  output$baixar <- downloadHandler(
    filename = function() "niveis_retorno.csv",
    content = function(file) {
      a <- ajuste()
      tb <- tabela()
      tb$bloco <- input$rotulo
      tb$origem <- if (input$fonte == "demo") "SIMULADO" else input$arquivo$name
      tb$aba <- if (input$fonte == "demo") "demonstracao" else input$folha
      tb$modelo <- a$modelo; tb$confianca <- confianca_atual()
      tb$inicio <- min(a$dados$ano); tb$fim <- max(a$dados$ano); tb$n <- nrow(a$dados)
      write.csv2(tb, file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )
}

shinyApp(ui, server)
