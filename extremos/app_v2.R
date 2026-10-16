# EXTREMOS EM VIÇOSA — versão 2 / Shiny / R
# Instale uma vez no console:
# install.packages(c("shiny", "bslib", "readxl", "extRemes", "distillery", "ismev", "tseries", "Kendall"))
# Abra este arquivo no RStudio e clique em Run App.
# Alternativa: shiny::runApp("caminho/para/extremos_shiny")
# Cada linha deve ser UM maximo de janeiro por ano, ja extraido.
# O aplicativo nao calcula maximos a partir de observacoes diarias.
# Versão 2: janeiro observado, diagnósticos prévios e GEV/Gumbel estacionárias.
# Fonte dos valores: EXTREMOS.xlsx, aba JAN, colunas ANO e PRECIP, fornecido pelo autor.
# A planilha não documenta estação, instrumento ou cobertura dos registros diários.
# A definição de máxima diária mensal e a unidade mm seguem a descrição do autor.
# Não excluir 1986 (184,8 mm) sem verificação na fonte original.

pacotes <- c("shiny", "bslib", "readxl", "extRemes", "distillery", "ismev", "tseries", "Kendall")
faltam <- pacotes[!vapply(pacotes, requireNamespace, logical(1), quietly = TRUE)]
if (length(faltam)) stop("Instale os pacotes: ", paste(faltam, collapse = ", "))
library(shiny)

options(shiny.maxRequestSize = 20 * 1024^2)
cor_a <- "#2889c7"
cor_b <- "#dc7633"

# Dados observados: uma máxima diária de janeiro por ano, segundo o autor.
# Valores preservados exatamente como a coluna PRECIP da aba JAN.
janeiro_vicosa <- data.frame(
  ANO = 1968:2022,
  PRECIP = c(
    90, 51.6, 26.5, 35, 66.4, 38.6, 35.8, 96.6,
    26.7, 50.2, 73.6, 86.6, 52.9, 35.3, 41.6, 40.7,
    33.7, 75, 184.8, 48, 29.8, 15.1, 15.4, 74.8,
    40.4, 38.6, 51.6, 33.4, 21.9, 92.2, 133.4, 20,
    53.9, 108.6, 51.8, 71.6, 133.4, 46.2, 90.5, 92.7,
    80.6, 49, 38.2, 56.9, 89.8, 30, 34.4, 35.6,
    80.6, 22.4, 37.8, 24.2, 126.1, 27.6, 84.04
  )
)
stopifnot(nrow(janeiro_vicosa) == 55L, !anyNA(janeiro_vicosa),
          all(diff(janeiro_vicosa$ANO) == 1))

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
                     alpha = 1 - confianca, method = metodo, xrange = xrange, nint = 100, verbose = FALSE),
      error = function(e) {
        dica <- if (metodo == "proflik") " Ajuste os limites de busca: a grade deve conter o pico e os dois cruzamentos do perfil." else ""
        stop(sprintf("IC para T = %g anos: %s%s", T, conditionMessage(e), dica), call. = FALSE)
      }
    )
    limites <- as.numeric(ic)
    if (length(limites) != 3L || any(!is.finite(limites)))
      stop(sprintf("IC para T = %g nao retornou tres numeros finitos.", T))
    if (metodo == "proflik" && !is.null(xrange)) {
      margem <- diff(xrange) / 200
      if (limites[1] <= xrange[1] + margem || limites[3] >= xrange[2] - margem)
        stop("O intervalo encostou no limite da grade. Amplie a faixa de busca e recalcule o perfil.")
    }
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
         xlab = "Período de retorno (anos; mesmo mês)",
         ylab = "Precipitacao (mm)")
  }
  polygon(c(tb$T_anos, rev(tb$T_anos)),
          c(tb$inferior_mm, rev(tb$superior_mm)),
          col = grDevices::adjustcolor(cor, alpha.f = 0.16), border = NA)
  lines(tb$T_anos, tb$nivel_mm, col = cor, lwd = 2)
}

# Estes testes descrevem evidências; não certificam i.i.d. nem decidem o modelo.
diagnosticar_pressupostos <- function(d, alpha = 0.05, lag = 5L) {
  x <- d$chuva
  regular <- all(diff(d$ano) == 1)
  executar <- function(fun, permitido = TRUE) {
    if (!permitido) return(list(erro = "Não calculado: lacunas anuais na janela."))
    avisos <- character()
    resultado <- tryCatch(withCallingHandlers(fun(), warning = function(w) {
      avisos <<- c(avisos, conditionMessage(w)); invokeRestart("muffleWarning")
    }), error = function(e) list(erro = conditionMessage(e)))
    if (is.null(resultado$erro)) resultado$avisos <- avisos
    resultado
  }
  k_adf <- trunc((length(x) - 1)^(1/3))
  testes <- list(
    adf = executar(function() tseries::adf.test(x, k = k_adf), regular),
    kpss = executar(function() tseries::kpss.test(x, null = "Level"), regular),
    mk = executar(function() {
      m <- Kendall::MannKendall(x)
      list(statistic = c(tau = as.numeric(m$tau)), p.value = as.numeric(m$sl))
    }),
    lb = executar(function() stats::Box.test(x, lag = lag, type = "Ljung-Box"), regular)
  )
  nomes <- c(paste0("ADF (", k_adf, " defasagens)"), "KPSS em nível",
             "Mann-Kendall (usual)", paste0("Ljung-Box (", lag, " defasagens)"))
  h0 <- c("A série possui raiz unitária.", "A série é estacionária em nível.",
          "Não há tendência monotônica.", "Autocorrelações até a defasagem escolhida são nulas.")
  interpretar <- list(
    c("Rejeita raiz unitária; não garante estacionariedade em nível.", "Não rejeita raiz unitária; resultado insuficiente para concluir estacionariedade."),
    c("Evidência contra estacionariedade em nível.", "Não detectou evidência contra estacionariedade em nível."),
    c("Detectou tendência monotônica.", "Não detectou tendência monotônica."),
    c("Detectou autocorrelação no conjunto de defasagens.", "Não detectou autocorrelação no conjunto de defasagens.")
  )
  decisao <- function(r) {
    if (!is.null(r$erro)) return(NA)
    pv <- as.numeric(r$p.value)
    if (length(pv) != 1L || !is.finite(pv)) return(NA)
    if (any(grepl("smaller than", r$avisos))) return(if (pv <= alpha) TRUE else NA)
    if (any(grepl("greater than", r$avisos))) return(if (pv >= alpha) FALSE else NA)
    pv < alpha
  }
  tabela <- do.call(rbind, lapply(seq_along(testes), function(i) {
    r <- testes[[i]]
    if (!is.null(r$erro)) return(data.frame(
      Teste = nomes[i], Hipotese_nula = h0[i], Estatistica = "—", Valor_p = "—",
      Interpretacao = r$erro, check.names = FALSE))
    pv <- as.numeric(r$p.value)
    if (length(pv) != 1L || !is.finite(pv)) stop("Um teste retornou valor-p inválido.")
    prefixo <- if (any(grepl("smaller than", r$avisos))) "< " else
      if (any(grepl("greater than", r$avisos))) "> " else ""
    rejeita <- decisao(r)
    texto <- if (is.na(rejeita)) "Valor-p limitado pela tabela: decisão indeterminada neste α." else
      interpretar[[i]][if (rejeita) 1 else 2]
    data.frame(Teste = nomes[i], Hipotese_nula = h0[i],
               Estatistica = formatC(as.numeric(r$statistic)[1], digits = 4, format = "f"),
               Valor_p = paste0(prefixo, format.pval(pv, digits = 4, eps = 0.0001)),
               Interpretacao = texto, check.names = FALSE)
  }))
  sinal <- function(id) decisao(testes[[id]])
  revisar <- any(vapply(c("kpss", "mk", "lb"), function(i) isTRUE(sinal(i)), logical(1)))
  if (any(vapply(testes, function(r) !is.null(r$erro), logical(1))) && regular) {
    sintese <- "Um ou mais testes não puderam ser calculados. A avaliação está incompleta; consulte as mensagens da tabela antes de interpretar o ajuste."
  } else if (!regular) {
    sintese <- "Há lacunas anuais: a avaliação temporal está incompleta. ADF, KPSS e Ljung-Box foram omitidos. Revise a cobertura dos dados antes do ajuste."
  } else if (revisar) {
    sintese <- "Há evidências que exigem revisar a abordagem estacionária e/ou a dependência. A inclusão de covariáveis pode tratar mudanças sistemáticas nos parâmetros, mas não resolve automaticamente dependência temporal."
  } else if (any(vapply(testes, function(r) is.na(decisao(r)), logical(1)))) {
    sintese <- "Os limites dos valores-p não permitem decidir todos os testes no nível escolhido. Consulte as hipóteses e os sinais < ou > na tabela; a avaliação é inconclusiva."
  } else {
    sintese <- "KPSS, Mann-Kendall e Ljung-Box não sinalizaram violações ao nível escolhido. Isso permite explorar a abordagem estacionária com cautela; não confirma i.i.d. nem substitui o exame dos gráficos. Confira também o resultado específico do ADF."
  }
  list(tabela = tabela, testes = testes, sintese = sintese)
}

ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "flatly"),
  tags$head(tags$style(HTML(".table-scroll {overflow-x:auto;} pre {white-space:pre-wrap; overflow-wrap:anywhere;} .nav-tabs {display:flex;flex-wrap:wrap;}"))),
  titlePanel("Extremos de precipitação em Viçosa"),
  sidebarLayout(
    sidebarPanel(width = 3,
      radioButtons("fonte", "Origem dos dados", c("Viçosa: janeiro (1968–2022)" = "janeiro",
                                                 "Minha planilha Excel" = "excel")),
      conditionalPanel("input.fonte == 'excel'",
        fileInput("arquivo", "Carregar EXTREMOS.xlsx", accept = c(".xlsx", ".xls")),
        uiOutput("folhas_ui"), uiOutput("colunas_ui"),
        conditionalPanel("input.ano_col == '__sequencia__'",
          numericInput("ano_inicio", "Ano da primeira linha", 1968, step = 1),
          checkboxInput("confirmar", "Confirmo uma linha por ano, sem anos ausentes e em ordem", FALSE)
        )
      ),
      conditionalPanel("input.fonte == 'excel'",
        textInput("rotulo", "Descrição do bloco importado", "Janeiro")
      ),
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
          p("Uma observação representa o máximo diário do mês escolhido em um ano. O exemplo incorporado corresponde a janeiro em Viçosa."),
          verbatimTextOutput("resumo"), plotOutput("serie"),
          div(class = "table-scroll", tableOutput("dados_tabela")),
          actionButton("ir_pressupostos", "Prosseguir para os pressupostos")
        ),
        tabPanel("Pressupostos", value = "pressupostos",
          h4("A série sustenta uma análise estacionária?"),
          p("Avalie os gráficos, as hipóteses e a qualidade dos registros. Os testes são exploratórios e se referem à janela A. Não há correção por testes múltiplos."),
          fluidRow(column(6,
            selectInput("alpha_testes", "Significância dos testes (α)",
                        c("1%" = "0.01", "5%" = "0.05", "10%" = "0.10"), "0.05")),
            column(6, uiOutput("lag_ui"))),
          div(class = "table-scroll", tableOutput("testes_tabela")),
          uiOutput("sintese_testes"),
          plotOutput("serie_pressupostos", height = "300px"),
          plotOutput("dependencia", height = "280px"),
          p("Lacunas anuais impedem ADF, KPSS e Ljung-Box nesta versão: retirar anos e tratar os restantes como consecutivos alteraria o significado das defasagens. Mann-Kendall usa a ordem dos anos válidos."),
          p("Mann-Kendall é apresentado na forma usual, que pressupõe independência sob a hipótese nula. Se houver dependência, seu valor-p precisa de reavaliação; não é aplicada correção automática."),
          p("O ADF do tseries inclui constante e tendência linear; rejeitar raiz unitária não exclui uma tendência determinística."),
          p("Referências dos procedimentos: tseries::adf.test(), tseries::kpss.test(null = 'Level'), Kendall::MannKendall() e stats::Box.test(type = 'Ljung-Box')."),
          actionButton("ir_ajuste", "Prosseguir para o ajuste exploratório")
        ),
        tabPanel("Ajuste", value = "ajuste",
          uiOutput("lembrete_ajuste"), verbatimTextOutput("parametros"), plotOutput("densidade"),
          p("O sinal da forma indica o comportamento da cauda estimada. Sua incerteza deve ser considerada; a estimativa isolada nao define conclusivamente a familia.")
        ),
        tabPanel("Diagnosticos", value = "diagnosticos",
          p("P-P, Q-Q, níveis de retorno e histograma com densidade ajustada, no painel conjunto do ismev. O pacote refaz o ajuste nos mesmos dados e na distribuição selecionada; os demais resultados usam extRemes."),
          plotOutput("diagnosticos", height = "650px"),
          p("Estes gráficos avaliam a distribuição marginal ajustada. Os intervalos nativos do painel ismev são de 95%; o seletor de confiança da barra lateral vale para as tabelas e curvas do extRemes."),
          p("Em telas estreitas, gire o telefone para visualizar o conjunto 2 × 2 com mais espaço.")
        ),
        tabPanel("Niveis de retorno", value = "retorno",
          plotOutput("curva_retorno"), tableOutput("retornos"),
          verbatimTextOutput("interpretacao"),
          numericInput("critico", "Valor critico de precipitacao (mm)", 100, min = 0),
          numericInput("horizonte", "Horizonte de interesse (anos)", 30, min = 1, max = 1000),
          verbatimTextOutput("risco"),
          numericInput("perfil_low", "Limite inferior da busca do perfil (mm)", 60),
          numericInput("perfil_up", "Limite superior da busca do perfil (mm)", 170),
          helpText("Esses valores delimitam a grade de precipitacao em que o perfil e avaliado. Ajuste-os para abranger o pico e os dois cruzamentos com a linha critica."),
          actionButton("perfil", "Calcular IC por perfil (extRemes)"),
          helpText("O perfil pode demorar. Recalcule se alterar dados, modelo, T ou confianca."),
          verbatimTextOutput("resultado_perfil")
        ),
        tabPanel("Conferencia ismev", value = "ismev",
          p("Estimativas do ajuste ismev para os mesmos dados e a mesma distribuição selecionada. Os quatro diagnósticos estão reunidos na aba Diagnósticos."),
          tableOutput("parametros_ismev"),
          p("O perfil abaixo usa T e os limites de busca informados na aba Niveis de retorno. Cada pacote otimiza seu proprio ajuste; pequenas diferencas numericas sao possiveis."),
          helpText("gev.prof() requer GEV. Para Gumbel, use o cálculo de perfil do extRemes."),
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
          p("Os testes da aba Pressupostos não comprovam i.i.d. Ausência de autocorrelação não garante independência e estacionariedade em média/variância não garante distribuição marginal constante. Não detectar tendência não exclui mudanças na dispersão ou na cauda."),
          p("Versão 2: exemplo observado de janeiro incorporado. Sem POT/GPD, covariáveis ou teste KS."),
          p("A planilha fornecida não documenta a estação, o instrumento ou a completude das observações diárias usadas para extrair cada máximo. Esses metadados devem acompanhar a aplicação científica."),
          p("Pacotes: shiny, bslib, readxl, extRemes, distillery, ismev, tseries e Kendall. codigo de ajuste: extRemes::fevd(), method = 'MLE'.")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  rotulo_atual <- reactive({
    if (input$fonte == "janeiro") "Janeiro" else {
      req(input$rotulo)
      input$rotulo
    }
  })
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
    if (input$fonte == "janeiro") return(janeiro_vicosa)
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
    if (input$fonte == "janeiro") return(data.frame(ano = b$ANO, chuva = b$PRECIP))
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
  observeEvent(input$ir_pressupostos, updateTabsetPanel(session, "aba", selected = "pressupostos"))
  observeEvent(input$ir_ajuste, updateTabsetPanel(session, "aba", selected = "ajuste"))
  output$lag_ui <- renderUI({
    n <- nrow(selecionados())
    max_lag <- max(1L, min(10L, floor(n / 5)))
    numericInput("lag_lb", "Defasagens do Ljung-Box (anos)",
                 value = min(5L, max_lag), min = 1L, max = max_lag, step = 1)
  })
  avaliacao <- reactive({
    d <- selecionados()
    req(input$alpha_testes, input$lag_lb)
    alpha <- as.numeric(input$alpha_testes)
    validate(need(is.finite(alpha) && alpha > 0 && alpha < 1, "Selecione a significância."))
    n <- nrow(d)
    validate(need(n >= 15, "Selecione pelo menos 15 máximos para este painel de testes."),
             need(length(unique(d$chuva)) > 1, "Os testes não se aplicam a uma série constante."))
    lag <- as.numeric(input$lag_lb)
    validate(need(length(lag) == 1L && is.finite(lag) && lag == floor(lag) &&
                    lag >= 1 && lag <= min(10L, floor(n / 5)),
                  "Ajuste a defasagem para o tamanho da janela."))
    diagnosticar_pressupostos(d, alpha, lag)
  })
  output$testes_tabela <- renderTable(avaliacao()$tabela, striped = TRUE,
                                     bordered = TRUE, spacing = "s")
  output$sintese_testes <- renderUI({
    a <- avaliacao()
    div(class = "alert alert-info", tags$strong("Síntese dos diagnósticos"),
        p(a$sintese),
        p("Os resultados não comprovam que as observações sejam independentes e identicamente distribuídas. Considere também mudanças na dispersão, na cauda e no processo de medição."))
  })
  output$lembrete_ajuste <- renderUI({
    div(class = "alert alert-info",
        "Esta etapa ajusta modelos estacionários. Consulte a aba Pressupostos antes de interpretar níveis de retorno. Tendência, dependência ou mudanças no processo exigem revisão da especificação; esta versão não ajusta covariáveis.")
  })
  output$serie_pressupostos <- renderPlot({
    d <- selecionados()
    plot(d$ano, d$chuva, type = "b", pch = 19, col = cor_a,
         xlab = "Ano", ylab = "Máximo do mês (mm)",
         main = paste("Evolução da série:", rotulo_atual()))
    lines(stats::lowess(d$ano, d$chuva), col = cor_b, lwd = 2)
    legend("topright", c("Máximos observados", "Suavização exploratória"),
           col = c(cor_a, cor_b), lty = 1, bty = "n", cex = 0.8)
  })
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
    if (input$fonte == "janeiro") {
      div(class = "alert alert-info",
          "Dados observados de Viçosa: máximas diárias de janeiro por ano, de 1968 a 2022 (mm). Fonte: planilha fornecida pelo autor, aba JAN. O exemplo já está carregado.")
    } else {
      req(input$arquivo, input$folha)
      div(class = "alert alert-info", paste("Arquivo:", input$arquivo$name,
          "| Aba:", input$folha, "| Bloco:", rotulo_atual()))
    }
  })
  output$resumo <- renderPrint({
    d <- selecionados(); b <- dados()
    cat("Bloco:", rotulo_atual(), "\nJanela selecionada:", paste(input$janela, collapse = " a "),
        "\nMaximos validos:", nrow(d), "\nAnos com precipitacao ausente na planilha:", sum(is.na(b$chuva)),
        "\nAnos sem linha na planilha:", length(setdiff(seq(min(b$ano), max(b$ano)), b$ano)), "\n\n")
    print(summary(d$chuva))
  })
  output$dados_tabela <- renderTable(selecionados(), digits = 2)
  output$serie <- renderPlot({
    d <- selecionados()
    plot(d$ano, d$chuva, type = "b", pch = 19, col = cor_a,
         xlab = "Ano", ylab = "Maximo do mes (mm)", main = rotulo_atual())
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
    b <- ajuste_ismev()
    op <- par(no.readonly = TRUE); on.exit(par(op))
    if (input$modelo == "GEV") ismev::gev.diag(b) else ismev::gum.diag(b)
  }, res = 100)
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
    cat(sprintf("Probabilidade de excedencia em cada %s: %.2f%% por ano.\n", rotulo_atual(), 100 / T))
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
    validate(need(nrow(d) >= 15, "Selecione pelo menos 15 maximos."),
             need(length(unique(d$chuva)) >= 5, "Poucos valores distintos para ajustar o modelo."))
    exigir(seguro({
      b <- if (input$modelo == "GEV") ismev::gev.fit(d$chuva, show = FALSE)
           else ismev::gum.fit(d$chuva, show = FALSE)
      if (b$conv != 0 || any(!is.finite(b$mle)))
        stop("O ajuste ismev nao convergiu ou retornou parametros invalidos.")
      b
    }))
  })
  output$parametros_ismev <- renderTable({
    b <- ajuste_ismev()
    data.frame(parametro = if (input$modelo == "GEV") c("Localização", "Escala", "Forma") else c("Localização", "Escala"),
               estimativa = as.numeric(b$mle), erro_padrao = as.numeric(b$se))
  }, digits = 4)
  pedido_ismev <- eventReactive(input$perfil_ismev, {
    validate(need(input$modelo == "GEV", "Selecione GEV para usar gev.prof(). Para Gumbel, use o perfil do extRemes."))
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
      tb$bloco <- rotulo_atual()
      tb$origem <- if (input$fonte == "janeiro") "EXTREMOS.xlsx (valores incorporados)" else input$arquivo$name
      tb$aba <- if (input$fonte == "janeiro") "JAN" else input$folha
      tb$modelo <- a$modelo; tb$confianca <- confianca_atual()
      tb$inicio <- min(a$dados$ano); tb$fim <- max(a$dados$ano); tb$n <- nrow(a$dados)
      write.csv2(tb, file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )
}

shinyApp(ui, server)
