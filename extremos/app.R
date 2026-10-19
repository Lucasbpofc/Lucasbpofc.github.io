# EXTREMOS DE PRECIPITAÇÃO — versão 8 / Shiny / R
# Instale uma vez no console:
# install.packages(c("shiny", "bslib", "readxl", "extRemes", "distillery", "tseries", "Kendall", "ggplot2", "patchwork"))
# Abra este arquivo no RStudio e clique em Run App.
# Alternativa: shiny::runApp("caminho/para/extremos_shiny")
# Cada linha deve ser UM máximo por ano, já extraído (ano inteiro ou mês específico).
# O aplicativo nao calcula maximos a partir de observacoes diarias.
# Versão 8: três testes de pressupostos e textos voltados ao público não especializado.
# Fonte dos valores: EXTREMOS.xlsx, aba JAN, colunas ANO e PRECIP, fornecido pelo autor.
# A planilha não documenta estação, instrumento ou cobertura dos registros diários.
# A definição de máxima diária mensal e a unidade mm seguem a descrição do autor.
# Não excluir 1986 (184,8 mm) sem verificação na fonte original.

pacotes <- c("shiny", "bslib", "readxl", "extRemes", "distillery", "tseries", "Kendall", "ggplot2", "patchwork")
faltam <- pacotes[!vapply(pacotes, requireNamespace, logical(1), quietly = TRUE)]
if (length(faltam)) stop("Instale os pacotes: ", paste(faltam, collapse = ", "))
library(shiny)

# Atributos HTML dos controles precisam de ponto decimal, inclusive em pt_BR.
# Vírgulas nos resultados são definidas explicitamente por rotulos_num/formatC.
options(OutDec = ".")

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

# Série fornecida pelo autor na conversa, em ordem cronológica de 1961 a 2025.
# Máximas diárias anuais de precipitação em Teófilo Otoni, em mm.
anual_teofilo <- data.frame(
  ANO = 1961:2025,
  PRECIP = c(
    63.9, 70.2, 75.8, 108.1, 86.4, 68.9, 46.2, 54.4, 62.5, 82.9, 66.8, 61.4,
    62.2, 73.1, 150.3, 69.0, 59.8, 54.1, 83.0, 58.6, 102.7, 59.9, 92.8, 70.9,
    79.2, 68.1, 95.4, 47.6, 62.0, 155.9, 66.0, 89.4, 72.6, 68.6, 60.9, 78.0,
    68.7, 57.4, 56.0, 89.6, 80.4, 246.4, 64.0, 120.7, 91.0, 102.1, 49.2, 96.5,
    104.4, 72.0, 69.8, 145.0, 82.6, 82.2, 75.8, 137.6, 70.2, 58.0, 64.2, 93.6,
    75.6, 96.4, 44.8, 91.4, 67.2
  )
)
stopifnot(nrow(anual_teofilo) == 65L, !anyNA(anual_teofilo),
          all(diff(anual_teofilo$ANO) == 1))

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
  validate(need(is.null(x[["erro"]]), x[["erro"]]))
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
         xlab = "Período de retorno (anos; 1 bloco/ano)",
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
    if (is.null(resultado[["erro"]])) resultado$avisos <- avisos
    resultado
  }
  k_adf <- trunc((length(x) - 1)^(1/3))
  testes <- list(
    adf = executar(function() tseries::adf.test(x, k = k_adf), regular),
    mk = executar(function() {
      m <- Kendall::MannKendall(x)
      list(statistic = c(tau = as.numeric(m$tau)), p.value = as.numeric(m$sl))
    }),
    lb = executar(function() stats::Box.test(x, lag = lag, type = "Ljung-Box"), regular)
  )
  nomes <- c(paste0("Dickey–Fuller aumentado (", k_adf, " defasagens)"),
             "Mann-Kendall (usual)", paste0("Ljung-Box (", lag, " defasagens)"))
  h0 <- c("A série possui raiz unitária (uma forma de não estacionariedade).",
          "Não há tendência monotônica.", "Autocorrelações até a defasagem escolhida são nulas.")
  interpretar <- list(
    c("Rejeita raiz unitária; não garante estacionariedade em nível.", "Não rejeita raiz unitária; resultado insuficiente para concluir estacionariedade."),
    c("Detectou tendência monotônica.", "Não detectou tendência monotônica."),
    c("Detectou autocorrelação no conjunto de defasagens.", "Não detectou autocorrelação no conjunto de defasagens.")
  )
  decisao <- function(r) {
    if (!is.null(r[["erro"]])) return(NA)
    pv <- as.numeric(r$p.value)
    if (length(pv) != 1L || !is.finite(pv)) return(NA)
    if (any(grepl("smaller than", r$avisos))) return(if (pv <= alpha) TRUE else NA)
    if (any(grepl("greater than", r$avisos))) return(if (pv >= alpha) FALSE else NA)
    pv < alpha
  }
  tabela <- do.call(rbind, lapply(seq_along(testes), function(i) {
    r <- testes[[i]]
    if (!is.null(r[["erro"]])) return(data.frame(
      Teste = nomes[i], Hipotese_nula = h0[i], Estatistica = "—", Valor_p = "—",
      Interpretacao = r[["erro"]], check.names = FALSE))
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
  revisar <- any(vapply(c("mk", "lb"), function(i) isTRUE(sinal(i)), logical(1)))
  if (any(vapply(testes, function(r) !is.null(r[["erro"]]), logical(1))) && regular) {
    sintese <- "Um ou mais testes não puderam ser calculados. A avaliação está incompleta; consulte as mensagens da tabela antes de interpretar o ajuste."
  } else if (!regular) {
    sintese <- "Há lacunas anuais: a avaliação temporal está incompleta. ADF e Ljung-Box foram omitidos. Revise a cobertura dos dados antes do ajuste."
  } else if (revisar) {
    sintese <- "Há evidências que exigem revisar a abordagem estacionária e/ou a dependência. A inclusão de covariáveis pode tratar mudanças sistemáticas nos parâmetros, mas não resolve automaticamente dependência temporal."
  } else if (any(vapply(testes, function(r) is.na(decisao(r)), logical(1)))) {
    sintese <- "Os limites dos valores-p não permitem decidir todos os testes no nível escolhido. Consulte as hipóteses e os sinais < ou > na tabela; a avaliação é inconclusiva."
  } else {
    sintese <- if (isTRUE(sinal("adf")))
      "O ADF rejeitou a hipótese de raiz unitária. Mann–Kendall e Ljung–Box não detectaram tendência monotônica ou autocorrelação nas defasagens avaliadas. É possível explorar o ajuste estacionário com cautela, examinando também os gráficos; esses resultados não comprovam i.i.d."
    else
      "O ADF não rejeitou a hipótese de raiz unitária. Mesmo sem tendência ou autocorrelação detectadas pelos outros testes, a avaliação da estacionariedade permanece inconclusiva. Examine os gráficos e a qualidade dos registros antes de interpretar o ajuste."
  }
  list(tabela = tabela, testes = testes, sintese = sintese)
}

# Visual padronizado: azul, curva vermelha e eixos com vírgula decimal.
rotulos_num <- function(x) format(x, trim = TRUE, scientific = FALSE,
                                  decimal.mark = ",", big.mark = ".")
tema_extremos <- function() {
  ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = 12),
                   panel.grid.minor = ggplot2::element_blank(),
                   legend.position = "bottom", legend.title = ggplot2::element_text(size = 10))
}
eixos_num <- function() list(ggplot2::scale_x_continuous(labels = rotulos_num),
                             ggplot2::scale_y_continuous(labels = rotulos_num))
plot_mensagem <- function(titulo, texto) {
  ggplot2::ggplot() + ggplot2::annotate("text", x = 0, y = 0, label = texto,
                                        size = 3.5) + ggplot2::labs(title = titulo) + ggplot2::theme_void()
}
histograma_extremos <- function(x, bins = 12L, a = NULL) {
  d <- data.frame(chuva = x)
  p <- ggplot2::ggplot(d, ggplot2::aes(x = chuva)) +
    ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(density),
                                         fill = ggplot2::after_stat(count)),
                            bins = bins, color = "#26354a", linewidth = 0.35) +
    ggplot2::scale_fill_gradient(low = "lightblue", high = "#154b8a", name = "Frequência") +
    ggplot2::geom_point(ggplot2::aes(y = 0), shape = 16, size = 1.4, alpha = 0.65) +
    ggplot2::labs(title = if (is.null(a)) "Histograma da precipitação" else paste("Histograma e curva", a$modelo),
                  x = "Precipitação (mm)", y = "Densidade") + eixos_num() + tema_extremos()
  if (!is.null(a)) {
    faixa <- range(c(x, quantil(a, c(0.01, 0.995))))
    xx <- seq(max(0, faixa[1]), faixa[2], length.out = 500)
    yy <- extRemes::devd(xx, loc = a$mu, scale = a$sigma, shape = a$xi, type = "GEV")
    curva_d <- data.frame(x = xx, y = yy)
    curva_d <- curva_d[is.finite(curva_d$y), ]
    p <- p + ggplot2::geom_line(data = curva_d, ggplot2::aes(x, y), inherit.aes = FALSE,
                                color = "#c93636", linewidth = 1)
  }
  p
}
plot_correlacao <- function(x, parcial = FALSE) {
  h <- min(20L, max(1L, floor(length(x) / 3)))
  a <- if (parcial) stats::pacf(x, lag.max = h, plot = FALSE) else stats::acf(x, lag.max = h, plot = FALSE)
  d <- data.frame(lag = as.numeric(a$lag), valor = as.numeric(a$acf))
  limite <- stats::qnorm(0.975) / sqrt(length(x))
  ggplot2::ggplot(d, ggplot2::aes(lag, valor)) +
    ggplot2::geom_hline(yintercept = 0, color = "grey40") +
    ggplot2::geom_hline(yintercept = c(-limite, limite), linetype = 2, color = "#c93636") +
    ggplot2::geom_segment(ggplot2::aes(xend = lag, yend = 0), color = "#19558e", linewidth = 0.7) +
    ggplot2::labs(title = if (parcial) "PACF" else "ACF", x = "Defasagem (anos)",
                  y = if (parcial) "Autocorrelação parcial" else "Autocorrelação") +
    eixos_num() + tema_extremos()
}
painel_descritivo <- function(d, temporal = TRUE, anos_reais = TRUE, bins = 12L, ncol = 2L) {
  p1 <- histograma_extremos(d$chuva, bins)
  top <- d[order(-d$chuva), ][seq_len(min(10L, nrow(d))), ]
  top$rotulo <- if (anos_reais) as.character(top$ano) else paste0("Obs. ", top$ano)
  top$rotulo <- factor(top$rotulo, levels = top$rotulo)
  p2 <- ggplot2::ggplot(top, ggplot2::aes(rotulo, chuva, fill = chuva)) +
    ggplot2::geom_col(color = "#26354a", linewidth = 0.35) +
    ggplot2::scale_fill_gradient(low = "lightblue", high = "#154b8a", name = "mm") +
    ggplot2::scale_y_continuous(labels = rotulos_num) +
    ggplot2::labs(title = "Dez maiores máximos", x = if (anos_reais) "Ano" else "Posição no vetor",
                  y = "Precipitação (mm)") + tema_extremos() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  regular <- temporal && all(diff(d$ano) == 1)
  p3 <- if (regular) plot_correlacao(d$chuva) else plot_mensagem("ACF", "Confirme a ordem temporal\ne verifique lacunas anuais.")
  p4 <- if (regular) plot_correlacao(d$chuva, TRUE) else plot_mensagem("PACF", "Confirme a ordem temporal\ne verifique lacunas anuais.")
  patchwork::wrap_plots(list(p1, p2, p3, p4), ncol = ncol, widths = rep(1, ncol))
}
painel_ajuste <- function(a, conf = 0.95, bins = 12L, ncol = 2L) {
  x <- sort(a$dados$chuva); n <- length(x); pp <- seq_len(n) / (n + 1)
  d <- data.frame(empirica = pp, ajustada = as.numeric(acumulada(a, x)),
                  teorico = as.numeric(quantil(a, pp)), observado = x)
  p1 <- ggplot2::ggplot(d, ggplot2::aes(empirica, ajustada)) +
    ggplot2::geom_abline(intercept = 0, slope = 1, color = "#c93636", linewidth = 0.8) +
    ggplot2::geom_point(shape = 21, fill = "lightblue", size = 2, color = "#173d69") +
    ggplot2::labs(title = "P-P plot", x = "Probabilidade empírica", y = "Probabilidade ajustada") +
    ggplot2::coord_equal(xlim = c(0,1), ylim = c(0,1)) + eixos_num() + tema_extremos()
  p2 <- ggplot2::ggplot(d, ggplot2::aes(teorico, observado)) +
    ggplot2::geom_abline(intercept = 0, slope = 1, color = "#c93636", linewidth = 0.8) +
    ggplot2::geom_point(shape = 21, fill = "lightblue", size = 2, color = "#173d69") +
    ggplot2::labs(title = "Q-Q plot", x = "Quantis teóricos (mm)", y = "Quantis observados (mm)") +
    eixos_num() + tema_extremos()
  p3 <- histograma_extremos(x, bins, a)
  t <- exp(seq(log(1.1), log(max(100, 2*n)), length.out = 70))
  tb <- niveis(a, t, conf)
  emp <- data.frame(T = 1 / (1 - pp), nivel = x)
  p4 <- ggplot2::ggplot(tb, ggplot2::aes(T_anos, nivel_mm)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = inferior_mm, ymax = superior_mm), fill = "lightblue", alpha = 0.5) +
    ggplot2::geom_line(color = "#c93636", linewidth = 1) +
    ggplot2::geom_point(data = emp, ggplot2::aes(T, nivel), inherit.aes = FALSE,
                        shape = 21, fill = "white", size = 1.7) +
    ggplot2::scale_x_log10(labels = rotulos_num) + ggplot2::scale_y_continuous(labels = rotulos_num) +
    ggplot2::labs(title = "Níveis de retorno", x = "Período de retorno (anos; 1 bloco/ano)",
                  y = "Precipitação (mm)", subtitle = paste0("IC pontual de ", 100*conf, "% pelo método delta")) + tema_extremos()
  patchwork::wrap_plots(list(p1,p2,p3,p4), ncol = ncol, widths = rep(1, ncol))
}

# Os quatro painéis usam a série selecionada. A linha linear é apenas descritiva.
painel_pressupostos <- function(d, ncol = 2L) {
  base <- ggplot2::ggplot(d, ggplot2::aes(ano, chuva)) +
    ggplot2::geom_line(color = "#19558e") + ggplot2::geom_point(color = "#19558e", size = 1.3) +
    ggplot2::labs(x = "Ano", y = "Precipitação (mm)") + eixos_num() + tema_extremos()
  p1 <- base + ggplot2::labs(title = "Série observada")
  p2 <- base + ggplot2::geom_hline(yintercept = mean(d$chuva), color = "#c93636", linewidth = 0.8) +
    ggplot2::labs(title = "Média de referência", subtitle = "Linha horizontal: média")
  p3 <- if (all(diff(d$ano) == 1)) plot_correlacao(d$chuva) else plot_mensagem("ACF", "Há lacunas anuais.")
  linear <- stats::lm(chuva ~ ano, data = d)
  linha <- data.frame(ano = sort(unique(d$ano)))
  linha$chuva <- as.numeric(stats::predict(linear, newdata = linha))
  inclinacao <- unname(stats::coef(linear)["ano"])
  p4 <- base + ggplot2::geom_line(data = linha, color = "#c93636", linewidth = 0.9) +
    ggplot2::labs(title = "Tendência na série observada",
                  subtitle = paste0("Linha linear: ",formatC(inclinacao,format="f",digits=2,decimal.mark=",")," mm/ano"))
  patchwork::wrap_plots(list(p1,p2,p3,p4), ncol = ncol, widths = rep(1,ncol))
}

# Estatística KS calculada nos dois lados dos saltos da CDF empírica,
# incluindo amostras com empates. O valor-p usual de ks.test não é usado.
estatistica_ks <- function(x, a) {
  x <- sort(x); n <- length(x); fx <- as.numeric(acumulada(a, x))
  if (!n || any(!is.finite(fx))) stop("CDF inválida no cálculo do KS.")
  max(seq_len(n)/n - fx, fx - (seq_len(n)-1)/n)
}
ks_bootstrap <- function(a, B = 499L, seed = 2026L, passo = 0,
                         progresso = function(i, total) NULL) {
  if (length(B)!=1L || !is.finite(B) || B != floor(B) || B < 19L || B > 4999L)
    stop("Informe entre 19 e 4999 réplicas inteiras.")
  if (length(seed)!=1L || !is.finite(seed) || seed != floor(seed) || seed < 1 || seed > .Machine$integer.max)
    stop("Semente inválida.")
  if (length(passo)!=1L || !is.finite(passo) || passo < 0) stop("Resolução inválida.")
  x <- a$dados$chuva; n <- length(x)
  if (passo > 0 && any(abs(x/passo - round(x/passo)) > 1e-6))
    stop("A resolução escolhida não é compatível com todos os dados. Escolha uma resolução menor ou use contínuo.")
  existia <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (existia) estado <- get(".Random.seed", envir = .GlobalEnv)
  on.exit(if (existia) assign(".Random.seed", estado, envir = .GlobalEnv) else
    if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) rm(".Random.seed", envir = .GlobalEnv))
  set.seed(seed)
  observado <- estatistica_ks(x, a)
  ds <- rep(NA_real_, B); mensagens <- character()
  for (b in seq_len(B)) {
    sim <- extRemes::revd(n, loc = a$mu, scale = a$sigma, shape = a$xi, type = "GEV")
    if (passo > 0) sim <- round(sim / passo) * passo
    r <- tryCatch({
      ab <- ajustar(data.frame(ano = seq_len(n), chuva = sim), a$modelo)
      estatistica_ks(sim, ab)
    }, error = function(e) {mensagens <<- c(mensagens, conditionMessage(e)); NA_real_})
    ds[b] <- r
    if (b %% max(1L, floor(B/100)) == 0L || b == B) progresso(b, B)
  }
  validos <- sum(is.finite(ds)); falhas <- B - validos
  if (!validos) stop("Nenhum ajuste bootstrap foi válido. Revise o modelo e os dados.")
  excedem <- sum(ds >= observado, na.rm = TRUE)
  p <- (1 + excedem) / (validos + 1)
  list(D = observado, p = p, B = B, validos = validos, falhas = falhas,
       p_min = (1+excedem)/(B+1), p_max = (1+excedem+falhas)/(B+1),
       erro_mc = sqrt(p*(1-p)/(validos+1)), seed = seed, passo = passo,
       empates = n - length(unique(x)), mensagens = unique(mensagens), estatisticas = ds)
}

ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "flatly"),
  tags$head(tags$style(HTML(".table-scroll {overflow-x:auto;} pre {white-space:pre-wrap; overflow-wrap:anywhere;} .nav-tabs {display:flex;flex-wrap:wrap;}"))),
  titlePanel("Teoria dos Valores Extremos: da modelagem à aplicação"),
  p("Versão 8 • Material complementar ao pôster • Explore a GEV, analise dados reais e interprete níveis de retorno."),
  sidebarLayout(
    sidebarPanel(width = 3,
                 radioButtons("fonte", "Origem dos dados", c("Viçosa: janeiro (1968–2022)" = "janeiro",
                                                             "Teófilo Otoni: anuais (1961–2025)" = "teofilo",
                                                             "Importar meus dados" = "excel"), selected = "teofilo"),
                 conditionalPanel("input.fonte == 'excel'",
                                  fileInput("arquivo", "Carregar máximos por ano", accept = c(".xlsx", ".xls", ".csv")),
                                  helpText("Excel ou CSV com uma coluna de ano e uma de precipitação (mm), já resumida em uma máxima por ano. Registros diários brutos precisam ser agregados antes."),
                                  uiOutput("csv_opcoes"),
                                  uiOutput("folhas_ui"), uiOutput("colunas_ui"),
                                  conditionalPanel("input.ano_col == '__sequencia__'",
                                                   numericInput("ano_inicio", "Ano da primeira linha", 1968, step = 1),
                                                   checkboxInput("confirmar", "Confirmo uma linha por ano, sem anos ausentes e em ordem", FALSE)
                                  )
                 ),
                 conditionalPanel("input.fonte == 'excel'",
                                  selectInput("tipo_bloco", "Bloco dos máximos importados", c("Ano inteiro" = "anual", "Mês específico" = "mensal")),
                                  textInput("rotulo", "Descrição da série importada", "Minha série")
                 ),
                 helpText("Uma linha = um máximo diário por bloco, com um bloco por ano. Unidade: mm."),
                 selectInput("colunas_graficos", "Painéis de gráficos", c("Duas colunas" = "2", "Uma coluna (telefone)" = "1"), "2"),
                 sliderInput("bins", "Classes do histograma", min = 5, max = 20, value = 12, step = 1),
                 uiOutput("janela_ui"),
                 selectInput("modelo", "Distribuicao", c("GEV", "Gumbel")),
                 selectInput("conf", "Confianca dos intervalos",
                             choices = c("90%" = "90", "95%" = "95", "99%" = "99"),
                             selected = "95"),
                 hr(),
                 helpText("Ajustes estacionarios por maxima verossimilhanca. Os intervalos da curva usam o metodo delta."),
                 downloadButton("baixar", "Baixar niveis de retorno")
    ),
    mainPanel(width = 9,
              uiOutput("origem_ui"),
              tabsetPanel(id = "aba",
                          tabPanel("Roteiro e teoria", value = "roteiro",
                                   h4("Do pôster à aplicação"),
                                   p("Este aplicativo complementa o passo a passo do trabalho com dois exemplos observados de precipitação, a exploração teórica da GEV e a possibilidade de importar seus próprios dados."),
                                   tags$ol(tags$li("Explore os parâmetros na aba Explorar GEV."),
                                           tags$li("Consulte a origem, a unidade e o bloco em Dados."),
                                           tags$li("Examine estacionariedade, tendência e autocorrelação em Pressupostos."),
                                           tags$li("Ajuste GEV ou Gumbel por máxima verossimilhança e examine os quatro diagnósticos."),
                                           tags$li("Execute o KS com bootstrap; interprete níveis de retorno e probabilidades de excedência.")),
                                   h4("Qual método para quais dados?"),
                                   p("Máximos em blocos: selecionamos o maior valor de cada bloco de observações. Em Teófilo Otoni, usamos a maior precipitação diária de cada ano (1961–2025); em Viçosa, a maior precipitação diária de janeiro de cada ano (1968–2022). Ambos os exemplos possuem uma observação por ano, em milímetros, mas representam blocos diferentes."),
                                   p("Picos acima de limiar (POT): excessos são modelados pela Pareto Generalizada (GPD). Essa abordagem integra a fundamentação do trabalho; os ajustes executados aqui são de máximos em blocos."),
                                   h4("Estacionário ou com covariáveis?"),
                                   p("Em um modelo estacionário, localização, escala e forma são constantes. Mudanças sistemáticas podem motivar modelos com covariáveis, por exemplo μ(t) = β₀ + β₁t e log σ(t) = γ₀ + γ₁t. O aplicativo executa os ajustes estacionários; essa extensão é apresentada como orientação metodológica."),
                                   p("Os testes ajudam a investigar diferentes aspectos do comportamento da série. Outros procedimentos, como os testes de Pettitt e KPSS, podem complementar essa avaliação. Para facilitar o entendimento do público, nesta versão utilizaremos apenas os três testes apresentados: Dickey–Fuller aumentado, Mann–Kendall e Ljung–Box. Seus resultados devem ser interpretados em conjunto com os gráficos e com o conhecimento do problema estudado."),
                                   p("Os parâmetros são estimados pelo método da máxima verossimilhança, com a função extRemes::fevd(method = 'MLE'), no software R."),
                                   p("Exemplo do pôster: Teófilo Otoni, 1961–2025; GEV; confiança 95%; T = 10 anos. Fonte: BDMEP/INMET, conforme informado pelo autor. A identificação da estação e a cobertura diária devem acompanhar a análise."),
                                   h4("Três perguntas sobre a série"),
                                   tags$ul(tags$li("Dickey–Fuller aumentado (ADF): há evidência contra raiz unitária, uma forma de não estacionariedade? H₀: existe raiz unitária. H₁: não existe raiz unitária; a série é estacionária em torno da constante e da tendência consideradas pelo teste."),
                                           tags$li("Mann–Kendall: há uma direção de mudança ao longo do tempo? H₀: não há tendência monotônica. H₁: há tendência crescente ou decrescente."),
                                           tags$li("Ljung–Box: há autocorrelação entre os anos? H₀: as autocorrelações até a defasagem selecionada são nulas. H₁: pelo menos uma é diferente de zero.")),
                                   h4("Origem e importação dos dados"),
                                   p("Os dados dos exemplos foram obtidos no BDMEP, do INMET, conforme informado pelo autor. Para analisar outra localidade, importe Excel (.xlsx ou .xls) ou CSV com uma coluna de ano e uma de precipitação máxima em mm. Exemplo de nomes: ANO e PRECIP."),
                                   p("Cada linha deve conter um único máximo por ano: do ano inteiro ou de um mesmo mês, como janeiro, em todos os anos. Selecione as colunas e informe o bloco na importação. O aplicativo recebe máximos já extraídos; os registros diários brutos precisam ser agregados antes. Preserve anos ausentes e não substitua dados faltantes por zero."),
                                   actionButton("exemplo_poster", "Carregar o exemplo do pôster")
                          ),
                          tabPanel("Explorar GEV", value = "explorar",
                                   p("Exploração teórica: altere os parâmetros para observar a cauda e os níveis de retorno. Esses controles não alteram o ajuste dos dados reais."),
                                   fluidRow(column(4, numericInput("expl_mu", "Localização μ (mm)", 70, step = 5)),
                                            column(4, numericInput("expl_sigma", "Escala σ (mm)", 20, min = 0.1, step = 1)),
                                            column(4, sliderInput("expl_xi", "Forma ξ", min = -0.5, max = 0.5, value = 0.2, step = 0.05))),
                                   verbatimTextOutput("expl_familia"),
                                   plotOutput("expl_densidade", height = "330px"),
                                   plotOutput("expl_retorno", height = "330px"),
                                   tableOutput("expl_tabela"),
                                   p("São curvas com parâmetros fixados pelo usuário, sem intervalos de confiança. A GEV com ξ < 0 é a Weibull reversa, com limite superior finito. A classificação da cauda a partir de dados depende também da incerteza de ξ.")
                          ),
                          tabPanel("Dados", value = "dados",
                                   p("Selecione um exemplo ou importe seus máximos. Viçosa usa apenas janeiro; Teófilo Otoni usa o ano inteiro. Os períodos de retorno são anuais em ambos os casos, mas se referem a blocos diferentes."),
                                   verbatimTextOutput("resumo"), plotOutput("serie"),
                                   uiOutput("descritivo_ui"),
                                   tags$details(tags$summary("Consultar os valores"), div(class = "table-scroll", tableOutput("dados_tabela"))),
                                   actionButton("ir_pressupostos", "Prosseguir para os pressupostos")
                          ),
                          tabPanel("Pressupostos", value = "pressupostos",
                                   h4("A série sustenta uma análise estacionária?"),
                                   p("Avalie os gráficos, as hipóteses e a qualidade dos registros. Os testes são exploratórios e se referem à janela selecionada. Não há correção por testes múltiplos."),
                                   p("Use três perguntas como guia: há sinais de não estacionariedade (ADF), de tendência ao longo do tempo (Mann–Kendall) ou de autocorrelação entre anos (Ljung–Box)?"),
                                   fluidRow(column(6,
                                                   selectInput("alpha_testes", "Significância dos testes (α)",
                                                               c("1%" = "0.01", "5%" = "0.05", "10%" = "0.10"), "0.05")),
                                            column(6, uiOutput("lag_ui"))),
                                   div(class = "table-scroll", tableOutput("testes_tabela")),
                                   uiOutput("sintese_testes"),
                                   uiOutput("pressupostos_ui"),
                                   p("Os quatro gráficos usam os dados reais da janela selecionada, inclusive na importação. No quarto painel, a linha vermelha descreve a tendência linear estimada em mm por ano; uma linha inclinada, isoladamente, não comprova tendência estatisticamente significativa. Consulte o teste de Mann–Kendall na tabela."),
                                   p("Lacunas anuais impedem ADF e Ljung-Box nesta versão: retirar anos e tratar os restantes como consecutivos alteraria o significado das defasagens. Mann-Kendall usa a ordem dos anos válidos."),
                                   p("Mann-Kendall é apresentado na forma usual, que pressupõe independência sob a hipótese nula. Se houver dependência, seu valor-p precisa de reavaliação; não é aplicada correção automática."),
                                   p("O ADF do tseries inclui constante e tendência linear; rejeitar raiz unitária não exclui uma tendência determinística."),
                                   p("Referências dos procedimentos: tseries::adf.test(), Kendall::MannKendall() e stats::Box.test(type = 'Ljung-Box')."),
                                   actionButton("ir_ajuste", "Prosseguir para o ajuste exploratório")
                          ),
                          tabPanel("Ajuste", value = "ajuste",
                                   uiOutput("lembrete_ajuste"), verbatimTextOutput("parametros"), plotOutput("densidade"),
                                   p("O sinal da forma indica o comportamento da cauda estimada. Sua incerteza deve ser considerada; a estimativa isolada nao define conclusivamente a familia.")
                          ),
                          tabPanel("Diagnosticos", value = "diagnosticos",
                                   p("P-P, Q-Q, histograma com densidade e níveis de retorno, todos baseados no mesmo ajuste extRemes mostrado na aba Ajuste. Os intervalos seguem o nível de confiança selecionado."),
                                   uiOutput("diagnosticos_ui"),
                                   p("Nenhum limite fixo corta valores extremos. Os gráficos usam a faixa dos dados e do modelo. No telefone, escolha Uma coluna na barra lateral."),
                                   h4("Aderência por Kolmogorov–Smirnov"),
                                   p("H₀: a distribuição marginal pertence à família selecionada, com parâmetros desconhecidos. O bootstrap paramétrico simula e reajusta a mesma família em cada réplica. Não é utilizado o valor-p usual de ks.test() para parâmetros conhecidos."),
                                   fluidRow(column(4, selectInput("B_ks", "Réplicas bootstrap", c("199 (exploração)" = "199", "499" = "499", "999" = "999", "1999" = "1999"), "499")),
                                            column(4, numericInput("seed_ks", "Semente", 2026, min = 1, max = 2147483647, step = 1)),
                                            column(4, selectInput("passo_ks", "Resolução dos registros (mm)", c("Contínuo / não arredondar" = "0", "0,01 mm" = "0.01", "0,1 mm" = "0.1", "1 mm" = "1"), "0"))),
                                   selectInput("alpha_ks", "Significância do KS", c("1%" = "0.01", "5%" = "0.05", "10%" = "0.10"), "0.05"),
                                   p("Há valores repetidos nas séries. Se forem resultado de arredondamento, informe a resolução efetiva do instrumento. Com resolução positiva, as amostras simuladas são arredondadas antes do reajuste; os valores observados permanecem intactos."),
                                   actionButton("executar_ks", "Avaliar aderência por bootstrap"),
                                   helpText("O cálculo pode demorar. Ao alterar os dados, a distribuição ou as opções do bootstrap, execute novamente."),
                                   verbatimTextOutput("resultado_ks"),
                                   p("Não rejeitar H₀ não comprova um bom ajuste. O KS avalia a distribuição inteira e não substitui a inspeção da cauda e dos níveis de retorno. A calibração pressupõe blocos independentes e distribuição estacionária.")
                          ),
                          tabPanel("Niveis de retorno", value = "retorno",
                                   h4("Explore o período de retorno"),
                                   p("Mova o controle para destacar na curva o nível associado a T. O período de retorno não é o tamanho da série: o ajuste permanece o mesmo enquanto você move o controle."),
                                   selectInput("retorno_max", "Explorar períodos de retorno até", c("50 anos" = "50", "100 anos" = "100", "200 anos" = "200", "500 anos" = "500", "1000 anos" = "1000"), "100"),
                                   sliderInput("retorno", "Período de retorno T (anos)", min = 2, max = 100, value = 10, step = 1, sep = "",
                                               animate = animationOptions(interval = 1200, loop = FALSE)),
                                   helpText("Use o botão de reprodução do controle para percorrer os períodos automaticamente. O eixo horizontal da curva é logarítmico."),
                                   uiOutput("resumo_retorno"),
                                   plotOutput("curva_retorno", height = "430px"),
                                   p("A faixa representa intervalos pontuais pelo método delta; não é uma banda simultânea. Ao mover o controle, muda o quantil consultado, não os parâmetros ajustados."),
                                   h4("Tabela de níveis de retorno"),
                                   div(class = "table-scroll", tableOutput("retornos")),
                                   helpText("A tabela mantém os períodos de referência e inclui o período selecionado no controle."),
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
                          tabPanel("Como interpretar", value = "ajuda",
                                   
                                   h4("Quais chuvas estamos analisando?"),
                                   p("Usamos um valor por ano: a maior quantidade de chuva registrada em um único dia. Em Viçosa, consideramos somente janeiro. Em Teófilo Otoni, consideramos o ano inteiro."),
                                   h4("O que significa um nível de retorno?"),
                                   p("Imagine que o aplicativo estime 140 mm para um período de retorno de 30 anos. No longo prazo, isso corresponde, em média, a um ano com chuva com máxima diária acima desse valor a cada 30 anos."),
                                   p("Isso não é uma previsão de quando essa chuva acontecerá. O valor pode ser superado em anos seguidos ou passar mais de 30 anos sem ser superado. Os 140 mm são apenas um exemplo: consulte na tabela os valores estimados para os dados escolhidos."),
                                   p("Em Viçosa, essa interpretação vale para as chuvas de janeiro. Em Teófilo Otoni, vale para as chuvas do ano inteiro."),
                                   h4("Como usar o controle deslizante?"),
                                   p("Mova o controle para escolher o período de retorno. O gráfico destaca o nível de chuva correspondente, e a tabela apresenta os valores estimados. Mudar esse período não altera os dados nem refaz o ajuste: apenas consulta outro ponto da curva."),
                                   h4("Qual é a incerteza dessas estimativas?"),
                                   p("Os valores calculados são estimativas. Os intervalos apresentados ajudam a mostrar sua incerteza: quanto mais amplo o intervalo, menor a precisão do resultado."),
                                   
                                   p("Poucos anos de dados e períodos de retorno muito longos podem
    tornar as estimativas mais incertas. Por exemplo, estimar um
    nível de 100 anos com apenas 30 anos de registros exige projetar
    o comportamento da chuva além do período observado."),
                                   
                                   p("Recomenda-se pelo menos 50 observações de máximos, para se ter uma estimativa mais segura."),
                                   
                                   h4("O que os testes nos dizem?"),
                                   p("Os testes da aba Pressupostos ajudam a identificar tendência
    ao longo do tempo e relações entre as observações de diferentes
    anos. Eles também investigam se há sinais de que o comportamento
    da série muda ao longo do tempo."),
                                   
                                   p("Não encontrar um sinal de problema não garante que tudo esteja
    adequado. Por isso, observe também os gráficos e a qualidade
    dos registros antes de interpretar os resultados."),
                                   
                                   h4("Em quais condições os resultados fazem sentido?"),
                                   p("Esta versão considera que o comportamento das chuvas permanece
    estável ao longo do período analisado. Para calcular a chance de
    uma superação ao longo de vários anos, também considera que a
    ocorrência em um ano não altera a chance nos demais."),
                                   
                                   p("Se houver mudanças importantes ao longo do tempo, pode ser
    necessário utilizar um modelo que represente essas mudanças."),
                                   
                                   h4("Posso analisar meus próprios dados?"),
                                   p("Sim. Importe um arquivo Excel ou CSV com uma coluna de ano e
    outra com a maior chuva diária do período escolhido, em
    milímetros. Use uma linha por ano e mantenha o mesmo critério
    em toda a série: o ano inteiro ou um mesmo mês em todos os anos."),
                                   
                                   p("Os máximos precisam estar calculados antes da importação.
    Confira a estação de origem, a unidade de medida e se existem
    falhas nos registros. Não substitua dados ausentes por zero:
    um ano com registros incompletos pode esconder sua maior chuva."),
                                   
                                   tags$details(
                                     tags$summary("Informações "),
                                     p("O aplicativo permite explorar a GEV, ajustar GEV
      ou Gumbel aos dados, avaliar o ajuste e consultar níveis
      de retorno. Os ajustes realizados são estacionários."),
                                     p("Pacotes utilizados: shiny, bslib, readxl, extRemes, distillery,
      tseries, Kendall, ggplot2 e patchwork. Os parâmetros são
      estimados por máxima verossimilhança com extRemes::fevd(),
      usando method = 'MLE'.")
                                   ),
                                   hr(),
                                   
                                   h4("Conheça o autor"),
                                   p("Acompanhe meus projetos e entre em contato pelas redes:"),
                                   
                                   div(
                                     style = "display: flex; flex-wrap: wrap; gap: 10px;",
                                     
                                     tags$a(
                                       icon("instagram"), " Instagram",
                                       href = "https://www.instagram.com/lucasbello_01/",
                                       target = "_blank",
                                       rel = "noopener noreferrer",
                                       class = "btn",
                                       style = paste(
                                         "background-color: #C13584; color: white;",
                                         "border-radius: 8px; padding: 10px 16px;"
                                       )
                                     ),
                                     
                                     tags$a(
                                       icon("linkedin"), " LinkedIn",
                                       href = "https://www.linkedin.com/in/lucas-pereira-belo-3aa8a51a9/",
                                       target = "_blank",
                                       rel = "noopener noreferrer",
                                       class = "btn",
                                       style = paste(
                                         "background-color: #0A66C2; color: white;",
                                         "border-radius: 8px; padding: 10px 16px;"
                                       )
                                     ),
                                     
                                     tags$a(
                                       icon("github"), " GitHub",
                                       href = "https://github.com/Lucasbpofc",
                                       target = "_blank",
                                       rel = "noopener noreferrer",
                                       class = "btn",
                                       style = paste(
                                         "background-color: #24292F; color: white;",
                                         "border-radius: 8px; padding: 10px 16px;"
                                       )
                                     )
                                   )
                          )
              )
    )
  )
)

server <- function(input, output, session) {
  observeEvent(input$exemplo_poster, {
    updateRadioButtons(session, "fonte", selected = "teofilo")
    updateSelectInput(session, "modelo", selected = "GEV")
    updateSelectInput(session, "conf", selected = "95")
    updateSliderInput(session, "retorno", value = 10)
    updateSelectInput(session, "B_ks", selected = "999")
    updateSelectInput(session, "passo_ks", selected = "0.1")
    updateNumericInput(session, "seed_ks", value = 2026)
    updateSelectInput(session, "alpha_testes", selected = "0.05")
    updateSelectInput(session, "alpha_ks", selected = "0.05")
    updateNumericInput(session, "lag_lb", value = 5)
    updateSliderInput(session, "janela", value = c(1961,2025))
    updateTabsetPanel(session, "aba", selected = "dados")
  })
  explorado <- reactive({
    req(!is.null(input$expl_mu), !is.null(input$expl_sigma), !is.null(input$expl_xi))
    valores <- c(input$expl_mu,input$expl_sigma,input$expl_xi)
    validate(need(length(valores)==3L && all(is.finite(valores)) && input$expl_sigma > 0,
                  "Informe localização e forma finitas e escala maior que zero."))
    list(mu = input$expl_mu, sigma = input$expl_sigma, xi = input$expl_xi)
  })
  output$expl_familia <- renderPrint({
    a <- explorado()
    cat(if (abs(a$xi) < 1e-8) "Gumbel: ξ = 0." else if (a$xi > 0) "Fréchet: ξ > 0, cauda pesada." else "Weibull reversa: ξ < 0, limite superior finito.")
    if (abs(a$xi) >= 1e-8) cat(sprintf("\nLimite %s do suporte: %.2f mm.", if (a$xi > 0) "inferior" else "superior", a$mu - a$sigma/a$xi))
  })
  output$expl_tabela <- renderTable({
    a <- explorado(); T <- c(2,5,10,20,50,100)
    data.frame(T_anos = T, Prob_excedencia_anual = 1/T, Nivel_mm = quantil(a,1-1/T))
  }, digits = 3)
  # Saídas independentes: o explorador não depende da composição com patchwork.
  output$expl_densidade <- renderPlot({
    a <- explorado()
    faixa <- as.numeric(quantil(a,c(.001,.995)))
    validate(need(length(faixa)==2L && all(is.finite(faixa)) && diff(faixa)>0,
                  "Não foi possível calcular a faixa da densidade para esses parâmetros."))
    xx <- seq(faixa[1],faixa[2],length.out=500)
    d <- data.frame(x=xx,densidade=as.numeric(extRemes::devd(xx,loc=a$mu,scale=a$sigma,shape=a$xi,type="GEV")))
    validate(need(all(is.finite(d$densidade)),"A densidade não é finita para esses parâmetros."))
    p <- ggplot2::ggplot(d,ggplot2::aes(x,densidade)) +
      ggplot2::geom_line(color="#c93636",linewidth=1) +
      ggplot2::labs(title="Densidade GEV",x="Valor (mm)",y="Densidade") + eixos_num() + tema_extremos()
    print(p)
  },res=100)
  output$expl_retorno <- renderPlot({
    a <- explorado()
    T <- exp(seq(log(1.01),log(1000),length.out=300))
    d <- data.frame(T=T,z=as.numeric(quantil(a,1-1/T)))
    validate(need(all(is.finite(d$z)),"Não foi possível calcular os níveis para esses parâmetros."))
    p <- ggplot2::ggplot(d,ggplot2::aes(T,z)) +
      ggplot2::geom_line(color="#19558e",linewidth=1) +
      ggplot2::scale_x_log10(labels=rotulos_num) + ggplot2::scale_y_continuous(labels=rotulos_num) +
      ggplot2::labs(title="Níveis de retorno teóricos",x="T (anos; um bloco por ano)",y="Nível (mm)") + tema_extremos()
    print(p)
  },res=100)
  output$pressupostos_ui <- renderUI({ plotOutput("painel_pressupostos",height=if (colunas_painel()==1L) "1150px" else "650px") })
  output$painel_pressupostos <- renderPlot({print(painel_pressupostos(selecionados(),colunas_painel()))},res=100)
  
  rotulo_atual <- reactive({
    req(input$fonte)
    if (input$fonte == "janeiro") return("Janeiro em Viçosa")
    if (input$fonte == "teofilo") return("Máximas anuais de Teófilo Otoni")
    req(input$rotulo)
    input$rotulo
  })
  bloco_atual <- reactive({
    req(input$fonte)
    if (input$fonte == "janeiro") "Janeiro" else if (input$fonte == "teofilo") "Ano inteiro" else {
      req(input$tipo_bloco)
      if (input$tipo_bloco == "anual") "Ano inteiro" else "Mês específico"
    }
  })
  colunas_painel <- reactive({
    req(input$colunas_graficos)
    as.integer(input$colunas_graficos)
  })
  bins_atual <- reactive({
    req(input$bins)
    as.integer(input$bins)
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
  output$csv_opcoes <- renderUI({
    req(input$arquivo)
    if (tolower(tools::file_ext(input$arquivo$name)) != "csv") return(NULL)
    tagList(
      selectInput("sep_csv", "Separador do CSV", c("Ponto e vírgula" = ";", "Vírgula" = ",", "Tabulação" = "\t"), ";"),
      selectInput("dec_csv", "Separador decimal do CSV", c("Vírgula" = ",", "Ponto" = "."), ",")
    )
  })
  output$folhas_ui <- renderUI({
    req(input$arquivo)
    if (tolower(tools::file_ext(input$arquivo$name)) == "csv") return(helpText("Arquivo CSV selecionado."))
    folhas <- readxl::excel_sheets(input$arquivo$datapath)
    selectInput("folha", "Aba do Excel", folhas)
  })
  bruto <- reactive({
    req(input$fonte)
    if (input$fonte == "janeiro") return(janeiro_vicosa)
    if (input$fonte == "teofilo") return(anual_teofilo)
    req(input$arquivo)
    if (tolower(tools::file_ext(input$arquivo$name)) == "csv") {
      req(input$sep_csv, input$dec_csv)
      b <- utils::read.table(input$arquivo$datapath, header = TRUE,
                             sep = input$sep_csv, dec = input$dec_csv,
                             stringsAsFactors = FALSE, check.names = FALSE,
                             na.strings = c("", "NA"), comment.char = "")
      validate(need(ncol(b) >= 2, "O CSV precisa de ao menos duas colunas. Confira o separador."))
      b
    } else {
      req(input$folha)
      as.data.frame(readxl::read_excel(input$arquivo$datapath, sheet = input$folha))
    }
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
    if (input$fonte %in% c("janeiro", "teofilo")) return(data.frame(ano = b$ANO, chuva = b$PRECIP))
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
  output$janela_ui <- renderUI(seletor_janela("janela", "Janela dos dados (anos)"))
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
         xlab = "Ano", ylab = "Precipitação máxima diária (mm)",
         main = paste("Evolução da série:", rotulo_atual()))
    lines(stats::lowess(d$ano, d$chuva), col = cor_b, lwd = 2)
    legend("topright", c("Máximos observados", "Suavização exploratória"),
           col = c(cor_a, cor_b), lty = 1, bty = "n", cex = 0.8)
  })
  chave_ks <- reactive({
    req(input$B_ks, input$seed_ks, input$passo_ks)
    list(dados = selecionados(), modelo = input$modelo, fonte = input$fonte,
         B = as.integer(input$B_ks), seed = as.numeric(input$seed_ks), passo = as.numeric(input$passo_ks))
  })
  pedido_ks <- eventReactive(input$executar_ks, {
    k <- chave_ks(); a <- ajuste()
    resultado <- withProgress(message = "KS: simulando e reajustando o modelo...", value = 0, {
      anterior <- 0L
      seguro(ks_bootstrap(a, B = k$B, seed = k$seed, passo = k$passo,
                          progresso = function(i, total) {
                            incProgress((i - anterior)/total, detail = paste(i, "de", total))
                            anterior <<- i
                          }))
    })
    list(chave = k, resultado = resultado)
  }, ignoreInit = TRUE)
  output$resultado_ks <- renderPrint({
    req(pedido_ks(), input$alpha_ks)
    pedido <- pedido_ks()
    validate(need(identical(pedido$chave, chave_ks()), "Configuração alterada. Execute novamente o bootstrap."))
    r <- exigir(pedido$resultado); alpha <- as.numeric(input$alpha_ks)
    cat(sprintf("KS observado D = %.5f\n", r$D))
    cat(sprintf("Réplicas válidas: %d / %d; falhas de ajuste: %d\n", r$validos, r$B, r$falhas))
    cat(sprintf("Valor-p bootstrap = %.4f; erro de Monte Carlo aproximado = %.4f\n", r$p, r$erro_mc))
    if (r$falhas) cat(sprintf("O valor-p usa somente réplicas válidas. Limites tratando falhas como resultados desconhecidos: [%.4f; %.4f].\n", r$p_min, r$p_max))
    if (r$falhas/r$B > 0.05) {
      cat("Mais de 5% de falhas: resultado não recomendado para decisão. Revise o ajuste.\n")
    } else if (r$p_max < alpha) {
      cat("Evidência contra a adequação marginal da família selecionada no nível escolhido.\n")
    } else if (r$p_min >= alpha) {
      cat("Não foram encontradas evidências contra a adequação marginal no nível escolhido. Isso não comprova um bom ajuste.\n")
    } else cat("Decisão inconclusiva considerando as falhas ou a proximidade do nível de significância.\n")
    cat(sprintf("Semente: %d; resolução simulada: %g mm (0 = contínuo).\n", r$seed, r$passo))
    if (r$empates && r$passo == 0) cat("Há empates nos dados. O bootstrap atual assume modelo contínuo sem arredondamento. Confira a resolução dos registros.\n")
    if (r$p_min <= alpha + 2*r$erro_mc && r$p_max >= alpha - 2*r$erro_mc)
      cat("Resultado próximo de α considerando o erro de Monte Carlo: aumente o número de réplicas.\n")
    if (length(r$mensagens)) cat("Mensagens das falhas:", paste(r$mensagens, collapse = " | "), "\n")
  })
  ajuste <- reactive(exigir(seguro(ajustar(selecionados(), input$modelo))))
  limite_retorno <- reactive({
    req(input$retorno_max)
    v <- as.numeric(input$retorno_max)
    validate(need(length(v) == 1L && is.finite(v) && v %in% c(50,100,200,500,1000), "Selecione a extensão da curva."))
    v
  })
  observeEvent(input$retorno_max, {
    atual <- isolate(input$retorno)
    if (is.null(atual) || !is.finite(atual)) atual <- 10
    updateSliderInput(session, "retorno", min = 2, max = limite_retorno(), value = max(2,min(atual,limite_retorno())))
  })
  t_atual <- reactive({
    req(input$retorno)
    validate(need(length(input$retorno) == 1L && is.finite(input$retorno) && input$retorno >= 2 && input$retorno <= 1000,
                  "T deve estar entre 2 e 1000 anos."))
    input$retorno
  })
  # A curva e seus ICs são reutilizados enquanto apenas o controle é movido.
  grade <- reactive(exp(seq(log(2),log(limite_retorno()),length.out=100)))
  curva_tabela <- reactive(exigir(seguro(niveis(ajuste(),grade(),confianca_atual()))))
  nivel_selecionado <- reactive(exigir(seguro(niveis(ajuste(),t_atual(),confianca_atual()))))
  tabela_base <- reactive(exigir(seguro(niveis(ajuste(),c(2,5,10,20,50,100),confianca_atual()))))
  tabela <- reactive({
    tb <- tabela_base(); sel <- nivel_selecionado()
    if (!(t_atual() %in% tb$T_anos)) tb <- rbind(tb,sel)
    tb[order(tb$T_anos), , drop=FALSE]
  })
  output$origem_ui <- renderUI({
    req(input$fonte)
    texto <- if (input$fonte == "janeiro") {
      "Viçosa: 55 máximas diárias de janeiro, de 1968 a 2022 (mm). Fonte: BDMEP/INMET; valores fornecidos pelo autor em EXTREMOS.xlsx, aba JAN."
    } else if (input$fonte == "teofilo") {
      "Teófilo Otoni: 65 máximas diárias anuais, de 1961 a 2025 (mm). Fonte: BDMEP/INMET; vetor e período fornecidos pelo autor."
    } else {
      req(input$arquivo)
      paste("Arquivo:", input$arquivo$name, "| Série:", rotulo_atual(), "| Bloco:", bloco_atual())
    }
    div(class = "alert alert-info", texto)
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
         xlab = "Ano", ylab = "Precipitação máxima diária (mm)", main = rotulo_atual())
  })
  output$parametros <- renderPrint({
    a <- ajuste()
    print(summary(a$fit))
    if (length(a$avisos)) cat("\nAvisos do ajuste:\n", paste(a$avisos, collapse = "\n"))
  })
  output$densidade <- renderPlot({
    print(histograma_extremos(selecionados()$chuva, bins_atual(), ajuste()))
  }, res = 100)
  output$descritivo_ui <- renderUI({
    plotOutput("descritivo", height = if (colunas_painel() == 1L) "1350px" else "760px")
  })
  output$descritivo <- renderPlot({
    d <- selecionados()
    print(painel_descritivo(d, bins = bins_atual(), ncol = colunas_painel()))
  }, res = 100)
  output$diagnosticos_ui <- renderUI({
    plotOutput("diagnosticos", height = if (colunas_painel() == 1L) "1450px" else "800px")
  })
  output$diagnosticos <- renderPlot({
    print(painel_ajuste(ajuste(), conf = confianca_atual(), bins = bins_atual(), ncol = colunas_painel()))
  }, res = 100)
  output$dependencia <- renderPlot({
    d <- selecionados()
    validate(need(all(diff(d$ano) == 1), "ACF omitida: existem lacunas anuais na janela."))
    acf(d$chuva, main = "ACF dos maximos (defasagens em anos)")
  })
  output$resumo_retorno <- renderUI({
    sel <- nivel_selecionado(); T <- t_atual()
    fmt <- function(x) formatC(x,format="f",digits=2,decimal.mark=",")
    card <- function(titulo, valor) div(style="padding:12px;border:1px solid #d5e2ef;border-radius:8px;background:#f5f9fd;",
                                        tags$small(titulo),tags$br(),tags$strong(valor))
    div(style="display:grid;grid-template-columns:repeat(auto-fit,minmax(160px,1fr));gap:10px;margin:12px 0;",
        card("Período de retorno",paste(format(T,trim=TRUE),"anos")),
        card("Nível estimado",paste(fmt(sel$nivel_mm),"mm")),
        card(paste0("IC pontual de ",100*confianca_atual(),"%"),paste0("[",fmt(sel$inferior_mm),"; ",fmt(sel$superior_mm),"] mm")),
        card("Excedência por bloco anual",paste0(fmt(100/T),"%")))
  })
  output$curva_retorno <- renderPlot({
    tb <- curva_tabela(); sel <- nivel_selecionado(); T <- t_atual()
    p <- ggplot2::ggplot(tb,ggplot2::aes(T_anos,nivel_mm)) +
      ggplot2::geom_ribbon(ggplot2::aes(ymin=inferior_mm,ymax=superior_mm),fill="lightblue",alpha=.5) +
      ggplot2::geom_line(color=cor_a,linewidth=1) +
      ggplot2::geom_vline(xintercept=T,linetype=3,color="#c93636") +
      ggplot2::geom_hline(yintercept=sel$nivel_mm,linetype=3,color="#c93636") +
      ggplot2::geom_errorbar(data=sel,ggplot2::aes(ymin=inferior_mm,ymax=superior_mm),width=0,color="#c93636",linewidth=.8) +
      ggplot2::geom_point(data=sel,color="#c93636",size=3) +
      ggplot2::scale_x_log10(labels=rotulos_num) + ggplot2::scale_y_continuous(labels=rotulos_num) +
      ggplot2::labs(title="Nível de retorno por período",subtitle=paste("Destaque: T =",T,"anos •",input$modelo),
                    x="Período de retorno T (anos; escala logarítmica)",y="Precipitação máxima diária (mm)") + tema_extremos()
    print(p)
  },res=100)
  output$retornos <- renderTable(tabela(), digits = 2)
  output$interpretacao <- renderPrint({
    a <- ajuste(); T <- t_atual(); z <- quantil(a, 1 - 1 / T)
    cat(sprintf("Nivel de retorno de %.2f anos: %.2f mm.\n", T, z))
    cat(sprintf("Probabilidade de excedência por bloco (%s): %.2f%% por ano.\n", rotulo_atual(), 100 / T))
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
    cat(sprintf("Probabilidade de superar %.2f mm por bloco: %.2f%%.\n", input$critico, 100 * p))
    cat(sprintf("Probabilidade de ao menos uma superacao em %d anos: %.2f%%.\n", as.integer(input$horizonte), 100 * risco))
    cat("Cálculo sob independência entre anos e estacionariedade. Bloco:", bloco_atual())
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
    if (!is.null(r[["erro"]])) cat("Perfil indisponivel:", r[["erro"]]) else print(r, row.names = FALSE)
  })
  output$baixar <- downloadHandler(
    filename = function() "niveis_retorno.csv",
    content = function(file) {
      a <- ajuste()
      tb <- tabela()
      tb$bloco <- rotulo_atual()
      tb$origem <- if (input$fonte == "janeiro") "EXTREMOS.xlsx (valores incorporados)" else
        if (input$fonte == "teofilo") "Vetor fornecido pelo autor (1961-2025)" else input$arquivo$name
      tb$aba <- if (input$fonte == "janeiro") "JAN" else if (input$fonte == "teofilo") "Máximos anuais" else
        if (tolower(tools::file_ext(input$arquivo$name)) == "csv") "CSV" else input$folha
      tb$bloco <- bloco_atual()
      tb$modelo <- a$modelo; tb$confianca <- confianca_atual()
      tb$inicio <- min(a$dados$ano); tb$fim <- max(a$dados$ano); tb$n <- nrow(a$dados)
      write.csv2(tb, file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )
}

shinyApp(ui, server)
