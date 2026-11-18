# Execute este arquivo com o diretório de trabalho na pasta do projeto.
# No RStudio: abra o .Rproj incluído e execute source("Preparar_e_Renderizar.R").
pacotes <- c("rmarkdown", "knitr", "ggplot2", "dplyr", "tidyr", "readr", "DT")
faltantes <- pacotes[!vapply(pacotes, requireNamespace, logical(1), quietly = TRUE)]
if (length(faltantes)) install.packages(faltantes, repos = "https://cloud.r-project.org")
if (!rmarkdown::pandoc_available()) {
  stop("Pandoc não foi localizado. Abra este projeto no RStudio e execute novamente.")
}
rmarkdown::render("Analise_Criterios_Sobrevivencia.Rmd", output_file = "index.html",
                  envir = new.env(parent = globalenv()), encoding = "UTF-8")
message("Concluído: index.html. Abra para revisar e publique para obter a URL do QR code.")
