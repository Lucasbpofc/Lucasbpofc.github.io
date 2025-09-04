
# Carregue o pacote R/exams
library(exams)

# Defina uma semente para garantir que o resultado seja reprodutível
set.seed(42)

# Crie uma lista com o(s) arquivo(s) do seu exercício
# (Certifique-se de que este script está na mesma pasta que o .Rmd)
meus_exercicios <- list("TCLEXERCICIOS.Rmd")

# --- Gere as saídas ---

# 1. Gerar um PDF com 3 versões diferentes da mesma questão
exams2pdf(meus_exercicios, n = 3, name = "prova_tcl")

# 2. Gerar um arquivo para importar no Moodle
exams2moodle(meus_exercicios, name = "quiz_tcl_moodle")

# 3. Gerar um arquivo HTML para visualização rápida
exams2html(meus_exercicios, name = "preview_tcl")
