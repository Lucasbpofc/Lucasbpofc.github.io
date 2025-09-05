# Carregue o pacote R/exams
library(exams)

# Defina uma semente para garantir que o resultado seja reprodutível
set.seed(42)

# Crie uma lista com os 5 arquivos dos seus exercícios
# (Certifique-se de que este script está na mesma pasta que os .Rmd)

meus_exercicios <- list("EXERCICIO1.Rmd",
                        "EXERCICIO2.Rmd",
                        "EXERCICIO3.Rmd",
                        "EXERCICIO4.Rmd",
                        "EXERCICIO5.Rmd")

# --- Gere as saídas ---

# 1. Gerar um PDF com 3 versões diferentes da prova
exams2pdf(meus_exercicios, n = 3, name = "prova_completa_tcl_delta1")

# 2. Gerar um arquivo para importar no Moodle
exams2moodle(meus_exercicios, name = "quiz_prova_completa_tcl_delta1_moodle")

# 3. Gerar um arquivo HTML para visualização rápida
exams2html(meus_exercicios, name = "prova_completa_tcl_delta1")
