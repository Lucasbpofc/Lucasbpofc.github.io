# Carregue o pacote R/exams
library(exams)

# Defina uma semente para garantir que os resultados sejam reprodutíveis
set.seed(42)

# --- ALTERAÇÃO PRINCIPAL AQUI ---
# Crie um vetor com os nomes de TODOS os seus arquivos de exercício.
# Garanta que todos estes arquivos .Rmd estejam na mesma pasta que este script.
meus_exercicios <- c(
  "EXERCICIO1.Rmd",
  "EXERCICIO2.Rmd",
  "EXERCICIO3.Rmd",
  "EXERCICIO4.Rmd",
  "EXERCICIO5.Rmd"
)

# --- Gere as saídas ---

# 1. Gerar um PDF com 3 versões diferentes da prova completa
#    Cada PDF terá as 5 questões, com valores e ordem embaralhados.
exams2pdf(meus_exercicios, n = 3, name = "prova_completa_tcl_delta")

# 2. Gerar um arquivo para importar no Moodle com todas as 5 questões
exams2moodle(meus_exercicios, name = "quiz_completo_tcl_delta")

# 3. Gerar um arquivo HTML para visualização rápida da prova
exams2html(meus_exercicios, name = "preview_prova_completa")

# Mensagem de confirmação
cat("Arquivos da prova com 5 questões foram gerados com sucesso!\n")
