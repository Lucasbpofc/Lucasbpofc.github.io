################################################################################
##################          Lucas Pereira Belo          ########################
################################################################################

## Exemplo 1: Reordenando Níveis com fct_relevel()

library(forcats)
library(ggplot2)
dados <- data.frame(
  categoria = factor(c("Lucas C", "Verônica", "Samantha", "Lucas C", "Verônica")),
  valor = c(10, 20, 30, 15, 25)
)
dados$categoria <- fct_relevel(dados$categoria, "Samantha","Lucas C", "Verônica")
plot <- ggplot(dados, aes(categoria, valor)) +
  geom_bar(stat = "identity")
plot

## Exemplo 2: Reordenando por Frequência com fct_infreq()

dados <- data.frame(
  categoria = factor(c("Sarah", "Sarah", "Jonas", "Brenda", "Brenda", "Brenda", "Renata"))
)

# Reordenando por frequência
dados$categoria <- fct_infreq(dados$categoria)
dados$categoria
# Visualização
plot2 <- ggplot(dados, aes(categoria)) +
  geom_bar()
plot2

## Exemplo 3: Agrupando Níveis com fct_lump()

dados <- data.frame(
  categoria = factor(c("A", "A", "A", "B", "C", "C", "D", "E", "F"))
)

# Agrupando níveis menos frequentes
dados$categoria <- fct_lump(dados$categoria, n = 1)
dados$categoria
# Visualização
plot3 <- ggplot(dados, aes(categoria)) +
  geom_bar()
plot3

## Exemplo 4: Reordenando por outra variável com fct_reorder()

dados <- data.frame(
  categoria = factor(c("A", "B", "C", "D")),
  valor = c(10, 30, 20, 40)
)
dados$categoria

# Reordenando por valor (decrescente)
dados$categoria <- fct_reorder(dados$categoria, dados$valor, .desc = TRUE)
dados$categoria

# Visualização
plot4 <- ggplot(dados, aes(categoria, valor)) +
  geom_bar(stat = "identity")
plot4

### Exemplo 5: Alterando Níveis com fct_recode

fator <- factor(c("Ellie", "Joel", "Tommy", "Abby"))

# Alterando apenas uma
fct_recode(fator, "Carol" = "Ellie")

#Fuciona tanto com ou sem aspas

# Alterando todas
fct_recode(fator,
           Carol = "Ellie",
           Pedro = "Joel",
           Pedro = "Tommy",
           Carol = "Abby")

### Exemplo 6: Agrupando níveis com fct_collapse

dados <- data.frame(
  genero = factor(c("Ação", "Aventura", "Drama", "Romance", "Comédia", "Ação", "Romance", "Aventura")),
  bilheteria = c(100, 150, 80, 90, 120, 110, 95, 140)
)

# Agrupando níveis com fct_collapse
dados$genero <- fct_collapse(dados$genero,
                             "Ação/Aventura" = c("Ação", "Aventura"),
                             "Drama/Romance" = c("Drama", "Romance"),
                             "Outros" = "Comédia")
print(dados$genero)

# Visualização
plot <- ggplot(dados, aes(genero, bilheteria)) +
  geom_bar(stat = "identity") +
  labs(title = "Bilheteria por Grupo de Gêneros", x = "Gênero", y = "Bilheteria (milhões)")
plot

### Exemplo 7: Reordenando Níveis com lvls_reorder

fator <- factor(c("casado", "viuvo", "solteiro", "divorciado"))
fator

lvls_reorder(fator, c(3, 1, 2, 4))

## Exemplo 8: Renomeando Avaliações com lvls_revalue()

dados <- data.frame(
  Notas = factor(c("Aprovado", "Final", "Aprovado", "Reprovado", "Final")),
  pontuacao = c(70, 55, 90, 50, 75)
)

dados$Notas <- lvls_revalue(dados$Notas, c("A", "B", "C"))
print(dados$Notas)
plot <- ggplot(dados, aes(Notas, pontuacao)) +
  geom_bar(stat = "identity") +
  labs(title = "Situação final (Abreviada)", x = "Situação", y = "Pontuação")
print(plot)

## Exemplo 9: Expandindo níveis de um fator com a função lvls_expand

dados <- data.frame(
  nota = factor(c("A", "B", "C", "B", "A")),
  frequencia = c(2, 3, 1, 2, 1)
)
dados$nota <- lvls_expand(dados$nota, c("A", "B", "C", "D", "F","G"))
print(dados$nota)
plot <- ggplot(dados, aes(nota, frequencia)) +
  geom_bar(stat = "identity") +
  labs(title = "Frequência por Nota (Escala Completa)", x = "Nota", y = "Frequência")
print(plot)

plot(dados)
