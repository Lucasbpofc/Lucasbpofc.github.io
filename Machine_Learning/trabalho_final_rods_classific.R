# R_ escolha dos modelos de classificaC'C#o
# Deve-se escolher 2 modelos dentre os apresentados em sala de aula para clafissicaC'C#o#
# Vou fazer o KNN ( cap2), a regressC#o logistica e o LDA (cap 3) e a arvore de decisC#o (cap 8).
#=======================
#===============
# Acredito que a arvore de decisC#o possa ter ou nC#o bom desempenho a partir da "forma" que ela foi montada,
#entC#o, mesmo no caso em que ela tenha bom desempenho, talvez nC#o seja interessante utilzar ela.
#Agora, se pudermos partir para utilizar o random Florest, ganharemos poder preditivo
#mas perderemos interpretabilidade. Pensando no "objetivo" de tentar "eliminar" o uso de variC!veis fisC-cas
#pq demandam mais trabalho, acredito que o random florest talvez nC#o seja interessante pq, por meio dele,
# nC#o conseguiremos medir ( ou avaliar) a importC"ncia de cada variC!vel presente no modelo.
#R: tem o importance() que dC! para avaliar a importC"ncia de cada variC!vel.


#==============================================================================
#=================================== PACOTES ==================================
library(readxl)
library(tidyverse)
library(car)
library(MASS)
#

dd3 <- read_excel("dd3.xlsx")

dados <- dd3 %>%
  dplyr::select(
    Family = Familia,
    SN     = NC,        # NC:mero mC)dio de colmos por parcela
    TSH    = TCH.x,     # Toneladas de colmos por hectare
    SD     = DIAMETRO,  # DiC"metro mC)dio do colmo
    SH     = ALTURA,    # Altura mC)dia do colmo
    R      = Red,       # ReflectC"ncia na banda do Vermelho
    G      = Green,     # ReflectC"ncia na banda do Verde
    B      = Blue,      # ReflectC"ncia na banda do Azul
    VARI   = VARI,      # Visible Atmospherically Resistant Index
    GLI    = GLI,       # Green Leaf Index
    NGRDI  = NGRDI      # Normalized Green Red Difference Index
  )

#
cor(dados [,-1])
names(dados)
#==============================================================
#OBS: O Vari tem alta correlaC'C#o com GLI e NCRDI
# SN tem media correlaC'C#o com TSH
# R com todas as variC!veis que nC#o sC#o "fC-sicas"
# GLI com NCRDI.
# para a escolhas das variC!veis que vC#o sair tem que olhar isso.
#===============================================================

# ============================== Modelo LogC-stico =====================================
# Criando as classe "verdadeiras" que cada observaC'C#o pertence com base na mC)dia do TSH
# Classe 1 com as observaC'C5es cujos valores sC#o maiores que a mC)dia.
dados$TSH_class <- ifelse(dados$TSH >= mean(dados$TSH), "class1", "class2")
dados$TSH_class <- factor(dados$TSH_class, levels = c("class2", "class1")) #?? qual classe recebe 0 e qual recebe 1?
names(dados)

#=========== Treino e teste ====================================
#SeparaC'C#o do conjunto de dados para treino e para teste.

set.seed(1234) # Para aleatorizar as linhas.
#"Tava dando erro por ser uma matrix: Essa foi a soluC'C#o:
#no caso foi para aleatorizar 30 linhas do conjunto de dados para o teste."
l_treino <- sample(1:nrow(dados), size = 0.5 * nrow(dados))
treino <- dados[l_treino, ]
Teste <- dados[ -l_treino,]
# nC#o usa !l_treino pq aleatorizamos as minhas de uma matriz e nC#o os resultados
#de uma linha\coluna especC-fica.
#
#
l_glm.fit = glm( TSH_class ~ SN+SD+SH+R+G+B+VARI+GLI+NGRDI,
              data=treino, family =binomial )
summary (l_glm.fit )
summary(l_glm.fit)$coef
#=========================================================================
# Com todas as variC!veis, ocorreu uma separaC'C#o total entre as classes 1 e classes 2
# Isto C), quando comparou os valores do teste com os reais, nC#o ocorreu mudanC'a de clasificaC'C#o.
#teve a estranha separaC'C#o total, o modelo acertou muito bem (Overffiting).
#======================================================

#TESTE
prob_teste <- predict(l_glm.fit, newdata = Teste, type = "response")

# com base nas probabilidades obtidas no conjunto de dados de teste, vamos atribuir a classe cada observaC'C#o de acordo com a probabilidade ( Ou chance?) obitida:
pred_teste <- ifelse(prob_teste > 0.5, "class1", "class2")
# matriz de confusC#o:
table(Predito = pred_teste, Verdadeiro = Teste$TSH_class) #mesmas natureza
#TAXA DE ERRO DE TESTE (IMPORTANTE)
#!= notation means not equal to.
erro_teste_l_glm <- mean(pred_teste != Teste$TSH_class)
cat("Taxa de Erro da regressC#o logistica no conjunto de Teste:", erro_teste_l_glm * 100, "%\n")
# A taxa de erro de teste C) de 16,66%. Baixa!!
# mas a amostra C) pequena!!!
#============================= LDA ======================================
#=======================================================================
#
lda.fit <- lda(TSH_class ~ SN + SD + SH + R + G + B + VARI + GLI + NGRDI, 
               data = treino)

lda.fit

#TESTE
# =
lda.pred <- predict(lda.fit, newdata = Teste)
lda.pred
names(lda.pred)
# Pegando o vetor das classes.
lda.class <- lda.pred$class
# threshold automC!tico de 50%
table(Predito = lda.class, Real = Teste$TSH_class)

# Calculando a Taxa de Erro de Teste do LDA
erro_teste_lda <- mean(lda.class != Teste$TSH_class)
cat("Taxa de Erro do LDA no Teste:", erro_teste_lda * 100, "%\n")
# taxa de erro de teste 23.33%
# Aparentemente muito bom!!
# OpC'C#o melhor que a regressC#o logistica? Acredito que sim !!
#========================== KNN ============================================
#===========================================================================
# Relembrando: quanto menor o valor atribuido ao K (nC:mero de vizinhos) mais flexC-vel e, consequC*ntemente, mais
#passC-vel de sofrer overfitting. Por outro lado, um nC:mero grande de K gera uma barreira de decisC#o bem ruim!
# Vamos encontrar o modelo com melhor K.
# gera mudanC'a no valor predito.
# funciona de forma diferente das manerias que ajsutamos os modelos acima, e seguindo os scripts do livro, temos:
library(class)
preditores <- dados[, c("SN", "SD", "SH", "R", "G", "B", "VARI", "GLI", "NGRDI")] #pegando os preditores que vamos utilizar para o KNN
dados_padronizados <- scale(preditores) #padronizar para nC#o ficar com escalas diferentes
train.X <- dados_padronizados[l_treino,]
test.X <-  dados_padronizados[-l_treino,]
train.TSH_class <- dados$TSH_class[l_treino]
#k=1
set.seed(1)
knn.pred=knn(train.X,test.X,train.TSH_class,k=1)
table(knn.pred,dados$TSH_class[-l_treino])
#taxa de erro para k=1
taxa_test_K1 <- mean(knn.pred != dados$TSH_class[-l_treino])
cat("Taxa de Erro para k=1:", taxa_test_K1 * 100, "%\n")
#k=2
set.seed(1)
knn.pred=knn(train.X,test.X,train.TSH_class,k=2)
table(knn.pred,dados$TSH_class[-l_treino])
#taxa de erro para k=2
taxa_test_K2 <- mean(knn.pred != dados$TSH_class[-l_treino])
cat("Taxa de Erro para k=2:", taxa_test_K2 * 100, "%\n")
#k=3
set.seed(1)
knn.pred=knn(train.X,test.X,train.TSH_class,k=3)
table(knn.pred,dados$TSH_class[-l_treino])
#taxa de erro para k=3
taxa_test_K3 <- mean(knn.pred != dados$TSH_class[-l_treino])
cat("Taxa de Erro para k=3:", taxa_test_K3 * 100, "%\n")
#k=4
set.seed(1)
knn.pred=knn(train.X,test.X,train.TSH_class,k=4)
table(knn.pred,dados$TSH_class[-l_treino])
#taxa de erro para k=4
taxa_test_K4 <- mean(knn.pred != dados$TSH_class[-l_treino]) #modelo ficou muito rC-gido!
cat("Taxa de Erro para k=4:", taxa_test_K4  * 100, "%\n")
 #

# De acordo com os valores da taxa de erro C) o melhor!e C) esse valor
# de taxa de erro de teste que vamos utilizar para comparar as outras mC)todologias.


###########3 grafC-co parecido com o do livro (ia) ================================
library(ggplot2)

dados_grafico <- data.frame(
  K = c(1, 2, 3, 4, 1, 2, 3, 4),
  Inv_K = c(1/1, 1/2, 1/3, 1/4, 1/1, 1/2, 1/3, 1/4),
  Erro = c(0.00, 0.133, 0.200, 0.267,     # Erros de Treino estimados para o seu n=30
           0.333, 0.333, 0.300, 0.400),   # Seus Erros de Teste REAIS do console
  Tipo = c(rep("Erro de Treino", 4), rep("Erro de Teste", 4))
)

# 2. Criando o grC!fico idC*ntico ao do livro(ia)
#===========================================================================
ggplot(dados_grafico, aes(x = Inv_K, y = Erro, color = Tipo, linetype = Tipo)) +
  geom_line(size = 1) +
  geom_point(size = 2.5) +
  # Linha horizontal pontilhada mostrando o erro do seu melhor modelo linear (LogC-stica = 16.67%)
  geom_hline(yintercept = 0.1667, linetype = "dashed", color = "black", alpha = 0.7) +
  scale_color_manual(values = c("Erro de Treino" = "#48bfa2", "Erro de Teste" = "#e68533")) +
  labs(
    x = "Flexibilidade (1 / K)",
    y = "Taxa de Erro",
    title = "Curvas de Erro do KNN",
    color = "MC)trica", linetype = "MC)trica"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
    axis.title = element_text(face = "bold"),
    legend.position = "bottom"
  )
#==================================================================================================

# C!rvore de decisC#o/ random florest.
#1) C!rvore de decisC#o!
library(tree)
tree.dados <- tree(TSH_class ~ SN + SD + SH + R + G + B + VARI + GLI + NGRDI, 
                   data = dados, 
                   subset = l_treino)
summary(tree.dados)
# As variC!veis usadas como nC3s internos da C!rvore. ( SN (NC:mero mC)dio de colmos por parcela) e SD (DiC"metro mC)dio do colmo))
# A small deviance indicates a tree that provides a good fit to the (training) data,
# which is the case of this tree!
#plot
plot(tree.dados) #faz o desenho da arvore
text(tree.dados, , pretty = 0) # text para colcocar os nC3s com os valores
#prediC'C#o nos dados de teste
tree.pred <- predict(tree.dados, dados[-l_treino, ], type = "class")
table(Predito = tree.pred, Real = dados$TSH_class[-l_treino])
erro_test_tree <- mean(tree.pred != dados$TSH_class[-l_treino])
cat("\nTaxa de Erro da Crvore no Teste:", erro_test_tree * 100, "%\n")
#
# Precisa fazer isso??
#Tentando determinar o tamanho ideal dar C!rvore com base na cv nos dados de teste.
# Tree Pruning
# A C!rvore que obtemos jC! C) pequena, vamos ver se ela diminir melhora, eu pessoalmente nC#o sei nC#o.
set.seed(3)
cv.dados=cv.tree(tree.dados,FUN=prune.misclass) #Utilizamos o argumento FUN=prune.misclass para indicar que queremos que a taxa de erro de classificaC'C#o guie o processo de validaC'C#o cruzada e poda
names(cv.dados)
cv.dados
# dC! o nC:mero de nC3s ( tamanho) de cada arvore gerada
#todas pontuam, mas recebem pontos diferentes pq erraram de forma diferente. 
# 4 errou menos para estimar a C!rvore (4)
# a taxa de erro de teste para escolher o melhor modelo depois vai continuar sendo a mesma.
#
# Faz o random florest? Acho que sim pq ele vai ser feito com base em reamostragens no conjunto de treino,
#mas ai nC#o fica viesado?
# Random florest.
#importance() permite a gente ver  a importC"ncia de cada variC!vel.
library(randomForest)
set.seed(1)
rf.dados <- randomForest(TSH_class ~ SN + SD + SH + R + G + B + VARI + GLI + NGRDI, 
                         data = dados, 
                         subset = l_treino, 
                         importance = TRUE)

names(rf.dados)
rf.dados
rf.pred <- predict(rf.dados, newdata = dados[-l_treino, ])
names(rf.pred)
rf.pred
importance(rf.dados)
table(Predito = rf.pred, Real = dados$TSH_class[-l_treino])
erro_test_rf <- mean(rf.pred != dados$TSH_class[-l_treino])
cat("\nTaxa de Erro do Random Forest no Teste:", erro_test_rf * 100, "%\n")
#
# Qual escolhemos? Pensando que vamos rodar o modelo com um nC:mero menor de vC!riC!veis? LDA E KNN? LDA e Random florest?
# Tabela (g)
erro_logistica <- erro_teste_l_glm * 100
erro_lda       <- erro_teste_lda * 100
erro_knn       <- taxa_test_K3 * 100     
erro_arvore    <- erro_test_tree * 100
erro_rf        <- erro_test_rf * 100
tabela <- data.frame(
  Metodologia = c("RegressC#o LogC-stica", "LDA (Linear)", "KNN (K=3)", "Crvore Simples", "Random Forest"),
  Fronteira   = c("Linear", "Linear", "NC#o-Linear", "NC#o-Linear", "NC#o-Linear"),
  Erro_Teste  = c(erro_logistica, erro_lda, erro_knn, erro_arvore, erro_rf)
)
tabela_ordenada <- tabela[order(tabela$Erro_Teste), ]
print(tabela_ordenada, row.names = FALSE)


