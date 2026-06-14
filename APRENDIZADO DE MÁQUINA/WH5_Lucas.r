###########################################################
######                Lucas Pereira Belo             ######
######                luca.p.belo@ufv.br             ######
###########################################################


library(ISLR)
head(Auto)  
names(Auto)

##### Histograma das variáveis 

hist(Auto$weight, col = "blue")
hist(Auto$horsepower, col = "red")
hist(Auto$acceleration, col = "green")

##### b) Erro padrão da média das variáveis

# Média de cada variável
M1<- mean(Auto$weight); M1
M2<- mean(Auto$horsepower); M2
M3<- mean(Auto$acceleration); M3

# Tamanho amostral de cada uma (n)
n1<-length(Auto$weight); n1
n2<-length(Auto$horsepower); n2
n3<-length(Auto$acceleration); n3

# Desvio padrão de cada variável sd
s1<-sd(Auto$weight); s1
s2<-sd(Auto$horsepower); s2
s3<-sd(Auto$acceleration); s3

# Erro padrão da média utilizando a fórmula do enunciado
d1<- s1/sqrt(n1); d1
d2<- s2/sqrt(n2); d2
d3<- s3/sqrt(n3); d3

library(boot)

##### c) Estimativa bootstrap do erro padrão da média

# Fixando a semente 
set.seed(1)

# 2. Executando o bootstrap (usando R = 1000 replicações) para cada variável
boot_w <- boot(Auto$weight, media_fn, R = 1000); boot_w
boot_h <- boot(Auto$horsepower, media_fn, R = 1000); boot_h
boot_a <- boot(Auto$acceleration, media_fn, R = 1000); boot_a

##### d) O valor original referente a média é a primeira observação da saída 
# da função boot. A primeira saída, boot_w indicou 2977.584 que foi o mesmo valor 
# da média calculada no item M1. O segundo valor, referente ao bias (viés) 
# indicou -1.142212 esse valor indica uma diferença baixa entre o verdadeiro 
# valor da média e o valor associado a média das 1000 amostras Bootstrap. 
# O erro padrão indicou um valor de 42.96144, valor este, muito próximo do valor
# calculado manualmente. Vale destacar que o histograma apresentou uma leve assimetria 
# para direita, o que corrobora com o pequeno viés.

# Para o caso boot_h referente a variável horsepower o viés foi de -0.08902296
# novamente indicando uma baixa diferença entre o verdadeiro valore da média e a 
# média oriunda das 1000 amostras Bootstrap, o erro padrão da média apresentou 
# novamente valores muito próximos, o valor real foi de 1.944097 e o da função retornou
# 1.851167

# Para a variável acceleration o viés ficou praticamente nulo, com um valor de 
# -0.001234439, esse valor se reflete no comportamento do histograma, visto que 
# o histograma é aproximadamente simétrico, o que corrobora com as redução do viés.
