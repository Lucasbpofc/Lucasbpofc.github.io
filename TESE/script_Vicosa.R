################################################################################
############      Lucas Pareira Belo - lucas.p.belo@ufv.br       ###############
################################################################################

# Pacotes utilizados
library(readxl)
library(dplyr)
library(tseries)   
library(trend)     
library(randtests) 
library(goftest)   
library(extRemes)  
library(ismev)     
library(urca)      
library(DT)
library(kableExtra)

# Leitura e Definições
EXTREMOS <- read_excel("EXTREMOS.xlsx")
CHUVA <- EXTREMOS$PRECIP
year <- seq(1968, 2022, 1)

# 1. Encontrando os índices (posições) dos extremos
idx_max <- which.max(CHUVA) # Posição da maior chuva
idx_min <- which.min(CHUVA) # Posição da menor chuva

# 3. Resumo Estatístico Padrão
summary(EXTREMOS$PRECIP)

plot(CHUVA ~ year, type="b", main="Série Histórica: Precipitação (Jan)", 
     ylab="Precipitação (mm)", xlab="Ano", pch=19, col="#2980b9", lwd=2)
grid()

hist(CHUVA, main="Distribuição de Frequências", 
     xlab="Precipitação (mm)", col="#3498db", border="white", prob=TRUE)
lines(density(CHUVA), col="#c0392b", lwd=2)

# 1. An�lise Visual de Depend�ncia (ACF e PACF)
par(mfrow = c(1, 2)) 

acf(CHUVA, main = "Fun��o de Autocorrela��o (FAC)", ylab = "ACF")
pacf(CHUVA, main = "Autocorrela��o Parcial (FACP)", ylab = "PACF")

par(mfrow = c(1, 1)) 
