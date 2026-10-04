options(OutDec=",",scipen=99,digits=4)
library(extraDistr)
library(sn)
library(moments)
dados <- read.table("tudo.txt",h=T)
dados <- subset(dados, ano >= 2006)
x<-as.numeric(as.character(dados[,5]))
mu_hat <- mean(x)
sd_hat <- sd(x)
xs <- seq(min(x), max(x))
n <- length(x)
############Aqui estou criando as densidades
# Normal discreta
ddnorm2 <- function(x, mu, sigma) {
  pnorm((x - mu + 1)/sigma) - pnorm((x - mu)/sigma)
}
# \textit{skew}--normal discreta
ddskewnorm <- function(x, xi, omega, alpha) {
  psn(x + 1,xi = xi, omega = omega, alpha = alpha) -psn(x, xi = xi,omega = omega, alpha = alpha)
}

# Skellam (Poisson diferença)
dskellam <- function(x, lambda1, lambda2) {
  exp(-(lambda1 + lambda2)) * (lambda1/lambda2)^(x/2) * besselI(2 * sqrt(lambda1 * lambda2), abs(x))
}
#Weibull diferença discreta
dweibull_discrete <- function(x, q, beta){
  # garante que x seja inteiro não negativo
  if(any(x < 0)){
    stop("x deve ser >= 0")
  }  
  q^(x^beta) - q^((x+1)^beta) 
}
############
dweibull_diff <- function(d,
                          q1,
                          beta1,
                          q2,
                          beta2,
                          max_k = 200){
  
  sapply(d, function(di){
    
    if(di >= 0){
      
      sum(
        dweibull_discrete(
          0:max_k + di,
          q1,
          beta1
        ) *
          dweibull_discrete(
            0:max_k,
            q2,
            beta2
          )
      )
      
    } else {
      
      sum(
        dweibull_discrete(
          0:max_k,
          q1,
          beta1
        ) *
          dweibull_discrete(
            0:max_k-di,
            q2,
            beta2
          )
      )
      
    }
    
  })
}
############Aqui estou criando as logverossimilhancas das densidades
#loglik_norm <- function(mu, sigma) {sum(log(ddnorm2(x, mu, sigma)))}

loglik_norm <- function(par){
  
  mu    <- par[1]
  sigma <- exp(par[2])
  
  p <- ddnorm2(x, mu, sigma)
  
  -sum(log(p + 1e-12))
}

# \textit{skew}--normal discreta
loglik_skew <- function(par) {
  xi <- par[1]
  omega <- abs(par[2])
  alpha <- par[3]
  p <- ddskewnorm(x,xi,omega,alpha)
  -sum(log(p + 1e-12))
}

#loglik_skellam <- function(l1, l2) {sum(log(dskellam(x, l1, l2)))}

loglik_skellam <- function(par){
  
  lambda1 <- exp(par[1])
  lambda2 <- exp(par[2])
  
  p <- dskellam(
    x,
    lambda1,
    lambda2
  )
  
  -sum(log(p + 1e-12))
}


loglik_weibull_diff <- function(par){
  q1 <- 1/(1+exp(-par[1]))
  beta1 <- exp(par[2])  
  q2 <- 1/(1+exp(-par[3]))
  beta2 <- exp(par[4])
  sum(log(dweibull_diff(x,q1,beta1,q2,beta2)))  
}

fit_norm <- optim(
  par = c(mean(x), log(sd(x))),
  fn = loglik_norm,
  method = "BFGS"
)

fit_skew <- optim(par = c(mean(x),sd(x),0),fn = loglik_skew, method = "BFGS")


#skellam
lambda1_ini <- (var(x)+mean(x))/2
lambda2_ini <- (var(x)-mean(x))/2
lambda2_ini <- max(lambda2_ini,0.1)
fit_skellam <- optim(
  par=c(log(lambda1_ini),
        log(lambda2_ini)),
  fn=loglik_skellam,
  method="BFGS"
)
lambda1_hat <- exp(fit_skellam$par[1])
lambda2_hat <- exp(fit_skellam$par[2])
#
fit_weibull <- optim(
  par=c(0,0,0,0),
  fn=function(par)
    -loglik_weibull_diff(par),
  method="BFGS"
)
# parâmetros estimados na escala transformada
par_hat <- fit_weibull$par
# voltar para a escala original
q1_hat <- 1/(1+exp(-par_hat[1]))
beta1_hat <- exp(par_hat[2])
q2_hat <- 1/(1+exp(-par_hat[3]))
beta2_hat <- exp(par_hat[4])

ll_norm <- -fit_norm$value
ll_skew <- -fit_skew$value
ll_skel <- -fit_skellam $value
ll_weibull <- -fit_weibull$value

k_norm <- 2
k_skew <- 3
k_skel <- 2
k_weibull <- 4

# AIC
AIC_norm <- -2*ll_norm + 2*k_norm
AIC_skew <- -2*ll_skew + 2*k_skew
AIC_skel <- -2*ll_skel + 2*k_skel
AIC_weibull <- -2*ll_weibull + 2*k_weibull

# BIC
BIC_norm <- -2*ll_norm + log(n)*k_norm
BIC_skew <- -2*ll_skew + log(n)*k_skew
BIC_skel <- -2*ll_skel + log(n)*k_skel
BIC_weibull <- -2*ll_weibull + log(n)*k_weibull


###########################
xs <- seq(min(x), max(x), by = 1)

## Frequências observadas (valores reais)
freq_obs <- table(factor(x, levels = xs))
prob_obs <- as.numeric(freq_obs) / length(x)

## Probabilidades ajustadas

# Normal discreta
prob_norm <- ddnorm2(
  xs,
  mu = fit_norm$par[1],
  sigma = exp(fit_norm$par[2])
)

# Skew-normal discreta
prob_skew <- ddskewnorm(
  xs,
  xi = fit_skew$par[1],
  omega = abs(fit_skew$par[2]),
  alpha = fit_skew$par[3]
)

# Skellam
prob_skel <- dskellam(
  xs,
  lambda1 = lambda1_hat,
  lambda2 = lambda2_hat
)

# Weibull diferença discreta
prob_weib <- dweibull_diff(
  xs,
  q1 = q1_hat,
  beta1 = beta1_hat,
  q2 = q2_hat,
  beta2 = beta2_hat
)
simbolos=c(16,0,1,2,3)
## Gráfico
plot(xs, prob_obs,
     pch = simbolos[1],
     col = "black",
     cex = 1.2,
     xlab = "Diferença de gols entre mandante e visitante",
     ylab = "Probabilidade",
     ylim = c(0, max(c(prob_obs,
                       prob_norm,
                       prob_skew,
                       prob_skel,
                       prob_weib))),
     main = "Distribuições ajustadas e valores observados")

points(xs, prob_norm,
       pch = simbolos[2],      # círculo
       col = "blue",
       cex = 1.3)

points(xs, prob_skew,
       pch = simbolos[3],      # triângulo
       col = "red",
       cex = 1.3)

points(xs, prob_skel,
       pch = simbolos[4],      # quadrado
       col = "darkgreen",
       cex = 1.3)

points(xs, prob_weib,
       pch = simbolos[5],      # asterisco
       col = "purple",
       cex = 1.4)

legend("topright",
       legend = c("Observado",
                  "Normal discreta",
                  "Skew-normal discreta",
                  "Skellam",
                  "Weibull diferença"),
       pch = simbolos,
       col = c("black", "blue", "red", "darkgreen", "purple"),
       pt.cex = c(1.2, 1.3, 1.3, 1.3, 1.4),
       bty = "n")















