library(readxl)
library(purrr)
library(dplyr)

EXTREMOS <- read_excel("EXTREMOS.xlsx")
attach(EXTREMOS)
summarise(EXTREMOS)
summary(EXTREMOS)

# Para acessar a segunda folha, referente ao mês de fevereiro, deve-se utilizar
# folhas duplas.
attach(EXTREMOS)
CHUVA<-EXTREMOS$PRECIP
year=seq(1968,2022,1)
par(mfrow=c(1,2))
plot(CHUVA~year,type="b")
hist(CHUVA)

# Importa especificamente a folha 2
folha2 <- read_excel("EXTREMOS.xlsx", sheet = 2)
# O comando sheet indica qual folha o R esta lendo
# Define as variáveis baseadas nessa nova folha

CHUVA2 <- folha2$PRECIP   
year <- seq(1968, 2022, 1)

# Plota
par(mfrow=c(1,2))
plot(CHUVA2 ~ year, type="b", main="Folha 2 (Fev)", ylab="Precipitação")
hist(CHUVA2, main="Histograma Folha 2")

gev.loglik=function(theta){
  mu=theta[1]
  sigma=theta[2]
  xi=theta[3]
  m=min((1+(xi*(dataset-mu)/sigma)))                              #Nota 1
  if(m<0.00001)return(as.double(1000000))                         #Nota 2
  if(sigma<0.00001)return(as.double(1000000))                     #Nota 3
  if(xi==0){
    loglik=(-length(dataset)*log(sigma)-sum((dataset-mu)/sigma)
            -sum(exp(-((dataset-mu)/sigma))))                           #Nota 4
  }else{
    loglik=(-length(dataset)*log(sigma)
            -(1/xi+1)*sum(log(1+(xi*(dataset-mu)/sigma)))
            -sum((1+(xi*(dataset-mu)/sigma))**(-1/xi)))}                #Nota 5
  return(-loglik)}                                                #Nota 6
#######################################################################
dataset=CHUVA
theta=c(mean(CHUVA), sd(CHUVA), 0.1)
nlm(gev.loglik,theta)
A<-nlm(gev.loglik,theta,hessian=TRUE)
A
varcovar=solve(A$hessian)
sqrt(diag(varcovar))

ordered=sort(CHUVA)

empirical=vector('numeric',length(ordered))
for(i in 1:length(empirical)){
  empirical[i]=i/(length(dataset)+1)
}

GEV.DF=function(data,mu,sigma,xi){
  if(xi==0){
    GEV=exp(-exp(-((data-mu)/sigma)))}
  else{
    GEV=exp(-(1+xi*((data-mu)/sigma))^(-1/xi))}
  return(GEV)}

model=vector('numeric',length(dataset))
for(i in 1:length(model)){
  model[i]=GEV.DF(ordered[i],A$est[1],A$est[2],A$est[3])}


plot(model~empirical,main='Gráfico de probabilidade')
abline(0,1)

model.quantile=vector('numeric',length(dataset))
GEV.INV=function(data,mu,sigma,xi){
  if(xi==0){
    INV=mu-sigma*log(-log(1-data))}
  else{
    INV=mu+(sigma/xi)*(((-log(data))^(-xi))-1)}
  return(INV)
}
for(i in 1:length(model.quantile)){
  model.quantile[i]=GEV.INV(empirical[i],A$est[1],A$est[2],A$est[3])
}


plot(model.quantile~ordered,main='Gráfico quantil-quantil')
abline(0,1)


y10=-log(1-(1/10))
del=matrix(ncol=1,nrow=3)
del[1,1]=1
del[2,1]=-((A$est[3])^(-1))*(1-(y10^(-A$est[3])))
del[3,1]=(((A$est[2])*((A$est[3])^(-2))*(1-((y10)^(-A$est[3]))))
          -((A$est[2])*((A$est[3])^(-1))*((y10)^(-(A$est[3])))*log(y10)))
del.transpose=t(del)

sqrt(del.transpose%*%varcovar%*%del)

yrfun=function(A,r){
  yr=-log(1-(1/(r)))
  del=matrix(ncol=1,nrow=3)
  del[1,1]=1
  del[2,1]=-((A$est[3])^(-1))*(1-(yr^(-A$est[3])))
  del[3,1]=(((A$est[2])*((A$est[3])^(-2))*(1-((yr)^(-A$est[3]))))
            -((A$est[2])*((A$est[3])^(-1))*((yr)^(-(A$est[3])))*log(yr)))
  del.transpose=t(del)
  return(c(round(nlm(gev.loglik,theta,hessian=T)$estimate[1]+ ((nlm(gev.loglik,theta,hessian=T)$estimate[2])/(nlm(gev.loglik,theta,hessian=T)$estimate[3]))*((-log(1 - (r)^(-1)))^(-(nlm(gev.loglik,theta,hessian=T)$estimate[3])) - 1),4),round(sqrt(del.transpose%*%varcovar%*%del),4)))}

# Aqui c utiliza a matriz o comando yrfun(A,r) r é nível de retorno que vc quer. 

library("ismev")
B=gev.fit(CHUVA)
gev.diag(B)

par(mfrow=c(1,2))

# Deve-se ajustar esses valores até encontrar os máximos 
gev.prof(B,xlow=75,xup=160,10)  
gev.prof(B,xlow=110,xup=180,100)

library(extRemes)
fit1 <- fevd(CHUVA, units = "mm")
par(mfrow=c(1,2))
ci(fit1, method = "proflik", xrange = c(55, 90), verbose = TRUE,, return.period = c(4))
ci(fit1, method = "proflik", xrange = c(80, 160), verbose = TRUE,, return.period = c(10))
ci(fit1, method = "proflik", xrange = c(100, 220), verbose = TRUE,, return.period = c(20))

# Pode-se obter os valores dos intervalos nos gráficos
# Basta nomear alguma das variáveis de interesse.  


library("ismev")
options(OutDec=",")
wassaw=c(8.5,8.9,9.1,8.9,8.4,9.7,9.1,9.6,8.7,9.3,9.6,9.3,8.7,9.0,8.8,8.9,8.9,12.2,7.8,7.7,8.3,8.1,7.3,6.8,6.7,7.3,7.6,8.2,8.6,9.8,9.5,7.4,7.3,10.2,10.3,10.4,8.8,9.7,10.0,10.8,11.1,12.7,11.5,11.8,12.6,13.0,10.5,10.5,10.0,9.4)
year=seq(1968,2022,1)
par(mfrow=c(1,2))
plot(CHUVA~year,type="b")
hist(CHUVA,breaks = 12)
#######################################################################
gev.loglik=function(theta){
  mu=theta[1]
  sigma=theta[2]
  xi=theta[3]
  m=min((1+(xi*(dataset-mu)/sigma)))                              #Nota 1
  if(m<0.00001)return(as.double(1000000))                         #Nota 2
  if(sigma<0.00001)return(as.double(1000000))                     #Nota 3
  if(xi==0){
    loglik=(-length(dataset)*log(sigma)-sum((dataset-mu)/sigma)
            -sum(exp(-((dataset-mu)/sigma))))                           #Nota 4
  }else{
    loglik=(-length(dataset)*log(sigma)
            -(1/xi+1)*sum(log(1+(xi*(dataset-mu)/sigma)))
            -sum((1+(xi*(dataset-mu)/sigma))**(-1/xi)))}                #Nota 5
  return(-loglik)}                                                #Nota 6
#######################################################################
dataset=wassaw
theta=c(mean(wassaw), sd(wassaw), 0.1)
nlm(gev.loglik,theta)
A<-nlm(gev.loglik,theta,hessian=TRUE)
A
varcovar=solve(A$hessian)
sqrt(diag(varcovar))

ordered=sort(wassaw)

empirical=vector('numeric',length(ordered))
for(i in 1:length(empirical)){
  empirical[i]=i/(length(dataset)+1)
}

GEV.DF=function(data,mu,sigma,xi){
  if(xi==0){
    GEV=exp(-exp(-((data-mu)/sigma)))}
  else{
    GEV=exp(-(1+xi*((data-mu)/sigma))^(-1/xi))}
  return(GEV)}

model=vector('numeric',length(dataset))
for(i in 1:length(model)){
  model[i]=GEV.DF(ordered[i],A$est[1],A$est[2],A$est[3])}


plot(model~empirical,main='Gráfico de probabilidade')
abline(0,1)

model.quantile=vector('numeric',length(dataset))
GEV.INV=function(data,mu,sigma,xi){
  if(xi==0){
    INV=mu-sigma*log(-log(1-data))}
  else{
    INV=mu+(sigma/xi)*(((-log(data))^(-xi))-1)}
  return(INV)
}
for(i in 1:length(model.quantile)){
  model.quantile[i]=GEV.INV(empirical[i],A$est[1],A$est[2],A$est[3])
}


plot(model.quantile~ordered,main='Gráfico quantil-quantil')
abline(0,1)

y10=-log(1-(1/10))
del=matrix(ncol=1,nrow=3)
del[1,1]=1
del[2,1]=-((A$est[3])^(-1))*(1-(y10^(-A$est[3])))
del[3,1]=(((A$est[2])*((A$est[3])^(-2))*(1-((y10)^(-A$est[3]))))
          -((A$est[2])*((A$est[3])^(-1))*((y10)^(-(A$est[3])))*log(y10)))
del.transpose=t(del)

sqrt(del.transpose%*%varcovar%*%del)

yrfun=function(A,r){
  yr=-log(1-(1/(r)))
  del=matrix(ncol=1,nrow=3)
  del[1,1]=1
  del[2,1]=-((A$est[3])^(-1))*(1-(yr^(-A$est[3])))
  del[3,1]=(((A$est[2])*((A$est[3])^(-2))*(1-((yr)^(-A$est[3]))))
            -((A$est[2])*((A$est[3])^(-1))*((yr)^(-(A$est[3])))*log(yr)))
  del.transpose=t(del)
  return(c(round(nlm(gev.loglik,theta,hessian=T)$estimate[1]+ ((nlm(gev.loglik,theta,hessian=T)$estimate[2])/(nlm(gev.loglik,theta,hessian=T)$estimate[3]))*((-log(1 - (r)^(-1)))^(-(nlm(gev.loglik,theta,hessian=T)$estimate[3])) - 1),4),round(sqrt(del.transpose%*%varcovar%*%del),4)))}

# Aqui c utiliza a matriz o comando yrfun(A,r) r é nível de retorno que vc quer. 

library("ismev")
B=gev.fit(wassaw)
gev.diag(B)

par(mfrow=c(1,2))

# Deve-se ajustar esses valores até encontrar os máximos 
gev.prof(B,xlow=12,xup=26,100)  
gev.prof(B,xlow=12,xup=26,1000)

library(extRemes)
fit1 <- fevd(wassaw, units = "pés")
par(mfrow=c(1,2))
ci(fit1, method = "proflik", xrange = c(12, 28), verbose = TRUE,, return.period = c(100))
ci(fit1, method = "proflik", xrange = c(12, 28), verbose = TRUE,, return.period = c(1000))
# Pode-se obter os valores dos intervalos nos gráficos
# Basta nomear alguma das variáveis de interesse.  
