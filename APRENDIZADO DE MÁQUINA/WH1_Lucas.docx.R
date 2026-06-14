

LoadLibraries=function (){
  library (ISLR)
  library (MASS)
  print ("The libraries have been loaded.")
}
LoadLibraries()
saving_r2 <- function(lm_output){
  obj1 <- summary(lm_output)
  r2 <- obj1$r.squared
  return(r2)
}
#saving_r2(lm.fit2)
my_rmse <- function(original_y, yhat){
  rmse <- sqrt(mean((original_y-yhat)^2))
  return(rmse)
}
# testing
yy <- c(1, 2, 3, 4)
yyhat <- c(1, 1, 2, 3)
my_rmse(yy, yyhat)

#############################
# upload the Boston dataset
#############################

dd <- Boston # renaming the Boston dataset to easy notation below
dim(dd)

### fitting a simple linear model with n = 506
fit1 <- lm(medv ~ lstat, data = dd)
fit1
# finding the rmse for this model:
my_rmse(dd$medv, fit1$fitted.values)
### Quadratic Model
fit2 <- lm(medv~lstat + I(lstat^2), data = dd)
my_rmse(dd$medv, fit2$fitted.values)

### Cubic Model
fit3 <- lm(medv~lstat+I(lstat^2)+I(lstat^3), data = dd)
my_rmse(dd$medv, fit3$fitted.values)

# as we can see, the RMSE decreases as we consider a more flexible model
# try doing this for other polinomials
#---------------------------------------------------------------------------
# now, our main interest is better understanding the idea of RMSE  on a training
# dataset and how it compares with the RMSE calculated on a testing dataset.
# for this, let´s run the following toy example:

######################################
# trying the validation set approach
######################################
# first we select which rows of our dataset will belong to the training population
train <- sample(dim(dd)[1], size = dim(dd)[1]/2) # 50% training; 50% testing
# now, we split the dataset (dd) in training and testing

dd_train <- dd[train, ] # select all the rows that match the train object
dim(dd_train)
# now, fit each model on this training dataset. Obtain the RMSE for training and testing

###################################################
### Starting with a simple linear regression model
###################################################

fit1 <- lm(medv ~ lstat, data = dd_train)
fit1
# RMSE for training
my_rmse(dd$medv[train], fit1$fitted.values) # RMSE_train - grau 1
# calculating RMSE on the testing dataset
yhat <- fit1$coefficients[1] + fit1$coefficients[2] * dd$lstat[-
                                                                 train]
# alternatively, we could have used the predict function in R to obtain this yhat:
  yhat_other <- predict(fit1, newdata = dd[-train,])
  
  # comparing both vectors of predicted y
  
  head(cbind(yhat, yhat_other)) # showing the first 7 entries. The values are the same
  my_rmse(dd$medv[-train], yhat) # RMSE_test - grau 1
  plot(dd$medv[-train], yhat)
  abline(0,1)
  
  ###############################
  # now for the quadratic model
  ###############################
  fit2 <- lm(medv ~ lstat + I(lstat^2), data = dd_train)
  fit2
  my_rmse(dd$medv[train], fit2$fitted.values) # RMSE_train - grau 2
  # calculating RMSE on the testing dataset
  yhat2 <- fit2$coefficients[1] + fit2$coefficients[2] * dd$lstat[-train] + fit2$coefficients[3] * (dd$lstat^2)[-train]
  # or:
  
  yhat2_other <- predict(fit2, dd[-train,])
  head(cbind(yhat2, yhat2_other)) # showing the first 7 entries. The values are the same
  my_rmse(dd$medv[-train], yhat2) # RMSE_test - grau 2
  plot(dd$medv[-train], yhat2)
  abline(0,1)
  # now that we know the predict function can be used to predict y values from a fitted
  # model, we can use it on the rest of this script
  ###########################
  # now for the cubic model
  ###########################
  fit3 <- lm(medv ~ lstat + I(lstat^2) + I(lstat^3), data = dd_train)
  fit3
  my_rmse(dd$medv[train], fit3$fitted.values) # RMSE_train - grau 3
  # calculating RMSE on the testing dataset
  yhat3 <- predict(fit3, dd[-train,])
  my_rmse(dd$medv[-train], yhat3) # RMSE_test - grau 3
  plot(dd$medv[-train], yhat3)
  abline(0,1)
  
  # models (degree 4, 5, 6, 7, 8, 9, and 10) on this same trainingand test dataset
  # save the values RMSE_train in a vector named vector_RMSE_train; save the values
  # RMSE_test in a vector named vector_RMSE_test
  # 2) make a plot where the x-axis is the polinomial degree, and the y-axis is the
  # vector_RMSE_train and vector_RMSE_test (on the same plot)
  # obs: I do expect you will have a plot very similar to the one shown in
  # FIGURE 2.9 of our textbook.
  # 3) comment your results
  
  # Deixarei os comentário em português seguindo a ideia anterior
  
  # Primeiro, criamos os vetores vazios com 10 posições para armazenar os resultados
  vector_RMSE_train <- numeric(10)
  vector_RMSE_test <- numeric(10)
  
  # Salvando os resultados dos modelos 1, 2 e 3 rodados nos exemplos
  vector_RMSE_train[1] <- my_rmse(dd$medv[train], fit1$fitted.values)
  vector_RMSE_test[1]  <- my_rmse(dd$medv[-train], yhat)
  
  vector_RMSE_train[2] <- my_rmse(dd$medv[train], fit2$fitted.values)
  vector_RMSE_test[2]  <- my_rmse(dd$medv[-train], yhat2)
  
  vector_RMSE_train[3] <- my_rmse(dd$medv[train], fit3$fitted.values)
  vector_RMSE_test[3]  <- my_rmse(dd$medv[-train], yhat3)
  
  # Para um modelo de grau 4
  
  fit4 <- lm(medv ~ lstat + I(lstat^2) + I(lstat^3) + I(lstat^4), data = dd_train)
  vector_RMSE_train[4] <- my_rmse(dd$medv[train], fit4$fitted.values); vector_RMSE_train[4]
  
  yhat4 <- predict(fit4, dd[-train,])
  vector_RMSE_test[4] <- my_rmse(dd$medv[-train], yhat4); vector_RMSE_test[4]
  
  # Para um modelo de grau 5

  fit5 <- lm(medv ~ lstat + I(lstat^2) + I(lstat^3) + I(lstat^4) + I(lstat^5), data = dd_train)
  vector_RMSE_train[5] <- my_rmse(dd$medv[train], fit5$fitted.values); vector_RMSE_train[5]
  
  yhat5 <- predict(fit5, dd[-train,])
  vector_RMSE_test[5] <- my_rmse(dd$medv[-train], yhat5); vector_RMSE_test[5]
  
  # Para um modelo de grau 6

  fit6 <- lm(medv ~ lstat + I(lstat^2) + I(lstat^3) + I(lstat^4) + I(lstat^5) + I(lstat^6), data = dd_train)
  vector_RMSE_train[6] <- my_rmse(dd$medv[train], fit6$fitted.values); vector_RMSE_train[6]
  
  yhat6 <- predict(fit6, dd[-train,])
  vector_RMSE_test[6] <- my_rmse(dd$medv[-train], yhat6); vector_RMSE_test[6]
  
  # Para um modelo de grau 7

  fit7 <- lm(medv ~ lstat + I(lstat^2) + I(lstat^3) + I(lstat^4) + I(lstat^5) + I(lstat^6) + I(lstat^7), data = dd_train)
  vector_RMSE_train[7] <- my_rmse(dd$medv[train], fit7$fitted.values); vector_RMSE_train[7]
  
  yhat7 <- predict(fit7, dd[-train,])
  vector_RMSE_test[7] <- my_rmse(dd$medv[-train], yhat7);vector_RMSE_test[7]

  # Para um modelo de grau 8

  fit8 <- lm(medv ~ lstat + I(lstat^2) + I(lstat^3) + I(lstat^4) + I(lstat^5) + I(lstat^6) + I(lstat^7) + I(lstat^8), data = dd_train)
  vector_RMSE_train[8] <- my_rmse(dd$medv[train], fit8$fitted.values); vector_RMSE_train[8]
  
  yhat8 <- predict(fit8, dd[-train,])
  vector_RMSE_test[8] <- my_rmse(dd$medv[-train], yhat8); vector_RMSE_test[8]
  
  # Para um modelo de grau 9

  fit9 <- lm(medv ~ lstat + I(lstat^2) + I(lstat^3) + I(lstat^4) + I(lstat^5) + I(lstat^6) + I(lstat^7) + I(lstat^8) + I(lstat^9), data = dd_train)
  vector_RMSE_train[9] <- my_rmse(dd$medv[train], fit9$fitted.values); vector_RMSE_train[9]
  
  yhat9 <- predict(fit9, dd[-train,])
  vector_RMSE_test[9] <- my_rmse(dd$medv[-train], yhat9); vector_RMSE_test[9]

  # Para um modelo de grau 10

  fit10 <- lm(medv ~ lstat + I(lstat^2) + I(lstat^3) + I(lstat^4) + I(lstat^5) + I(lstat^6) + I(lstat^7) + I(lstat^8) + I(lstat^9) + I(lstat^10), data = dd_train)
  vector_RMSE_train[10] <- my_rmse(dd$medv[train], fit10$fitted.values); vector_RMSE_train[10]
  
  yhat10 <- predict(fit10, dd[-train,])
  vector_RMSE_test[10] <- my_rmse(dd$medv[-train], yhat10);vector_RMSE_test[10]
  
  
  #---------------------------------------------------------------------------
  # 2) make a plot where the x-axis is the polinomial degree, and the y-axis is the
  # vector_RMSE_train and vector_RMSE_test (on the same plot)
  #---------------------------------------------------------------------------
  
  degrees <- 1:10
  
  plot(degrees, vector_RMSE_train, type = "b", col = "blue", lwd = 2, 
       ylim = range(c(vector_RMSE_train, vector_RMSE_test)),
       xlab = "Polynomial Degree (Flexibility)", 
       ylab = "RMSE",
       main = "Training vs Testing RMSE")
  
  lines(degrees, vector_RMSE_test, type = "b", col = "red", lwd = 2)
  
  legend("topright", legend = c("RMSE Train", "RMSE Test"), 
         col = c("blue", "red"), lty = 1, lwd = 2)
  
  #---------------------------------------------------------------------------
  # 4) Creating a summary table and finding the optimal model mathematically
  #---------------------------------------------------------------------------
  
  # Juntando os vetores em uma tabela (Data Frame)
  results_table <- data.frame(
    Degree = 1:10,
    RMSE_Train = vector_RMSE_train,
    RMSE_Test = vector_RMSE_test
  )
  
  print(results_table)

  #---------------------------------------------------------------------------
  # 3) comment your results
  #---------------------------------------------------------------------------
  # A inclusão de modelos polinomiais de maiores graus faz com que o RMSE no conjunto
  # de treino diminua. A grande questão é que ao diminuir exacerbadamente o RMSE com 
  # um polinômio de grau muito alto, como polinômios de grau maior que 6, isso gera 
  # problemas de overfitting, o que reflete em predições ruins e não necessariamente em 
  # um RMSE baixo no conjunto de teste.
  # O equilíbrio ideal é encontrado quando o RMSE de test encontra o menor valor, 
  # que no nosso caso foi para o polinômio de grau 5, com um RMSE de 5.538326 no conjunto de teste
