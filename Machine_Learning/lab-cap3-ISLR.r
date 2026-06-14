#####################################################################################
########################### Prof. Luiz Alexandre Peternelli #########################
###########################             UFV                 #########################
#####################################################################################

# chapter 3 from textbook - with adaptations

### 3.6 Lab: Linear Regression
### 3.6.1 Libraries

# The library() function is used to load libraries, or groups of functions and
# data sets that are not included in the base R distribution. Basic functions
# that perform least squares linear regression and other simple analyses come
# standard with the base distribution, but more exotic functions require additional
# libraries. Here we load the MASS package, which is a very large
# collection of data sets and functions. We also load the ISLR package, which
# includes the data sets associated with this book.

library (MASS)
library (ISLR)

# If you receive an error message when loading any of these libraries, it
# likely indicates that the corresponding library has not yet been installed
# on your system. Some libraries, such as MASS, come with R and do not need to
# be separately installed on your computer. However, other packages, such as
# ISLR, must be downloaded the first time they are used. This can be done directly
# from within R. For example, on a Windows system, select the Install
# package option under the Packages tab. After you select any mirror site, a
# list of available packages will appear. Simply select the package you wish to
# install and R will automatically download the package. Alternatively, this
# can be done at the R command line via install.packages("ISLR"). This installation
# only needs to be done the first time you use a package. However,
# the library() function must be called each time you wish to use a given
# package.

### 3.6.2 Simple Linear Regression

# The MASS library contains the Boston data set, which records medv (median
# house value) for 506 neighborhoods around Boston. We will seek to predict
# medv using 13 predictors such as rm (average number of rooms per house),
# age (average age of houses), and lstat (percent of households with low
# socioeconomic status).

fix(Boston)
names(Boston)

# To find out more about the data set, we can type ?Boston.
# We will start by using the lm() function to fit a simple linear regression
# lm() model, with medv as the response and lstat as the predictor. The basic
# syntax is lm(y∼x,data), where y is the response, x is the predictor, and
# data is the data set in which these two variables are kept.

lm.fit =lm(medv~lstat)

# The command causes an error because R does not know where to find
# the variables medv and lstat. The next line tells R that the variables are
# in Boston. If we attach Boston, the first line works fine because R now
# recognizes the variables.

lm.fit =lm(medv~lstat ,data=Boston )
attach (Boston )
lm.fit =lm(medv~lstat)

# If we type lm.fit, some basic information about the model is output.
# For more detailed information, we use summary(lm.fit). This gives us pvalues
# and standard errors for the coefficients, as well as the R2 statistic
# and F-statistic for the model.

lm.fit
summary (lm.fit)
names(lm.fit)
res.vector<-lm.fit$residuals
hist(res.vector)

# We can use the names() function in order to find out what other pieces
# of information are stored in lm.fit. Although we can extract these quantities
# by name—e.g. lm.fit$coefficients—it is safer to use the extractor
# functions like coef() to access them.

names(lm.fit)
coef(lm.fit)

# In order to obtain a confidence interval for the coefficient estimates, we can
# use the confint() command.

confint (lm.fit)

# The predict() function can be used to produce confidence intervals and
# prediction intervals for the prediction of medv for a given value of lstat.

predict (lm.fit ,data.frame(lstat=c(5 ,10 ,15) ),
           interval ="confidence")

predict (lm.fit ,data.frame(lstat=c(5 ,10 ,15) ),
           interval ="prediction")
# Para a quinta observação o valor predito foi de 29.8
# Para a décima observação o valor predito foi de 25.05
# Para a vigésima observação o valore predito foi de 20.3
lm.fit$coefficients[1]+lm.fit$coefficients[2] * 5


yhat_pred<- predict(lm.fit, data.frame(lstat=lstat))

# For instance, the 95% confidence interval associated with a lstat value of
# 10 is (24.47, 25.63), and the 95% prediction interval is (12.828, 37.28). As
# expected, the confidence and prediction intervals are centered around the
# same point (a predicted value of 25.05 for medv when lstat equals 10), but
# the latter are substantially wider.

# We will now plot medv and lstat along with the least squares regression
# line using the plot() and abline() functions.

plot(lstat ,medv)
abline (lm.fit, col = "darkblue")
abline (h=20, lty = 2, col = "blue")
?abline

# There is some evidence for non-linearity in the relationship between lstat
# and medv. We will explore this issue later in this lab.

# The abline() function can be used to draw any line, not just the least
# squares regression line. To draw a line with intercept a and slope b, we
# type abline(a,b). Below we experiment with some additional settings for
# plotting lines and points. The lwd=3 command causes the width of the
# regression line to be increased by a factor of 3; this works for the plot()
# and lines() functions also.We can also use the pch option to create different
# plotting symbols.
dev.off()
abline (lm.fit ,lwd =2)
abline (lm.fit ,lwd =3, col ="darkred")
plot(lstat ,medv ,col ="black")
plot(lstat ,medv ,pch =20)
plot(lstat ,medv ,pch ="+")
plot (1:20 ,1:20, pch =1:20)

lstat2<-lstat^2
lmfit2<-lm(medv ~ lstat + lstat2)
lmfit2
lmfit2$coefficients[1] + lmfit2$coefficients[2] * 5 + lmfit2$coefficients[3] * 5^2
par(mfrow = c(2,2))
plot(lmfit2)

# Next we examine some diagnostic plots, several of which were discussed
# in Section 3.3.3. Four diagnostic plots are automatically produced by applying
# the plot() function directly to the output from lm(). In general, this
# command will produce one plot at a time, and hitting Enter will generate
# the next plot. However, it is often convenient to view all four plots together.
# We can achieve this by using the par() function, which tells R to split the
# display screen into separate panels so that multiple plots can be viewed simultaneously.
# For example, par(mfrow=c(2,2)) divides the plotting region
# into a 2 × 2 grid of panels.

par(mfrow =c(2,2))
plot(lm.fit)
dev.off() # ou utilizar par(mfrow = c(1,1))

# Alternatively, we can compute the residuals from a linear regression fit
# using the residuals() function. The function rstudent() will return the
# studentized residuals, and we can use this function to plot the residuals
# against the fitted values.

plot(predict (lm.fit), residuals (lm.fit))
plot(predict (lm.fit), rstudent (lm.fit))

# On the basis of the residual plots, there is some evidence of non-linearity.
# Leverage statistics can be computed for any number of predictors using the
# hatvalues() function.

plot(hatvalues (lm.fit ))
which.max (hatvalues (lm.fit))

# The which.max() function identifies the index of the largest element of a
# vector. In this case, it tells us which observation has the largest leverage
# statistic.

### 3.6.3 Multiple Linear Regression

# In order to fit a multiple linear regression model using least squares, we
# again use the lm() function. The syntax lm(y∼x1+x2+x3) is used to fit a
# model with three predictors, x1, x2, and x3. The summary() function now
# outputs the regression coefficients for all the predictors.

lm.fit =lm(medv~lstat+age ,data=Boston )
summary (lm.fit)

# The Boston data set contains 13 variables, and so it would be cumbersome
# to have to type all of these in order to perform a regression using all of the
# predictors. Instead, we can use the following short-hand:

lm.fit =lm(medv~.,data=Boston )
summary (lm.fit)

# We can access the individual components of a summary object by name
# (type ?summary.lm to see what is available). Hence summary(lm.fit)$r.sq
# gives us the R2, and summary(lm.fit)$sigma gives us the RSE. The vif()
# function, part of the car package, can be used to compute variance inflation
# factors. Most VIF’s are low to moderate for this data. The car package is
# not part of the base R installation so it must be downloaded the first time
# you use it via the install.packages option in R.

library (car)
vif(lm.fit)

# What if we would like to perform a regression using all of the variables but
# one? For example, in the above regression output, age has a high p-value.
# So we may wish to run a regression excluding this predictor. The following
# syntax results in a regression using all predictors except age.

lm.fit1=lm(medv~.-age ,data=Boston )
summary (lm.fit1)

# Alternatively, the update() function can be used.

lm.fit1=update (lm.fit , ~.-age)

### 3.6.4 Interaction Terms

# It is easy to include interaction terms in a linear model using the lm() function.
# The syntax lstat:black tells R to include an interaction term between
# lstat and black. The syntax lstat*age simultaneously includes lstat, age,
# and the interaction term lstat×age as predictors; it is a shorthand for
# lstat+age+lstat:age.

summary(lm(medv~lstat *age ,data=Boston ))

### 3.6.5 Non-linear Transformations of the Predictors

# The lm() function can also accommodate non-linear transformations of the
# predictors. For instance, given a predictor X, we can create a predictor X2
# using I(X^2). The function I() is needed since the ^ has a special meaning
# in a formula; wrapping as we do allows the standard usage in R, which is
# to raise X to the power 2. We now perform a regression of medv onto lstat
# and lstat^2.

lm.fit2=lm(medv~lstat +I(lstat ^2))
summary (lm.fit2)
par(mfrow = c(2,2))
plot(lm.fit2)
# The near-zero p-value associated with the quadratic term suggests that
# it leads to an improved model. We use the anova() function to further
# quantify the extent to which the quadratic fit is superior to the linear fit.

lm.fit =lm(medv~lstat)
anova(lm.fit ,lm.fit2)

# Here Model 1 represents the linear submodel containing only one predictor,
# lstat, while Model 2 corresponds to the larger quadratic model that has two
# predictors, lstat and lstat2. The anova() function performs a hypothesis
# test comparing the two models. The null hypothesis is that the two models
# fit the data equally well, and the alternative hypothesis is that the full
# model is superior. Here the F-statistic is 135 and the associated p-value is
# virtually zero. This provides very clear evidence that the model containing
# the predictors lstat and lstat2 is far superior to the model that only
# contains the predictor lstat. This is not surprising, since earlier we saw
# evidence for non-linearity in the relationship between medv and lstat. If we
# type

par(mfrow=c(2,2))
plot(lm.fit2)
dev.off()

# then we see that when the lstat2 term is included in the model, there is
# little discernible pattern in the residuals.
# In order to create a cubic fit, we can include a predictor of the form
# I(X^3). However, this approach can start to get cumbersome for higherorder
# polynomials. A better approach involves using the poly() function
# to create the polynomial within lm(). For example, the following command
# produces a fifth-order polynomial fit:

lm.fit5=lm(medv~poly(lstat ,2))
summary (lm.fit5)
plot(lm.fit5)

# This suggests that including additional polynomial terms, up to fifth order,
# leads to an improvement in the model fit! However, further investigation of
# the data reveals that no polynomial terms beyond fifth order have significant
# p-values in a regression fit.

# Of course, we are in no way restricted to using polynomial transformations
# of the predictors. Here we try a log transformation.

summary (lm(medv~log(rm),data=Boston ))

### 3.6.6 Qualitative Predictors

# We will now examine the Carseats data, which is part of the ISLR library.
# We will attempt to predict Sales (child car seat sales) in 400 locations
# based on a number of predictors.

library(ISLR)
data("Carseats")
fix( Carseats )
names(Carseats )
dim(Carseats)

# The Carseats data includes qualitative predictors such as Shelveloc, an indicator
# of the quality of the shelving location—that is, the space within
# a store in which the car seat is displayed—at each location. The predictor
# Shelveloc takes on three possible values, Bad, Medium, and Good.

# Given a qualitative variable such as Shelveloc, R generates dummy variables
# automatically. Below we fit a multiple regression model that includes some
# interaction terms.

lm.fit =lm(Sales~.+ Income :Advertising +Price :Age ,data=Carseats )
summary (lm.fit)

# The contrasts() function returns the coding that R uses for the dummy
# variables.

attach (Carseats )
contrasts (ShelveLoc )

# Use ?contrasts to learn about other contrasts, and how to set them.

# R has created a ShelveLocGood dummy variable that takes on a value of
# 1 if the shelving location is good, and 0 otherwise. It has also created a
# ShelveLocMedium dummy variable that equals 1 if the shelving location is
# medium, and 0 otherwise. A bad shelving location corresponds to a zero
# for each of the two dummy variables. The fact that the coefficient for
# ShelveLocGood in the regression output is positive indicates that a good
# shelving location is associated with high sales (relative to a bad location).
# And ShelveLocMedium has a smaller positive coefficient, indicating that a
# medium shelving location leads to higher sales than a bad shelving location
# but lower sales than a good shelving location.

### 3.6.7 Writing Functions

# As we have seen, R comes with many useful functions, and still more functions
# are available by way of R libraries. However, we will often be interested
# in performing an operation for which no function is available. In this
# setting, we may want to write our own function. For instance, below we
# provide a simple function that reads in the ISLR and MASS libraries, called
# LoadLibraries(). Before we have created the function, R returns an error if
# we try to call it.

LoadLibraries=function (){
  library (ISLR)
  library (MASS)
  print ("The libraries have been loaded.")
  }

# Now if we type in LoadLibraries, R will tell us what is in the function.

LoadLibraries

# If we call the function, the libraries are loaded in and the print statement
# is output.

LoadLibraries()

saving_r2 <- function(lm_output){
  obj1<- summary(lm_output)
  r2<- obj1$r.squared
  return(r2)
}

saving_r2(lm.fit2)

saving_r2(fit1)
saving_r2(fit2)
saving_r2(fit3)


my_rmse <- function(original_y, yhat){
  rmse<- sqrt(mean((original_y-yhat)^2))
  return(rmse)
}

yy <- c(1,2,3,4)
yyhat<-c(1,1,2,3)
my_rmse(yy, yyhat)

fit1<- lm(medv ~ lstat)
fit1

my_rmse(medv, predict(fit1))
# Pode-se utilizar predict(fit1) ou fit1$fitted.values

my_rmse(medv, fit1$fitted.values)

# Now for quadratic model 
fit2<-lm(medv~lstat+I(lstat^2)+I(lstat^3))
my_rmse(medv, fit2$fitted.values)

my_rmse(medv, predict(fit2))


# Cúbic Model
fit3<- lm(medv ~ lstat + I(lstat^2) + I(lstat^3))
my_rmse(medv, fit3$fitted.values)

# Trying the validation set approach
train <- sample(dim(Boston)[1], size = dim(Boston)[1]/2)
boston_train<-Boston[train,]
dim(boston_train)

# now fit each model on this training data set 
fit1<- lm(medv~lstat, data = boston_train)
fit1

# Calculating RMSE on the trainning dataset
my_rmse(medv[train], fit1$fitted.values)

# Calculating RMSE on the testing dataset
yhat<- fit1$coefficients[1]+fit1$coefficients[2]*lstat[-train]
my_rmse(medv[-train], yhat)
plot(medv[-train], yhat)
abline(0,1)


# Now for quadratic model 
fit2<-lm(medv~lstat+I(lstat^2), data = boston_train)
my_rmse(medv[train], fit2$fitted.values)

# Calculating RMSE on the testing dataset fit2
yhat2<- fit2$coefficients[1]+fit2$coefficients[2]*lstat[-train]
my_rmse(medv[-train], yhat2)
plot(medv[-train], yhat)
abline(0,1)

# Para casa, comparar diferentes polinômios e verificar o RMSEtreino e 
# RMSEtest

