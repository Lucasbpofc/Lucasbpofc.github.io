##########################################################################
#####           Autor: professor Luiz Alexandre Peternelli           #####
#####           peternelli@ufv.br                                    #####
#####                                                                #####
##########################################################################

### chapter 6 from textbook - with adaptations

### 6.5 Lab 1: Subset Selection Methods

### 6.5.1 Best Subset Selection

# Here we apply the best subset selection approach to the Hitters data. We
# wish to predict a baseball player’s Salary on the basis of various statistics
# associated with performance in the previous year.

# First of all, we note that the Salary variable is missing for some of the
# players. The is.na() function can be used to identify the missing observa
# tions. It returns a vector of the same length as the input vector, with a TRUE
# for any elements that are missing, and a FALSE for non-missing elements.
# The sum() function can then be used to count all of the missing elements.

library (ISLR)
fix(Hitters )
names(Hitters )
dim(Hitters )
sum(is.na(Hitters$Salary))

# Hence we see that Salary is missing for 59 players. The na.omit() function
# removes all of the rows that have missing values in any variable.

Hitters = na.omit(Hitters )
dim(Hitters )
sum(is.na(Hitters ))

# The regsubsets() function (part of the leaps library) performs best sub
# set selection by identifying the best model that contains a given number
# of predictors, where best is quantified using RSS. The syntax is the same
# as for lm(). The summary() command outputs the best set of variables for
# each model size.

library (leaps)
regfit.full = regsubsets (Salary ~ .,Hitters )
summary (regfit.full)

# An asterisk indicates that a given variable is included in the corresponding
# model. For instance, this output indicates that the best two-variable model
# contains only Hits and CRBI. By default, regsubsets() only reports results
# up to the best eight-variable model. But the nvmax option can be used
# in order to return as many variables as are desired. Here we fit up to a
# 19-variable model.

regfit.full = regsubsets (Salary~.,data=Hitters ,nvmax =19)
reg.summary = summary (regfit.full)

# The summary() function also returns R2, RSS, adjusted R2, Cp, and BIC.
# We can examine these to try to select the best overall model.

names(reg.summary )

# For instance, we see that the R2 statistic increases from 32%, when only
# one variable is included in the model, to almost 55 %, when all variables
# are included. As expected, the R2 statistic increases monotonically as more
# variables are included.

reg.summary$rsq
plot(1:19,reg.summary$rsq)

# Plotting RSS, adjusted R2, Cp, and BIC for all of the models at once will
# help us decide which model to select. Note the type="l" option tells R to
# connect the plotted points with lines.

par(mfrow =c(2,2))
plot(reg.summary$rss ,xlab=" Number of Variables ",ylab=" RSS",
       type="l")
plot(reg.summary$adjr2 ,xlab =" Number of Variables ",
       ylab=" Adjusted RSq",type="l")

# The points() command works like the plot() command, except that it
# puts points on a plot that has already been created, instead of creating a
# new plot. The which.max() function can be used to identify the location of
# the maximum point of a vector. We will now plot a red dot to indicate the
# model with the largest adjusted R2 statistic.

which.max (reg.summary$adjr2)
points (11, reg.summary$adjr2[11], col ="red",cex =2, pch =20)

# In a similar fashion we can plot the Cp and BIC statistics, and indicate the
# models with the smallest statistic using which.min().

plot(reg.summary$cp ,xlab =" Number of Variables ",ylab="Cp",
       type="l")
which.min (reg.summary$cp )
points (10, reg.summary$cp [10], col ="red",cex =2, pch =20)
which.min (reg.summary$bic )
plot(reg.summary$bic ,xlab=" Number of Variables ",ylab=" BIC",
       type="l")
points (6, reg.summary$bic [6], col =" red",cex =2, pch =20)
dev.off()

# The regsubsets() function has a built-in plot() command which can
# be used to display the selected variables for the best model with a given
# number of predictors, ranked according to the BIC, Cp, adjusted R2, or
# AIC. To find out more about this function, type ?plot.regsubsets.

plot(regfit.full ,scale ="r2")
plot(regfit.full ,scale ="adjr2")
plot(regfit.full ,scale ="Cp")
plot(regfit.full ,scale ="bic")

# The top row of each plot contains a black square for each variable selected
# according to the optimal model associated with that statistic. For instance,
# we see that several models share a BIC close to −150. However, the model
# with the lowest BIC is the six-variable model that contains only AtBat,
# Hits, Walks, CRBI, DivisionW, and PutOuts. We can use the coef() function
# to see the coefficient estimates associated with this model.

coef(regfit.full ,6)


### 6.5.2 Forward and Backward Stepwise Selection

# We can also use the regsubsets() function to perform forward stepwise
# or backward stepwise selection, using the argument method="forward" or
# method="backward".

regfit.fwd=regsubsets (Salary ~ .,data=Hitters ,nvmax =19,
                          method ="forward")
summary (regfit.fwd )
regfit.bwd=regsubsets (Salary~.,data=Hitters ,nvmax =19,
                          method ="backward")
summary (regfit.bwd )

# For instance, we see that using forward stepwise selection, the best one variable
# model contains only CRBI, and the best two-variable model additionally
# includes Hits. For this data, the best one-variable through sixvariable
# models are each identical for best subset and forward selection.
# However, the best seven-variable models identified by forward stepwise selection,
# backward stepwise selection, and best subset selection are different.

coef(regfit.full ,7)
coef(regfit.fwd ,7)
coef(regfit.bwd ,7)


### 6.5.3 Choosing Among Models Using the Validation Set
    #Approach and Cross-Validation

# we better return to this section after the cross-validation lesson.

# We just saw that it is possible to choose among a set of models of different
# sizes using Cp, BIC, and adjusted R2. We will now consider how to do this
# using the validation set and cross-validation approaches.
# In order for these approaches to yield accurate estimates of the test
# error, we must use only the training observations to perform all aspects of
# model-fitting—including variable selection. Therefore, the determination of
# which model of a given size is best must be made using only the training
# observations. This point is subtle but important. If the full data set is used
# to perform the best subset selection step, the validation set errors and
# cross-validation errors that we obtain will not be accurate estimates of the
# test error.

# In order to use the validation set approach, we begin by splitting the
# observations into a training set and a test set. We do this by creating
# a random vector, train, of elements equal to TRUE if the corresponding
# observation is in the training set, and FALSE otherwise. The vector test has
# a TRUE if the observation is in the test set, and a FALSE otherwise. Note the
# ! in the command to create test causes TRUEs to be switched to FALSEs and
# vice versa. We also set a random seed so that the user will obtain the same
# training set/test set split.

set.seed (1)
train=sample (c(TRUE ,FALSE), nrow(Hitters ),rep=TRUE)
table(train)
test =(! train )

# Now, we apply regsubsets() to the training set in order to perform best
# subset selection.

regfit.best=regsubsets (Salary~.,data=Hitters [train ,],
                         nvmax =19)

# Notice that we subset the Hitters data frame directly in the call in order
# to access only the training subset of the data, using the expression
# Hitters[train,]. We now compute the validation set error for the best
# model of each model size. We first make a model matrix from the test
# data.

test.mat=model.matrix (Salary~.,data=Hitters [test ,])

# The model.matrix() function is used in many regression packages for build
# ing an “X” matrix from data. Now we run a loop, and for each size i, we 
# extract the coefficients from regfit.best for the best model of that size,
# multiply them into the appropriate columns of the test model matrix to
# form the predictions, and compute the test MSE.

val.errors =rep(NA ,19)
for(i in 1:19){
  coefi=coef(regfit.best ,id=i)
  pred=test.mat [,names(coefi)]%*% coefi
  val.errors [i]= mean(( Hitters$Salary[test]-pred)^2)
}

# We find that the best model is the one that contains ten variables.

val.errors

which.min (val.errors )
coef(regfit.best ,10)

# This was a little tedious, partly because there is no predict() method
# for regsubsets(). Since we will be using this function again, we can capture
# our steps above and write our own predict method.

predict.regsubsets = function (object ,newdata ,id ,...){
 form = as.formula (object$call [[2]])
 mat = model.matrix (form ,newdata )
 coefi = coef(object ,id=id)
 xvars = names (coefi )
 mat[,xvars ]%*% coefi
}

# Our function pretty much mimics what we did above. The only complex
# part is how we extracted the formula used in the call to regsubsets(). We
# demonstrate how we use this function below, when we do cross-validation.
# Finally, we perform best subset selection on the full data set, and select
# the best ten-variable model. It is important that we make use of the full
# data set in order to obtain more accurate coefficient estimates. Note that
# we perform best subset selection on the full data set and select the best tenvariable
# model, rather than simply using the variables that were obtained
# from the training set, because the best ten-variable model on the full data
# set may differ from the corresponding model on the training set.

regfit.best=regsubsets (Salary~.,data=Hitters ,nvmax =19)
coef(regfit.best ,10)

# In fact, we see that the best ten-variable model on the full data set has a
# different set of variables than the best ten-variable model on the training
# set.

# We now try to choose among the models of different sizes using crossvalidation.
# This approach is somewhat involved, as we must perform best
# subset selection within each of the k training sets. Despite this, we see that
# with its clever subsetting syntax, R makes this job quite easy. First, we
# create a vector that allocates each observation to one of k = 10 folds, and
# we create a matrix in which we will store the results.

k = 10
set.seed (1)
folds = sample (1:k,nrow(Hitters ),replace =TRUE)
table(folds)
cv.errors = matrix (NA ,k,19, dimnames =list(NULL , paste (1:19) ))

# Now we write a for loop that performs cross-validation. In the jth fold, the
# elements of folds that equal j are in the test set, and the remainder are in
# the training set. We make our predictions for each model size (using our
# new predict() method), compute the test errors on the appropriate subset,
# and store them in the appropriate slot in the matrix cv.errors.

for(j in 1:k){
 best.fit = regsubsets (Salary~.,data=Hitters [folds !=j,],
                      nvmax =19)
 for(i in 1:19) {
  pred = predict (best.fit ,Hitters [folds ==j,], id=i)
  cv.errors [j,i]=mean( (Hitters$Salary[folds ==j]-pred)^2)
  }
}

# This has given us a 10×19 matrix, of which the (i, j)th element corresponds
# to the test MSE for the ith cross-validation fold for the best j-variable
# model. We use the apply() function to average over the columns of this
# matrix in order to obtain a vector for which the jth element is the crossvalidation
# error for the j-variable model.

mean.cv.errors =apply(cv.errors ,2, mean)
mean.cv.errors
par(mfrow = c(1,1))
plot(mean.cv.errors ,type="b")

# We see that cross-validation selects an 11-variable model. We now perform
# best subset selection on the full data set in order to obtain the 11-variable
# model.

reg.best = regsubsets (Salary~.,data=Hitters , nvmax =19)
coef(reg.best ,11)


##################################################
### 6.6 Lab 2: Ridge Regression and the Lasso

# We will use the glmnet package in order to perform ridge regression and
# the lasso. The main function in this package is glmnet(), which can be used
# to fit ridge regression models, lasso models, and more. This function has
# slightly different syntax from other model-fitting functions that we have
# encountered thus far in this book. In particular, we must pass in an x
# matrix as well as a y vector, and we do not use the y ∼ x syntax. We will
# now perform ridge regression and the lasso in order to predict Salary on
# the Hitters data. Before proceeding ensure that the missing values have
# been removed from the data, as described in Section 6.5.

library (ISLR)
fix(Hitters )
names(Hitters )
dim(Hitters )
sum(is.na(Hitters$Salary))

Hitters = na.omit(Hitters )
dim(Hitters )
sum(is.na(Hitters ))

x = model.matrix (Salary~.,Hitters )[,-1]
# x <- model.matrix (Salary~AtBat+Hits, Hitters)[,-1]
y = Hitters$Salary

# The model.matrix() function is particularly useful for creating x; not only
# does it produce a matrix corresponding to the 19 predictors but it also
# automatically transforms any qualitative variables into dummy variables.
# The latter property is important because glmnet() can only take numerical,
# quantitative inputs.

### 6.6.1 Ridge Regression

# The glmnet() function has an alpha argument that determines what type
# of model is fit. If alpha=0 then a ridge regression model is fit, and if alpha=1
# then a lasso model is fit. We first fit a ridge regression model.

library (glmnet )
grid = 10^seq (10,-2, length = 100)
ridge.mod = glmnet (x,y,alpha =0, lambda =grid)

# By default the glmnet() function performs ridge regression for an automatically
# selected range of λ values. However, here we have chosen to implement
# the function over a grid of values ranging from λ = 1010 to λ = 10−2, essentially
# covering the full range of scenarios from the null model containing
# only the intercept, to the least squares fit. As we will see, we can also compute
# model fits for a particular value of λ that is not one of the original
# grid values. Note that by default, the glmnet() function standardizes the
# variables so that they are on the same scale. To turn off this default setting,
# use the argument standardize=FALSE.

# Associated with each value of λ is a vector of ridge regression coefficients,
# stored in a matrix that can be accessed by coef(). In this case, it is a 20×100
# matrix, with 20 rows (one for each predictor, plus an intercept) and 100
# columns (one for each value of λ).

dim(coef(ridge.mod ))

# We expect the coefficient estimates to be much smaller, in terms of L2 norm,
# when a large value of λ is used, as compared to when a small value of λ is
# used. These are the coefficients when λ = 11,498, along with their L2 norm:

ridge.mod$lambda [50]
coef(ridge.mod)[,50]
sqrt(sum(coef(ridge.mod)[ -1 ,50]^2) )

# In contrast, here are the coefficients when λ = 705, along with their L2
# norm. Note the much larger L2 norm of the coefficients associated with this
# smaller value of λ.

ridge.mod$lambda [60]
coef(ridge.mod)[,60]
sqrt(sum(coef(ridge.mod)[ -1 ,60]^2) )

# We can use the predict() function for a number of purposes. For instance,
# we can obtain the ridge regression coefficients for a new value of λ, say 50:

predict (ridge.mod ,s=50, type ="coefficients")[1:20 ,]
round(predict (ridge.mod ,s=50, type ="coefficients")[1:20 ,],3)

# We now split the samples into a training set and a test set in order
# to estimate the test error of ridge regression and the lasso. There are two
# common ways to randomly split a data set. The first is to produce a random
# vector of TRUE, FALSE elements and select the observations corresponding to
# TRUE for the training data. The second is to randomly choose a subset of
# numbers between 1 and n; these can then be used as the indices for the
# training observations. The two approaches work equally well. We used the
# former method in Section 6.5.3. Here we demonstrate the latter approach.
# We first set a random seed so that the results obtained will be reproducible.

set.seed (1)
train = sample (1: nrow(x), nrow(x)/2); length(train)
test = (- train )
y.test = y[test]

# Next we fit a ridge regression model on the training set, and evaluate
# its MSE on the test set, using λ = 4. Note the use of the predict()
# function again. This time we get predictions for a test set, by replacing
# type="coefficients" with the newx argument.

ridge.mod = glmnet (x[train ,],y[train],alpha =0, lambda =grid, thresh =1e-12)
ridge.pred = predict (ridge.mod ,s=4, newx=x[test ,])
mean(( ridge.pred - y.test)^2)

# The test MSE is 101037. Note that if we had instead simply fit a model
# with just an intercept, we would have predicted each test observation using
# the mean of the training observations. In that case, we could compute the
# test set MSE like this:

mean(( mean(y[train ])-y.test)^2)

# We could also get the same result by fitting a ridge regression model with
# a very large value of λ. Note that 1e10 means 1010.

ridge.pred = predict (ridge.mod ,s=1e10 ,newx=x[test ,])
mean(( ridge.pred -y.test)^2)

# So fitting a ridge regression model with λ = 4 leads to a much lower test
# MSE than fitting a model with just an intercept. We now check whether
# there is any benefit to performing ridge regression with λ = 4 instead of
# just performing least squares regression. Recall that least squares is simply
# ridge regression with λ = 0.
# obs: 
# In order for glmnet() to yield the exact least squares coefficients when λ = 0,
# we use the argument exact=T when calling the predict() function. Otherwise, the
# predict() function will interpolate over the grid of λ values used in fitting the
# glmnet() model, yielding approximate results. When we use exact=T, there remains
# a slight discrepancy in the third decimal place between the output of glmnet() when
# λ = 0 and the output of lm(); this is due to numerical approximation on the part of
# glmnet().

ridge.pred = predict (ridge.mod ,s=0, newx=x[test ,], exact=T)
mean(( ridge.pred -y.test)^2)
lm(y~x, subset = train)
predict (ridge.mod ,s=0, exact =T, type="coefficients") [1:20 ,]

# In general, if we want to fit a (unpenalized) least squares model, then
# we should use the lm() function, since that function provides more useful
# outputs, such as standard errors and p-values for the coefficients.
# In general, instead of arbitrarily choosing λ = 4, it would be better to
# use cross-validation to choose the tuning parameter λ. We can do this using
# the built-in cross-validation function, cv.glmnet(). By default, the function
# performs ten-fold cross-validation, though this can be changed using the
# argument nfolds. Note that we set a random seed first so our results will
# be reproducible, since the choice of the cross-validation folds is random.

set.seed (1)
cv.out = cv.glmnet (x[train ,],y[train],alpha =0)
plot(cv.out)
bestlam = cv.out$lambda.min
bestlam
exp(bestlam)

# Therefore, we see that the value of λ that results in the smallest crossvalidation
# error is 212. What is the test MSE associated with this value of
# λ?

ridge.pred = predict (ridge.mod ,s=bestlam ,newx=x[test ,])
mean(( ridge.pred -y.test)^2)

# This represents a further improvement over the test MSE that we got using
# λ = 4. Finally, we refit our ridge regression model on the full data set,
# using the value of λ chosen by cross-validation, and examine the coefficient
# estimates.

out=glmnet (x,y,alpha =0)
predict (out ,type="coefficients",s=bestlam )[1:20 ,]
barplot(predict (out ,type="coefficients",s=bestlam )[1:20 ,])

# As expected, none of the coefficients are zero—ridge regression does not
# perform variable selection!

### 6.6.2 The Lasso

# We saw that ridge regression with a wise choice of λ can outperform least
# squares as well as the null model on the Hitters data set. We now ask
# whether the lasso can yield either a more accurate or a more interpretable
# model than ridge regression. In order to fit a lasso model, we once again
# use the glmnet() function; however, this time we use the argument alpha=1.
# Other than that change, we proceed just as we did in fitting a ridge model.

lasso.mod = glmnet (x[train ,],y[train],alpha =1, lambda =grid)
plot(lasso.mod)

# We can see from the coefficient plot that depending on the choice of tuning
# parameter, some of the coefficients will be exactly equal to zero. We now
# perform cross-validation and compute the associated test error.

set.seed (1)
cv.out = cv.glmnet (x[train ,],y[train],alpha =1)
plot(cv.out)
bestlam = cv.out$lambda.min
lasso.pred = predict (lasso.mod ,s=bestlam ,newx=x[test ,])
mean(( lasso.pred - y.test)^2)

# This is substantially lower than the test set MSE of the null model and of
# least squares, and very similar to the test MSE of ridge regression with λ
# chosen by cross-validation.
# However, the lasso has a substantial advantage over ridge regression in
# that the resulting coefficient estimates are sparse. Here we see that 12 of
# the 19 coefficient estimates are exactly zero. So the lasso model with λ
# chosen by cross-validation contains only seven variables.

out=glmnet (x,y,alpha =1, lambda =grid)
lasso.coef = predict (out ,type ="coefficients",s=bestlam )[1:20 ,]
lasso.coef

lasso.coef[lasso.coef !=0]
lasso.coef[lasso.coef ==0]


### 6.7 Lab 3: PCR and PLS Regression

### 6.7.1 Principal Components Regression

# Principal components regression (PCR) can be performed using the pcr()
# function, which is part of the pls library.We now apply PCR to the Hitters
# data, in order to predict Salary. Again, ensure that the missing values have
# been removed from the data, as described in Section 6.5.

library (pls)
set.seed (2)
pcr.fit = pcr(Salary ~ ., data=Hitters ,scale=TRUE ,
              validation ="CV")

# The syntax for the pcr() function is similar to that for lm(), with a few
# additional options. Setting scale=TRUE has the effect of standardizing each
# predictor, using (6.6), prior to generating the principal components, so that
# the scale on which each variable is measured will not have an effect. Setting
# validation="CV" causes pcr() to compute the ten-fold cross-validation error
# for each possible value ofM, the number of principal components used. The
# resulting fit can be examined using summary().

summary (pcr.fit )

# The CV score is provided for each possible number of components, ranging
# from M = 0 onwards. (We have printed the CV output only up to M = 4.)
# Note that pcr() reports the root mean squared error; in order to obtain
# the usual MSE, we must square this quantity. For instance, a root mean
# squared error of 352.8 corresponds to an MSE of 352.82 = 124,468.
# One can also plot the cross-validation scores using the validationplot()
# function. Using val.type="MSEP" will cause the cross-validation MSE to be
# plotted.

validationplot(pcr.fit ,val.type="MSEP")

# We see that the smallest cross-validation error occurs when M = 16 components
# are used. This is barely fewer than M = 19, which amounts to
# simply performing least squares, because when all of the components are
# used in PCR no dimension reduction occurs. However, from the plot we
# also see that the cross-validation error is roughly the same when only one
# component is included in the model. This suggests that a model that uses
# just a small number of components might suffice.
# The summary() function also provides the percentage of variance explained
# in the predictors and in the response using different numbers of components.
# This concept is discussed in greater detail in Chapter 10. Briefly,
# we can think of this as the amount of information about the predictors or
# the response that is captured using M principal components. For example,
# setting M = 1 only captures 38.31% of all the variance, or information, in
# the predictors. In contrast, using M = 6 increases the value to 88.63%. If
# we were to use all M = p = 19 components, this would increase to 100%.
# We now perform PCR on the training data and evaluate its test set
# performance.

set.seed (1)
pcr.fit = pcr(Salary~., data=Hitters ,subset =train ,scale =TRUE ,
          validation ="CV")
validationplot(pcr.fit ,val.type="MSEP")

# Now we find that the lowest cross-validation error occurs when M = 7
# component are used. We compute the test MSE as follows.

pcr.pred = predict (pcr.fit ,x[test ,], ncomp =7)
mean((pcr.pred -y.test)^2)
plot(y.test,pcr.pred)
abline(0,1)

# This test set MSE is competitive with the results obtained using ridge regression
# and the lasso. However, as a result of the way PCR is implemented,
# the final model is more difficult to interpret because it does not perform
# any kind of variable selection or even directly produce coefficient estimates.
# Finally, we fit PCR on the full data set, using M = 7, the number of
# components identified by cross-validation.

pcr.fit=pcr(y~x,scale =TRUE ,ncomp =7)
summary (pcr.fit )

### 6.7.2 Partial Least Squares

# We implement partial least squares (PLS) using the plsr() function, also
# in the pls library. The syntax is just like that of the pcr() function.

set.seed (1)
pls.fit=plsr(Salary~., data=Hitters ,subset =train ,scale=TRUE ,
           validation ="CV")
summary (pls.fit )
validationplot(pls.fit ,val.type="MSEP")

# The lowest cross-validation error occurs when only M = 2 partial least
# squares directions are used. We now evaluate the corresponding test set
# MSE.

pls.pred=predict (pls.fit ,x[test ,], ncomp =2)
mean((pls.pred -y.test)^2)

# The test MSE is comparable to, but slightly higher than, the test MSE
# obtained using ridge regression, the lasso, and PCR.
# Finally, we perform PLS using the full data set, using M = 2, the number
# of components identified by cross-validation.

pls.fit=plsr(Salary~., data=Hitters ,scale=TRUE ,ncomp =2)
summary (pls.fit )

# Notice that the percentage of variance in Salary that the two-component
# PLS fit explains, 46.40%, is almost as much as that explained using the
# final seven-component model PCR fit, 46.69 %. This is because PCR only
# attempts to maximize the amount of variance explained in the predictors,
# while PLS searches for directions that explain variance in both the predictors
# and the response.

