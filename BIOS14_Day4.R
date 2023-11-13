rm(list = ls())

##Multiple regression####

##Artificial data##

set.seed(187)
x1 <-  rnorm(200, 10, 2) #predictor variable 1
x2 <-  0.5*x1 + rnorm(200, 0, 4) #predictor variable 2
y <-  0.7*x1 + 2.2*x2 + rnorm(200, 0, 4) #response variable dependent on x1 and x2
m <-  lm(y~x1+x2) #fit multiple regression model
coefs <-  summary(m)$coef #coefficients of the multiple regression
summary(m) #summary table

##If we do the variance of the predicted values##

y_hat <- coefs[1,1] + coefs[2,1]*x1 + coefs[3,1]*x2 #predicted values 
#coefs[1,1] is the intercept, coefs[2,1] is the slope of x1 and coefs[3,1] slope of x2
var(y_hat) #we can then calculate the variance of the predicted values of y

##Total variance explained by the model##

var(y_hat)/var(y) #how much of the variance is explained by the variance of the predicted values?
#in this case we have 86.82% which is the r^2 value that we find in the summary of m (line 11)

##Now let's check the variance explained by each one of the predictors x1 and x2 
##we do this by keeping x1 and x2 constant at their mean each time. 

#x1
y_hat1 <- coefs[1,1] + coefs[2,1] * x1 + coefs[3,1] * mean(x2)
var(y_hat1) #variance of x1
var(y_hat1) / var(y) #x1 alone explains 1.63 of the variance of the response variable

#x2

y_hat2 <- coefs[1,1] + coefs[2,1] * mean(x2) + coefs[3,1] * x2
var(y_hat2) #variance of x2
var(y_hat2) / var(y)  #x2 alone explains 80.59 of the variance of the response variable

##Let's check the contribution of both variances

var(y_hat1) + var(y_hat2) # = 80.902 whereas var(y_hat) = 85.4221
#This means that part of the variance of the predicted values is not given by the effect
#of the sum of the variances alone. This means that the two variables are not independent
#they would be independent of COV(X,Y) = 0. This is not the case here, meaning that 
#Both variables are correlated to some extent at least
#The formula of the total variance of two dependent variables is as follows: 
#Var(x+y) = Var(x) + Var(y) + 2Cov(x,y)

##In our case we have two predictors that appear to be related to some extent
2*cov(y_hat1, y_hat2) #giving the rest of the part of the variance explained by the predicted values

##We end up having
var(y_hat1) + var(y_hat2) + 2*cov(y_hat1, y_hat2) #back to the variance of y_hat

##Another way to calculate the independent variances is with the formula
##V(x) = beta(x)^2*var(x)
coefs[2,1]^2*var(x1) #We find the same variance of x1 we had before
coefs[3,1]^2*var(x2) #We find the same variance of x2 we had before

##MATRIX FORM!!!

t(coefs[2:3,1]) %*% cov(cbind(x1,x2)) %*% coefs[2:3,1]

##Standardize predictor variables and get slopes with units of standard deviations

x1_z <- (x1 - mean(x1))/sd(x1) #standardize x1
x2_z <-  (x2 - mean(x2))/sd(x2) #standardize x2
m <-  lm(y ~ x1_z + x2_z) #new model with standardized values 
summary(m) #summary of the new model

##Santandardize predictor variables and get slopes with units of means
##These slopes are technically called elasticity the mean is the same as mean(y) once again

x1_m <-  (x1 - mean(x1))/mean(x1)
x2_m <-  (x2 - mean(x2))/mean(x2)
summary(lm(y ~ x1_m + x2_m))

##Variance inflation factors to check multicollinearity

m1 <-  lm(x1~x2)
r2 <-  summary(m1)$r.squared
VIF <- 1/(1-r2)
print(VIF)

##Working with real data##

plants <-  read.csv(file="alpineplants.csv")

carexm <- lm(plants$Carex.bigelowii ~ plants$mean_T_winter + plants$max_T_winter + 
               plants$min_T_winter + plants$mean_T_summer + plants$max_T_summer + 
               plants$min_T_summer + plants$light + plants$snow + plants$soil_moist + 
               plants$altitude)
coefs <- summary(carexm)$coef

summary(carexm) #our rsquared is actually not very explanatory

#check the variance of each of the predictors
carexvars <- numeric(nrow(coefs)-1)
yhatmat <- matrix(ncol = 10, nrow = nrow(plants))
y_hat <- numeric(nrow(plants))
for (i in 2:nrow(coefs)) {
  carexvars[i-1] <- coefs[i,1]^2 * var(plants[,(i + 1)], na.rm = T)
  yhatmat[,(i-1)] <- coefs[i,1]*plants$Carex.bigelowii
  y_hat <- y_hat + coefs[i,1]*plants$Carex.bigelowii
}

y_hat <- coefs[1,1] + y_hat
var(y_hat) / var(plants$Carex.bigelowii)

##Need to spend more time on this later as I'm not managing right now


####ANCOVA####

##Artificial data##

set.seed(12)
x <-  rnorm(200, 50, 5)
gr <-  factor(c(rep("Male", 100), rep("Female", 100)))
y <-  -2 + 1.5*x + rnorm(200, 0, 5)
y[101:200] <-  2 + 0.95*x[101:200] + rnorm(100, 0, 6) #We make sure that the other part of the data is different in terms of all
plot(x, y, pch=c(1,16)[as.numeric(gr)], las=1)

##Analysis of ANCOVA##
m <-  lm(y~x*gr)
anova(m)
summary(m)

##If we want to not have a reference group

m2 <-  lm(y ~ -1 + gr + x:gr)
summary(m2)