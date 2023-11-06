rm(list = ls())
##Getting started: creating an artificial set of data in here so we can see linear relationships##

set.seed(85) #set the 
x <-  rnorm(n=200, mean=10, sd=2) #create a series of random numbers as the predictor variable
y <-  0.4*x + rnorm(200, 0, 1) #use the predictor variable to calculate the response variable
                             #note that for the response variable we are including a random number so there is not
                             # a complately linear relationship but some variation from the predictor
par(pty = "s")
plot(x, y, las=1, #makes the x-axis horizontal!
     xlab="Leaf length (mm)",
     ylab="Leaf width (mm)") #plot everything and let's think of it as leaf length and width
#Using linear model#

m <- lm(y~x) #linear model
# str(m) #structure of m

cf <- m$coefficients #extract coefficients
# abline(a = cf[1], b = cf[2]) #add the line of the predicted variables

predvals <- cf[1] + cf[2] * x#calculate the actual predicted variables

#plot everything together#
par(mfrow=c(1,2)) #we create a plot with two panels
plot(x, y, las=1,
     xlab="Leaf length (mm)",
     ylab="Leaf width (mm)") #we plot x as a function of y
abline(m) #we include the line with the predicted values
segments(x, y, x, predvals) #we add segments going from x to y that remain in x
hist(residuals(m), xlab="", las=1) #We check that the residuals seem normally distributed

par(mfrow = c(2,2))
plot(m) #this gives us the full information of our model, we have (by panel):
#1) the residuals vs fitted
#2) Q-Q plot between standardized residuals and theoretical quantiles
#3) sqrt(standardized) residuals as a funciton of fitted values
#4) residuals vs leverage 

##Another way of plotting the fitted model but this time within range of variables#

newx <-  seq(min(x), max(x), length.out=200) #we know that the sequence will not be out of boundaries
predy <-  cf[1] + cf[2]*newx #we have the vector of variables
par(mfrow = c(1,1))
plot(x, y, las=1,
     xlab="Leaf length (mm)",
     ylab="Leaf width (mm)")
lines(newx, predy) #lines only in the range of our datapoints

##Check now the summary of our model##

summary(m) #median close to 0 = values are close to 0 in median and 1a and 3q are 
# similarly distanced to the median (symmetrical)


##Exercise: bootstrapping (non-parametric) to derive standard error from slope##
df <- cbind.data.frame(x,y)
n_samples <- 1000
slope <- numeric(n_samples)

for(i in 1:n_samples){
  samplexy <-  df[sample(1:nrow(df), replace = T),]
  m <- lm(samplexy$y~samplexy$x)
  slope[i] = m$coefficients[2]
}
sdslope <- sd(slope)
n <- n_samples
se_slope <- sqrt(var(slope)/length(x))
print(se_slope)

##checking that the slope is following the formula##
cov(x,y) / var(x) #it does  
(cf[2]*(mean(x) + sd(x))) - (cf[2]*mean(x)) #we can ignore the intercept here because 
#the increase is too big perhaps?

cor(x,y) ^ 2 #R-squared statistic #43% of the variance is explained by x
y_hat <- cf[1] + cf[2]*x
var(y_hat)
var(y_hat)/var(y)
cf[2]^2*var(x)

##Working with real data##

birds <- read.csv("bird_allometry.csv")
m <- lm(log(birds$brain_mass)~log(birds$body_mass))
plot(log(birds$brain_mass)~log(birds$body_mass), bg = "lightblue",
     pch = 21)
cf <- m$coefficients
newx <-  seq(min(log(birds$body_mass)), max(log(birds$body_mass)), length.out=nrow(birds)) #we know that the sequence will not be out of boundaries
predy <-  cf[1] + cf[2]*newx #we have the vector of variables
lines(predy~newx)
#if we check all the statistics and so on we see that Rsquare is very high so strong significance
#also standard error is very low compared to the estimate so that works well also


##Optional exercise##


sd_vector <- seq(0, 1, length.out = 1000)
for (i in seq_along(sd_vector)) {
  x <- rnorm(200, 0, sd_vector[i])
  y <- x * 0.4
  m <- lm(y~x)
  m$coefficients[2]
  
  
}
