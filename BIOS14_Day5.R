##Day 5 BIOS14##
rm(list = ls())
##Understanding distributions that we will use for the GLM####
# rbinom(3, 10, c(0.1, 0.5, 0.9))
x <- seq(from=0, to=1, by=0.01)
v_b <- x*(1-x) #Binomial variance
plot(x, v_b, type="l", xlab="Probability", ylab="Theoretical variance", las=1)

##Using the logit function to transform our data##
logit <-  function(x) {
  return(log(x/(1-x))) 
}

invlogit <-  function(x) {
  return(1/(1+exp(-x)))
}
x <-  runif(200)
logit_x <-  logit(x)
par(mfrow = c(2,2))
hist(x, las = 1) ##From the hist we can see that the values of x are not normally distributed
hist(logit_x, las = 1) ##but if we check the logit hist then it would seem that we approach something better

xx <-  seq(-5, 5, 0.01)
plot(xx, invlogit(xx), type="l", las=1,
     xlab="Logit (x)",
     ylab="P") ##In this case we have a sigmoid function as higher values of x will make the denominator t
#to be closer and closer to being equal to 1 (converging but never reaching it) meaning that we will slowly approache
#a probability of 1 as we go to infinity but also with characteristics of slower and accelerated
#parts of the curve as in any sigmoid
plot(x, invlogit(logit_x), las=1) #With this plot we actually check that invlogit is indeed the invers
#of the logit function we are using
#We can also see this as the inverse logit is a sigmoid with limits in the y axis whereas the 
#logit function if we consider the x axis instead. 

##Here we check that another possibility is to use a probit link
plot(xx, invlogit(xx), type="l", las=1,
     xlab="Logit/Probit (x)",
     ylab="P") #The probit is essentially the cumulative density function of the normal distribution
#Depending on the variance of the probit we then have a more sharp or relaxed increase. In this case
#we don't really focus on this but it is very interesting to know
#See formula in my derivatives function to understand the CDF of a normal
lines(xx, pnorm(xx), lty=2)
legend("topleft", legend=c("Logit", "Probit"),
       lty=c(1,2))

##Logistic regression##

x <-  rnorm(200, 10, 3)
eta <-  -2 + 0.4*x + rnorm(200, 0, 2)
p <-  invlogit(eta)
y <-  rbinom(200, 1, p)
par(mfrow=c(1,3))
plot(x, eta, las=1) #plot of how it will look after the transformation
plot(x, p, las=1) #plot of the invlogit to understand how the function works with the link y = g^-1(n)
plot(x, y, las=1) #plot of how the data actually looks initially

##Linear mixed model##
m <-  glm(y~x, family = binomial(link="logit"))
summary(m)

##Trying to replicate a plot that is given##
coefs <-  summary(m)$coef
x_pred <-  seq(from=min(x), to=max(x), by=0.01)
y_hat <-  coefs[1,1] + coefs[2,1]*x_pred
p_hat <-  invlogit(y_hat)
value_x <- which(p_hat >= 0.5)[1]
par(mfrow = c(1,1))
plot(x, y) #plotting our x and y values 
lines(x_pred, p_hat) #We're plotting the regression line of the model
abline(h = 0.5, lty = 2) #We're checking where we have a probability of 0.5
abline(v = x_pred[value_x], lty = 2) #Checking the value of x for a probability 0.5

##Getting kinda r_squared form GLM
library(MuMIn)
r.squaredGLMM(m)#for the r_squared it's how much of the variance is explained by the predictors
#we should take the better method. We take the value on the left (fixed factors) as the ones
#on the right correspond to random effects

##Coefficient of discrimination##
y_hat <-  coefs[1,1] + coefs[2,1]*x
p_hat <-  invlogit(y_hat)
mean(p_hat[which(y==1)]) - mean(p_hat[which(y==0)]) #This tells us the distance between the capacity
#of the model to check for successes and failures. The closer D is to 1 it means that it really
#discriminates well between successes (1) and failures (0) and the lower it is then it is 
#not as good at discriminating them

##Working with real data!!!####

dat <-  read.csv("dormancy.csv")
names(dat)

##Fitting the same model in two different ways##

subdat <-  dat[dat$pop=="CC",] ##subdata of the dataset that we have
germ <-  subdat$germ2 * subdat$nseed #Successes as the proportion of seeds that were germinated multiplied by the number of seeds sown
#we can have 0 germinations or 1 or 2
notgerm <-  subdat$nseed - germ #Failures as the number of seeds that were not germinated 
#we have values of 2 (not germinated at all), 1 (half germinated) and 0 (all germinated)
mod1 <-  glm(cbind(germ, notgerm) ~ timetosowing, "binomial", data=subdat)
mod2 <-  glm(germ2 ~ timetosowing, "binomial", weights=nseed, data=subdat)
logLik(mod1) == logLik(mod2)

##Estimate duration of after-ripening required for expected germination to be 0.5
mod3 <-  glm(germ2 ~ timetosowing + MCseed, "binomial", weights=nseed, data=subdat)
summary(mod3)

##Plotting our model##
plot(subdat$timetosowing, subdat$germ2,
     xlab="Duration of after-ripening (days)",
     ylab="Germination rate", las=1)

##Now we bring back the model into the scale we want to check it
xvals <- seq(min(subdat$timetosowing, na.rm=T),
            max(subdat$timetosowing, na.rm=T), 0.01)

coefs = summary(mod3)$coef
y_hat = coefs[1,1] + coefs[2,1]*xvals
lines(xvals, invlogit(y_hat))
y_hat2 = coefs[1,1] + coefs[2,1]*xvals + coefs[3,1]*sd(subdat$MCseed)
lines(xvals, invlogit(y_hat2), lty=2)
y_hat3 = coefs[1,1] + coefs[2,1]*xvals - coefs[3,1]*sd(subdat$MCseed)
lines(xvals, invlogit(y_hat3), lty=2)

-coefs[1,1]/coefs[2,1]## after-ripening needed for a 50% germination rate
abline(v = -coefs[1,1]/coefs[2,1], lty = 2)
abline(h = 0.5, lty = 2)


#Seed size effect
-(coefs[1,1] + coefs[3,1]*sd(subdat$MCseed))/coefs[2,1]
-(coefs[1,1] - coefs[3,1]*sd(subdat$MCseed))/coefs[2,1]