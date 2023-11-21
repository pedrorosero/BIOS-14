rm(list = ls())
library(MASS)
library(MuMIn)
##BIOS 14 - Day 6 - GLMM poisson and negative binomial##

#Artificial data#
x <-  rpois(200, 3)
hist(x, las=1)

#Different representation for the poisson distribution with different
#values of lamba
x <-  seq(0, 20, 1)
y <-  dpois(x, lambda=1)
plot(x,y, type="b", las=1, xlab="k", ylab="P(x=k)", pch=16, col=1)
points(x, dpois(x, lambda=3), type="b", pch=16, col=2)
points(x, dpois(x, lambda=10), type="b", pch=16, col=3)
legend("topright", col=1:3, pch=16,
       legend=c(expression(paste(lambda, " = 1")),
                expression(paste(lambda, " = 3")),
                expression(paste(lambda, " = 10"))))

##Simulated data for GLM

x <-  rnorm(200, 10, 3) #we get values of x from a normal distribution
eta <-  -2 + 0.2*x #we create a linear variation of x 
y <-  ceiling(exp(eta + rpois(200, 0.3))) #we have an exponential of the data to have a transformation and also
#values that are entire numbers 
par(mfrow=c(1,2))
plot(x, eta, las=1)
plot(x, y, las=1)

m <-  glm(y~x, family="poisson")
summary(m) #the values are reported here in a log-scale, if we want the actual values
#to have a representation of what they initially meant we can transform them back through
#the exponential

##Predicted values with standard error

plot(x, y, las=1, col="darkgrey", pch=16)
xx <-  seq(min(x), max(x), 0.01)
y_hat <-  predict(m, newdata=list(x=xx), type="response", se.fit=T) 
lines(xx, y_hat$fit) ##Fitted model back-transformed and predicted values plotted
# lines(xx, y_hat$fit+1.96*y_hat$se.fit, lty=2) #predicted values with Standard Error interval
# lines(xx, y_hat$fit-1.96*y_hat$se.fit, lty=2) #predicted values with Standard Error interval

polygon(c(xx, rev(xx)),
        c(y_hat$fit+1.96*y_hat$se.fit,
          rev(y_hat$fit-1.96*y_hat$se.fit)),
        col = rgb(0,1,0,.5), border = FALSE) 
##Here we plot the confidence interval as a polygon, this is interesting

##Let's check now the pseudo r-squared that we have here.

r.squaredGLMM(m)

1-(m$deviance/m$null.deviance)

##Let's check overdispersion now 

set.seed(1)
x <-  rnorm(200, 10, 3)
eta <-  -2 + 0.2*x
y <-  floor(exp(eta + rnbinom(200, 1, mu=.8)))
par(mfrow=c(1,2))
plot(x, eta, las=1)
plot(x, y, las=1)

m <-  glm(y~x, family="poisson")
summary(m)



##negative binomial distribution####
m <-  glm.nb(y~x)
summary(m)



###Real dataset --> report exercise####
dat <-  read.csv("Eulaema.csv")
head(dat)
par(pty = "s")
##Checking our data first##
plot(dat$Eulaema_nigrita, xaxt = "n", xlab = "", 
     ylab = expression(italic("E. nigrita")~"Abundance")) #we have shitloads of overdispersion
m <- glm(Eulaema_nigrita~MAP+forest.+Pseason, data = dat, family = "poisson")
summary(m)

m <- glm.nb(Eulaema_nigrita~MAP+forest.+Pseason, data = dat)
summary(m)
1- m$deviance/m$null.deviance #pseudo Rsquared
# plot(dat)
#the altitude and temperature are correlated (makes sense)
#MAT and Tseason also kinda 

par(mfrow = c(2,2))

##Plotting the values of Eulaema_nigrita
plot(dat$Eulaema_nigrita~dat$forest., pch = 1, col = "grey", 
     xlab = "Forest cover (proportion)", 
     ylab = expression(italic("E. nigrita")~"Abundance"),
     las = 1)


newforest <-  seq(min(dat$forest.), max(dat$forest.), length.out=200)
newMAP <-  rep(mean(dat$MAP), length(newforest))
newTseason <- rep(mean(dat$Tseason), length(newforest))
newPseason <- rep(mean(dat$Pseason), length(newforest))
y_hat <-  predict(m, newdata=list(MAP = newMAP, Pseason = newPseason,
                                forest.=newforest),
                type="response", se.fit = T)
lines(newforest, y_hat$fit,lwd=3, col = "red")

polygon(c(newforest, rev(newforest)),
        c(y_hat$fit+1.96*y_hat$se.fit,
          rev(y_hat$fit-1.96*y_hat$se.fit)),
        col = rgb(1,0.25,0,0.25), border = FALSE)

##Mean annual precipitation##
plot(dat$Eulaema_nigrita~dat$MAP, pch = 1, col = "grey", 
     xlab = "Mean Annual Precipitation (mm)", 
     ylab = expression(italic("E. nigrita")~"Abundance"),
     las = 1)


newMAP <-  seq(min(dat$MAP), max(dat$MAP), length.out = 200)
newforest <-  rep(mean(dat$forest.), length(newMAP))
newTseason <- rep(mean(dat$Tseason), length(newforest))
newPseason <- rep(mean(dat$Pseason), length(newforest))
y_hat <-  predict(m, newdata=list(MAP = newMAP, Pseason = newPseason,
                                  forest.=newforest),
                  type="response", se.fit = T)
lines(newMAP, y_hat$fit,lwd=3, col = "blue")

polygon(c(newMAP, rev(newMAP)),
        c(y_hat$fit+1.96*y_hat$se.fit,
          rev(y_hat$fit-1.96*y_hat$se.fit)),
        col = rgb(0.68,0.84,0.9,0.5), border = FALSE)

##seasonal precipitation##
plot(dat$Eulaema_nigrita~dat$Pseason, pch = 1, col = "grey", 
     xlab = "Precipitation seasonality (%)", 
     ylab = expression(italic("E. nigrita")~"Abundance"),
     las = 1)

newPseason <- seq(min(dat$Pseason), max(dat$Pseason), length.out = 200)
newMAP <-  rep(mean(dat$MAP), length(newPseason))
newforest <-  rep(mean(dat$forest.), length(newMAP))
newTseason <- rep(mean(dat$Tseason), length(newforest))
y_hat <-  predict(m, newdata=list(MAP = newMAP, Pseason = newPseason,
                                  forest.=newforest),
                  type="response", se.fit = T)
lines(newPseason, y_hat$fit,lwd=3, col = "green")

polygon(c(newPseason, rev(newPseason)),
        c(y_hat$fit+1.96*y_hat$se.fit,
          rev(y_hat$fit-1.96*y_hat$se.fit)),
        col = rgb(0.56,0.93,0.56,0.5), border = FALSE)


##Hurdle model####
#two separate analysis where the first analysis part 
#takes all the zeros and the other counts given where at least 1 was observed
y1 <- ((y>1)*1)

m1 <-  glm(y1~x, family="binomial" (link="logit"))
y2 <-  y
y2[which(y==0)] = NA
m2 <-  glm(y2~x, family="poisson", na=na.exclude)

coefs1 <-  summary(m1)$coef
coefs2 <-  summary(m2)$coef
y_hat1 <-  coefs1[1,1] + coefs1[2,1]*x
y_hat2 <-  coefs2[1,1] + coefs2[2,1]*x
y_pred <-  invlogit(y_hat1)*exp(y_hat2)
par(mfrow=c(1,3))

invlogit <-  function(x) {
  return(1/(1+exp(-x)))
}
plot(x, invlogit(y_hat1), las=1)
plot(x, exp(y_hat2), las=1)
plot(x, y_pred, las=1)
