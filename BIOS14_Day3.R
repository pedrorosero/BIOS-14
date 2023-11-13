rm(list = ls())

##ANOVA from simluated data##


set.seed(100)
groups <-  as.factor(rep(c("Low", "Medium", "High"), each=50))
x <-  c(rnorm(50, 10, 3), rnorm(50, 13, 3), rnorm(50, 14, 3))
plot(groups, x, las=1, xlab="",
     ylab="Body size (g)")

###How to also add the points to the boxplot##
stripchart(x ~ groups,
           data = NULL,
           method = "jitter",
           pch = 1, col = "grey",
           vertical = TRUE,
           add = TRUE)
##Now we include also the means of each group in black 
meanbygroup <- aggregate(x~groups, FUN=mean)
stripchart(meanbygroup$x ~ meanbygroup$groups,
           data = NULL,
           method = "jitter",
           pch = 19, col = "black",
           vertical = TRUE,
           add = TRUE)

##Let's perform the ANOVA!!!##

m = lm(x~groups)
myanova <- anova(m)

#Calculating the sum of squares of our ANOVA
SS_T <-  myanova$`Sum Sq`[1] + myanova$`Sum Sq`[2]

##Total variance of my model##
tot_var <- SS_T/sum(myanova$Df)
var(x) ## we can check that it's the same variance as the previous line

##We can know how much the groups explain the total variance we have##

groups_var <- myanova$`Sum Sq`[1]
propvar_groups <- groups_var/SS_T ##21% of the variance is explained by the groups  

##Summary of the anova##
summary(m)

##In this case the control group is "High" because of the alphabetic order
##So the anova will first calculate the estimated mean of this factor
##The next two estimates is the comparison between the first estimated factor and
##the on that we first estimated (to see if there is a difference)

###We can change the order in which we want the factors to be assessed:
groups <-  factor(groups, levels=c("Low", "Medium", "High"))
m <- lm(x~groups)
summary(m)

##This is interesting because in essence we don't end up having a pairwise 
##one-on-one analysis with the anova so it is indeed necessary to perform a 
##post-hoc if we want to have that full information afterwards

##if we want to only extract the mean of each group we can do it as follows: 

m <-  lm(x ~ groups - 1) #The minus 1 here removes the intercept
summary(m)$coef
confint(m) #confindence interval of m



####Two-way ANOVA####
##Load dataset##

dat <- read.csv("butterflies.csv", sep = ";",stringsAsFactors=T) #dataset
#We add a letter to maternal and larval to merge them but knowing if they are maternal
#Or larval host. We thus create 4 groups in total
dat$MaternalHost <-  paste0(datt$MaternalHost, "M") #Add a letter to maternal
dat$LarvalHost <-  paste0(dat$LarvalHost, "L") #Add letter to Larval
means <-  tapply(dat$DevelopmentTime, list(dat$MaternalHost, dat$LarvalHost), mean)
means

##Plot of devlopmental time (days)##
{
par(pty = "s", xpd = T)
plot(means[1,], type = "b", ylim = c(min(means)-1, max(means)+1),
     ylab = "Devlopmental time (days)", las = 1, xlab = "Larval Host",
     xaxt = "n", xlim = c(0.75,2.25), cex.lab = 1.5)
segments(x0 = 1, x1 = 1, y0 = min(means)-1.5, y1 = min(means)-1.75)
segments(x0 = 2, x1 = 2, y0 = min(means)-1.5, y1 = min(means)-1.75)
text(1, min(means)-2.5, "Barbarea", cex = 1.1)
text(2, min(means)-2.5, "Berteroa", cex = 1.1)
lines(means[2,], type = "b", lty = 2, pch = 16)

legend("topleft", pch = c(1, 16), legend = c("Barbarea", "Berteroa"),
       title  = "Maternal host", bty = "n", 
       inset = 0.1)
}

devtm <- lm(dat$DevelopmentTime~dat$LarvalHost*dat$MaternalHost)
summary(devtm)
anova(devtm)
par(mfrow = c(2,2))
plot(devtm)
par(mfrow = c(1,1))
hist(dat$DevelopmentTime) ##Not normally distributed, looks horrible

##Plot of adult weight (g)##
meansAW <- tapply(dat$AdultWeight, list(dat$MaternalHost, dat$LarvalHost), mean)
{
  par(pty = "s", xpd = T)
  plot(meansAW[1,], type = "b", ylim = c(min(meansAW)-1, max(meansAW)+1),
       ylab = "Adult Weight (g)", las = 1, xlab = "Larval Host",
       xaxt = "n", xlim = c(0.75,2.25), cex.lab = 1.5)
  segments(x0 = 1, x1 = 1, y0 = min(meansAW)-2.5, y1 = min(meansAW)-1.75)
  segments(x0 = 2, x1 = 2, y0 = min(meansAW)-2.5, y1 = min(meansAW)-1.75)
  text(1, min(meansAW)-3.5, "Barbarea", cex = 1.1)
  text(2, min(meansAW)-3.5, "Berteroa", cex = 1.1)
  lines(meansAW[2,], type = "b", lty = 2, pch = 16)
  legend("topright", pch = c(1, 16), legend = c("Barbarea", "Berteroa"),
         title  = "Maternal host", bty = "n", 
         inset = 0.1)
}
AWm <- lm(dat$AdultWeight~dat$LarvalHost*dat$MaternalHost)
summary(AWm)
anova(AWm)
par(mfrow = c(2,2))
plot(AWm)
par(mfrow = c(1,1))
hist(dat$AdultWeight)

##Plot of growth date g.day-1)##
meansGR <- tapply(dat$GrowthRate, list(dat$MaternalHost, dat$LarvalHost), mean)

{
  par(pty = "s", xpd = T)
  plot(meansGR[1,], type = "b", ylim = c(min(meansGR), max(meansGR)),
       ylab = expression("Growth rate (g.days"^-1~")"), las = 1, xlab = "Larval Host",
       xaxt = "n", xlim = c(0.75,2.25), cex.lab = 1.5)
  segments(x0 = 1, x1 = 1, y0 = min(meansGR)-0.0025, y1 = min(meansGR)-0.0012)
  segments(x0 = 2, x1 = 2, y0 = min(meansGR)-0.0025, y1 = min(meansGR)-0.0012)
  text(1, min(meansGR)-0.004, "Barbarea", cex = 1.1)
  text(2, min(meansGR)-0.004, "Berteroa", cex = 1.1)
  lines(meansGR[2,], type = "b", lty = 2, pch = 16)

  legend("bottomleft", pch = c(1, 16), legend = c("Barbarea", "Berteroa"),
         title  = "Maternal host", bty = "n", 
         inset = 0.1)
}
devGRm <- lm(dat$GrowthRate~dat$LarvalHost*dat$MaternalHost)
summary(devGRm)
devGRanova <- anova(devGRm)

# plot(dat$DevelopmentTime~dat$LarvalHost*dat$MaternalHost)
