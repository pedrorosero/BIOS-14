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

dat <- read.csv("butterflies.csv", sep = ";") #dataset
#We add a letter to maternal and larval to merge them but knowing if they are maternal
#Or larval host. We thus create 4 groups in total
dat$MaternalHost <-  paste0(dat$MaternalHost, "M") #Add a letter to maternal
dat$LarvalHost <-  paste0(dat$LarvalHost, "L") #Add letter to Larval
means <-  tapply(dat$DevelopmentTime, list(dat$MaternalHost, dat$LarvalHost), mean)
means





