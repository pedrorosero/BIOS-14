##First day BIOS14
##Simulating data from distributions####

x = rnorm(n=1000000, mean=5, sd=1)
mean(x)
sd(x)

hist(x, las=1, main="")

##Bootstrapping ####
set.seed(1)
x = rnorm(50, 10, 2)
se_x = sqrt(var(x)/length(x))

out = NULL
cvout <- NULL
for(i in 1:10000){
  sample = sample(x, replace=TRUE)
  out[i] = mean(sample)
  cvout[i] <- sd(sample)/mean(sample)
}
hist(out, las=1, main="")

sd(out)
se_x
quantile(out, c(0.025, 0.975))
mean(x) - 1.96*se_x
mean(x) + 1.96*se_x

##Calculating 95% confidence interval for the CV
cvquantile <- quantile(cvout, c(0.025, 0.975))
print(cvquantile)
hist(cvout, las=1, main="")
abline(v = cvquantile[1])
abline(v = cvquantile[2])




##Exercise extra####

sd_vector <- seq(0, 12, length.out = 1000)
sdcvmat <- matrix(ncol = 2, nrow = length(sd_vector))

for (i in seq_along(sd_vector)){
  x <- rnorm(100, 50, sd_vector[i])
  sdx <- sd(log(x))
  cvx <- sd(x)/mean(x)
  sdcvmat[i,1] <- sdx
  sdcvmat[i,2] <- cvx
}

plot(sdcvmat[,1]~sdcvmat[,2])
# abline(lm(sdcvmat[,1]~sdcvmat[,2]), col = "red")
abline(a = 0, b = 1, col = "blue", lwd = 3)


fdsanfooidsunfioqw <- 1243243