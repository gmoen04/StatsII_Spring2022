#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

lapply(c(),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#####################
# Problem 1
#####################
# Note 1: We must compare the model CDF of the normal distribution to the cauchy
# empirical CDF to see if the distributions match
set.seed(1234)

data_1 <- (rcauchy(1000, location = 0, scale = 1)) # dataset from the cauchy distribution
empirical <- rnorm(1000, mean = 0, sd = 1) # dataset from the normal distribution for reference

#plotting the distributions on a chart
df_g1 <- data.frame(height = round(rcauchy(1000, location = 0, scale = 1)))
head(df)
df_g2 <- data.frame(height = round(rnorm(200, mean = 0, sd = 1)))
head(df)

ggplot(df_g1, aes(height)) + stat_ecdf(geom = "point") # See "Plot 1" below
ggplot(df_g2, aes(height)) + stat_ecdf(geom = "point") # See "Plot 2" below

ks.test(data_1, "pnorm") # built-in test comparing data_1 with normal distribution
# Results D = 0.14802
# p-value < 0.00000000000000022

#Function calculating the same values
Kol_Smir <- function(data_1, empirical) {
  n = 1000 # set the desired sum limit
  ECDF <- ecdf(data_1)
  empiricalCDF <- ECDF(data_1) # ecdf from rauchy distribution
  D <- max(abs(empiricalCDF - pnorm(empirical))) # max value of difference in ECDFs
  summed <- NULL #try to convert the D-value to a p-value and print it
    for(i in 1:33){
    summed <- c(summed, exp((-(2*i - 1)^2*pi^2) / ((8*D)^2)))
    }
  pValue <- sqrt(2*pi)/D*sum(summed)
  print(pValue)
}
print(D)

# With a p-value of less than 0.05 we can reject te null hypothesis and say that our
# sample data doesn't come from the normal distribution

#####################
# Problem 2
#####################

set.seed(1234)
x = runif(200, 1, 10)
data <- data.frame(x = runif(200, 1, 10))
data$y <- 0 + 2.75*data$x + rnorm(200, 0, 1.5) # intercept + slope*x + noise
pdf("data_dist.pdf")
plot(data$x, data$y)
dev.off()

# OLS regression with Newton-Raphson algorithm (BFGS)

# derive our log-likelihood function for uniform distribution
uniform_likelihood <- function(outcome, input, parameter) {
  p <- exp(parameter[1] + parameter[2]*input)/(1+exp(parameter[1] + parameter[2]*input))
  -sum(dunif(outcome, 1, p, log=TRUE))
}
#optimise our log-likelihood function
results_uniform <- optim(fn=uniform_likelihood, outcome=data$y, input=x, par=0:1, hessian=T, method="BFGS")
  
#coefficients
results_uniform$par
  
#lm function to confirm equivalent results
coef(lm(y ~ x, data=data)) 
# Intercept of 0.2956 and x value = 2.7221
#
