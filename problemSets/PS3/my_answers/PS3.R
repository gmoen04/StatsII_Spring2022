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

# load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

lapply(c("stargazer", "stringr", "tidyverse", "ggplot2", "nnet", "pscl", "AER"),  pkgTest)

# set wd for current folder
setwd("/Users/garethmoen/Documents/PG Dip - ASDS/Stats II/MyProblemSets/PS3/my_answers")

#### Problem 1 ####
#### 1. Unordered multinomial logit ####

# load data
gdp <- read.csv("gdpChange.csv")

# View data structure
summary(gdp)
str(gdp)

# Convert values to character strings
gdp2 <- gdp 
gdp2$GDPWdiff2 <- gdp$GDPWdiff
  
gdp2$GDPWdiff2[gdp2$GDPWdiff == 0] <- "no change"
gdp2$GDPWdiff2[gdp2$GDPWdiff < 0] <- "negative"
gdp2$GDPWdiff2[gdp2$GDPWdiff > 0] <- "positive"

# Remove order from factors
#gdp2$GDPWdiff2 <- factor(gdp2$GDPWdiff2, ordered = FALSE)

# Set reference level for factors
gdp2$GDPWdiff2 <- relevel(as.factor(gdp2$GDPWdiff2), ref = "no change")

# Remove additional column data
gdp2 <- subset(gdp2, select = c(REG, OIL, GDPWdiff2))

summary(gdp2)
str(gdp2)

# Unordered multinomial logit
multinom_1 <- multinom(GDPWdiff2 ~ REG + OIL, data = gdp2)

# Getting multinom_1 information, including coefficients
multinom_1

# Specifically coefficients
coef_1 <- exp(coef(multinom_1))

# Cutoff points
confint_exp_m1 <- exp(confint(multinom_1))

# Interpretation of coefficients and cutoff points

# So in any particular country, there is a 5.865 times increase in the baseline
# odds that there will be positive GDP growth if that country is a democracy. 

# The 95% confidence intervals for this 5.865 value are 1.304 & 26.373

#### 2. Ordered Multinomial Logit ####

gdp3 <- gdp 
gdp3$GDPWdiff3 <- gdp$GDPWdiff

gdp3$GDPWdiff3[gdp3$GDPWdiff == 0] <- "no change"
gdp3$GDPWdiff3[gdp3$GDPWdiff < 0] <- "negative"
gdp3$GDPWdiff3[gdp3$GDPWdiff > 0] <- "positive"

str(gdp3)

# Remove additional column data
gdp3 <- subset(gdp3, select = c(REG, OIL, GDPWdiff3))

# Check new data structure
summary(gdp3)

#Check nature of ordering for gdp3$GDPWdiff3
is.ordered(gdp3$GDPWdiff3) # returns FALSE

# Make it ordered
as.ordered(gdp3$GDPWdiff3) # returns "Levels: negative < no change < positive"

summary(gdp3)

# Ordered multinomial logit
multinom_2 <- multinom(GDPWdiff3 ~ REG + OIL, data = gdp3)

# Getting multinom_2 information, including coefficients
multinom_2

# Specifically coefficients
exp(coef(multinom_2))

# And cutoff points
exp(confint(multinom_2))

# Interpretation of coefficients and cutoff points for ordered multinomial
# In our ordered multonomial logit with "negative" set as our reference there
# is a 1.476 times increase in the baseline odds that there will be positive 
# GDP growth if that country is a democracy.

# The 95% confidence intervals for this 1.476 value are 1.273 & 1.712

#### Q2 ####
#### a ####

# Load data
data <- read.csv("MexicoMuniData.csv")
View(data)
str(data)

# Convert variables to factors
as.factor(data$PAN.governor.06)
as.factor(data$competitive.district)

# Poisson Regression Model
data_poisson <- glm (PAN.visits.06 ~ competitive.district + marginality.06 + PAN.governor.06, data=data, family=poisson)
stargazer(data_poisson, type = "text")

# From these results it looks like being a competitive district is not is a 
# significant predictor of increased visits.

coeffs <- coefficients(data_poisson)
coeffs_exp <- exp(coeffs)

# A 1 unit increase in competitiveness (from 0 to 1, safe to close / swing 
# district) decreases our expected number of visits by a multiplicative factor
# of 0.92 (the exponent value of -0.081, indeed the log odds standard error of
# this value is 0.171, which means the true value could be 0).

# Let's look at a histogram of visits
hist(data$PAN.visits.06,
     main = paste("Histogram of visits"),
     xlab = "Number of visits", 
     ylab = "Frequency")

# The large number of '0' values in visits may indicate that a Poisson regresson
# may not be a good fit. To confirm this let's run a dispersion test.

disp_test <- dispersiontest(data_poisson)

# The dispersion test reveals a z-score of 1.067 with a p-value of 0.143.

# Let's run a ZIP regression and compare to be sure
zeroinfly_poisson <- zeroinfl(PAN.visits.06 ~ competitive.district + marginality.06 + PAN.governor.06, data=data, dist='poisson')
summary(zeroinfly_poisson)

# Now to compare which is better I'll use a Vuong test of the two models
vuong(data_poisson, zeroinfly_poisson)

# With the null hypothesis being that the two models are indistinguishable, 
# the results of this voung comparison have p-values of more than 0.10 
# suggesting that our ordinary Poisson regression model is sufficient.

#### b ####
stargazer(data_poisson, type = "text")

# From the above results it looks like marginality is a significant negative
# predictor of visits.

# Looking at the exponentiated coefficients tells us that a 1 unit increase in
# marginality decreases our expected number of visits by a multiplicative factor 
# of 0.12 (the exponent value of -2.08).

# From the same results having a PAN-affiliate governor is also a significant 
# predictor of visits

# The exponentiated coefficients tells us that a 1 unit increase in
# PAN-affiliated governor (from not being affiliate to being affiliated) 
# decreases our expected number of visits by a multiplicative factor 
# of 0.7322 (the exponent value of -.312).

#### c ####
# Estimated mean number of visits
# Going back to the coefficients of the original model
coeffs

# The predicted mean number of visits for a district that was competitive(1), 
# had an average poverty level(0), and a PAN governon (1) 
exp(coeffs[1] + coeffs[2]*1 + coeffs[3]*0 + coeffs[4]*1)

# This give us an estimated number of visits of 0.0149
