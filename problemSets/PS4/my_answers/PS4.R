##################################
# PS4 #
##################################

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

lapply(c("survival", "eha", "tidyverse", "ggfortify", "stargazer"),  pkgTest)

# set wd for current folder
setwd(dirname("/Users/garethmoen/Documents/PG Dip - ASDS/Stats II/MyProblemSets/PS4/my_answers"))

# Load data
data(infants)

# Creating a Surv object
infant_surv <- Surv(enter, exit, event)
# infant_surv <- with(infants, Surv(enter, exit, event)) # Alternative option

# Plot Kaplan-Meier curves
# Overall survival
km <- survfit(infant_surv ~ 1, data = infants) 
autoplot(km,
         main = "Overall survival Rate of Infants",
         xlab = "Time",
         ylab= "Survival Rate")
# ~ 1 'depends on 1' for proportion

# Survival divided between control group and study group
km_groups <- survfit(Surv(infants$enter, infants$exit, infants$event) ~ mother, data = infants)
autoplot(km_groups,
          main = "Survival Rate of Infants Whose Mothers are Alive or Dead",
          xlab = "Time",
          ylab= "Survival Rate")

# As we can see, the survival rate for infants whose mothers have died is much lower
# than infants whole mothers were alive.

# Cox Proportional Hazard Regression
cox <- coxph(Surv(enter, exit, event) ~ age + sex, data = infants)
survfit(cox)
summary(cox)

# Interpretation of the Cox PH regression results
# A 1 unit increase in age results in a decrease in hazard rate by a factor of 
# 0.96, holding everything else at its mean. A 1 unit increase in sex 
# (from female to male) results in a decrease in hazard rate by a factor of 
# 0.62, holding everything else at its mean. 

# Assessing the model with a Chisq test
drop1(cox, test = "Chisq")

# Running a Chisq test on the model
# Our p-scores in this test are 0.2803 and 0.3507 for sex and age respectively,
# suggesting that neither is significant using this model. 

# Plot Cumulative Hazard Function
plot_cox <- coxreg(Surv(enter, exit, event) ~ age + sex, data = infants)
plot(plot_cox,
     main = "Cumulative Hazard Function",
     xlab = "Duration",
     ylab = "Hazard Rate",
     xlim = c(0, 365))

# Interpretation of the Cumulative Hazard Function
# As can be seen from the plot, the hazard rate increases sharply over the first
# hundred days then becomes more gradual. Overall the rate is quite low, 
# topping out at around 1.5%

