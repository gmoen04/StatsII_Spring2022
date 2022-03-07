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

lapply(c("stargazer", "stringr", "tidyverse", "ggplot2", "FactoMineR"),  pkgTest)

# set wd for current folder
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#####################
# Problem 1
#####################

# load data
load(url("https://github.com/ASDS-TCD/StatsII_Spring2022/blob/main/datasets/climateSupport.RData?raw=true"))
clsu <- climateSupport

# View data structure
summary(clsu)
str(clsu)

# Logit regression, additive model
reg <- glm(choice ~ countries + sanctions, 
           data = clsu, 
           family = binomial) #link = "logit"

summary(reg)

## Optional reframing variables as binary variables with 0 and 1 values
# library(FactoMineR)
# clsu_2 <- tab.disjonctif(clsu)
# clsu_2 <- as.data.frame(clsu_2)
# 
# clsu_flt <- clsu_2[,-clsu_2$`Not supported`] # Remove "not supported"
# 
# reg_2 <- glm(Supported ~ ., 
#              data = clsu_flt, 
#              family = binomial)
# summary(reg_2)
## Not used in the end as they seem to reframe everything in reference to 
## a single variable, 'Supported' in this case

# Exponentiate coefficients to get odds of coefficients
exp(coef(reg))

# Save the values
#stargazer(reg_sum, reg_exp, type = "latex")

# Likelihood ratio test (unfinished beyond here)
reg_null <- glm(as.factor(choice) ~ 1, data = clsu, family = "binomial") # 1 = fit an intercept only (i.e. sort of a "mean")
anova(reg_null, reg, test = "Chisq")

# Extracting confidence intervals (of the coefficients)
exp(confint(reg)) # Transform to odds ratio using exp()

# An option for making a data.frame of conf ints and coefficients
conf_reg <- data.frame(cbind(lower = exp(confint(reg)[,1]), 
                             coefs = exp(coef(reg)), 
                             upper = exp(confint(reg)[,2])))

# Then using this to make a plot
ggplot(data = conf_reg, mapping = aes(x = row.names(conf_reg), y = coefs)) +
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper), colour = "red") + 
  coord_flip()
