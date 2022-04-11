#### Replication Study ####

##################################
# Gareth Moen #
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

# Load necessary packages
lapply(c("tidyverse", "forcats", "nnet", "ggplot2", "dplyr", "stargazer", "conflicted", "data.table", "Hmisc", "stringr", "stringi", "lfe", "xtable", "here", "readxl", "stringdist", "mfx", "sciplot"),  pkgTest)

rm(list=ls())
conflict_prefer("first", "dplyr")
conflict_prefer("last", "dplyr")
conflict_prefer("here", "here")
conflict_prefer("list", "base")
conflict_prefer("select", "dplyr")

specify_decimal <- function(x, k) format(as.numeric(round(x, k), nsmall=k))

# set wd for current folder

setwd("/Users/garethmoen/Documents/PG Dip - ASDS/Stats II/Replication")
getwd()

# Import data
load("dataverse_files/lev2019.Rda")

#### Figure 1 ####


lev2019_d<-lev2019[,list(outcome1=mean(outcome1,na.rm=TRUE),
                         outcome1_se=se(outcome1,na.rm=TRUE),
                         outcome3=mean(outcome3,na.rm=TRUE),
                         outcome3_se=se(outcome3,na.rm=TRUE)
                         
),by=c("treat")]
lev2019_d$treat<-as.character(lev2019_d$treat)
lev2019_d$treatname[lev2019_d$treat==0]<-"  Election\nCommissions\n\n(Control)"
lev2019_d$treatname[lev2019_d$treat==1]<-" Candidate\nFiltering\n\n(Treatment #1)"
lev2019_d$treatname[lev2019_d$treat==3]<-"Workplace\nMobilization\n\n(Treatment #3)"
lev2019_d$treatname[lev2019_d$treat==2]<-"Organizing\nCarousels\n\n(Treatment #2)"

ggplot(data=lev2019_d, aes(x=treatname, y=outcome1, fill=treatname)) + geom_bar(colour="black", stat="identity",position=position_dodge(),size=.3) + geom_errorbar(aes(ymin = outcome1-outcome1_se, ymax = outcome1+outcome1_se), width = 0.2)+ theme_bw()+ theme(legend.position="bottom",axis.text=element_text(size=16),axis.title=element_text(size=18),legend.text=element_text(size=12),plot.title = element_text(hjust = 0.5,size=22))+ coord_cartesian(ylim=c(2,5))+ geom_text(aes(label=specify_decimal(outcome1,2)), position=position_dodge(width=0.9), vjust=-1.3,size=5)+xlab("") + scale_fill_grey( name="",breaks=c("1", "2", "3"))+    xlab("") + ylab("Level of Anger\n")+ guides(fill=FALSE)+ggtitle("Panel A: Respondent Anger over Electoral Integrity")

#ggplot(data=lev2019_d, aes(x=treatname, y=outcome3, fill=treatname)) + geom_bar(colour="black", stat="identity",position=position_dodge(),size=.3) + geom_errorbar(aes(ymin = outcome3-outcome3_se, ymax = outcome3+outcome3_se), width = 0.2) + theme_bw()+ theme(legend.position="bottom",axis.text=element_text(size=16),axis.title=element_text(size=18),legend.text=element_text(size=12),plot.title = element_text(hjust = 0.5,size=22))+ coord_cartesian(ylim=c(2,4))+ scale_y_continuous(breaks=c(2,3,4))+ geom_text(aes(label=specify_decimal(outcome3,2)), position=position_dodge(width=0.9), vjust=-1.3,size=5)+xlab("") + scale_fill_grey( name="",breaks=c("1", "2", "3"))+    xlab("") + ylab("Likelihood of Protesting\n")+ guides(fill=FALSE)+ggtitle("Panel B: Respondent Likelihood of Protesting")
ggplot(data=lev2019_d, aes(x=treatname, y=outcome3, fill=treatname)) + geom_bar(colour="black", stat="identity",position=position_dodge(),size=.3) + geom_errorbar(aes(ymin = outcome3-outcome3_se, ymax = outcome3+outcome3_se), width = 0.2) + theme_bw()+ theme(legend.position="bottom",axis.text=element_text(size=16),axis.title=element_text(size=18),legend.text=element_text(size=12),plot.title = element_text(hjust = 0.5,size=22))+ coord_cartesian(ylim=c(2,4))+ scale_y_continuous(breaks=c(2,3,4))+ geom_text(aes(label=specify_decimal(outcome3,2)), position=position_dodge(width=0.9), vjust=-1.3,size=5)+xlab("") + scale_fill_grey( name="",breaks=c("1", "2", "3"))+    xlab("") + ylab("Likelihood of Protesting\n")+ guides(fill=FALSE)

#### Table 2 ####
# Regression results
lev2019$treat_factor<-relevel(factor(lev2019$treat), ref = "0")

est1<-felm(outcome1~treat_factor|0|0|0, data=subset(lev2019))

est2<-felm(outcome1~treat_factor + male+logage+edu+econ+townsize+employed+voted2018|0|0|0, data=subset(lev2019))

lev2019$treat_factor<-relevel(factor(lev2019$treat), ref = "3")

est3<-felm(outcome1~treat_factor|0|0|0, data=subset(lev2019, treat!=0))

est4<-felm(outcome1~treat_factor + male+logage+edu+econ+townsize+employed+voted2018|0|0|0, data=subset(lev2019 , treat!=0))

lev2019$treat_factor<-relevel(factor(lev2019$treat), ref = "0")

est5<-felm(outcome3 ~ treat_factor, data=lev2019)

est6<-felm(outcome3 ~ treat_factor + male + logage + edu + econ + townsize + employed + voted2018, data=lev2019)

lev2019$treat_factor<-relevel(factor(lev2019$treat), ref = "3")

est7<-felm(outcome3~treat_factor|0|0|0, data=subset(lev2019, treat!=0))

est8<-felm(outcome3~treat_factor + male+logage+edu+econ+townsize+employed+voted2018|0|0|0, data=subset(lev2019 , treat!=0))

#### My own analysis ####

# Create my own df with relevant variables from 'lev2019'
df <- lev2019 %>%
  dplyr::select(male, 
         age, 
         edu,
         putin_approval,
         employed,
         income,
         internetusage
  )

# Remove NA values
df_2 <- na.omit(df)

#Histogram of Putin approval, use 'df' with 'putin_approval' as integer
ggplot(data = df, mapping = aes(x = putin_approval), color = red) +
  geom_histogram(binwidth = 0.5) +
  labs(title = "Approval level for Putin in 2019 survey",
  x = "Level of support",
  y = "Number"
  )

# Scatterplot of Putin approval by age
ggplot(data = df_2) + 
  geom_point(mapping = aes(x = age, y = putin_approval), position = "jitter") +
  labs(title = "Putin approval by age",
    x="Age",
    y="Approval level")

# Scatterplot of Putin approval by education
ggplot(data = df_2) + 
  geom_point(mapping = aes(x = edu, y = putin_approval), position = "jitter") +
  labs(title = "Putin approval by Education level",
       x="Education level",
       y="Approval level")

# Scatterplot of Putin approval by internet usage
ggplot(data = df_2) + 
  geom_point(mapping = aes(x = internetusage, y = putin_approval), position = "jitter") +
  labs(title = "Putin approval by level of internet usage",
       x="Internet usage",
       y="Approval level")

# Scatterplot of Putin approval by income
ggplot(data = df_2) + 
  geom_point(mapping = aes(x = income, y = putin_approval), position = "jitter") +
  labs(title = "Putin approval by income",
       x="Income",
       y="Approval level")

# Scatterplot of Putin approval by employed
ggplot(data = df_2) + 
  geom_point(mapping = aes(x = employed, y = putin_approval), position = "jitter") +
  labs(title = "Putin approval by employment status",
    x="Unemployed = 0, Employed = 1",
    y="Approval level")

# Scatterplot of Putin approval by gender
ggplot(data = df_2) + 
  geom_point(mapping = aes(x = male, y = putin_approval), position = "jitter") +
  labs(title = "Putin approval by gender",
       x="Female = 0, Male = 1",
       y="Approval level")

# None of these scatterplots look to be good predictors of approval of Putin, 
# but I'll run a regression to be sure

# Make 'putin_approval' a factor
df_2$putin_approval <- factor(df_2$putin_approval, 
                               levels = c("1", "2", "3", "4")
                              )

# Confirm 'putin_approval' is a factor
class(df_2$putin_approval)

# Putin approval regression
# Multinomial regression for unordered categories
# Reference: support = 1 (definitely not), support = 2 (probably not)
# support = 3 (probably yes)
# support = 4 (definitely yes)
pa_multi <- multinom(putin_approval ~ male + age + edu + employed + income + internetusage, data = df_2)
pa_multi

# Exponentiate the coefficients to get odds
pa_multi_exp <- exp(coef(pa_multi)[,c(1:7)])
pa_multi_exp
stargazer(pa_multi_exp, type = "text")

# Interpretation 1
# There is an change in the baseline odds that somebody will have level 4 
# support for Putin by 1.05 times when that person is employed (vs unemployed).

# Interpretation 2
# There is a change in the baseline odds that somebody will have level 4 
# support for Putin by 0.8 times when that person has a higher internet usage.

# Interpretation 3
# There is an change in the baseline odds that somebody will have level 4 
# support for Putin by 0.59 times when that person is male (vs female).

# However none of these figures are significant. 