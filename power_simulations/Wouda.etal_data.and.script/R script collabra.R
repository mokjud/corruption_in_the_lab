
#set the working directory
setwd('~/Dropbox/RM BSI/letter/Data')

#read data
data1 = read.csv2 ("Data_Study_2_collabra.csv")#get data

#load the library
library(afex)

#First get model 1 with the mixed function
model1 <- mixed(proportion_doubles ~ Treatment_name + (1 | DyadID), method = "PB", 
                family = binomial, data = data1, args.test = list(nsim = 1000))
#Now get the other modelwith the mixed function
model2 <- mixed(cbind(total_doubles,total_trials - total_doubles) ~ Treatment_name + (1 | DyadID), 
                method = "PB", family = binomial, data = data1, args.test = list(nsim = 1000))
#get the p-values with the anova function
anova(model1)
anova(model2)



library (pbkrtest)
library(lme4)
library (ggplot2)
library (lattice)
library (car)
library(afex)
library(effects)
library (coda)
library(coefplot2)
library(parallel)
library(boot)
library(psych)
library (pastecs)
library (foreign)
library (Hmisc)
library (ggthemes)
library(reshape)
library(ltm)
library (pastecs)
library (car)
library (QuantPsyc)
library(R.utils)
library(gridExtra)
library(scales)




model1 <- mixed(proportion_doubles ~ Treatment_name + (1 | DyadID), method = "PB", family = binomial, data = data1, args.test = list(nsim = 1000))
anova(model1)
summary(model1)
model1

model2 <- mixed(cbind(total_doubles,total_trials - total_doubles) ~ Treatment_name + (1 | DyadID), method = "PB", family = binomial, data = data1, args.test = list(nsim = 1000))
anova(model2)
summary(model2)




model11 <- mixed(proportion_doubles ~ Treatment_name + (1 | DyadID), method = "PB", family = binomial, data = data1, args.test = list(nsim = 1000))
model11
anova(model11)
model22 <- mixed(cbind(total_doubles,total_trials - total_doubles) ~ Treatment_name + (1 | DyadID), method = "PB", family = binomial, data = data1, args.test = list(nsim = 1000))
model22







model1 <- mixed(proportion_doubles ~ Treatment_name + (1 | DyadID), method = "PB", 
                family = binomial, data = data1, args.test = list(nsim = 1000))
model2 <- mixed(cbind(total_doubles,total_trials - total_doubles) ~ Treatment_name + (1 | DyadID), 
                method = "PB", family = binomial, data = data1, args.test = list(nsim = 1000))
anova(model1)
anova(model2)






model1=model111
model2=model222
model111 <- mixed(proportion_doubles ~ Treatment_name + (1 | DyadID), method = "PB", family = binomial, data = data1, args.test = list(nsim = 1000))
model111
anova(model111)

model222 <- mixed(cbind(total_doubles,total_trials - total_doubles) ~ Treatment_name + (1 | DyadID), method = "PB", family = binomial, data = data1, args.test = list(nsim = 1000))
anova(model222)


















data2 = read.csv ("Data_Study2(trial).csv")

gm1 <- mixed(double ~ Treatment_name + (1 + Period | DyadID),  method = "LRT", family = binomial, data = data2)
summary(gm1)
gm1

gm2 <- mixed(double ~ Treatment_name + (1 + Period | DyadID),  method = "PB", family = binomial, data = data2, args.test = list(nsim = 1000))
anova(gm2)
gm1

gm2 <- mixed(double ~ Treatment_name  + (1 + Period| DyadID),  method = "PB", family = binomial(link = "logit"), data = data2, args.test = list(nsim = 1000))
summary(gm2)
gm1

?mixed



