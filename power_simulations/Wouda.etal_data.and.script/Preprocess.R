# library (pbkrtest)
# library(lme4)
# library (ggplot2)
# library (lattice)
# library (car)
# library(afex)
# library(effects)
# library (coda)
# library(coefplot2)
# library(parallel)
# library(boot)
# library(psych)
# library (pastecs)
# library (foreign)
# library (Hmisc)
# library (ggthemes)
# library(reshape)
# library(ltm)
# library (pastecs)
# library (car)
# library (QuantPsyc)
# library(R.utils)
# library(gridExtra)
# library(scales)

#setwd('~/Dropbox/RM BSI/2nd Year/Major/Study 2/data')
setwd('C:/Users/fedor/OneDrive/Documents/R/Corruption/Wouda.etal_data.and.script')

####read data####

#get data, for each session
data1 = read.csv ("Data1.csv", sep= ";")
data2 = read.csv ("Data2.csv", sep= ";")
data3 = read.csv ("Data3.csv", sep= ";")
data4 = read.csv ("Data4.csv", sep= ";")

#get demo, for each session
demo1 = read.csv ("Demo1.csv", sep= ";")
demo2 = read.csv ("Demo2.csv", sep= ";")
demo3 = read.csv ("Demo3.csv", sep= ";")
demo4 = read.csv ("Demo4.csv", sep= ";")


data1_demo = merge(data1,demo1,by="Subject")
data2_demo = merge(data2,demo2,by="Subject")
data3_demo = merge(data3,demo3,by="Subject")
data4_demo = merge(data4,demo4,by="Subject")

demo = rbind (demo1, demo2, demo3, demo4)
mean(demo$Leeftijd) # kor
sd(demo$Leeftijd)
table(demo$Geslacht) # nem

#data <- rbind(data1_demo, data2_demo,data3_demo, data4_demo)
data <- rbind(data2_demo,data3_demo, data4_demo)

data_M<-data[data$Period %in% c(20),]#for manipulation check

#get treatment 2
data_C2<-data[data$myTreatment %in% c(2),]

data_C2 <- data_C2[order(data_C2$Session, data_C2$myGroupType),]#order data
data_C2_20<-data_C2[data_C2$Period %in% c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20),]#get rid of single shot trial

#get treatment 9
data_C9<-data[data$myTreatment %in% c(9),]

#data_C9 <- data_C9[order(data_C9$Session, data_C9$myGroupType),]#order data
data_C9_20<-data_C9[data_C9$Period %in% c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20),]#get rid of single shot trial


#### process Treatment 2 ####
max(data_C2_20$myGroup2)
dfs = list()
for (i in 1:max(data_C2_20$myGroup2))
{
  df = subset(data_C2_20, myGroup2 == i)
  dfs[[length(dfs)+1]] = df
}

dfs2 = list()

for (h in 1:max(data_C2_20$myGroup2))
{
  
  g = dfs[h]
  g = as.data.frame(g)
  g = g[order(g$Period),]#order data

  #first create one variable for all responses in one colum
  for (i in 1: nrow(g)){
    if (g$myGroupType[i] == 1){
      g$getal[i] = g$dec.01[i]
    } else if (g$myGroupType[i] == 2){
      g$getal[i] = g$dec.02[i]
    }
  }
  
  #assign(paste("data_C2_20_G", h, sep = ""), g) 
  g_1dyad_A = g[g$myGroupType %in% c(1),]#get player A
  g_1dyad_B = g[g$myGroupType %in% c(2),]#get player B
  
  #creating new variable PlayerA and PLayerB with corresonding respons
  for (i in 1: nrow(g_1dyad_A)){
    g_1dyad_A$PlayerA[i] = g_1dyad_A$getal[i]
  }
  for (i in 1: nrow(g_1dyad_B)){
    g_1dyad_B$PlayerB[i] = g_1dyad_B$getal[i]
  }
  
  g_1dyad_A <- g_1dyad_A[order(g_1dyad_A$Session, g_1dyad_A$Period),]#order data
  g_1dyad_B <- g_1dyad_B[order(g_1dyad_B$Session, g_1dyad_B$Period),]#order data
  
  g_Dyads <- subset(g_1dyad_A, select = c(Session, Period))
  
  
  g_Dyads$PlayerA = g_1dyad_A$PlayerA
  g_Dyads$PlayerB = g_1dyad_B$PlayerB
  
  for (i in 1: nrow(g_Dyads)){
    g_Dyads$double[i] = 0
  }
  for(i in seq(1, nrow(g_Dyads), 1)){
    if (g_Dyads$PlayerA[i] == g_Dyads$PlayerB[i]){
      g_Dyads$double[i] = 1
      
    }
  } 
  for(i in seq(1, nrow(g_Dyads), 20)){
    current_doubles = 0
    for(j in seq(0, 19,1)){
      current_doubles = current_doubles + g_Dyads$double[i+j] 
      g_Dyads$total_doubles [i+j] = current_doubles
    }
  }
  
  g_Dyads$Subject_A = g_1dyad_A$Subject
  g_Dyads$MyGroup2_A = g_1dyad_A$myGroup2
  #g_Dyads$Studie_A = g_1dyad_A$Studie
  g_Dyads$Leeftijd_A = g_1dyad_A$Leeftijd
  g_Dyads$Geslacht_A = g_1dyad_A$Geslacht
  #g_Dyads$Manipulatie_A = g_1dyad_A$Manipulatie

  g_Dyads$Subject_B = g_1dyad_B$Subject
  g_Dyads$MyGroup2_B = g_1dyad_B$myGroup2
  #g_Dyads$Studie_B = g_1dyad_B$Studie
  g_Dyads$Leeftijd_B = g_1dyad_B$Leeftijd
  g_Dyads$Geslacht_B = g_1dyad_B$Geslacht
  #g_Dyads$Manipulatie_B = g_1dyad_B$Manipulatie
  
  g_Dyads_trial20 = g_Dyads[g_Dyads$Period %in% c(20),]#get last trial
  
  assign(paste("s", h, sep = ""), g_Dyads_trial20)
  assign(paste("t", h, sep = ""), g_Dyads)
  
}

a_T2_last_trial <- rbind(s1,s2,s3,s4,s5,s6)
a_T2_all_trials <- rbind(t1,t2,t3,t4,t5,t6)

a_T2_last_trial$Treatment = 2
a_T2_all_trials$Treatment = 2
a_T2_last_trial$Treatment_name = "Low behavioral norm"
a_T2_all_trials$Treatment_name = "aLow"

#### process Treatment 9 ####
dfs = list()
for (i in 1:max(data_C9_20$myGroup2))
{
  df = subset(data_C9_20, myGroup2 == i)
  dfs[[length(dfs)+1]] = df
}

dfs2 = list()

for (h in 1:max(data_C9_20$myGroup2))
{
  
  g = dfs[h]
  g = as.data.frame(g)
  g = g[order(g$Period),]#order data
  
  #first create one variable for all responses in one colum
  for (i in 1: nrow(g)){
    if (g$myGroupType[i] == 1){
      g$getal[i] = g$dec.01[i]
    } else if (g$myGroupType[i] == 2){
      g$getal[i] = g$dec.02[i]
    }
  }
  
  #assign(paste("data_C9_20_G", h, sep = ""), g) 
  g_1dyad_A = g[g$myGroupType %in% c(1),]#get player A
  g_1dyad_B = g[g$myGroupType %in% c(2),]#get player B
  
  
  #creating new variable PlayerA and PLayerB with corresonding respons
  for (i in 1: nrow(g_1dyad_A)){
    g_1dyad_A$PlayerA[i] = g_1dyad_A$getal[i]
  }
  for (i in 1: nrow(g_1dyad_B)){
    g_1dyad_B$PlayerB[i] = g_1dyad_B$getal[i]
  }
  
  g_1dyad_A <- g_1dyad_A[order(g_1dyad_A$Session, g_1dyad_A$Period),]#order data
  g_1dyad_B <- g_1dyad_B[order(g_1dyad_B$Session, g_1dyad_B$Period),]#order data
  
  g_Dyads <- subset(g_1dyad_A, select = c(Session, Period))
  
  
  g_Dyads$PlayerA = g_1dyad_A$PlayerA
  g_Dyads$PlayerB = g_1dyad_B$PlayerB
  
  for (i in 1: nrow(g_Dyads)){
    g_Dyads$double[i] = 0
  }
  for(i in seq(1, nrow(g_Dyads), 1)){
    if (g_Dyads$PlayerA[i] == g_Dyads$PlayerB[i]){
      g_Dyads$double[i] = 1
      
    }
  } 
  for(i in seq(1, nrow(g_Dyads), 20)){
    current_doubles = 0
    for(j in seq(0, 19,1)){
      current_doubles = current_doubles + g_Dyads$double[i+j] 
      g_Dyads$total_doubles [i+j] = current_doubles
    }
  }
  
  g_Dyads$Subject_A = g_1dyad_A$Subject
  g_Dyads$MyGroup2_A = g_1dyad_A$myGroup2
  #g_Dyads$Studie_A = g_1dyad_A$Studie
  g_Dyads$Leeftijd_A = g_1dyad_A$Leeftijd
  g_Dyads$Geslacht_A = g_1dyad_A$Geslacht
  #g_Dyads$Manipulatie_A = g_1dyad_A$Manipulatie
  
  g_Dyads$Subject_B = g_1dyad_B$Subject
  g_Dyads$MyGroup2_B = g_1dyad_B$myGroup2
  #g_Dyads$Studie_B = g_1dyad_B$Studie
  g_Dyads$Leeftijd_B = g_1dyad_B$Leeftijd
  g_Dyads$Geslacht_B = g_1dyad_B$Geslacht
  g_Dyads$SONA_B = g_1dyad_B$SONA
  #g_Dyads$Manipulatie_B = g_1dyad_B$Manipulatie
  
  g_Dyads_trial20 = g_Dyads[g_Dyads$Period %in% c(20),]#get last trial
  
  assign(paste("s", h, sep = ""), g_Dyads_trial20)
  assign(paste("t", h, sep = ""), g_Dyads)
  
}

a_T9_last_trial <- rbind(s1,s2,s3,s4,s5,s6,s7)
a_T9_all_trials <- rbind(t1,t2,t3,t4,t5,t6,t7)

a_T9_last_trial$Treatment = 9
a_T9_all_trials$Treatment = 9
a_T9_last_trial$Treatment_name = "High behavioral norm"
a_T9_all_trials$Treatment_name = "high"

mean(a_T2_last_trial$total_doubles)
median(a_T2_last_trial$total_doubles)
sd(a_T2_last_trial$total_doubles)

mean(a_T9_last_trial$total_doubles)
median(a_T9_last_trial$total_doubles)
sd(a_T9_last_trial$total_doubles)

x = 0
for(i in seq(1, nrow(a_T2_all_trials), 20)){
  x = x + 1
  for(j in seq(0, 19,1)){
    a_T2_all_trials$DyadID[i+j] = x 
  }
}

for(i in seq(1, nrow(a_T2_last_trial))){
  a_T2_last_trial$DyadID[i] = i 
}


x = 0
for(i in seq(1, nrow(a_T9_all_trials), 20)){
  x = x + 1
  for(j in seq(0, 19,1)){
    a_T9_all_trials$DyadID[i+j] = x 
  }
}

for(i in seq(1, nrow(a_T9_last_trial))){
  a_T9_last_trial$DyadID[i] = i 
}


####means and SD ####

mean(a_T9_last_trial$total_doubles)
mean(a_T2_last_trial$total_doubles)
sd(a_T9_last_trial$total_doubles)
sd(a_T2_last_trial$total_doubles)
table(a_T9_last_trial$total_doubles)
table(a_T2_last_trial$total_doubles)

list2 = order(a_T2_last_trial$total_doubles)
order(list2)

a29 = rbind(a_T2_last_trial,a_T9_last_trial)

m1 = lm(total_doubles ~ Treatment + Manipulatie_B, data = a29)
summary (m1)

cor.test(a29$total_doubles, a29$Manipulatie_B)

####combine treatments in one dataset ####

a_data_T2_T9_last_trials <- rbind(a_T2_last_trial, a_T9_last_trial)
a_data_T2_T9_all_trials <- rbind(a_T2_all_trials, a_T9_all_trials)
a_data_T2_T9_all_trials$DyadID = 99

x = 0
for(i in seq(1, nrow(a_data_T2_T9_all_trials), 20)){
  x = x + 1
  for(j in seq(0, 19,1)){
    a_data_T2_T9_all_trials$DyadID[i+j] = x 
  }
}

a_data_T2_T9_last_trials$DyadID = 99
for(i in seq(1, nrow(a_data_T2_T9_last_trials))){
  a_data_T2_T9_last_trials$DyadID[i] = i 
}

d = a_data_T2_T9_last_trials
d$total_trials = 20
a <- subset(d, select = c(DyadID, total_doubles, total_trials, Treatment_name))

a_a = a_data_T2_T9_all_trials

##########################################################################################

# Save data to datafile
save(a_a, a_T2_all_trials, a_T9_all_trials, a_T2_last_trial, a_T9_last_trial, file = "Wouda_data.RData")


# Distribution of reported values by Player As
valueA_distr_low <- table(a_T2_all_trials$PlayerA) # in the low behavioral treatment group
valueA_distr_high <- table(a_T9_all_trials$PlayerA) # in the high behavioral treatment group

Low <- valueA_distr_low/sum(valueA_distr_low)
High <- valueA_distr_high/sum(valueA_distr_high)

barplot(rbind(Low, High), beside=TRUE, 
        legend.text=TRUE,
        xlab = "Reported values",
        ylab = "Proportion")

# The number of doubles by dyads
double_count_low <- a_T2_last_trial$total_doubles # in the low behavioral treatment group
double_count_high <- a_T9_last_trial$total_doubles # in the high behavioral treatment group

double_prop_low <- sum(double_count_low) / (length(double_count_low)*20)
double_prop_high <- sum(double_count_high) / (length(double_count_high)*20)

boxplot(double_count_low, double_count_high, 
        names = c("Low", "High"), 
        ylab = "The number of doubles/dyad",
        xlab = "Behavioral treatment group")

