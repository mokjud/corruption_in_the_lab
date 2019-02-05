### CORRUPTION PROJECT ###
### 2019. JANUAR 24. ###
########################

## LIBRARIES
library(dplyr)

## DATA
  #trying the stat on the data of Wiesel and Shalvi 2014 The collaborative roots of corruption
  df <- read.csv("/Users/mokosjudit/Dropbox/doktori _ mj/corruption/statistics paper/Weisel, O._Shalvi_dataset1_germany.csv") # data of Wiesel and Shave 2014
  str(df)
  
  
## is it true: subject riport doubles more often than random
  #random: the expected number of doubles reported by each pair in 20 trials: 3.33
  #make a new column: did they report a double? Y/N
  #df_w_NA <- 
    df_w_NA <- df[-c(which(is.na(df$Dec.01))),] #delete NA
    df_w_NA <- df_w_NA[-c(which(is.na(df_w_NA$Dec.02))),] #delete NA
    df_w_NA <- df_w_NA[-c(which(df_w_NA$Period==0)),]    #delete period==0
    
  for(i in 1:length(df_w_NA$Dec.01)) {
    if(df_w_NA$Dec.01[i]==df_w_NA$Dec.02[i])
    {df_w_NA$double[i] <- 1} #if double, be 1
    else
      {df_w_NA$double[i] <- 0}
  }
    df_w_NA$double <- as.numeric(df_w_NA$double)
    
    
   agg.df <-  aggregate(double ~ Subject+Session, data=df_w_NA, sum) #number of the doubles
   
   ## Wilcoxon signed rank test: number of doubles == 3.33 ?
   wilcox.test( agg.df$double, 3.33, alternative = "g") #small sample size
   
  
  
  
  
  #make a new vector: the proportion of reported double of one subject
  