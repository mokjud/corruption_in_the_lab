### CORRUPTION IN THE LAB ###
### DATA CLEARING PIPELINE ### 
### STARTED 2019. OCT. 28. ###
##############################

# As the original xls output file of the ztree is not suitable for R, we converted all the original output xls files to csv by hand. 
# You can find these converted files in the supplementary materials. To reproduce this code, please download it and follow the comments of this script. 
# The Files folder of the supplementary material contains 
  # the original ztree output files converted to csv files, 
  # in the "template_datasets" folder 4 empty template tables (proba_dishonest_charity.csv, proba_dishonest_nocharity.csv, proba_honest_charity.csv, proba_honest_nocharity.csv)
  # the table that is the result of this script and is used by the Stat.Rmd code (data_ztree.cs)

## Setup

  rm(list=ls())
  library("dplyr")

  #download the folder of the original output files of ztree from the supplementary materials
  datadir <- "/Users/mokosjudit/Google Drive/Korrupció_a_laborban_ötletek/eles_nevaltoztassrajta/csv/" # set that folder here 
  setwd(datadir)
  #setwd("/Users/mokosjudit/Google Drive/Korrupció_a_laborban_ötletek/eles_nevaltoztassrajta/csv")
  
## Create a list with that contains all the output file of ztree
  temp <- list.files(pattern="*.csv") #list csv filenames in the folder
  myfiles <- lapply(temp, read.csv) #creating a list that contains all the original datasets. the elements of the list are data.frames 
  
  # the variables of myfiles by defoult are factors. this loop makes everything character. 
    for(i in 1:length(temp))
    {myfiles[[i]][,1:length(myfiles[[i]])] <- sapply(myfiles[[i]][,1:length(myfiles[[i]])], as.character)}

## sort the datafiles into 4 categories based on the type of the game 
  # filling up 4 empty data.frames
    dishonest_charity <- read.csv(paste(datadir, "template_datasets/proba_dishonest_charity.csv", sep=""), header = T)  #reading this file might lead to warninng message that doesn't effect the script (In read.table(file = file, header = header, sep = sep, quote = quote,  :incomplete final line found by readTableHeader)
    dishonest_nocharity <- read.csv(paste(datadir, "template_datasets/proba_dishonest_nocharity.csv", sep=""), header = T)
    honest_charity <- read.csv(paste(datadir, "template_datasets/proba_honest_charity.csv", sep=""), header = T)
    honest_nocharity <- read.csv(paste(datadir, "template_datasets/proba_honest_nocharity.csv", sep=""), header = T)

  # make all the variable character to avoid potential dataloss
    dishonest_charity[,1:length(dishonest_charity)] <- sapply(dishonest_charity[,1:length(dishonest_charity)], as.character)
    dishonest_nocharity[,1:length(dishonest_nocharity)] <- sapply(dishonest_nocharity[,1:length(dishonest_nocharity)], as.character)
    honest_charity[,1:length(honest_charity)] <- sapply(honest_charity[,1:length(honest_charity)], as.character)
    honest_nocharity[,1:length(honest_nocharity)] <- sapply(honest_nocharity[,1:length(honest_nocharity)], as.character)

    
  # loop for sorting the elements of the list into the right dataframe. 
    
    #creating counters for the loop
      counter.dishonest_charity <- 1
      counter.dishonest_nocharity <- 1
      counter.honest_charity <- 1
      counter.honest_nocharity <- 1

    for(i in 1:length(temp)){
      if(myfiles[[i]]$X.4[3]=="dishonest_charity")
      {
        counter.dishonest_charity <- counter.dishonest_charity+1
        dishonest_charity[counter.dishonest_charity, ] <- myfiles[[i]][3,]
      }
      else
      {
        if(myfiles[[i]]$X.4[3]=="dishonest_nocharity")
        {
          counter.dishonest_nocharity <- counter.dishonest_nocharity+1
          dishonest_nocharity[counter.dishonest_nocharity, ] <- myfiles[[i]][3,]            
        }
        else
        {
          if(myfiles[[i]]$X.4[3]=="honest_charity")
          {
            counter.honest_charity <- counter.honest_charity+1
            honest_charity[counter.honest_charity, ] <- myfiles[[i]][3,]            
          }
          else
          {
            if(myfiles[[i]]$X.4[3]=="honest_nocharity")
            {
              counter.honest_nocharity <- counter.honest_nocharity+1
              honest_nocharity[counter.honest_nocharity, ] <- myfiles[[i]][3,]            
            } } } } }

#Create the dataframe that Stat.Rmd uses
  dishonest_charity_rawdata <- data.frame(Time=dishonest_charity[,1], 
                                          ID=dishonest_charity$IDnumber, 
                                          age=dishonest_charity$kor ,
                                          sex=dishonest_charity$nem ,
                                          Foundation=dishonest_charity$alapitvany,
                                          Condition= "CD",
                                          Game="C", 
                                          Partner="D", 
                                          Index=rep(c(1:20), each=length(dishonest_charity$subjects)), 
                                          ValueA=c(dishonest_charity$dobas1, dishonest_charity$dobas2, dishonest_charity$dobas3, dishonest_charity$dobas4, 
                                                   dishonest_charity$dobas5, dishonest_charity$dobas6, dishonest_charity$dobas7, dishonest_charity$dobas8,
                                                   dishonest_charity$dobas9, dishonest_charity$dobas10, dishonest_charity$dobas11, dishonest_charity$dobas12,
                                                   dishonest_charity$dobas13, dishonest_charity$dobas14, dishonest_charity$dobas15, dishonest_charity$dobas16,
                                                   dishonest_charity$dobas17, dishonest_charity$dobas18, dishonest_charity$dobas19, dishonest_charity$dobas20), 
                                          ValueB=c(dishonest_charity$dishonest1, dishonest_charity$dishonest2, dishonest_charity$dishonest3, dishonest_charity$dishonest4, 
                                                   dishonest_charity$dishonest5, dishonest_charity$dishonest6, dishonest_charity$dishonest7, dishonest_charity$dishonest8,
                                                   dishonest_charity$dishonest9, dishonest_charity$dishonest10, dishonest_charity$dishonest11, dishonest_charity$dishonest12,
                                                   dishonest_charity$dishonest13, dishonest_charity$dishonest14, dishonest_charity$dishonest15, dishonest_charity$dishonest16,
                                                   dishonest_charity$dishonest17, dishonest_charity$dishonest18, dishonest_charity$dishonest19, dishonest_charity$dishonest20), 
                                          Double=c(dishonest_charity$double1, dishonest_charity$double2, dishonest_charity$double3, dishonest_charity$double4, 
                                                   dishonest_charity$double5, dishonest_charity$double6, dishonest_charity$double7, dishonest_charity$double8,
                                                   dishonest_charity$double9, dishonest_charity$double10, dishonest_charity$double11, dishonest_charity$double12,
                                                   dishonest_charity$double13, dishonest_charity$double14, dishonest_charity$double15, dishonest_charity$double16,
                                                   dishonest_charity$double17, dishonest_charity$double18, dishonest_charity$double19, dishonest_charity$double20)
                                          )

  dishonest_nocharity_rawdata <- data.frame(Time=dishonest_nocharity[,1], 
                                            ID=dishonest_nocharity$IDnumber, 
                                            age=dishonest_nocharity$kor ,
                                            sex=dishonest_nocharity$nem ,
                                            Foundation=NA,
                                            Condition= "SD",
                                            Game="S", 
                                            Partner="D", 
                                            Index=rep(c(1:20), each=length(dishonest_nocharity$subjects)), 
                                            ValueA=c(dishonest_nocharity$dobas1, dishonest_nocharity$dobas2, dishonest_nocharity$dobas3, dishonest_nocharity$dobas4, 
                                                     dishonest_nocharity$dobas5, dishonest_nocharity$dobas6, dishonest_nocharity$dobas7, dishonest_nocharity$dobas8,
                                                     dishonest_nocharity$dobas9, dishonest_nocharity$dobas10, dishonest_nocharity$dobas11, dishonest_nocharity$dobas12,
                                                     dishonest_nocharity$dobas13, dishonest_nocharity$dobas14, dishonest_nocharity$dobas15, dishonest_nocharity$dobas16,
                                                     dishonest_nocharity$dobas17, dishonest_nocharity$dobas18, dishonest_nocharity$dobas19, dishonest_nocharity$dobas20), 
                                            ValueB=c(dishonest_nocharity$dishonest1, dishonest_nocharity$dishonest2, dishonest_nocharity$dishonest3, dishonest_nocharity$dishonest4, 
                                                     dishonest_nocharity$dishonest5, dishonest_nocharity$dishonest6, dishonest_nocharity$dishonest7, dishonest_nocharity$dishonest8,
                                                     dishonest_nocharity$dishonest9, dishonest_nocharity$dishonest10, dishonest_nocharity$dishonest11, dishonest_nocharity$dishonest12,
                                                     dishonest_nocharity$dishonest13, dishonest_nocharity$dishonest14, dishonest_nocharity$dishonest15, dishonest_nocharity$dishonest16,
                                                     dishonest_nocharity$dishonest17, dishonest_nocharity$dishonest18, dishonest_nocharity$dishonest19, dishonest_nocharity$dishonest20), 
                                            Double=c(dishonest_nocharity$double1, dishonest_nocharity$double2, dishonest_nocharity$double3, dishonest_nocharity$double4, 
                                                     dishonest_nocharity$double5, dishonest_nocharity$double6, dishonest_nocharity$double7, dishonest_nocharity$double8,
                                                     dishonest_nocharity$double9, dishonest_nocharity$double10, dishonest_nocharity$double11, dishonest_nocharity$double12,
                                                     dishonest_nocharity$double13, dishonest_nocharity$double14, dishonest_nocharity$double15, dishonest_nocharity$double16,
                                                     dishonest_nocharity$double17, dishonest_nocharity$double18, dishonest_nocharity$double19, dishonest_nocharity$double20)
                                            )

  honest_nocharity_rawdata <- data.frame(Time=honest_nocharity[,1], 
                                         ID=honest_nocharity$IDnumber, 
                                         age=honest_nocharity$kor ,
                                         sex=honest_nocharity$nem ,
                                         Foundation=NA,
                                         Condition= "SH",
                                         Game="S", 
                                         Partner="H", 
                                         Index=rep(c(1:20), each=length(honest_nocharity$subjects)), 
                                         ValueA=c(honest_nocharity$dobas1, honest_nocharity$dobas2, honest_nocharity$dobas3, honest_nocharity$dobas4, 
                                                  honest_nocharity$dobas5, honest_nocharity$dobas6, honest_nocharity$dobas7, honest_nocharity$dobas8,
                                                  honest_nocharity$dobas9, honest_nocharity$dobas10, honest_nocharity$dobas11, honest_nocharity$dobas12,
                                                  honest_nocharity$dobas13, honest_nocharity$dobas14, honest_nocharity$dobas15, honest_nocharity$dobas16,
                                                  honest_nocharity$dobas17, honest_nocharity$dobas18, honest_nocharity$dobas19, honest_nocharity$dobas20), 
                                         ValueB=c(honest_nocharity$honest1, honest_nocharity$honest2, honest_nocharity$honest3, honest_nocharity$honest4, 
                                                  honest_nocharity$honest5, honest_nocharity$honest6, honest_nocharity$honest7, honest_nocharity$honest8,
                                                  honest_nocharity$honest9, honest_nocharity$honest10, honest_nocharity$honest11, honest_nocharity$honest12,
                                                  honest_nocharity$honest13, honest_nocharity$honest14, honest_nocharity$honest15, honest_nocharity$honest16,
                                                  honest_nocharity$honest17, honest_nocharity$honest18, honest_nocharity$honest19, honest_nocharity$honest20), 
                                         Double=c(honest_nocharity$double1, honest_nocharity$double2, honest_nocharity$double3, honest_nocharity$double4, 
                                                  honest_nocharity$double5, honest_nocharity$double6, honest_nocharity$double7, honest_nocharity$double8,
                                                  honest_nocharity$double9, honest_nocharity$double10, honest_nocharity$double11, honest_nocharity$double12,
                                                  honest_nocharity$double13, honest_nocharity$double14, honest_nocharity$double15, honest_nocharity$double16,
                                                  honest_nocharity$double17, honest_nocharity$double18, honest_nocharity$double19, honest_nocharity$double20)
                                          )


  honest_charity_rawdata <- data.frame(Time=honest_charity[,1], 
                                       ID=honest_charity$IDnumber, 
                                       age=honest_charity$kor ,
                                       sex=honest_charity$nem ,
                                       Foundation=honest_charity$alapitvany,
                                       Condition= "CH",
                                       Game="C", 
                                       Partner="H", 
                                       Index=rep(c(1:20), each=length(honest_charity$subjects)), 
                                       ValueA=c(honest_charity$dobas1, honest_charity$dobas2, honest_charity$dobas3, honest_charity$dobas4, 
                                                honest_charity$dobas5, honest_charity$dobas6, honest_charity$dobas7, honest_charity$dobas8,
                                                honest_charity$dobas9, honest_charity$dobas10, honest_charity$dobas11, honest_charity$dobas12,
                                                honest_charity$dobas13, honest_charity$dobas14, honest_charity$dobas15, honest_charity$dobas16,
                                                honest_charity$dobas17, honest_charity$dobas18, honest_charity$dobas19, honest_charity$dobas20), 
                                       ValueB=c(honest_charity$honest1, honest_charity$honest2, honest_charity$honest3, honest_charity$honest4, 
                                                honest_charity$honest5, honest_charity$honest6, honest_charity$honest7, honest_charity$honest8,
                                                honest_charity$honest9, honest_charity$honest10, honest_charity$honest11, honest_charity$honest12,
                                                honest_charity$honest13, honest_charity$honest14, honest_charity$honest15, honest_charity$honest16,
                                                honest_charity$honest17, honest_charity$honest18, honest_charity$honest19, honest_charity$honest20), 
                                       Double=c(honest_charity$double1, honest_charity$double2, honest_charity$double3, honest_charity$double4, 
                                                honest_charity$double5, honest_charity$double6, honest_charity$double7, honest_charity$double8,
                                                honest_charity$double9, honest_charity$double10, honest_charity$double11, honest_charity$double12,
                                                honest_charity$double13, honest_charity$double14, honest_charity$double15, honest_charity$double16,
                                                honest_charity$double17, honest_charity$double18, honest_charity$double19, honest_charity$double20)
                                          )


  #make all the variable character to avoid potential dataloss. 
    dishonest_charity_rawdata[,1:length(dishonest_charity_rawdata)] <- sapply(dishonest_charity_rawdata[,1:length(dishonest_charity_rawdata)], as.character)
    dishonest_nocharity_rawdata[,1:length(dishonest_nocharity_rawdata)] <- sapply(dishonest_nocharity_rawdata[,1:length(dishonest_nocharity_rawdata)], as.character)
    honest_charity_rawdata[,1:length(honest_charity_rawdata)] <- sapply(honest_charity_rawdata[,1:length(honest_charity_rawdata)], as.character)
    honest_nocharity_rawdata[,1:length(honest_nocharity_rawdata)] <- sapply(honest_nocharity_rawdata[,1:length(honest_nocharity_rawdata)], as.character)

## create one dataset that contains the results of the four games
  fulldata_rawdata <- data.frame(Time=c(dishonest_charity_rawdata$Time, dishonest_nocharity_rawdata$Time, honest_charity_rawdata$Time, honest_nocharity_rawdata$Time),
                                 ID=c(dishonest_charity_rawdata$ID, dishonest_nocharity_rawdata$ID, honest_charity_rawdata$ID, honest_nocharity_rawdata$ID), 
                                 age=c(dishonest_charity_rawdata$age, dishonest_nocharity_rawdata$age, honest_charity_rawdata$age, honest_nocharity_rawdata$age), 
                                 sex=c(dishonest_charity_rawdata$sex, dishonest_nocharity_rawdata$sex, honest_charity_rawdata$sex, honest_nocharity_rawdata$sex), 
                                 Foundation=c(dishonest_charity_rawdata$Foundation, dishonest_nocharity_rawdata$Foundation, honest_charity_rawdata$Foundation, honest_nocharity_rawdata$Foundation),
                                 Condition= c(dishonest_charity_rawdata$Condition, dishonest_nocharity_rawdata$Condition, honest_charity_rawdata$Condition, honest_nocharity_rawdata$Condition),
                                 Game=c(dishonest_charity_rawdata$Game, dishonest_nocharity_rawdata$Game, honest_charity_rawdata$Game, honest_nocharity_rawdata$Game),
                                 Partner=c(dishonest_charity_rawdata$Partner, dishonest_nocharity_rawdata$Partner, honest_charity_rawdata$Partner, honest_nocharity_rawdata$Partner),
                                 Index=c(dishonest_charity_rawdata$Index, dishonest_nocharity_rawdata$Index, honest_charity_rawdata$Index, honest_nocharity_rawdata$Index),
                                 ValueA=c(dishonest_charity_rawdata$ValueA, dishonest_nocharity_rawdata$ValueA, honest_charity_rawdata$ValueA, honest_nocharity_rawdata$ValueA),
                                 ValueB=c(dishonest_charity_rawdata$ValueB, dishonest_nocharity_rawdata$ValueB, honest_charity_rawdata$ValueB, honest_nocharity_rawdata$ValueB),
                                 double_original=c(dishonest_charity_rawdata$Double, dishonest_nocharity_rawdata$Double, honest_charity_rawdata$Double, honest_nocharity_rawdata$Double), 
                                 Fingerratio=NA, 
                                 Q1=NA, 
                                 Q2=NA)
  
  # make all the variable character to avoid potential dataloss. 
    fulldata_rawdata[,1:length(fulldata_rawdata)] <- sapply(fulldata_rawdata[,1:length(fulldata_rawdata)], as.character)
  
  # adding column called Double - TRUE if it was a double, and FALSE it wasn't. 
    fulldata_rawdata$Double<- ifelse(fulldata_rawdata$double_original==1, TRUE, FALSE)
  

 

## games that was played by the experimenters has the ID number of 1000 or 999. These rows need to be deleted. 

  fulldata_rawdata <- filter(fulldata_rawdata, ID!=1000) #exclude trial cases. 
  fulldata_rawdata <- filter(fulldata_rawdata, ID!=999) #exclude trial cases. 
  
## convert all the variables to the right format. (everything was character to avoid potential dataloss)  
  fulldata_rawdata$ID <- as.integer(fulldata_rawdata$ID)
  fulldata_rawdata$age <- as.integer(fulldata_rawdata$age)
  fulldata_rawdata$sex <- as.factor(fulldata_rawdata$sex)
  fulldata_rawdata$Condition <- as.factor(fulldata_rawdata$Condition)
  fulldata_rawdata$Game <- as.factor(fulldata_rawdata$Game)
  fulldata_rawdata$Partner <- as.factor(fulldata_rawdata$Partner)
  fulldata_rawdata$Index <- as.integer(fulldata_rawdata$Index)
  fulldata_rawdata$ValueA <- as.integer(fulldata_rawdata$ValueA)
  fulldata_rawdata$ValueB <- as.integer(fulldata_rawdata$ValueB)
  
  
  
  
  
  ### ADDING THE RESULTS OF THE QUESTIONNAIRES TO THE DATASET
  questions <- read.csv("/Users/mokosjudit/Google Drive/Korrupció_a_laborban_ötletek/eles_nevaltoztassrajta/kerdoiv_eredmenyek/OTKA K128289 (válaszok)2019.nov.4.csv")
  question_length <-   dim(questions)[2]
  fulldata_length <- dim(fulldata_rawdata)[2]+1
  final_length <- question_length+fulldata_length
  fulldata_rawdata[,c(fulldata_length:final_length)] <- NA
  colnames(fulldata_rawdata)[c(fulldata_length:final_length)] <-   colnames(questions)
  
  length(colnames(questions))
  
  
  for(i in 1: length(fulldata_rawdata$ID))
  {
    questions[which(questions$Kerjuk.írja.be.a.korabban.kapott.negyjegyu.szamot.==fulldata_rawdata$ID[2]), ]
    
  }
  
  
  questions[which(questions$Kerjuk.írja.be.a.korabban.kapott.negyjegyu.szamot.==fulldata_rawdata$ID[2]), ]
  fulldata_rawdata$ID
  
  

## save the dataframe into a csv
  write.csv(fulldata_rawdata, file = paste(datadir, "/data_ztree.csv", sep="")) # this is the csv file that is used by Stat.Rmd
  

