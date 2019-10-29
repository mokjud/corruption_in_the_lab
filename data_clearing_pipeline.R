### CORRUPTION IN THE LAB ###
### DATA CLEARING PIPELINE ### 
### STARTED 2019. OCT. 28. ###
##############################

# must to do by hand: convert all the original ztree file to csv. the file the is created by ztree is not a normal xls, therefore R can't use it. 


## LIBRARIES



## getting all the datafiles into a list called myfiles


  setwd("/Users/mokosjudit/Google Drive/Korrupció_a_laborban_ötletek/eles_nevaltoztassrajta/csv")
  temp <- list.files(pattern="*.csv") #list the files' name in that folder that are csv. 
  myfiles <- lapply(temp, read.csv) #creating a list that contains all the original dataset. the elements of the list are data.frames 

  
## sort the datafiles into 4 categories based on the type of type of the game (myfiles[[1]]$X.4[3]=="dishonest_charity"|"honest_charity"|"dishonest_nocharity"|"honest_nocharity")
## create four data.frame for the four game types
  #creating 4 empty dataframes to fill up
    dishonest_charity <- read.csv("/Users/mokosjudit/Google Drive/Korrupció_a_laborban_ötletek/eles_nevaltoztassrajta/csv/template_datasets/proba_dishonest_charity.csv", header = T)
    dishonest_nocharity <- read.csv("/Users/mokosjudit/Google Drive/Korrupció_a_laborban_ötletek/eles_nevaltoztassrajta/csv/template_datasets/proba_dishonest_nocharity.csv", header = T)
    honest_charity <- read.csv("/Users/mokosjudit/Google Drive/Korrupció_a_laborban_ötletek/eles_nevaltoztassrajta/csv/template_datasets/proba_honest_charity.csv", header = T)
    honest_nocharity <- read.csv("/Users/mokosjudit/Google Drive/Korrupció_a_laborban_ötletek/eles_nevaltoztassrajta/csv/template_datasets/proba_honest_nocharity.csv", header = T)
    
  # creating counters for the loop
    counter.dishonest_charity <- 1
    counter.dishonest_nocharity <- 1
    counter.honest_charity <- 1
    counter.honest_nocharity <- 1
  
  # loop for sorting the elements of the list into the right dataframe. 
  
    for(i in 1:length(temp)){
      if(myfiles[[i]]$X.4[3]=="dishonest_charity")
        {
        counter.dishonest_charity <- counter.dishonest_charity+1
        #print("its dishones charity")
        dishonest_charity[counter.dishonest_charity, ] <- myfiles[[i]][3,]
        }
        else
          {
          #print("nope")
            if(myfiles[[i]]$X.4[3]=="dishonest_nocharity")
            {
              counter.dishonest_nocharity <- counter.dishonest_nocharity+1
              #print("it's dishonest nocharity")
              dishonest_nocharity[counter.dishonest_nocharity, ] <- myfiles[[i]][3,]            
            }
            else
            {
              if(myfiles[[i]]$X.4[3]=="honest_charity")
              {
                counter.honest_charity <- counter.honest_charity+1
                #print("it's honest charity")
                honest_charity[counter.honest_charity, ] <- myfiles[[i]][3,]            
              }
              else
              {
                if(myfiles[[i]]$X.4[3]=="honest_nocharity")
                {
                  counter.honest_nocharity <- counter.honest_nocharity+1
                  #print("it's honest nocharity")
                  honest_nocharity[counter.honest_nocharity, ] <- myfiles[[i]][3,]            
                } } } } }
  
## create one dataset that contains all the information
  
  
    
    
    