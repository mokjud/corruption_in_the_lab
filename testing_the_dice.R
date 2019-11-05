### CORRUPTION IN THE LAB ###
### DATA CLEARING PIPELINE FOR PILOT DATA ### 
### STARTED 2019. NOV 5 ###
##############################

# We used a standard dice from a poker dice. We tested the diec to make sure it's even. The dice was rolled 703 times. 



### TESTING THE DICE
#used test: chi-square goodness-of-fit test
megfigy <- c(124, 116, 119, 114, 108, 122)
n=sum(megfigy)
valosz <- rep(1/6, 6)
chisq.test(x=megfigy, p=valosz)  

