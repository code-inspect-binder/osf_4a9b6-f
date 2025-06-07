###########################################################################
# 
# STATISTICAL ANALYSES ADDITIONAL GAZE FOLLOWING TASK (EXPLORATORY)
# BLINDED FOR REVIEW
# March 2019 
#
###########################################################################

rm(list = ls(all = TRUE)) # clears everything
graphics.off()            # close all open graphics
setwd("") # insert working directory

# read data
Data <- read.table(file = "AllData_GF_DS.txt", sep = '\t', header = TRUE)

##### One sample test Standard condition
t.test(Data$DS, mu=0, alternative="two.sided")
d.onesample.Stand <- (mean(Data$DS) - 0) / sd(Data$DS) #Effect size cohen's d
d.onesample.Stand
# means & SDs
mean(Data$DS, na.rm=TRUE) # M = 3
sd(Data$DS, na.rm=TRUE) # SD = 1.66
## Supplemental Material:
# To check whether the participants followed gaze at all, we tested the gaze following score against chance level by running a one sample test against zero. 
# Children followed gaze in the additional gaze-following task (M = 3; SD = 1.66; t(29) = 9.89, p = .00, d = 1.8). 


##### Correlation GF & Difference Score
Data$DiffSocCon <- as.numeric(Data$DiffSocCon)
cor.test(Data$DS, Data$DiffSocCon, method = "pearson") #only younger
## Supplemental Material:
# Gaze following abilities were not correlated with visual learning abilities, N = 30 , r(28) = 1.04, p = .30. 
# Gaze following abilities were not correlated with visual learning abilities, N = 30 , r(28) = 1.04, p = .30. 


#####  Mediansplit 

Data$PropSoc <- as.numeric(Data$PropSoc)
Learners <- Data[Data$MedianGroups == "1",] 
NoLearners <- Data[Data$MedianGroups == "2",] 

# Pairwise comparison of GF abilities in subgroups
t.test(Learners$PropSoc, NoLearners$PropSoc, paired=F, na.rm=TRUE)
mean(Learners$PropSoc, na.rm=TRUE) # M = .87
mean(NoLearners$PropSoc, na.rm=TRUE) # M = .74
sd(Learners$PropSoc, na.rm=TRUE) # SD = .13
sd(NoLearners$PropSoc, na.rm=TRUE) # SD = .19
## Supplemental Material:
# In addition to our pre-registered plan, we compared the proportional number of congruent trials between enhanced and less enhanced learners 
# in the visual learning task (see main document section ‘Data Analyses and Coding’ for the procedure of sub-sample creation). 
# The proportional number of trials in which children followed gaze was significantly higher for the sub-sample of enhanced learners (M = .87; SD = .13) compared to less enhanced learners (M = .74; SD = .19, t(25) = 2.05; p = .05).



