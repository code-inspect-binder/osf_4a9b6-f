###########################################################################
# 
# STATISTICAL ANALYSES PREFERENTIAL LOOKING TASK 
# BLINDED FOR REVIEW
# March 2019 
#
###########################################################################

# set working directory
rm(list = ls(all = TRUE)) # clears everything
graphics.off()            # close all open graphics
setwd("") # insert working directory

# read tables
PrefLookLT <- read.table(file = "PrefLooking_Summary_LT.txt", sep = '\t', header = TRUE) 
Median_DiffSc <- read.table(file = "./AllData_PrefLook_Median_DiffSc.txt", sep = '\t', header = TRUE) # for Analyses with Medianplit based on Difference Score
Median_Beta <- read.table(file = "./AllData_PrefLook_Median_Beta.txt", sep = '\t', header = TRUE) # for Analyses with Mediansplit based on Beta coefficient


####################################################################################
################### Looking Times (Pre-Registered) #################################

### Meaning PRE 
#t-test
t.test(PrefLookLT$relSocPre.mean, mu=0.50, alternative="two.sided")

mean(PrefLookLT$relSocPre.mean, na.rm = TRUE) # M = .48
sd(PrefLookLT$relSocPre.mean, na.rm = TRUE) # SD = .14

#### Meaning POST 
#t-test
t.test(PrefLookLT$relSocPost.mean, mu=0.50, alternative="two.sided")

mean(PrefLookLT$relSocPost.mean, na.rm = TRUE) # M = .46
sd(PrefLookLT$relSocPost.mean, na.rm = TRUE) #  SD = .15

###Pairwise comparison pre and post looking time to social stimuli
t.test(PrefLookLT$relSocPre.mean, PrefLookLT$relSocPost.mean, paired=T)

## Manuscript:
# The mean proportional looking time at the social interaction shape did not differ from chance level—neither before (M = .48, SD = .14; t(30) = −.65; p = .52), 
# nor after the learning task (M = .46, SD = .15; t(31) =  −1.58; p = .12). There was no difference between pre- and post-test (t(30) = 1.18; p = .25). 



####################################################################################
################################### MedianSplit ####################################

##### Procedure 1: Based on Latency Difference score (Mean sacc latency social condition - mean sacc latency control condition)
##### not pre-registered (exploratory)

Learners_DiffSc <- Median_DiffSc[Median_DiffSc$MedianGroups == "1",] 
NoLearners_DiffSc <- Median_DiffSc[Median_DiffSc$MedianGroups == "2",] 

# MEANING: pair-wise comparison learners and no-learners POST Learning
t.test(Learners_DiffSc$relSocPost.mean, NoLearners_DiffSc$relSocPost.mean, paired=F)
mean(Learners_DiffSc$relSocPost.mean, na.rm = TRUE) # .52
mean(NoLearners_DiffSc$relSocPost.mean, na.rm = TRUE) # .39
sd(Learners_DiffSc$relSocPost.mean, na.rm = TRUE) #.12
sd(NoLearners_DiffSc$relSocPost.mean, na.rm = TRUE) #.17

n1 <- nrow(Learners_DiffSc)  #prep for effect size cohen's d 
n2 <- nrow(NoLearners_DiffSc)    #prep for effect size cohen's d
sdPool <- sqrt(((n1-1)*var(Learners_DiffSc$relSocPost.mean, na.rm = TRUE) + (n2-1)*var(NoLearners_DiffSc$relSocPost.mean, na.rm = TRUE)) / (n1+n2-2))  #prep for effect size cohen's d (pooled SD)
d.paired <- (mean(Learners_DiffSc$relSocPost.mean, na.rm = TRUE) - mean(NoLearners_DiffSc$relSocPost.mean, na.rm = TRUE)) / sdPool #Effect size cohen's d
d.paired
# One-sample t-test
t.test(Learners_DiffSc$relSocPost.mean, mu=0.50, alternative="two.sided") # though no preference  against chance
## Manuscript
# The group comparison revealed that children with enhanced performance in the learning task looked relatively longer at the 
# social interaction shape (M = .52; SD = .12) compared to less enhanced learners (M = .39; SD = .17, t(27) = 2.33; p = .03, d = .82). 
# However, the mean proportion score of the enhanced learners did not exceed chance level (t(15) = .56; p = .58).


# MEANING: pair-wise comparison learners and no-learners PRE Learning
t.test(Learners_DiffSc$relSocPre.mean, NoLearners_DiffSc$relSocPre.mean, paired=F)
mean(Learners_DiffSc$relSocPre.mean, na.rm = TRUE) # M = .53
mean(NoLearners_DiffSc$relSocPre.mean, na.rm = TRUE) # M = .44
sd(Learners_DiffSc$relSocPre.mean, na.rm = TRUE) # SD = .13
sd(NoLearners_DiffSc$relSocPre.mean, na.rm = TRUE) # SD = .13




##### Procedure 2: Based on Difference score of Beta coefficient of individual learning curves (BetaSoc - BetaCon)
##### Pre- registerered

## Supplemental Material:
# As described in the pre-registration, we explored sub-group differences between enhanced and less-enhanced learners further by using a more sophisticated 
# group assignment procedure (see also Mani & Huettig, 2014). For this purpose, we divided the sample based on a median split of a beta-coefficient difference score, 
# calculated for each individual by subtracting the beta-coefficient of their learning function during social interaction trials from the beta-coefficient of their 
# learning function during control trials. This procedure was more sophisticated than using mean latencies, since it focused on latency changes over time. 
Median_Beta$z.DiffBeta = as.vector(scale(Median_Beta$DiffBeta))
Median_Beta$z.ConBeta = as.vector(scale(Median_Beta$ConBeta))
Median_Beta$z.SocBeta = as.vector(scale(Median_Beta$SocBeta))

Learners_Beta <- Median_Beta[Median_Beta$MedianGroup == "1",] 
NoLearners_Beta <- Median_Beta[Median_Beta$MedianGroup == "2",] 

# MEANING: pair-wise comparison learners and no-learners POST Learning
t.test(Learners_Beta$relSocPost.mean, NoLearners_Beta$relSocPost.mean, paired=F)
mean(Learners_Beta$relSocPost.mean, na.rm = TRUE) # .42
mean(NoLearners_Beta$relSocPost.mean, na.rm = TRUE) # .49
sd(Learners_Beta$relSocPost.mean, na.rm = TRUE)
sd(NoLearners_Beta$relSocPost.mean, na.rm = TRUE)
## Supplemental Material:
# The two sub-groups did not differ in their proportional looking time at the social interaction shape after the learning phase (t(30) = −1.38; p = .18).




###########################################################################
# 
# STATISTICAL ANALYSES MANUAL FORCED-CHOICE TASK 
# BLINDED FOR REVIEW
# March 2019 
#
###########################################################################

## Data are in participant file

binom.test(x = 13, n = 27, p = 0.5,
           alternative = c("two.sided"),
           conf.level = 0.95)
## Supplemental Material:
# Children did not prefer one shape over the other. Thirteen out of 27 children (48%) touched the social shape first (p = 1). 


