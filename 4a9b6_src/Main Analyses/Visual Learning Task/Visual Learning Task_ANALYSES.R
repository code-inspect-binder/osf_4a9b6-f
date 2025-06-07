###########################################################################
# 
# STATISTICAL ANALYSES VISUAL LEARNING TASK 
# BLINDED FOR REVIEW
# March 2019 
#
###########################################################################

# set working directory
setwd("") # insert working directory
rm(list = ls(all = TRUE)) 
graphics.off()           

#load.packages
require(lme4)
library(lme4)
library("exactRankTests")

# read data
OverallSocCon <- read.table(file = "AllData_Stats.txt", sep = '\t', header = TRUE) # for models with sacc latency and looking time
OverallSoc <- OverallSocCon[OverallSocCon$Condition == "Soc",] # for means and sds within condition social
OverallCon <- OverallSocCon[OverallSocCon$Condition == "Con",] # for means and sds within condition control
OverallSocCon_pred <- read.table(file = "AllData_Binom.txt", sep = '\t', header = TRUE) # for binomial model with predicitive looks
OverallFirstLook <- read.table(file = "AllData_FirstLook.txt", sep = '\t', header = TRUE) # for first look analyses


############################## SACCADIC LATENCY ANALYSES ##############################

########  Main Model INTERACTION Condition*Trial ###############

## Manuscript:
# To compare the change in saccadic latency to the cued target region between conditions, 
# we conducted a general linear mixed model (GLMM; Gaussian error distribution) for saccadic latency, 
# including the interaction between trial (24 trials) and condition (social interaction, non-interactive control) as fixed effect. 
# As random effects, we included subject as intercept, as well as random slopes for trial on subject, condition, 
# and the interaction between trial and condition. P-values for the individual fixed effects were based on likelihood ratio tests 
# comparing the full models with respective reduced models using the drop1-function in R. 

# z-standardizing
OverallSocCon$z.TrialPosition <- as.vector(scale(OverallSocCon$TrialPosition))

# Condition.code
OverallSocCon$Condition.code <- as.vector(scale(as.numeric(OverallSocCon$Condition)))

model1 = lmer(SaccLatency ~ Condition*z.TrialPosition + (1|ID) + (0+z.TrialPosition|ID) + (0+Condition.code|ID) + (0+Condition.code:z.TrialPosition|ID), data=OverallSocCon) 
drop1(model1, test="Chisq")
summary(model1)
## Manuscript:
# Infants’ learning between the two conditions did not change over time, χ2(1) = .87, p = .35. 


########  Main Model MAIN EFFECTS Condition + Trial ###############

model2 = lmer(SaccLatency ~ Condition + z.TrialPosition + (1|ID) + (0+z.TrialPosition|ID) + (0+Condition.code|ID)+ (0+Condition.code:z.TrialPosition|ID), data=OverallSocCon) 
drop1(model2, test="Chisq")
summary(model2)

# means and SDs 
mean(OverallSoc$SaccLatency, na.rm = TRUE) #[1] 1107.347
sd(OverallSoc$SaccLatency, na.rm = TRUE) #[1] 331.6805
mean(OverallCon$SaccLatency, na.rm = TRUE) #[1] 1284.797
sd(OverallCon$SaccLatency, na.rm = TRUE) #[1] 198.0448
## Manuscript:
# Instead and overall, infants were faster to anticipate the location of the social target videos (M = 1107.35 ms, SD = 331.68) 
# compared to the non-interactive control videos (M = 1284.79 ms, SD = 198.04; χ2(1) = 9.53, p = .002, estimate = −172.18, SE = 52.5; see Figure 2). 
# We did not find a main effect of trial, χ2(1) = .08, p = .78. 


########  number of valid trials ###############

OverallSocCon_pred_Soc <- OverallSocCon_pred[OverallSocCon_pred$Condition == "Soc",]
OverallSocCon_pred_Con <- OverallSocCon_pred[OverallSocCon_pred$Condition == "Con",]
mean(OverallSocCon_pred_Soc$total_valid) # 11.03
sd(OverallSocCon_pred_Soc$total_valid) # 1.20
min(OverallSocCon_pred_Soc$total_valid) # 8
mean(OverallSocCon_pred_Con$total_valid) # 11.03
sd(OverallSocCon_pred_Con$total_valid) # 1.12
min(OverallSocCon_pred_Con$total_valid) # 8
## Manuscript
# After excluding trials due to these criteria, all participants contributed at least 8 valid social interaction trials (M = 11.03, SD = 1.20) 
# and 8 control trials (M = 11.03, SD = 1.12) to our final dataset. 


########  Additional analysis: first trial analysis ###############
OverallSocCon_trial1 <- OverallSocCon[OverallSocCon$Trial == "1",] #subset with trial 1 only

res = lmer(SaccLatency ~ Condition + (1|ID), data=OverallSocCon_trial1) 
drop1(res, test="Chisq")
## Supplemental Material
# First, an exploratory comparison of saccadic latencies on trial 1 showed that the main effect of condition was not present initially, 
# suggesting that it rather emerged during the learning task, χ2(1) = 3.47, p = .06. 


########  Additional analysis: first 6 trials analysis ###############
OverallSocCon_first6 <- OverallSocCon[OverallSocCon$Trial == "1" | OverallSocCon$Trial == "2" | OverallSocCon$Trial == "3" | OverallSocCon$Trial == "4" | OverallSocCon$Trial == "5" | OverallSocCon$Trial == "6",]
contr=lmerControl(check.conv.singular = .makeCC(action = "ignore",  tol = 1e-4)) # für 6 trial analyse: That leaves in the default tolerance (which makeCC needs) but suppresses the singular fit test. 

res = lmer(SaccLatency ~ Condition*z.TrialPosition + (1|ID) + (0+z.TrialPosition|ID) + (0+Condition.code|ID) + (0+Condition.code:z.TrialPosition|ID), data=OverallSocCon_first6, control=contr) #control damit Warnmeldung mit singular fit nicht kommt.
drop1(res, test="Chisq")
summary(res)
## Supplemental Material
# Following the assumption that this general decrease in attention had caused the continuous increase in saccadic latencies throughout the second half of the experiment, 
# we ran an exploratory GLMM for saccadic latency over the first six trials only, revealing a significant interaction between condition and trial 
# (χ2(1) = 4.65, p = .03, estimate = −89.72, SE = 41.58). 


########  Additional analysis: Predictive gaze shifts ###############

# Condition.code
OverallSocCon_pred$Condition.code <- as.vector(scale(as.numeric(OverallSocCon_pred$Condition)))

# Binomial model
Model <- glmer(formula = cbind(pred,nonpred) ~ Condition + (1+Condition.code||ID), family = binomial, data=OverallSocCon_pred)
drop1(Model, test = "Chisq")
summary(Model)

# means and sds
mean(OverallSocCon_pred$Prop_pred[OverallSocCon_pred$Condition == "Soc"]) # mean proportion social interaction condition = .45
sd(OverallSocCon_pred$Prop_pred[OverallSocCon_pred$Condition == "Soc"]) # SD social interaction condition = .35
mean(OverallSocCon_pred$Prop_pred[OverallSocCon_pred$Condition == "Con"]) # mean proportion control condition = .18
sd(OverallSocCon_pred$Prop_pred[OverallSocCon_pred$Condition == "Con"]) # SD control condition = .20

# total number of predictive gaze shifts
sum(OverallSoc$SaccLatency <= 1200, na.rm = TRUE) # 162
sum(OverallCon$SaccLatency <= 1200, na.rm = TRUE) # 62
## Manuscript
# A GLMM for predictive and reactive gaze shifts (binomial error structure) revealed that infants performed more predictive eye-movements during 
# social interaction trials (mean proportion = .45, SD = .35) compared to control trials (mean proportion = .18, SD = .20; χ2(1) = 8.78, p = .003, estimate = 1.90, SE = .62). 
# In the GLMM, condition was included as fixed effect, subject as random intercept, as well as a random slope of trial on subject. 


########  Additional analysis: First Look Pattern ###############

####One-sample t-tests
t.test(OverallFirstLook$SocCue_SocProp, mu=0.50, alternative="two.sided")
t.test(OverallFirstLook$ConCue_ConProp, mu=0.50, alternative="two.sided")
###Pairwise comparison between conditions
t.test(OverallFirstLook$SocCue_SocProp, OverallFirstLook$ConCue_ConProp, paired=T)
#mean and sd 
mean(OverallFirstLook$SocCue_SocProp, na.rm=TRUE) # 0.72
mean(OverallFirstLook$ConCue_ConProp, na.rm=TRUE) # 0.72
sd(OverallFirstLook$SocCue_SocProp, na.rm=TRUE) # 0.32
sd(OverallFirstLook$ConCue_ConProp, na.rm=TRUE) # 0.31
## Manuscript
# Two one-sample t-tests showed that the proportional number of first looks at the correct target region was greater than chance in 
# both conditions (social interaction condition: M = .72, SD = .32; t(31) = 3.90, p = <.001; control condition: M = .72, SD = .31; t(31) = 4.14, p = <.001), 
# with no difference between conditions (paired t-test, t(31) = −.02, p = .98). 

#total number of first looks
sum(OverallFirstLook$SocCue_SocAOI) #261
sum(OverallFirstLook$SocCue_ConAOI) #103
sum(OverallFirstLook$ConCue_SocAOI) #101
sum(OverallFirstLook$ConCue_ConAOI) #266
## Supplemental Material
# In complementing the first look analysis in the main document, Table S2 shows the total number of first looks at the two target AOIs following social interaction and control cue.


########  Additional analysis: Looking time ###############

########  Model INTERACTION Condition*Trial ###############
contr=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1000000)) # sonst konvergiert Modell nicht
res = lmer(LookingTime ~ Condition*z.TrialPosition + (1|ID) + (0+z.TrialPosition|ID) + (0+Condition.code|ID) + (0+Condition.code:z.TrialPosition|ID), data=OverallSocCon, control = contr) 
drop1(res, test="Chisq")
summary(res)

########  Model MAIN EFFECTS Condition + Trial ###############
res = lmer(LookingTime ~ Condition + z.TrialPosition + (1|ID) + (0+z.TrialPosition|ID) + (0+Condition.code|ID), data=OverallSocCon) 
drop1(res, test="Chisq")
summary(res)
## Manuscript
# In order to explore possible habituation effects to the target videos as one possible explanation of this pattern, we conducted the same GLMMs for looking time at the target videos as we ran for our main analysis of saccadic latency. 
# We found a continuous decrease in looking time at the target videos, indicating a general decrease in sustained attention throughout the experiment (main effect of trial, χ2(1) = 11.12, p < .001, estimate = −187.09, SE = 52.06).


