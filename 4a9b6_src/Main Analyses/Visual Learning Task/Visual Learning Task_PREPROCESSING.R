###########################################################################
# 
# PRE-PROCESSING VISUAL LEARNING data SACC LATENCIES
# BLINDED FOR REVIEW
# March 2019 
#
###########################################################################

# set working directory
setwd("") # insert working directory
rm(list = ls(all = TRUE)) # clears everything
graphics.off()            # close all open graphics

# Set directories
recsDir <- "./recs/"
tablesDir <- "./WriteTables_SL_Firstlook/"

# reads all txt files in recs folder
flist <- list.files(recsDir)

# Loop over all textfiles in recs folder
for (i in 1:length(flist)) {
  
  # Clear workspace.
  rm(list = setdiff(ls(), c('i', "recsDir", "tablesDir", "flist")))
  graphics.off()            # close all open graphics
  
  # import helper functions
  source("./Learning_functions.r")

  # read tsv files 
  myData <- read.table(file = file.path(recsDir, flist[i]), sep = '\t', header = TRUE)
  
  currentSubjectName <- flist[i]
  
  # create a new column 'Events'
  myData['Events'] <- NA
  
  # create a new column 'Samples' that acts as a ID
  myData$Samples <- seq.int(nrow(myData))
  
  
  # Remove unnecessary columns
  myData <- myData[, c(
    "Samples",
    "ParticipantName",
    "RecordingName",
    "RecordingTimestamp",
    "StudioEventIndex",
    "StudioEvent",
    "StudioEventData",
    "FixationIndex",
    "GazeEventType",
    "GazeEventDuration",
    "GazePointIndex",
    "GazePointLeftX..ADCSpx.",
    "GazePointLeftY..ADCSpx.",
    "GazePointRightX..ADCSpx.",
    "GazePointRightY..ADCSpx.",
    "GazePointX..ADCSpx.",
    "GazePointY..ADCSpx.",
    "Events",
    "PupilLeft",
    "PupilRight"
  )]
  
  ###following #3 important for overall boundaries for trials (beginning very first trial (scene started attention grab), end very last trial (whatever the last learning event of the last shown trial is - flexible))
  #1 get first trial position/index of Attentiongrabber
  firstTrialIndex <- which(grepl('Phase_Learn_Subphase_PreGap', myData$StudioEventData))[1]
  firstTrialAttentiongrabber <- myData$StudioEventIndex[firstTrialIndex] - 2  # to get to Attentiongrabber StudioEventIndex (-2 to get on SceneStarted)
  #2 get the corresponding rownumber (row in which attentiongrabber starts)
  firstTrialAttentiongrabberIndex <- which(myData$StudioEventIndex == firstTrialAttentiongrabber)
  #3 get last ... new approach (function)
  lastTrialLastSceneEndedIndex <- GetLastLearningEventSample()
  
  # create Trial column
  myData['Trials'] <- NA
  # set it to 0
  trialCounter <- 0
  
  # set boundaries for total events (events coloumn) --> more than just learning phase (wanting etc)
  # max(myData$StudioEventIndex, na.rm = TRUE) is the index for 'MovieEnded', howerver we are looking for ...
  # ... the last 'SceneEnded', which is located at the index of max(...) - 1
  lastSceneEnded <- max(myData$StudioEventIndex, na.rm = TRUE) - 1  # movie end index - 1
  
  # initial start for copying the text from StudioEventData to the 'Events' column
  even <- 2
  odd <- 3
  # we are looking at what index number 2 and number 3 are located at StudioEventIndex (set boundaries for one event (e.g. "calibrationupleft"))
  startIndex <- which(myData$StudioEventIndex == even)
  endIndex <- which(myData$StudioEventIndex == odd)
  
  #Following loop fills up Event coloum ("vorgehen R" document step 1)
  for (i in seq(even, lastSceneEnded, by = even)) {
    for (j in startIndex:endIndex) {
      # writing the StudioEventData in all rows of 'Events'
      myData[j, 'Events'] <- as.character(myData[startIndex, 'StudioEventData'])
      
      # write the Trial counter
      # it checks if the Phase_Attentiongrabber is part of StudioEventData and it makes sure that the boundaries are within first and last trial
      # the modulo checks if the Attentiongrabber is start or end: Even is always start of an event, where odd is always the end
      # (expression %% 2 == 0) is checking if the expression is odd or even, because it is either returning 0 or 1. (%% 2 == 0) means even, where (%% 2 == 1) means odd
      if (grepl('Phase_Attentiongrabber', as.character(myData[j, 'StudioEventData'])) && j >= firstTrialAttentiongrabberIndex && j <= lastTrialLastSceneEndedIndex && myData[j, 'StudioEventIndex'] %% 2 == 0) {
        trialCounter <- trialCounter + 1
        myData[j, 'Trials'] <- trialCounter
      } else if (trialCounter > 0 && j <= lastTrialLastSceneEndedIndex) {
        myData[j, 'Trials'] <- trialCounter
      }
    }
    
    # update start and end index e.g. to 4 and 5, 6 and 7, ..., even and odd -> never from odd to even auffüllen, dadurch wird nicht zwsichen scene stop der vorherigen scene und scene start der nächsten scene aufgefüllt (siehe step 1 in doc)
    startIndex <- which(myData$StudioEventIndex == i + even)
    endIndex <- which(myData$StudioEventIndex == i + odd)
  }
  
  # creating a backup of the data frame (Kopie von mydata bevor weiter daran garbeitet wird)
  # for a later step we need to read out some information of Subphase_Cue, since Phase_Learn_Subphase_CueDelay is not available when...
  # ... subject is not looking at the screen at all
  myDataFull <- myData
  
  
  # grepl is RegEx (Regular Expression) statement (l is for logical)
  # the grepl statement looks for a given string, e.g. "SceneStarted"
  # it is important that these rows get removed first
  myData <- myData[!grepl("SceneStarted", myData$StudioEvent),]
  myData <- myData[!grepl("SceneEnded", myData$StudioEvent),]
  
  
  # since when there is the event _CueDelay_ the trigger for a valid trial got called, however it can happen that all CueDelays...
  # ... are unclassified (that means no Fixation) and they get removed in a later step, resulting that there is no CueDelay to count from
  # Because of that we look for the very first CueDelay Sample of each trial and check if it is a Fixation, and if not, we change it to Fixation
  for (i in 1:trialCounter) {
    if (is.na((as.character(myData$GazeEventType[which(myData$Trials == i & grepl("Phase_Learn_Subphase_CueDelay", myData$Events))[1]]) != 'Fixation'))) 
      next
    else
      myData$GazeEventType[which(myData$Trials == i & grepl("Phase_Learn_Subphase_CueDelay", myData$Events))[1]] <- "Fixation"
  }
  
  
  ### DELETING ROWS ###
  
  # delete all rows that contain NA in the column trials (alle samples zwischen scene stop und nächstem start löschen; und allen samples vor und nach der learning phase)
  myData <- myData[!is.na(myData$Trials), ]
  
  # Now we can delete all rows that do not(!) contain 'Fixation' in colum GazeEventType
  # This also removes trials where subject did not looked at all to the screen (Case 3: caseSkip)
  myData <- myData[grepl("Fixation", myData$GazeEventType),]
  
  # delete all samples that contain gaze coordinates outside the screen, i.e., for left eye and right eye: Remove all samples with x or y < 0 OR x > 1920 OR y > 1080
  myData <- myData[with(myData, (
    ((GazePointLeftX..ADCSpx. >= 0 & GazePointLeftX..ADCSpx. <= 1920) | is.na(myData$GazePointLeftX..ADCSpx.)) &
      ((GazePointLeftY..ADCSpx. >= 0 & GazePointLeftY..ADCSpx. <= 1080) | is.na(myData$GazePointLeftY..ADCSpx.)) &
      ((GazePointRightX..ADCSpx. >= 0 & GazePointRightX..ADCSpx. <= 1920) | is.na(myData$GazePointRightX..ADCSpx.)) &
      ((GazePointRightY..ADCSpx. >= 0 & GazePointRightY..ADCSpx. <= 1080) | is.na(myData$GazePointRightY..ADCSpx.))
  )),]
  
  # delte rows that contain Phase_FillerVid
  myData <- myData[!grepl("Phase_FillerVid", myData$Events),]
  myData <- myData[!grepl("Phase_BlackScreen", myData$Events),]
  
  #######################
  
  ### create AOIs
  AOI_Left <- matrix(c(111, 327, 709, 753), nrow = 2, ncol = 2)
  AOI_Right <- matrix(c(1211, 327, 1809, 753), nrow = 2, ncol = 2)
  
  # create a new column 'AOI'
  myData['AOI'] <- NA
  
  # check for all gazpoints if they are in a AOI surface using the columns ...
  # ... GazePointX..ADCSpx. and GazePointY..ADCSpx.
  gazeLength <- length(myData$GazePointX..ADCSpx.)  # you could also Gaze...Y. This is the total numbers of rows for all fixation coords
  for (i in 1:gazeLength) {
    
    # since there are some GazePointX and GazePointY that have no data (NA) we just skip then using next
    if (is.na(myData$GazePointX..ADCSpx.[i]) || is.na(myData$GazePointY..ADCSpx.[i])) {
      next  # next means use the next (sample) in the loop/row
    }
    else {
      myData$AOI[i] <- AoiChecker(myData$GazePointX..ADCSpx.[i], myData$GazePointY..ADCSpx.[i]) 
    }
  }
  
  # Hard code that the very first CueDelay has an AOI of FALSE
  for (i in 1:trialCounter) {
    myData$AOI[which(myData$Trials == i & grepl("Phase_Learn_Subphase_CueDelay", myData$Events))[1]] <- FALSE
  }
  
  
  ################################################################################
  ################################################################################
  # SUMMARY DATAFRAME
  ################################################################################
  
  # remove all rows that contain Attentiongrabber and Cue_Shape and PreGap
  myData <- myData[!grepl("Phase_Attentiongrabber", myData$Events),]
  myData <- myData[!grepl("Phase_Learn_Subphase_Cue_Shape", myData$Events),]
  myData <- myData[!grepl("Phase_Learn_Subphase_PreGap", myData$Events),]
  
  # initiate first look arrays NEU!!
  firstLeft <- array(1:trialCounter)
  firstRight <- array(1:trialCounter)
  
  # create a subset of myData (safes some calculations in the next steps)
  # this subset only has the columns AOI and Trials
  myDataSub <- myData[c('Samples', 'Events', 'Trials', 'AOI')]
  
  # count how many samples it took the subject to look for the first time at a certain AOI
  # All samples get subtracted by -1 to achieve zero-based onsets for the old version
  for (i in 1:trialCounter) {
    myDataSubI <- myDataSub[ which( myDataSub$Trials %in% i) , ]
    
    ## first occurance of ... using samples difference from target minus first sample
    ## differnce is: targetAOI minus first sample CueDelay
    firstLeft[i] <- myDataSubI$Samples[which(grepl('AOI_Left', myDataSubI$AOI))][1] - myDataSubI$Samples[1]
    firstRight[i] <- myDataSubI$Samples[which(grepl('AOI_Right', myDataSubI$AOI))][1] - myDataSubI$Samples[1]
  }
  
  # create all template dataframes for all subjects
  
  
  # create new summary dataframe
  summaryDF <- data.frame(matrix(vector(), 0, 10, dimnames=list(c(), c("Trial", "AOI_Left", "AOI_Right", "AOI_Correct", "Shape", "ShapeMeaning", "PosCue", "PosTarget", "Rule", "Exp"))), stringsAsFactors=F)
  
  for (i in 1:trialCounter) {
    summaryDF[i,1] <- i
    summaryDF[i,2] <- firstLeft[i]
    summaryDF[i,3] <- firstRight[i]

    # check if the trial is actually existing:
    if (is.na(which(myData$Trials == i)[1])) {
      healthyTrial <- FALSE
    } else if (!is.na(which(myData$Trials == i)[1])) {
      healthyTrial <- TRUE
    }
  
    # unlisting the first entry[1] of CueDelay of trial i NEU!!
    # sample:
    # 1     2   3       4       5          6          7       8        9              10        11       12       13          14        15     16
    # "Exp","9","Phase","Learn","Subphase","CueDelay","Shape","Circle","ShapeMeaning","Control","PosCue","UpLeft","PosTarget","LowLeft","Rule","vertical"
    
    if (healthyTrial == TRUE) {
      summaryDF[i,5] <- unlist(strsplit(myData$Events[which(myData$Trials == i)[1]], split="_", fixed=T))[8]
      summaryDF[i,6] <- unlist(strsplit(myData$Events[which(myData$Trials == i)[1]], split="_", fixed=T))[10]
      summaryDF[i,7] <- unlist(strsplit(myData$Events[which(myData$Trials == i)[1]], split="_", fixed=T))[12]
      summaryDF[i,8] <- unlist(strsplit(myData$Events[which(myData$Trials == i)[1]], split="_", fixed=T))[14]
      summaryDF[i,9] <- unlist(strsplit(myData$Events[which(myData$Trials == i)[1]], split="_", fixed=T))[16]
      summaryDF[i,10] <- unlist(strsplit(myData$Events[which(myData$Trials == i)[1]], split="_", fixed=T))[2]
      
      # AOIcorrectChecker(i,unlist(strsplit(myData$Events[which(myData$Trials == i)[1]], split="_", fixed=T))[14])
      summaryDF[i,4] <- AOIcorrectChecker(i,unlist(strsplit(myData$Events[which(myData$Trials == i)[1]], split="_", fixed=T))[14])
      
    } else if (healthyTrial == FALSE) {
      summaryDF[i,4] <- NA
      summaryDF[i,5] <- unlist(strsplit(myDataFull$Events[which(myDataFull$Trials == i & grepl('Phase_Learn_Subphase_Cue', myDataFull$Events))][1], split="_", fixed=T))[8]
      summaryDF[i,6] <- unlist(strsplit(myDataFull$Events[which(myDataFull$Trials == i & grepl('Phase_Learn_Subphase_Cue', myDataFull$Events))][1], split="_", fixed=T))[10]
      summaryDF[i,7] <- unlist(strsplit(myDataFull$Events[which(myDataFull$Trials == i & grepl('Phase_Learn_Subphase_Cue', myDataFull$Events))][1], split="_", fixed=T))[12]
      summaryDF[i,8] <- unlist(strsplit(myDataFull$Events[which(myDataFull$Trials == i & grepl('Phase_Learn_Subphase_Cue', myDataFull$Events))][1], split="_", fixed=T))[14]
      summaryDF[i,9] <- unlist(strsplit(myDataFull$Events[which(myDataFull$Trials == i & grepl('Phase_Learn_Subphase_Cue', myDataFull$Events))][1], split="_", fixed=T))[16]
      summaryDF[i,10] <- unlist(strsplit(myDataFull$Events[which(myDataFull$Trials == i & grepl('Phase_Learn_Subphase_Cue', myDataFull$Events))][1], split="_", fixed=T))[2]
     }
    
  }

AOIcorrect_ms = summaryDF$AOI_Correct*(1000/120)
summaryDF$Correct_AOI <- AOIcorrect_ms
summaryDF$AOI_Incorrect <- ""
summaryDF$FirstAOI <- ""

summaryDF$AOI_Incorrect <- as.numeric(summaryDF$AOI_Incorrect)

# Write AOI_Incorrect column
for (i in 1:trialCounter) {

  if (summaryDF$PosTarget[i] == "Left")
    summaryDF$AOI_Incorrect[i] <- summaryDF$AOI_Right[i]
  
  if (summaryDF$PosTarget[i] == "Right")
    summaryDF$AOI_Incorrect[i] <- summaryDF$AOI_Left[i]
}

for (i in 1:trialCounter) {
  if(!is.na(summaryDF$AOI_Correct[i]) && !is.na(summaryDF$AOI_Incorrect[i])) {
    if (summaryDF$ShapeMeaning[i] == "Social" && summaryDF$AOI_Correct[i] < summaryDF$AOI_Incorrect[i])
      summaryDF$FirstAOI[i] <- "Social"
    
    if (summaryDF$ShapeMeaning[i] == "Social" && summaryDF$AOI_Correct[i] > summaryDF$AOI_Incorrect[i])
      summaryDF$FirstAOI[i] <- "Control"
    
    if (summaryDF$ShapeMeaning[i] == "Control" && summaryDF$AOI_Correct[i] < summaryDF$AOI_Incorrect[i])
      summaryDF$FirstAOI[i] <- "Control"
    
    if (summaryDF$ShapeMeaning[i] == "Control" && summaryDF$AOI_Correct[i] > summaryDF$AOI_Incorrect[i])
      summaryDF$FirstAOI[i] <- "Social"
  }
  
  
  if(is.na(summaryDF$AOI_Incorrect[i]) || is.na(summaryDF$AOI_Correct[i])) {
    
    if (summaryDF$ShapeMeaning[i] == "Social" && is.na(summaryDF$AOI_Correct[i]))
      summaryDF$FirstAOI[i] <- "Control"
    
    if (summaryDF$ShapeMeaning[i] == "Social" && is.na(summaryDF$AOI_Incorrect[i]))
      summaryDF$FirstAOI[i] <- "Social"
    
    if (summaryDF$ShapeMeaning[i] == "Control" && is.na(summaryDF$AOI_Correct[i]))
      summaryDF$FirstAOI[i] <- "Social"
    
    if (summaryDF$ShapeMeaning[i] == "Control" && is.na(summaryDF$AOI_Incorrect[i]))
      summaryDF$FirstAOI[i] <- "Control"
    
    # both correct and incorrect is na
    if (is.na(summaryDF$AOI_Correct[i]) && is.na(summaryDF$AOI_Correct[i]))
      summaryDF$FirstAOI[i] <- "NA"
         
    }
  }
}


### Write TSV for statistical processing
socialSummaryDF$TrialsSocial <- seq.int(nrow(socialSummaryDF))
controlSummaryDF$TrialsControl <- seq.int(nrow(controlSummaryDF))

write.table(controlSummaryDF, file = paste(tablesDir,"controlSummary_",currentSubjectName), sep="\t", quote = FALSE)
write.table(socialSummaryDF, file = paste(tablesDir,"socialSummary_",currentSubjectName), sep="\t", quote = FALSE)
}



   
  ########################## Exclude Outliers #############################
socialSummaryDF <- subset(summaryDF, summaryDF$ShapeMeaning=='Social')
controlSummaryDF <- subset(summaryDF, summaryDF$ShapeMeaning=='Control')

#calculate SD and Mean for social and control
SDsocial<-3*sd(socialSummaryDF$Correct_AOI, na.rm=TRUE) 
SDcontrol<-3*sd(controlSummaryDF$Correct_AOI, na.rm=TRUE) 

MeanSocial<-mean(socialSummaryDF$Correct_AOI, na.rm=TRUE)
MeanControl<-mean(controlSummaryDF$Correct_AOI, na.rm=TRUE)

#set criteria for Outlier
GreaterOutliersocial <- MeanSocial+SDsocial
SmallerOutliersocial <- MeanSocial-SDsocial
GreaterOutliercontrol <- MeanControl+SDcontrol
SmallerOutliercontrol <- MeanControl-SDcontrol

# Remove Outlier
BackupSocialSummaryDF <- socialSummaryDF
socialSummaryDF$Correct_AOI[which(socialSummaryDF$Correct_AOI > GreaterOutliersocial)] <- NA
socialSummaryDF$Correct_AOI[which(socialSummaryDF$Correct_AOI < SmallerOutliersocial)] <- NA

BackupControlSummaryDF <- controlSummaryDF
controlSummaryDF$Correct_AOI[which(controlSummaryDF$Correct_AOI > GreaterOutliercontrol)] <- NA
controlSummaryDF$Correct_AOI[which(controlSummaryDF$Correct_AOI < SmallerOutliercontrol)] <- NA

# Remove Data >2000
socialSummaryDF$Correct_AOI[which(socialSummaryDF$Correct_AOI > 2000)] <- NA
controlSummaryDF$Correct_AOI[which(controlSummaryDF$Correct_AOI > 2000)] <- NA













