rm(list = ls(all = TRUE)) # clears everything
graphics.off()            # close all open graphics

setwd("")

recsDir <- "./recs/"
tablesDir <- "./SummaryTables_PrefLook/"

flist <- list.files(recsDir)

summaryProp <- data.frame(matrix(vector(), 0, 8, dimnames=list(c(), c("relCirPre.mean", "relTriPre.mean", "relSocPre.mean", "relConPre.mean", "relCirPost.mean", "relTriPost.mean", "relSocPost.mean", "relConPost.mean"))), stringsAsFactors=F)
summaryWantingDF <- data.frame(matrix(vector(), 0, 16, dimnames = list(c(), c("Trial1PreCir", "Trial1PreTri", "Trial1PreSoc", "Trial1PreCon", "Trial2PreCir", "Trial2PreTri", "Trial2PreSoc", "Trial2PreCon", "Trial1PostCir", "Trial1PostTri", "Trial1PostSoc", "Trial1PostCon", "Trial2PostCir", "Trial2PostTri", "Trial2PostSoc", "Trial2PostCon"))), stringsAsFactors = F)

#for (j in 3:3) {
for (j in 1:length(flist)) {
  
  rm(list = setdiff(ls(), c('j', "recsDir", "flist", "summaryWantingDF", "summaryProp")))
  source("./functionsWanting.r")
  
  myData <- read.table(file = file.path(recsDir, flist[j]), sep = '\t', header = TRUE)
  
  myData$Samples <- seq.int(nrow(myData))
  
  myData$AOI <- NA
  
  myData <- myData[, c(
    "Samples",
    "RecordingTimestamp",
    "StudioEventIndex",
    "StudioEvent",
    "StudioEventData",
    "GazeEventType",
    "GazeEventDuration",
    "GazePointX..ADCSpx.",
    "GazePointY..ADCSpx.",
    "AOI"
  )]
  
  
  
  AOI_Left <- matrix(c(406, 370, 746, 710), nrow = 2, ncol = 2)
  AOI_Right <- matrix(c(1174, 370, 1514, 710), nrow = 2, ncol = 2)
  
  
  for (i in 1:length(myData$GazePointX..ADCSpx.)) {
    
    # since there are some GazePointX and GazePointY that have no data (NA) we just skip then using next
    if (is.na(myData$GazePointX..ADCSpx.[i]) || is.na(myData$GazePointY..ADCSpx.[i])) {
      next  # next means use the next (sample) in the loop/row
    }
    else {
      myData$AOI[i] <- AoiChecker(myData$GazePointX..ADCSpx.[i], myData$GazePointY..ADCSpx.[i]) 
    }
  }
  
  trial1PreStr <- GetWantingPreStrings(1)
  trial2PreStr <- GetWantingPreStrings(3)
  trial1PostStr <- GetWantingPostStrings(1)
  trial2PostStr <- GetWantingPostStrings(3)
  
  
  trial1Pre <- subset(myData, myData$Samples > GetWantingPreIndexes()[1] & myData$Samples < GetWantingPreIndexes()[2])
  trial2Pre <- subset(myData, myData$Samples > GetWantingPreIndexes()[3] & myData$Samples < GetWantingPreIndexes()[4])
  
  if (is.na(GetWantingPostIndexes()[1])) {
    trial1Post <- NA
    trial2Post <- NA
  } else {
    trial1Post <- subset(myData, myData$Samples > GetWantingPostIndexes()[1] & myData$Samples < GetWantingPostIndexes()[2])
    trial2Post <- subset(myData, myData$Samples > GetWantingPostIndexes()[3] & myData$Samples < GetWantingPostIndexes()[4])
  }
  
  
  # delete all GazeEventTypes that are not Fixation
  trial1Pre <- trial1Pre[grepl("Fixation", trial1Pre$GazeEventType),]
  trial2Pre <- trial2Pre[grepl("Fixation", trial2Pre$GazeEventType),]
  if (is.na(GetWantingPostIndexes()[1])) {
    trial1Post <- NA
    trial2Post <- NA
  } else {
    trial1Post <- trial1Post[grepl("Fixation", trial1Post$GazeEventType),]
    trial2Post <- trial2Post[grepl("Fixation", trial2Post$GazeEventType),]
  }
  
  trial1PreAoiLeftLenght <- length(which(trial1Pre$AOI == "AOI_Left"))
  trial1PreAoiRightLenght <- length(which(trial1Pre$AOI == "AOI_Right"))
  trial2PreAoiLeftLenght <- length(which(trial2Pre$AOI == "AOI_Left"))
  trial2PreAoiRightLenght <- length(which(trial2Pre$AOI == "AOI_Right"))
  
  if (is.na(GetWantingPostIndexes()[1])) {
    trial1PostAoiLeftLenght <- NA
    trial1PostAoiRightLenght <- NA
    trial2PostAoiLeftLenght <- NA
    trial2PostAoiRightLenght <- NA
  } else {
    trial1PostAoiLeftLenght <- length(which(trial1Post$AOI == "AOI_Left"))
    trial1PostAoiRightLenght <- length(which(trial1Post$AOI == "AOI_Right"))
    trial2PostAoiLeftLenght <- length(which(trial2Post$AOI == "AOI_Left"))
    trial2PostAoiRightLenght <- length(which(trial2Post$AOI == "AOI_Right"))
  }
  
  # [1]   "Exp"          "4"            "Phase"        "WantingPre"   "LeftShape"    "Triangle"     "LeftMeaning"  "Social"       "RightShape"  
  # [10]  "Circle"       "RightMeaning" "Control"      "PosSocial"    "Left"         "SocialShape"  "Triangle"
  
  # if left shape[6] is circle assign leftLength otherwise rightLength
  if (trial1PreStr[6] == "Circle") summaryWantingDF[j,1] <- trial1PreAoiLeftLenght else summaryWantingDF[j,1] <- trial1PreAoiRightLenght
  
  # if right shape[10] is circle assign rightLength othweise leftLength
  if (trial1PreStr[10] == "Triangle") summaryWantingDF[j,2] <- trial1PreAoiRightLenght else summaryWantingDF[j,2] <- trial1PreAoiLeftLenght
  
  # if PosSocial[14] is left assign leftLength otherwise rightLength 
  if (trial1PreStr[14] == "Left") summaryWantingDF[j,3] <- trial1PreAoiLeftLenght else summaryWantingDF[j,3] <- trial1PreAoiRightLenght
  
  # if PosSocial[14] is right assign left to control otherwise right to control
  if (trial1PreStr[14] == "Right") summaryWantingDF[j,4] <- trial1PreAoiLeftLenght else summaryWantingDF[j,4] <- trial1PreAoiRightLenght
  
  if (trial2PreStr[6] == "Circle") summaryWantingDF[j,5] <- trial2PreAoiLeftLenght else summaryWantingDF[j,5] <- trial2PreAoiRightLenght
  if (trial2PreStr[10] == "Triangle") summaryWantingDF[j,6] <- trial2PreAoiRightLenght else summaryWantingDF[j,6] <- trial2PreAoiLeftLenght
  if (trial2PreStr[14] == "Left") summaryWantingDF[j,7] <- trial2PreAoiLeftLenght else summaryWantingDF[j,7] <- trial2PreAoiRightLenght
  if (trial2PreStr[14] == "Right") summaryWantingDF[j,8] <- trial2PreAoiLeftLenght else summaryWantingDF[j,8] <- trial2PreAoiRightLenght
  
  ## Post
  if (!is.na(trial1PostStr[1])) {
    if (trial1PostStr[6] == "Circle") summaryWantingDF[j,9] <- trial1PostAoiLeftLenght else summaryWantingDF[j,9] <- trial1PostAoiRightLenght
    if (trial1PostStr[10] == "Triangle") summaryWantingDF[j,10] <- trial1PostAoiRightLenght else summaryWantingDF[j,10] <- trial1PostAoiLeftLenght
    if (trial1PostStr[14] == "Left") summaryWantingDF[j,11] <- trial1PostAoiLeftLenght else summaryWantingDF[j,11] <- trial1PostAoiRightLenght
    if (trial1PostStr[14] == "Right") summaryWantingDF[j,12] <- trial1PostAoiLeftLenght else summaryWantingDF[j,12] <- trial1PostAoiRightLenght
  }
  if (!is.na(trial2PostStr[1])) {
    if (trial2PostStr[6] == "Circle") summaryWantingDF[j,13] <- trial2PostAoiLeftLenght else summaryWantingDF[j,13] <- trial2PostAoiRightLenght
    if (trial2PostStr[10] == "Triangle") summaryWantingDF[j,14] <- trial2PostAoiRightLenght else summaryWantingDF[j,14] <- trial2PostAoiLeftLenght
    if (trial2PostStr[14] == "Left") summaryWantingDF[j,15] <- trial2PostAoiLeftLenght else summaryWantingDF[j,15] <- trial2PostAoiRightLenght
    if (trial2PostStr[14] == "Right") summaryWantingDF[j,16] <- trial2PostAoiLeftLenght else summaryWantingDF[j,16] <- trial2PostAoiRightLenght
  }

}

################################## Proportion Scores ##################################

summaryWantingDF$row <- seq.int(nrow(summaryWantingDF))

for (i in summaryWantingDF$row) {
  
  ###calculate proportionscores
  #pre
  relCirPreTrial1 = summaryWantingDF$Trial1PreCir / (summaryWantingDF$Trial1PreCir + summaryWantingDF$Trial1PreTri)
  relCirPreTrial2 = summaryWantingDF$Trial2PreCir / (summaryWantingDF$Trial2PreCir + summaryWantingDF$Trial2PreTri)
  relCirPre.mean = (relCirPreTrial1 + relCirPreTrial2)/2

  relTriPreTrial1 = summaryWantingDF$Trial1PreTri / (summaryWantingDF$Trial1PreCir + summaryWantingDF$Trial1PreTri)
  relTriPreTrial2 = summaryWantingDF$Trial2PreTri / (summaryWantingDF$Trial2PreCir + summaryWantingDF$Trial2PreTri)
  relTriPre.mean = (relTriPreTrial1 + relTriPreTrial2)/2

  relSocPreTrial1 = summaryWantingDF$Trial1PreSoc / (summaryWantingDF$Trial1PreSoc + summaryWantingDF$Trial1PreCon)
  relSocPreTrial2 = summaryWantingDF$Trial2PreSoc / (summaryWantingDF$Trial2PreSoc + summaryWantingDF$Trial2PreCon)
  relSocPre.mean = (relSocPreTrial1 + relSocPreTrial2)/2

  relConPreTrial1 = summaryWantingDF$Trial1PreCon / (summaryWantingDF$Trial1PreSoc + summaryWantingDF$Trial1PreCon)
  relConPreTrial2 = summaryWantingDF$Trial2PreCon / (summaryWantingDF$Trial2PreSoc + summaryWantingDF$Trial2PreCon)
  relConPre.mean = (relConPreTrial1 + relConPreTrial2)/2

  #post
  relCirPostTrial1 = summaryWantingDF$Trial1PostCir / (summaryWantingDF$Trial1PostCir + summaryWantingDF$Trial1PostTri)
  relCirPostTrial2 = summaryWantingDF$Trial2PostCir / (summaryWantingDF$Trial2PostCir + summaryWantingDF$Trial2PostTri)
  relCirPost.mean = (relCirPostTrial1 + relCirPostTrial2)/2

  relTriPostTrial1 = summaryWantingDF$Trial1PostTri / (summaryWantingDF$Trial1PostCir + summaryWantingDF$Trial1PostTri)
  relTriPostTrial2 = summaryWantingDF$Trial2PostTri / (summaryWantingDF$Trial2PostCir + summaryWantingDF$Trial2PostTri)
  relTriPost.mean = (relTriPostTrial1 + relTriPostTrial2)/2

  relSocPostTrial1 = summaryWantingDF$Trial1PostSoc / (summaryWantingDF$Trial1PostSoc + summaryWantingDF$Trial1PostCon)
  relSocPostTrial2 = summaryWantingDF$Trial2PostSoc / (summaryWantingDF$Trial2PostSoc + summaryWantingDF$Trial2PostCon)
  relSocPost.mean = (relSocPostTrial1 + relSocPostTrial2)/2
  
  relConPostTrial1 = summaryWantingDF$Trial1PostCon / (summaryWantingDF$Trial1PostSoc + summaryWantingDF$Trial1PostCon)
  relConPostTrial2 = summaryWantingDF$Trial2PostCon / (summaryWantingDF$Trial2PostSoc + summaryWantingDF$Trial2PostCon)
  relConPost.mean = (relConPostTrial1 + relConPostTrial2)/2

  #fill in data frame
  summaryProp[i,1] = rbind(relCirPost.mean[i])
  summaryProp[i,2] = rbind(relTriPost.mean[i])
  summaryProp[i,3] = rbind(relSocPost.mean[i])
  summaryProp[i,4] = rbind(relConPost.mean[i])
  summaryProp[i,5] = rbind(relCirPre.mean[i])
  summaryProp[i,6] = rbind(relTriPre.mean[i])
  summaryProp[i,7] = rbind(relSocPre.mean[i])
  summaryProp[i,8] = rbind(relConPre.mean[i])

}

tablesDir <- "./SummaryTables_PrefLook/"
write.table(summaryProp, file = paste(tablesDir,"PrefLooking_Summary_LT.txt"), sep="\t", quote = FALSE)



