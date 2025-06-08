# This gets the last LearningEventSample based on the StudioEventIndex Column
# this function should be run before anything gets removed from the data
GetLastLearningEventSample <- function() {
  
  # Get all attentiongrabber events
  grabber <- myData$StudioEventIndex[which(grepl('Phase_Attentiongrabber', myData$StudioEventData))]
  # get all odd events (so we end up with SceneEnded Attentiongrabber events)
  grabber <- grabber[(grabber %% 2 == 1)]
  
  # get all pregap events
  pregap <- myData$StudioEventIndex[which(grepl('Phase_Learn_Subphase_PreGap', myData$StudioEventData))]
  # get all even events (so we end up with SceneStarted PreGap Events)
  pregap <- pregap[(pregap %% 2 == 0)]
  
  # combine the both in a vector
  grabberPregapPairs <- c(grabber, pregap)
  
  # sort it
  grabberPregapPairs <- sort(grabberPregapPairs)
  
  # use the difference function to see wich Attentiongrabber is adjacent with PreGap and have a distance of 1 (that means they are direct neighbours) ...
  # ... and get the last pair of neighbours
  lastLearningAttentiongrabber <- grabberPregapPairs[tail(which(diff(grabberPregapPairs) == 1), n=1)]
  
  # iterate over all Samples in this last learning group to find the last event that is not(!) a PreGap, Cue, CueDelay, PostGap or Target
  foundLastLearningEvent <- FALSE
  
  # start with the First event after (+1) Attentiongrabber (usually PreGap is here)
  jj <- lastLearningAttentiongrabber + 1
  
  # define Strings that are allowed to follow after Attentiongrabber
  allowedNeighbours <- c('Phase_Learn_Subphase_PreGap', 'Phase_Learn_Subphase_Cue_Shape', 'Phase_Learn_Subphase_CueDelay', 'Phase_Learn_Subphase_PostGap', 'Phase_Learn_Subphase_Target')
  
  while (!foundLastLearningEvent) {
    
    lastLearningEvent <- as.character(myData$StudioEventData[which(myData$StudioEventIndex == jj)])
    for (i in 1:length(allowedNeighbours)) {
      if (grepl(allowedNeighbours[i], lastLearningEvent))
        break
      else if (i == length(allowedNeighbours) & !grepl(allowedNeighbours[i], lastLearningEvent))
        foundLastLearningEvent <- TRUE
    }
    
    jj <- jj + 1
   
  }
  
  return(myData$Samples[which(myData$StudioEventIndex == jj - 2)])
  
}


# checks if a pair of given coordinates belong to any AOI surface, and returns the AOI as string ...
# basically it checks for each x if its between x1 and x2, that is: |x1 <= x => x2| same for each y
AoiChecker <- function(x, y) {
  # check for AOI_UPLeft, that is check if x y is in the surface of AOI_UPLeft
  if (x >= AOI_Left[1, 1] && y >= AOI_Left[2, 1] && x <= AOI_Left[1, 2] && y <= AOI_Left[2, 2])
    return("AOI_Left")  # if x and(!) y was found within the boundaries return "AOI_UPLeft"
  
  # Check for AOI_UPRight
  if (x >= AOI_Right[1, 1] && y >= AOI_Right[2, 1] && x <= AOI_Right[1, 2] && y <= AOI_Right[2, 2])
    return("AOI_Right")
  
  else
    # if there is no match at all, the subject did not in any of the given AOIs, thus return false
    return(FALSE)
}



# helper function for the loop
AOIcorrectChecker <- function(i, PosTarget) {
  if (PosTarget == "Left") {
    return(summaryDF$AOI_Left[i])
  }
  
  if (PosTarget == "Right") {
    return(summaryDF$AOI_Right[i])
  }
  else {
    return(NA)
  }
}