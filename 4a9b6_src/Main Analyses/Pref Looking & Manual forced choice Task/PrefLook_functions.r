GetWantingPreIndexes <- function() {
  wantingPreIndexes <- myData$StudioEventIndex[which(grepl("Phase_WantingPre", myData$StudioEventData))]
  
  # checking if the first index pair[1] and the last[3] is adjacent, i.e., if their difference is 1:
  if ((diff(wantingPreIndexes) == 1)[1] &
      (diff(wantingPreIndexes) == 1)[3]) {
    return(c(
      which(myData$StudioEventIndex == wantingPreIndexes[1]),
      which(myData$StudioEventIndex == wantingPreIndexes[2]),
      which(myData$StudioEventIndex == wantingPreIndexes[3]),
      which(myData$StudioEventIndex == wantingPreIndexes[4])
    ))
  }
  
  return(NA)
}

GetWantingPostIndexes <- function() {
  
  if (length(which(grepl("Phase_WantingPost", myData$StudioEventData))) == 0) return(NA)
  
  wantingPostIndexes <- myData$StudioEventIndex[which(grepl("Phase_WantingPost", myData$StudioEventData))]
  
  # checking if the first index pair[1] and the last[3] is adjacent, i.e., if their difference is 1:
  if ((diff(wantingPostIndexes) == 1)[1] &
      (diff(wantingPostIndexes) == 1)[3]) {
    return(c(
      which(myData$StudioEventIndex == wantingPostIndexes[1]),
      which(myData$StudioEventIndex == wantingPostIndexes[2]),
      which(myData$StudioEventIndex == wantingPostIndexes[3]),
      which(myData$StudioEventIndex == wantingPostIndexes[4])
    ))
  }
  
  return(NA)
}

# Exp_4_Phase_WantingPre_LeftShape_Triangle_LeftMeaning_Social_RightShape_Circle_RightMeaning_Control_PosSocial_Left_SocialShape_Triangle
GetWantingPreStrings <- function(trialIndex) {
  return(unlist(strsplit(as.character(myData$StudioEventData[which(myData$Samples == GetWantingPreIndexes()[trialIndex])]), split = "_", fixed = T)))
}

GetWantingPostStrings <- function(trialIndex) {
  if (is.na(GetWantingPostIndexes()[1])) return(NA)
  return(unlist(strsplit(as.character(myData$StudioEventData[which(myData$Samples == GetWantingPostIndexes()[trialIndex])]), split = "_", fixed = T)))
}

AoiChecker <- function(x, y) {
  if (x >= AOI_Left[1, 1] && y >= AOI_Left[2, 1] && x <= AOI_Left[1, 2] && y <= AOI_Left[2, 2])
    return("AOI_Left")
  
  # Check for AOI_Right
  if (x >= AOI_Right[1, 1] && y >= AOI_Right[2, 1] && x <= AOI_Right[1, 2] && y <= AOI_Right[2, 2])
    return("AOI_Right")
  
  else
    # if there is no match at all, the subject did not in any of the given AOIs, thus return false
    return(FALSE)
}

GetValueFromStringKey <- function(string, key) {
  
  # destructure string into an array using "_" as the delimiter
  stringArr <- unlist(strsplit(as.character(string), split = "_", fixed = T))
  
  # check if stringArr is even, if not send warning
  if (length(stringArr) %% 2 != 0)
    stop("Provided string is not even. It must have this format: Key1_Value1_Key2_Value2_Key3_Value3")
  
  
  # get keys:
  keys <- stringArr[c(TRUE, FALSE)]
  
  # get values:
  values <- stringArr[c(FALSE, TRUE)]
  
  # combine
  lookup <- setNames(as.list(values), keys)
  
  return(lookup[[key]])
}

