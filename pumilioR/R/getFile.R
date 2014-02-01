getFile <- function(result, SoundID = NA, credentials = NA, pumiliologin = NA){
	#Function to download the file associated with a SoundID
	if (is.na(SoundID)){
		stop(" SoundID cannot be empty.")
	}
	
	if (!is.na(credentials)){
		soundfilePath <- unlist(result[result$SoundID==SoundID,]$FilePath)
		soundfilePath <- gsub("http://", paste("http://", credentials, "@", sep=""), soundfilePath)
	}else{
		soundfilePath <- unlist(result[result$SoundID==SoundID,]$FilePath)
		}
	
	localfile = basename(soundfilePath)
	download.file(soundfilePath, destfile = localfile, mode="wb", method="curl")
  
	#Return df of sound data
	invisible(localfile)
}