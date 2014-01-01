getFile <- function(result, SoundID = NA){
	#Function to download the file associated with a SoundID
	if (is.na(SoundID)){
		stop(" SoundID cannot be empty.")
	}
	
	soundfilePath <- unlist(result[result$SoundID==SoundID,]$FilePath)
	
	download.file(soundfilePath, destfile=basename(soundfilePath), mode="wb")
	
	#Return df of sound data
	invisible(basename(soundfilePath))
}