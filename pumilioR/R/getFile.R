getFile <- function(result, SoundID = NA, credentials = NA, pumiliologin = NA){
	#Function to download the file associated with a SoundID
	if (is.na(SoundID)){
		stop(" SoundID cannot be empty.")
	}
	
	if (.Platform$OS.type == "windows") {
    #Fix for Windows systems, rarely is CURL installed and the default way
    # to download is very limited. This uses Internet Explorer functions
	  setInternet2(TRUE)
	}else{
  	if (!is.na(credentials)){
  		#Check if curl is installed
  		if (system("curl -V", ignore.stderr = TRUE)!=0){
  			stop("curl was not found")
  		}
  	}
	}
    
		soundfilePath <- unlist(result[result$SoundID==SoundID,]$FilePath)
		soundfilePath <- gsub("http://", paste("http://", credentials, "@", sep=""), soundfilePath)
		localfile = basename(soundfilePath)
		download.file(soundfilePath, destfile = localfile, mode="wb", method="curl")
	}else{
		soundfilePath <- unlist(result[result$SoundID==SoundID,]$FilePath)
		localfile = basename(soundfilePath)
		download.file(soundfilePath, destfile = localfile, mode="wb")
		}
	
	#Return df of sound data
	invisible(localfile)
}