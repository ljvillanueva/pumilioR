getSounds <- function(pumilio_URL, SiteID = NA, ColID = NA, type = "all", credentials = NA, pumiliologin = NA){
  
	if (!is.na(credentials)){
		pumilio_URL <- gsub("http://", paste("http://", credentials, "@", sep=""), pumilio_URL)
		}
  
	#Function to get the info of sound files that match a query, 
	# or all sounds in archive is no query is used.
	if (getVersion(pumilio_URL, pumiliologin = pumiliologin) == FALSE){
		stop(" pumilioR only works with Pumilio version 2.6.0 or newer or you have no access rights.")
	}

	if (!is.na(pumiliologin)){
		pumilio_XML_URL <- paste(pumilio_URL, "xml.php?login=", pumiliologin, sep = "")
	}else{
		pumilio_XML_URL <- paste(pumilio_URL, "xml.php", sep = "")
	}
	
	if (type!="all" & is.na(SiteID) & is.na(ColID)){
		stop(" Both ColID and SiteID cannot be empty when type is not 'all'.")
	}
	
	pumilio_XML_URL <- paste(pumilio_URL, "xml.php", sep="")
	
	this_site_sounds <- xmlTreeParse(getURL(paste(pumilio_XML_URL, "?type=", type, "&SiteID=", SiteID, "&ColID=", ColID, sep="")))
	sounds_list <- xmlToList(node = this_site_sounds, addAttributes = TRUE)
	
	#Get sounds from parsed XML
	sound_list <- as.data.frame(t(sounds_list$Sounds), row.names = FALSE)
	
	cat(paste(" \n  Found ", dim(sound_list)[1], " results\n\n", sep=""))
  
	#Return df of sound data
	if (length(sound_list)>0){
		invisible(sound_list)
		}
	else{
		stop("No results from that query.")
	}
}