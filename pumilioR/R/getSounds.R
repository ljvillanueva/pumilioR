getSounds <- function(pumilio_URL, SiteID = NA, ColID = NA, type = "all"){
	#Function to get the info of sound files that match a query, 
	# or all sounds in archive is no query is used.
	if (getVersion(pumilio_URL) == FALSE){
		stop(" pumilioR only works with Pumilio version 2.6.0 or newer.")
	}
	
	if (type!="all" & is.na(SiteID) & is.na(ColID)){
		stop(" Both ColID and SiteID cannot be empty when type is not 'all'.")
	}
	
	pumilio_XML_URL <- paste(pumilio_URL, "xml.php", sep="")
	
	this_site_sounds <- xmlTreeParse(getURL(paste(pumilio_XML_URL, "?type=", type, "&SiteID=", SiteID, "&ColID=", ColID, sep="")))
	sounds_list <- xmlToList(node = this_site_sounds, addAttributes = TRUE)
	
	#Get sounds from parsed XML
	sound_list <- sounds_list$Sounds
	
	#Return df of sound data
	if (length(sound_list)>0){
		invisible(as.data.frame(t(sound_list), row.names = FALSE))
		}
	else{
		stop("No results from that query.")
	}
}