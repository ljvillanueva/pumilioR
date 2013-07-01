
#################################
# SETTINGS
#################################

#URL of the Pumilio installation, with trailing slash
#Move to examples
pumilio_URL = "http://localhost/plosone/"


getVersion <- function(pumilio_URL){
	
	pumilio_XML_URL <- paste(pumilio_URL, "xml.php", sep="")
	
	#Get XML contents
	pumilio_XML <- xmlTreeParse(pumilio_XML_URL, isURL = TRUE)
	
	pumilio_list <- xmlToList(node=pumilio_XML, addAttributes=TRUE)
	
	pumilio_version <- pumilio_list$pumilio_version
	
	pumilio_version <- unlist(strsplit(pumilio_version, "\\."))
	
	if (pumilio_version[1]>=2 & pumilio_version[2]>=6){
		return(TRUE)
	}else{
		return(FALSE)
	}
}

getSites <- function(pumilio_URL){
	
	if (getVersion(pumilio_URL)==FALSE){
		stop(" pumilioR only works with Pumilio version 2.6.0 or newer.")
	}
	
	pumilio_XML_URL <- paste(pumilio_URL, "xml.php", sep="")
	
	#Get XML contents
	pumilio_XML <- xmlTreeParse(pumilio_XML_URL, isURL = TRUE)
	
	pumilio_list <- xmlToList(node=pumilio_XML, addAttributes=TRUE)
	
	sites_list <- pumilio_list$Sites

	invisible(as.data.frame(t(sites_list), row.names=FALSE))
	}

getCollections <- function(pumilio_URL){
	
	if (getVersion(pumilio_URL)==FALSE){
		stop(" pumilioR only works with Pumilio version 2.6.0 or newer.")
	}
	
	pumilio_XML_URL <- paste(pumilio_URL, "xml.php", sep="")
	
	#Get XML contents
	pumilio_XML <- xmlTreeParse(pumilio_XML_URL, isURL = TRUE)
	
	pumilio_list <- xmlToList(node=pumilio_XML, addAttributes=TRUE)
	
	cols_list <- pumilio_list$Collections
	
	invisible(as.data.frame(t(cols_list), row.names=FALSE))
}

getSounds <- function(pumilio_URL, SiteID=NA, ColID=NA){
	
	if (getVersion(pumilio_URL)==FALSE){
		stop(" pumilioR only works with Pumilio version 2.6.0 or newer.")
	}
	
	if (is.na(SiteID) & is.na(ColID)){
		stop(" Both ColID and SiteID cannot be empty.")
	}
	
	if (is.na(SiteID)==FALSE & is.na(ColID)==FALSE){
		stop(" Only one of ColID and SiteID can be set.")
	}
	
	pumilio_XML_URL <- paste(pumilio_URL, "xml.php", sep="")
	
	if (is.na(SiteID)==FALSE){
		
		this_site_sounds <- xmlTreeParse(paste(pumilio_XML_URL, "?type=site&SiteID=", SiteID, sep=""), isURL = TRUE)
		sounds_list <- xmlToList(node=this_site_sounds, addAttributes=TRUE)
	}else{
		this_col_sounds <- xmlTreeParse(paste(pumilio_XML_URL, "?type=col&ColID=", ColID, sep=""), isURL = TRUE)
		sounds_list <- xmlToList(node=this_col_sounds, addAttributes=TRUE)
		}
	
	sound_list <- sounds_list$Sounds
	
	invisible(as.data.frame(t(sound_list), row.names=FALSE))
}