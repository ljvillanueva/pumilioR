getSounds <- function(pumilio_URL, SiteID=NA, ColID=NA, type="all"){
	
	if (getVersion(pumilio_URL)==FALSE){
		stop(" pumilioR only works with Pumilio version 2.6.0 or newer.")
	}
	
	if (type!="all" & is.na(SiteID) & is.na(ColID)){
		stop(" Both ColID and SiteID cannot be empty when type is not 'all'.")
	}
	
	#if (is.na(SiteID)==FALSE & is.na(ColID)==FALSE & ()){
	#	stop(" Only one of ColID and SiteID can be set.")
	#}
	
	pumilio_XML_URL <- paste(pumilio_URL, "xml.php", sep="")
	
	this_site_sounds <- xmlTreeParse(paste(pumilio_XML_URL, "?type=", type, "&SiteID=", SiteID, "&ColID=", ColID, sep=""), isURL = TRUE)
	sounds_list <- xmlToList(node=this_site_sounds, addAttributes=TRUE)
	
	sound_list <- sounds_list$Sounds
	
	invisible(as.data.frame(t(sound_list), row.names=FALSE))
}