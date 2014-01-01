getCollections <- function(pumilio_URL){
	
	if (getVersion(pumilio_URL) == FALSE){
		stop(" pumilioR only works with Pumilio version 2.6.0 or newer.")
	}
	
	pumilio_XML_URL <- paste(pumilio_URL, "xml.php", sep="")
	
	#Get XML contents
	pumilio_XML <- xmlTreeParse(pumilio_XML_URL, isURL = TRUE)
	
	pumilio_list <- xmlToList(node = pumilio_XML, addAttributes = TRUE)
	
	cols_list <- pumilio_list$Collections
	
	invisible(as.data.frame(t(cols_list), row.names = FALSE))
}