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