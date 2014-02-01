getVersion <- function(pumilio_URL, credentials = NA, pumiliologin = NA){

	if (!is.na(credentials)){
		pumilio_URL <- gsub("http://", paste("http://", credentials, "@", sep=""), pumilio_URL)
		}

	if (!is.na(pumiliologin)){
		pumilio_XML_URL <- paste(pumilio_URL, "xml.php?login=", pumiliologin, sep = "")
	}else{
		pumilio_XML_URL <- paste(pumilio_URL, "xml.php", sep = "")
	}
	
  #check valid url
	badurl <- function(...){
		stop("Could not open the XML file, please verify the URL is correct.")
		}
	
	#Get XML contents
	pumilio_XML <- xmlTreeParse(getURL(pumilio_XML_URL), error = badurl)
		
	pumilio_list <- xmlToList(node = pumilio_XML, addAttributes = TRUE)
	
	pumilio_version <- pumilio_list$pumilio_version
	
	#cat(paste("\n The site ", pumilio_URL, " is running version:\n\n   ", pumilio_version, "\n\n", sep=""))
  
	if (length(pumilio_version) == 0){
		ret = FALSE
	}else{
		pumilio_version <- unlist(strsplit(pumilio_version, "\\."))
		
		if (pumilio_version[1] > 2){
			ret = TRUE
		}else{
			if (pumilio_version[1] == 2){
				if (pumilio_version[2] >= 6){
					ret = TRUE
				}else{
					ret = FALSE
					}
			}else{
				ret = FALSE
				}
			}
		}
		if (ret == TRUE){
			pumilio_xml_access <- pumilio_list$pumilio_xml_access
			if (length(pumilio_xml_access) == 0){
				invisible(FALSE)
			}else{
				invisible(pumilio_xml_access)
			}
		}else{
			invisible(FALSE)
		}
	}	