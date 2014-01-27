getVersion <- function(pumilio_URL){
	
	pumilio_XML_URL <- paste(pumilio_URL, "xml.php", sep = "")
	
	#check valid url
	badurl <- function(...){
		stop("Could not open the XML file, please verify the URL is correct and that the permissions are set correctly.")
		}
	
	#Get XML contents
	pumilio_XML <- xmlTreeParse(getURL(pumilio_XML_URL), error = badurl)
		
	pumilio_list <- xmlToList(node = pumilio_XML, addAttributes = TRUE)
	
	pumilio_version <- pumilio_list$pumilio_version
	
	cat(paste("\n The site ", pumilio_URL, " is running version:\n\n   ", pumilio_version, "\n\n", sep=""))
  
	if (length(pumilio_version) == 0){
		return(FALSE)
	}else{
		
		pumilio_version <- unlist(strsplit(pumilio_version, "\\."))
		
   if (pumilio_version[1] > 2){
			return(TRUE)
		}else{
			if (pumilio_version[1] == 2){
				if (pumilio_version[2] >= 6){
					return(TRUE)
					}else{
						return(FALSE)
						}
				}else{
					return(FALSE)
				}
			}
		}
	}	