\name{getFile}

\alias{getFile}

\title{Download a sound file from Pumilio}

\description{Function to download one sound file, specified with \code{SoundID}, to the working directory from the result of a query using \code{getSounds}.}

\usage{
	getFile(result, SoundID = NA, credentials = NA)
	}

\arguments{
	\item{result}{ the result of the function \code{getSounds} }
	\item{SoundID}{ ID of a valid record, obtained using \code{getSounds} }
	\item{credentials}{ username and password, separated with a colon, for websites that require basic Apache webserver authentication }
	}

\value{
	Returns the filename.
	}

%\author{Luis J. Villanueva-Rivera}

\examples{
	\dontrun{
	#First, get a list of results
	results <- getSounds(pumilio_URL="http://pumilio.coquipr.com/pumilio/", 
		SiteID=111, type="site")
	
	#Then, get the file from the first row, identified with the SoundID = 4078
	getFile(results, SoundID = 4078)}
	}

\note{To use this function, the curl library needs to be installed. 
		In Linux install libcurl-devel. 
		
		For Windows, add this line to the script before calling the \code{getFile} function:
		
		\code{setInternet2(TRUE)}
		}

%\references{
 %}

\keyword{pumilioR}
\keyword{getFile}