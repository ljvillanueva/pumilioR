\name{getCollections}

\alias{getCollections}

\title{Get Collections from Pumilio}

\description{Function to get the collections with sounds from Pumilio.}

\usage{
	getCollections(pumilio_URL)
	}

\arguments{
	\item{pumilio_URL}{ URL of the Pumilio installation, with trailing slash. For example: "http://server.com/pumilio/" }
	}

\value{
	Returns a data frame with the collections data.
	\item{ColID}{ ID of the collection }
	\item{CollectionName}{ name of the collection }
	\item{Author}{ author of the collection }
	\item{FilesSource}{ source of the collection }
	\item{CollectionFullCitation}{ full citation to use for this collection }
	\item{URL}{ web page for more details of this collection }
	\item{notes}{ notes of the collection }
	}

%\author{Luis J. Villanueva-Rivera}

\examples{
cols <- getCollections(pumilio_URL = "http://pumilio.coquipr.com/pumilio/")
cols[1,]}

\note{
  For websites that require basic authentication, enter them in the 
  URL. Using the same example:
  
  \code{cols <- getCollections(pumilio_URL = "http://username:password@pumilio.coquipr.com/pumilio/")}
  
  or:
  
  \code{pumilio_address = "http://username:password@pumilio.coquipr.com/pumilio/"}
  
  \code{cols <- getCollections(pumilio_address)}
}

%\references{
 %}

\keyword{pumilioR}
\keyword{getCollections}
