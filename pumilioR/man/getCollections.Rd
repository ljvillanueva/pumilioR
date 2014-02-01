\name{getCollections}

\alias{getCollections}

\title{Get Collections from Pumilio}

\description{Function to get the collections with sounds from Pumilio.}

\usage{
	getCollections(pumilio_URL, credentials = NA, pumiliologin = NA)
	}

\arguments{
	\item{pumilio_URL}{ URL of the Pumilio installation, with trailing slash. For example: "http://server.com/pumilio/" }
	\item{credentials}{ username and password, separated with a colon, for websites that require basic Apache webserver authentication }
	\item{pumiliologin}{ username and password, separated with a colon, for Pumilio systems that require a valid user }
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

%\references{
 %}

\keyword{pumilioR}
\keyword{getCollections}