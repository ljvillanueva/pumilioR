\name{getSites}

\alias{getSites}

\title{Get Sites from Pumilio}

\description{Function to get the sites with sounds from Pumilio.}

\usage{
	getSites(pumilio_URL)
	}

\arguments{
	\item{pumilio_URL}{ URL of the Pumilio installation, with trailing slash. For example: "http://server.com/pumilio/" }
	}

\value{
	Returns a data frame with the sites data.
	\item{SiteID}{ ID of the site }
	\item{SiteName}{ name of the site }
	\item{latitude}{ latitude of the site, in decimal degrees }
	\item{longitude}{ longitude of the site, in decimal degrees }
	\item{elevation}{ elevation of the site, in meters above sea level }
	\item{notes}{ notes of the site }
	}

%\author{Luis J. Villanueva-Rivera}

\examples{
sites <- getSites(pumilio_URL = "http://pumilio.coquipr.com/pumilio/")	
sites[1,]}


%\references{
 %}

\keyword{pumilioR}
\keyword{getSites}
