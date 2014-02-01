\name{checkVersion}

\alias{checkVersion}

\title{Get Pumilio Version}

\description{Function to find if the version of the Pumilio system can work with this package. Version 2.6.0, or newer, of Pumilio is required and if the system allows querying.}

\usage{
	checkVersion(pumilio_URL, credentials = NA, pumiliologin = NA)
	}

\arguments{
	\item{pumilio_URL}{ URL of the Pumilio installation, with trailing slash. For example: "http://server.com/pumilio/" }
  	\item{credentials}{ username and password, separated with a colon, for websites that require basic Apache webserver authentication }
  	\item{pumiliologin}{ username and password, separated with a colon, for Pumilio systems that require a valid user }
	}

\value{
	Returns a boolean if the Pumilio version matches the requirement.}

%\author{Luis J. Villanueva-Rivera}

\examples{
	access <- checkVersion(pumilio_URL = "http://pumilio.coquipr.com/pumilio/")
	
	access
	}

%\references{
 %}

\keyword{pumilioR}
\keyword{checkVersion}