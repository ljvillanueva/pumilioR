\name{getVersion}

\alias{getVersion}

\title{Get Pumilio Version}

\description{Function to find the version of the Pumilio system because version 2.6.0, or newer, is required.}

\usage{
	getVersion(pumilio_URL)
	}

\arguments{
	\item{pumilio_URL}{ URL of the Pumilio installation, for example: "http://server.com/pumilio/" }
	}

\value{
	Returns a boolean if the Pumilio version matches the requirement.}

%\author{Luis J. Villanueva-Rivera}

\examples{
 getVersion(pumilio_URL="http://pumilio.coquipr.com/pumilio/")
	}

\note{
  For websites that require basic authentication, enter them in the 
  URL. For example:
  
  \code{getVersion(pumilio_URL = "http://username:password@pumilio.coquipr.com/pumilio/")}
}

%\references{
 %}

\keyword{pumilioR}
\keyword{getVersion}
