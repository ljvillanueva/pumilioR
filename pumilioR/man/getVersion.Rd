\name{getVersion}

\alias{getVersion}

\title{Get Pumilio Version}

\description{Function to find if the version of the Pumilio system can work with this package. Version 2.6.0, or newer, of Pumilio is required.}

\usage{
	getVersion(pumilio_URL, credentials = NA)
	}

\arguments{
	\item{pumilio_URL}{ URL of the Pumilio installation, for example: "http://server.com/pumilio/" }
  \item{credentials}{ username and password, separated with a colon, for websites that require basic authentication }
	}

\value{
	Returns a boolean if the Pumilio version matches the requirement.}

%\author{Luis J. Villanueva-Rivera}

\examples{
 getVersion(pumilio_URL = "http://pumilio.coquipr.com/pumilio/")
	}

\note{
   For websites that require basic authentication, use the \code{credentials} argument:
  
  \code{getVersion(pumilio_URL = "http://pumilio.coquipr.com/pumilio/",}
  
  \code{credentials = "username:password")}
}

%\references{
 %}

\keyword{pumilioR}
\keyword{getVersion}
