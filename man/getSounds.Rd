\name{getSounds}

\alias{getSounds}

\title{Get Sounds from Pumilio}

\description{Function to get the data from sounds that match the search query.}

\usage{
	getSounds(pumilio_URL, SiteID=NA, ColID=NA, type="all")
	}

\arguments{
	\item{pumilio_URL}{ URL of the Pumilio installation, for example: "http://server.com/pumilio/" }
	\item{SiteID}{ ID of a valid site, obtained using \code{getSites} }
	\item{ColID}{ ID of a valid collection, obtained using \code{getCollections} }
	\item{type}{ type of query to run: "all" to get all sounds in the archive; "site" for the site specified with \code{SiteID}; "col" for the collection specified with \code{ColID}; "both" for sounds that match both the site specified with \code{SiteID} and the collection specified with \code{ColID}. }
	}

\value{
	Returns a data frame with the sound data.
	\item{SoundID}{ ID of the soundfile in the database. }
	\item{OriginalFilename}{ filename of the soundfile. }
	\item{SoundName}{ filename of the soundfile, usually \code{OriginalFilename} without the extension. }
	\item{day}{ day part of the date when it was recorded. }
	\item{month}{ month part of the date when it was recorded. }
	\item{year}{ year part of the date when it was recorded. }
	\item{hour}{ hour part of the date when it was recorded. }
	\item{minutes}{ minutes part of the date when it was recorded. }
	\item{seconds}{ seconds part of the date when it was recorded. }
	\item{SamplingRate}{ sampling rate used to record the soundfile, in Hz. }
	\item{BitRate}{ bitrate of the file, in bits. }
	\item{Channels}{ number of channels. }
	\item{Duration}{ duration of the soundfile, in seconds. }
	\item{SoundFormat}{ format of the file. }
	\item{SensorID}{ ID of the sensor used to record the soundfile. }
	\item{Notes}{ notes of the soundfile. }
	\item{timestamp}{ date and time stamp when the soundfile was added to the archive. }
	\item{FileSize}{ file size of the soundfile, in bytes. }
	\item{FilePath}{ URL of the soundfile. }
	\item{AudioPreviewFormat}{ format of the preview file, usually mp3. }
	\item{AudioPreviewFilename}{ filename of the preview file. }
	\item{AudioPreviewFilePath}{ URL of the preview file. }
	}

\author{Luis J. Villanueva-Rivera}

%\examples{
%	}

%\references{
 %}

\keyword{pumilioR}
\keyword{getSounds}
