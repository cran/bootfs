\name{extract_features_rf_boruta}
\alias{extract_features_rf_boruta}

\title{
Helper for feature extraction for RF Boruta.
}
\description{
Helper for feature extraction for RF Boruta.
}
\usage{
extract_features_rf_boruta(res, SUBDIR=NULL)
}

\arguments{
  \item{res}{
	A result object from cvRFBORUTA.
}
  \item{SUBDIR}{
	If a String, extracted features are written in this subfolder. If NULL, extracted features are not written.
}
}
\details{
An internal function, which is usually not called directly.
}
\value{
The extracted features.
}
\author{
Christian Bender
}
\seealso{
\code{\link{cvRFBORUTA}}
}
\examples{
\dontrun{
#todo
}
}
\keyword{ internal }
