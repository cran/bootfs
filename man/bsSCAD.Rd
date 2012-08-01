\name{bsSCAD}
\alias{bsSCAD}
\title{
Perform SCAD SVM bootstrapping.
}
\description{
Use SCAD SVM algorithm and bootstrapping to derive a robust feature set.
}
\usage{
bsSCAD(logX, groupings, DIR=NULL, params = NULL)
%list(seed=123, bstr=100, maxiter=1000, maxevals=500,
%			bounds=NULL, saveres=TRUE, jitter=FALSE, fs.method="scad"))
}
\arguments{
  \item{logX}{
	The data matrix.
}
  \item{groupings}{
	The grouping variable.
}
  \item{DIR}{
	The output base directory.
}
  \item{params}{
	A parameter list with the following elements:
	\code{seed}: A random seed.
	\code{bstr}: Number of bootstrap iterations.
	\code{jitter}: Boolean. Use some additional noise for constant values.
	\code{maxiter}: Parameter for the SCAD SVM function.
	\code{maxevals}: Parameter for the SCAD SVM function.
	\code{bounds}: Parameter for the SCAD SVM function.
	\code{saveres}: Boolean. Stored results of bootstrapping in an R-data
	object.
	 \code{fs.method}: The method for feature selection. One out of \emph{scad, 1norm, scad+L2, DrHSVM}. See package penalizedSVM for the methods.
}
}
\details{
Usually called from \code{\link{doBS}}.
}
\value{
The result list of the bootstrapping.
}
\author{
Christian Bender (christian.bender@tron-mainz.de)
}
\seealso{
\code{\link{doBS}}
}
\examples{
\dontrun{
#todo
}
}
