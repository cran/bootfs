\name{roc}
\alias{roc}

\title{
Draw ROC curve.
}
\description{
Draw a ROC curve and calculate corresponding AUC.
}
\usage{
roc(pr, labels, measure = "tpr", x.measure = "fpr", colorize = TRUE, 
avg = "none", spread.estimate = "stddev", filter = NULL)
}

\arguments{
  \item{pr}{
	Vector holding prediction values.
}
  \item{labels}{
	Vector with the corresponding labels to \code{pr}.
}
  \item{measure}{
	Argument from \code{ROCR}.
}
  \item{x.measure}{
	Argument from \code{ROCR}.
}
  \item{colorize}{
	Argument from \code{ROCR}.
}
  \item{avg}{
	Argument from \code{ROCR}.
}
  \item{spread.estimate}{
	Argument from \code{ROCR}.
}
  \item{filter}{
	Cutoff, when to plot the roc.
}
}
\details{
Plotting of ROC curves using the \code{ROCR} package.
}
\value{
The AUC value.
}

\author{
Christian Bender (christian.bender@tron-mainz.de)
}

\seealso{
\code{ROCR}
}
\examples{
\dontrun{
#Todo.
}
}
\keyword{ internal }
