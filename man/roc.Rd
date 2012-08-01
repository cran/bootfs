\name{roc}
\alias{roc}
\alias{roc_binterval}
\title{
Draw ROC curve.
}
\description{
Draw a ROC curve and calculate corresponding AUC.
}
\usage{
roc(pr, labels, measure = "tpr", x.measure = "fpr", colorize = TRUE, 
avg = "none", spread.estimate = "stddev", filter = NULL)

roc_binterval(fitted, labels)

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
  \item{fitted}{
	List. Each element holds the prediction vector for one repeat, fold, etc.
}
  \item{labels}{
	List. Each element holds the true class vector for one repeat, fold, etc.
}
}
\details{
\code{roc}: Plotting of ROC curves using the \code{ROCR} package.\cr
\code{roc.binterval}: Add a Monte Carlo interval for the roc curve, using bootstrap samples out of the prediction/label vectors. Note that \code{roc} has to be called before.

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
