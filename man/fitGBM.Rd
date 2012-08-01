\name{fitGBM}
\alias{fitGBM}
\title{
Fit a Gradient Boosting Machine model.
}
\description{
Fit a Gradient Boosting Machine model.
}
\usage{
fitGBM(train, traing, ntree, shrinkage, interaction.depth,
	bag.fraction, train.fraction, n.minobsinnode, verbose)
}

\arguments{
  \item{train}{
	Data matrix.
}
  \item{traing}{
	Response vector.
}
  \item{ntree}{
	Number of boosting trees.
}
  \item{shrinkage}{
	Shrinkage parameter fro \code{gbm}.
}
  \item{interaction.depth}{
	Shrinkage parameter fro \code{gbm}.
}
  \item{bag.fraction}{
	Shrinkage parameter fro \code{gbm}.
}
  \item{train.fraction}{
	Shrinkage parameter fro \code{gbm}.
}
  \item{n.minobsinnode}{
	Shrinkage parameter fro \code{gbm}.
}
  \item{verbose}{
	Boolean. If TRUE print some messages.
}
}
\details{
Fit a boosting model.
}
\value{
TODO
}
\author{
Christian Bender
}

\seealso{
\code{\link{cvGBM}}
}
\examples{
\dontrun{
	TODO
}
}
\keyword{ ~internal }
