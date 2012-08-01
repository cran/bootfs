\name{select_cv_balanced}
\alias{select_cv_balanced}

\title{
Balanced training/test set selection for crossvalidation.
}
\description{
Balanced training/test set selection for crossvalidation.
}
\usage{
select_cv_balanced(yp, ncv, verbose=FALSE)
}
\arguments{
  \item{yp}{
    The grouping.
}
  \item{ncv}{
    Number of folds.
}
  \item{verbose}{
    Boolean. If TRUE print some informational messages.
}

}
\details{
Balanced training and test set selection.
}
\value{
The training and test sets.
}
\author{
Christian Bender (christian.bender@tron-mainz.de)
}
\keyword{ internal }
