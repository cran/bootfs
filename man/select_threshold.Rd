\name{select_threshold}
\alias{select_threshold}

\title{
Find optimal threshold for PAMR feature selection.
}
\description{
Find optimal threshold for PAMR feature selection. Prefers small number of features.
}
\usage{
select_threshold(histcv, max_allowed_feat = NULL)
}
\arguments{
  \item{histcv}{
    Crossvalidation history from pamr run.
}
  \item{max_allowed_feat}{
    Maximum number of features to be selected.
}
}
\details{
Depending on the threshold the number of selected features will change.
This function looks for the optimal number with respect to the training
error.
}
\value{
The threshold with minimal training error.
}
\author{
Christian Bender (christian.bender@tron-mainz.de)
}
\keyword{ internal }
