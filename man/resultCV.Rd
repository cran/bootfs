\name{resultCV}
\alias{resultCV}
\title{create a result plot for all performed crossvalidations}
\usage{
  resultCV(retCV)
}
\arguments{
  \item{retCV}{Return list from function
  \code{\link{doCV}}}
}
\value{
  void
}
\description{
  Use the ouptu of a call to \code{doCV} and create a
  summary of the performance for multiple methods.
}
\details{
  This function parses the returned list object from
  function \code{\link{doCV}} and extracts the performance
  data for each classification method. It assembles the
  data and generates either a ROC curve for each method
  (binary classification) or a boxplot representation of
  the multiclass AUC values gathered through the CV.
}
\seealso{
  \code{\link{doCV}}
}

