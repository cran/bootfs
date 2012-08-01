\name{extractsignatures}
\alias{extractsignatures}
\title{
    Helper for extracting all feature signatures from a bootstrapping result (single method).
}
\description{
    Helper for extracting all feature signatures from a bootstrapping result (single method).
}
\usage{
extractsignatures(res_bstr, strat)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{res_bstr}{
    The result object of a bootstrapping run.
}
  \item{strat}{
    The stratification column, i.e. an index of the \code{res_bstr} list.
}
}
\details{
    Retrieves all selected features for stratification group \code{strat}.
}
\value{
    A list holding the bootstrap feature lists, one item for one bootstrapping run.
}

\author{
Christian Bender
}

\keyword{ ~internal }
