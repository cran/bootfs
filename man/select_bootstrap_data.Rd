\name{select_bootstrap_data}
\alias{select_bootstrap_data}

\title{
Select bootstrapping samples preserving group member frequencies.
}
\description{
Select bootstrapping samples preserving group member frequencies.
}
\usage{
select_bootstrap_data(datX, datY)
}
\arguments{
  \item{datX}{
    The data matrix.
}
  \item{datY}{
    The grouping variable.
}
}
\details{
    Selects a bootstrap dataset from a data matrix \code{datX}. Proportions
    of the groups A and B, as found in the grouping variable \code{datY},
    is preserved.
}
\value{
A list with two elements:
\itemize{
\item{datX}{The bootstrap data matrix.}
\item{datY}{The bootstrap grouping variable.}
}
}
\author{
Christian Bender (christian.bender@tron-mainz.de)
}
\seealso{
doBootstrap
%\code{\link{doBootstrap}}
}
\examples{
\dontrun{
#todo
}
}
\keyword{ internal }
