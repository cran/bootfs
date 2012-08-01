\name{bstr_multi}
\alias{bstr_multi}

\title{
SVM SCAD bootstrapping classification and feature selection.
}
\description{
SVM SCAD bootstrapping classification and feature selection.
}
\usage{
bstr_multi(X, datX, bstr, seed = 123, fs.method = "scad", bounds, 
	maxiter = 1000, maxevals = 500)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{
%%     ~~Describe \code{X} here~~
}
  \item{datX}{
%%     ~~Describe \code{datX} here~~
}
  \item{bstr}{
%%     ~~Describe \code{bstr} here~~
}
  \item{seed}{
%%     ~~Describe \code{seed} here~~
}
  \item{fs.method}{
%%     ~~Describe \code{fs.method} here~~
}
  \item{bounds}{
%%     ~~Describe \code{bounds} here~~
}
  \item{maxiter}{
%%     ~~Describe \code{maxiter} here~~
}
  \item{maxevals}{
%%     ~~Describe \code{maxevals} here~~
}
}
\details{
An internal function, which is usually not called directly.
}
\value{
Alist with one element for each of the \code{bstr} bootstrap iterations. Each element contains the result of the \code{svm.fs} call.
}
\references{
Todo.
}
\author{
Christian Bender (christian.bender@tron-mainz.de)
}

\seealso{
Todo.
}
\examples{
\dontrun{
#todo
}
}
\keyword{ internal }
