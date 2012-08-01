\name{.my.run.interval}
\alias{.my.run.interval}
\title{
Function from package \code{penalizedSVM}
}
\description{
Function from package \code{penalizedSVM}
}
\usage{
.my.run.interval(x, y, bounds, parms.coding = "none", 
            fs.method, class.weights, seed = 123, 
            N = NULL, maxevals = 2000, fminlower = 0, 
            pdf.name = NULL, maxIter = NULL, 
            inner.val.method = "cv", verbose = TRUE, 
            show = "final", cross.inner = 5, ...)
}
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{y}{
%%     ~~Describe \code{y} here~~
}
  \item{bounds}{
%%     ~~Describe \code{bounds} here~~
}
  \item{parms.coding}{
%%     ~~Describe \code{parms.coding} here~~
}
  \item{fs.method}{
%%     ~~Describe \code{fs.method} here~~
}
  \item{class.weights}{
%%     ~~Describe \code{class.weights} here~~
}
  \item{seed}{
%%     ~~Describe \code{seed} here~~
}
  \item{N}{
%%     ~~Describe \code{N} here~~
}
  \item{maxevals}{
%%     ~~Describe \code{maxevals} here~~
}
  \item{fminlower}{
%%     ~~Describe \code{fminlower} here~~
}
  \item{pdf.name}{
%%     ~~Describe \code{pdf.name} here~~
}
  \item{maxIter}{
%%     ~~Describe \code{maxIter} here~~
}
  \item{inner.val.method}{
%%     ~~Describe \code{inner.val.method} here~~
}
  \item{verbose}{
%%     ~~Describe \code{verbose} here~~
}
  \item{show}{
%%     ~~Describe \code{show} here~~
}
  \item{cross.inner}{
%%     ~~Describe \code{cross.inner} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
}
\seealso{
\link[penalizedSVM]{penalizedSVM} 
}
\details{
This function is taken and modified from package \code{penalizedSVM}, written by Natalia Becker, Axel Benner, Wiebke Werft, since a bug in the original package always opened plot devices
unintendedly.
}
\keyword{ internal }
