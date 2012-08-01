\name{myEPSGO}
\alias{myEPSGO}
\title{
Function from package \code{penalizedSVM}
}
\description{
Function from package \code{penalizedSVM}
}
\usage{
myEPSGO(Q.func, bounds, parms.coding = "none", fminlower = 0, 
        flag.find.one.min = FALSE, show = c("none", "final", "all"), 
        N = NULL, maxevals = 500, pdf.name = NULL, pdf.width = 12, 
        pdf.height = 12, my.mfrow = c(1, 1), verbose = TRUE, 
        seed = 123, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Q.func}{
%%     ~~Describe \code{Q.func} here~~
}
  \item{bounds}{
%%     ~~Describe \code{bounds} here~~
}
  \item{parms.coding}{
%%     ~~Describe \code{parms.coding} here~~
}
  \item{fminlower}{
%%     ~~Describe \code{fminlower} here~~
}
  \item{flag.find.one.min}{
%%     ~~Describe \code{flag.find.one.min} here~~
}
  \item{show}{
%%     ~~Describe \code{show} here~~
}
  \item{N}{
%%     ~~Describe \code{N} here~~
}
  \item{maxevals}{
%%     ~~Describe \code{maxevals} here~~
}
  \item{pdf.name}{
%%     ~~Describe \code{pdf.name} here~~
}
  \item{pdf.width}{
%%     ~~Describe \code{pdf.width} here~~
}
  \item{pdf.height}{
%%     ~~Describe \code{pdf.height} here~~
}
  \item{my.mfrow}{
%%     ~~Describe \code{my.mfrow} here~~
}
  \item{verbose}{
%%     ~~Describe \code{verbose} here~~
}
  \item{seed}{
%%     ~~Describe \code{seed} here~~
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
