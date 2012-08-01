\name{myDirect}
\alias{myDirect}
\title{
Function from package \code{penalizedSVM}
}
\description{
Function from package \code{penalizedSVM}
}
\usage{
myDirect(Problem, bounds, maxits = 500, maxevals = 1000, maxdeep = 100, 
        testflag = 0, globalmin = 0, ep = 1e-04, tol = 0.01, 
        showits = c("none", "final", "all"), verbose = TRUE, 
        impcons = 0, pert = 1e-06, pdf.name = NULL, pdf.width = 12, 
        pdf.height = 12, my.mfrow = c(1, 1), ...)
}

\arguments{
  \item{Problem}{
%%     ~~Describe \code{Problem} here~~
}
  \item{bounds}{
%%     ~~Describe \code{bounds} here~~
}
  \item{maxits}{
%%     ~~Describe \code{maxits} here~~
}
  \item{maxevals}{
%%     ~~Describe \code{maxevals} here~~
}
  \item{maxdeep}{
%%     ~~Describe \code{maxdeep} here~~
}
  \item{testflag}{
%%     ~~Describe \code{testflag} here~~
}
  \item{globalmin}{
%%     ~~Describe \code{globalmin} here~~
}
  \item{ep}{
%%     ~~Describe \code{ep} here~~
}
  \item{tol}{
%%     ~~Describe \code{tol} here~~
}
  \item{showits}{
%%     ~~Describe \code{showits} here~~
}
  \item{verbose}{
%%     ~~Describe \code{verbose} here~~
}
  \item{impcons}{
%%     ~~Describe \code{impcons} here~~
}
  \item{pert}{
%%     ~~Describe \code{pert} here~~
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
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
}
\details{
This function is taken and modified from package \code{penalizedSVM},
since a bug in the original package always opened plot devices
unintendedly.
}
\keyword{ internal }

