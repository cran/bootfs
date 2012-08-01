\name{my.svm.fs.default}
\alias{my.svm.fs.default}

\title{
Function from package \code{penalizedSVM}
}
\description{
Function from package \code{penalizedSVM}
}
\usage{
\method{my.svm.fs}{default}(x, y, 
        fs.method = c("scad", "1norm", "scad+L2", "DrHSVM"), 
        grid.search = c("interval", "discrete"), lambda1.set = NULL, 
        lambda2.set = NULL, bounds = NULL, 
        parms.coding = c("log2", "none"), maxevals = 500, 
        inner.val.method = c("cv", "gacv"), cross.inner = 5, 
        show = c("none", "final"), calc.class.weights = FALSE, 
        class.weights = NULL, seed = 123, maxIter = 700, verbose = TRUE, ...)
%my.svm.fs.default(x, y, fs.method = c("scad", "1norm", "scad+L2", "DrHSVM"), grid.search = c("interval", "discrete"), lambda1.set = NULL, lambda2.set = NULL, bounds = NULL, parms.coding = c("log2", "none"), maxevals = 500, inner.val.method = c("cv", "gacv"), cross.inner = 5, show = c("none", "final"), calc.class.weights = FALSE, class.weights = NULL, seed = 123, maxIter = 700, verbose = TRUE, ...)
}
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{y}{
%%     ~~Describe \code{y} here~~
}
  \item{fs.method}{
%%     ~~Describe \code{fs.method} here~~
}
  \item{grid.search}{
%%     ~~Describe \code{grid.search} here~~
}
  \item{lambda1.set}{
%%     ~~Describe \code{lambda1.set} here~~
}
  \item{lambda2.set}{
%%     ~~Describe \code{lambda2.set} here~~
}
  \item{bounds}{
%%     ~~Describe \code{bounds} here~~
}
  \item{parms.coding}{
%%     ~~Describe \code{parms.coding} here~~
}
  \item{maxevals}{
%%     ~~Describe \code{maxevals} here~~
}
  \item{inner.val.method}{
%%     ~~Describe \code{inner.val.method} here~~
}
  \item{cross.inner}{
%%     ~~Describe \code{cross.inner} here~~
}
  \item{show}{
%%     ~~Describe \code{show} here~~
}
  \item{calc.class.weights}{
%%     ~~Describe \code{calc.class.weights} here~~
}
  \item{class.weights}{
%%     ~~Describe \code{class.weights} here~~
}
  \item{seed}{
%%     ~~Describe \code{seed} here~~
}
  \item{maxIter}{
%%     ~~Describe \code{maxIter} here~~
}
  \item{verbose}{
%%     ~~Describe \code{verbose} here~~
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
