\name{control_params}
\alias{control_params}
\title{Create control parameter object for the classifiers}
\usage{
  control_params(seed = 123, bstr = 100, ncv = 5,
    repeats = 10, saveres = TRUE, jitter = FALSE,
    maxiter = 1000, maxevals = 500, bounds = NULL,
    max_allowed_feat = NULL, n.threshold = 50,
    maxRuns = 300, localImp = TRUE,
    rfimportance = "MeanDecreaseAccuracy", ntree = 1000,
    shrinkage = 0.01, interaction.depth = 3,
    bag.fraction = 0.75, train.fraction = 0.75,
    n.minobsinnode = 3, n.cores = 1, verbose = TRUE)
}
\arguments{
  \item{seed}{A random seed to be set before the
  classification}

  \item{bstr}{Integer. Number of bootstrap iterations.}

  \item{ncv}{Integer. Number of crossvalidation folds.}

  \item{repeats}{Integer. Number of repeats for
  cross-validation.}

  \item{saveres}{Boolean. If TRUE, save results.}

  \item{jitter}{Boolean. If TRUE, generate a small amount
  of noise, if standard deviations for samples are zero.
  NOTE: Use with care!}

  \item{maxiter}{Integer. Maximum number of iterations in
  SCAD SVM. Parameter for SCAD SVM from \code{penalizedSVM}
  package.}

  \item{maxevals}{Integer. Parameter for SCAD SVM from
  \code{penalizedSVM} package.}

  \item{bounds}{Parameter for SCAD SVM from
  \code{penalizedSVM} package.}

  \item{max_allowed_feat}{Integer. PAMR parameter, bounding
  the maximum number of features reported.}

  \item{n.threshold}{Integer. PAMR parameter, number of
  thresholds to be generated.}

  \item{maxRuns}{Integer. RF_Boruta parameter, number of
  runs in Boruta selection.}

  \item{localImp}{Boolean. randomForest parameter; save
  local importances.}

  \item{rfimportance}{String. randomForest parameter; which
  importance measure should be used in the randomForest
  (method 'rf') to rank and select features? Either
  \code{MeanDecreaseGini} or \code{MeanDecreaseAccuracy}.
  Features are selected with \code{rfimportance} >= 1.}

  \item{ntree}{Integer. randomForest and GBM parameter;
  Number of trees to be used.}

  \item{shrinkage}{Double. GBM parameter; shrinkage step
  size.}

  \item{interaction.depth}{Integer. GBM parameter.}

  \item{bag.fraction}{Numeric in 0..1. GBM parameter;
  Fraction of bagged samples.}

  \item{train.fraction}{Numeric in 0..1. GBM paramter;
  Fraction of training samples.}

  \item{n.minobsinnode}{Integer. GBM parameter.}

  \item{n.cores}{Integer. GBM parameter.}

  \item{verbose}{Boolean. GBM parameter. Be verbose or
  not.}
}
\value{
  List with all named control parameters.
}
\description{
  This function creates a set of control parameters which
  is passed to the classifier functions.
}
\details{
  This function is used to define a set of control
  parameters used in the different methods. For each
  parameter, consult the respective help pages of the
  methodologies.
}
\examples{
\dontrun{ control_params() }
}
\seealso{
  \code{penalizedSVM} \code{randomForest} \code{gbm}
  \code{Boruta} \code{pamr}
}

