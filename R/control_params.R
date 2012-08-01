#' Create control parameter object for the classifiers
#'
#' This function creates a set of control parameters which is passed to the classifier functions.
#'
#' @param seed A random seed to be set before the classification
#' @param bstr Integer. Number of bootstrap iterations.
#' @param ncv Integer. Number of crossvalidation folds.
#' @param repeats Integer. Number of repeats for cross-validation.
#' @param saveres Boolean. If TRUE, save results.
#' @param jitter Boolean. If TRUE, generate a small amount of noise, if standard deviations for samples are zero. NOTE: Use with care!
#' @param maxiter Integer. Maximum number of iterations in SCAD SVM. Parameter for SCAD SVM from \code{penalizedSVM} package.
#' @param maxevals Integer. Parameter for SCAD SVM from \code{penalizedSVM} package.
#' @param bounds Parameter for SCAD SVM from \code{penalizedSVM} package.
#' @param max_allowed_feat Integer. PAMR parameter, bounding the maximum number of features reported.
#' @param n.threshold Integer. PAMR parameter, number of thresholds to be generated.
#' @param maxRuns Integer. RF_Boruta parameter, number of runs in Boruta selection.
#' @param localImp Boolean. randomForest parameter; save local importances.
#' @param rfimportance String. randomForest parameter; which importance measure should be used in the randomForest (method 'rf') to rank and select features? Either \code{MeanDecreaseGini} or \code{MeanDecreaseAccuracy}. Features are selected with \code{rfimportance} >= 1.
#' @param ntree Integer. randomForest and GBM parameter; Number of trees to be used.
#' @param shrinkage Double. GBM parameter; shrinkage step size.
#' @param interaction.depth Integer. GBM parameter.
#' @param bag.fraction Numeric in 0..1. GBM parameter; Fraction of bagged samples.
#' @param train.fraction Numeric in 0..1. GBM paramter; Fraction of training samples.
#' @param n.minobsinnode Integer. GBM parameter.
#' @param n.cores Integer. GBM parameter.
#' @param verbose Boolean. GBM parameter. Be verbose or not.
#' @details
#' This function is used to define a set of control parameters used in the different methods. For each parameter, consult the respective help pages of the methodologies.
#' @seealso
#' \code{penalizedSVM}
#' \code{randomForest}
#' \code{gbm}
#' \code{Boruta}
#' \code{pamr}
#' @return
#' List with all named control parameters.
#' @examples \dontrun{ control_params() }
control_params <- function(seed=123,
							bstr=100,
							ncv=5, repeats=10,
							saveres=TRUE,
							jitter=FALSE, ## general parameters
							maxiter=1000, maxevals=500, bounds=NULL,## scad parameters
							max_allowed_feat=NULL, n.threshold=50, ## pamr parameters
							maxRuns=300, ## rf_boruta
							localImp=TRUE, rfimportance="MeanDecreaseAccuracy", ## RF parameters
							ntree = 1000, ## GBM parameters, also RF
							shrinkage = 0.01, interaction.depth = 3, ## GBM parameters
							bag.fraction = 0.75, train.fraction = 0.75, 
							n.minobsinnode = 3, n.cores = 1, 
							verbose = TRUE)
 {
	params <- list(seed=seed,
			jitter=jitter, saveres=saveres,## general parameters
			bstr=bstr,
			ncv=ncv, repeats=repeats,
			maxiter=maxiter, maxevals=maxevals, bounds=bounds, ## scad parameters
			max_allowed_feat=max_allowed_feat, n.threshold=n.threshold, ## pamr parameters
			maxRuns=maxRuns, ## RF parameters (Boruta),
			localImp=localImp, rfimportance=rfimportance, ## RF parameters
			ntree = ntree, ## GBM parameters, also RF
			shrinkage = shrinkage, interaction.depth = interaction.depth,
			bag.fraction = bag.fraction, train.fraction = train.fraction, 
			n.minobsinnode = n.minobsinnode, n.cores = n.cores, 
			verbose = verbose)
	params
}
