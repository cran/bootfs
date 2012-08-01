doBS <- function(logX, groupings, ## data and grouping
				fs.methods=c("pamr","scad","rf_boruta"), ## method selection
				DIR="bs", ## output directory
				seed=123, bstr=100, saveres=TRUE, jitter=FALSE, ## general parameters
				maxiter=1000, maxevals=500, bounds=NULL,## scad parameters
				max_allowed_feat=NULL, n.threshold=50, ## pamr parameters
				maxRuns=300) { ## rf boruta parameters
	## check if correct method namings are passed
	stopifnot(all(fs.methods %in% c("pamr", "scad", "rf_boruta")))
	#stopifnot(require(ROCR))
		
	if(!file.exists(DIR))
		dir.create(DIR)

	## create parameter objects for the different methods
	paramsPAMR <- list(seed=seed, bstr=bstr, max_allowed_feat=max_allowed_feat, n.threshold=n.threshold, saveres=saveres, jitter=jitter)
		
	paramsSCAD <- list(seed=seed, bstr=bstr, bounds=bounds, maxevals=maxevals, maxiter=maxiter, saveres=saveres, jitter=jitter)
		
	paramsRFBORUTA <- list(seed=seed, bstr=bstr, maxRuns=maxRuns, saveres=saveres, jitter=jitter)

	results <- list()
	for(i in 1:length(fs.methods)) {
		if(fs.methods[i]=="pamr") {
			results[["pamr"]] <- bsPAMR(logX, groupings, DIR, paramsPAMR)
		} else if(fs.methods[i]=="scad") {
			results[["scad"]] <- bsSCAD(logX, groupings, DIR, paramsSCAD)
		} else if(fs.methods[i]=="rf_boruta") {
			results[["rf_boruta"]] <- bsRFBORUTA(logX, groupings, DIR, paramsRFBORUTA)
		}
	}

	results
}
