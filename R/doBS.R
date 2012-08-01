doBS <- function(logX, groupings, ## data and grouping
				fs.methods=c("pamr","scad","rf_boruta"), ## method selection
				DIR="bs", ## output directory
				seed=123, bstr=100, saveres=TRUE, jitter=FALSE, ## general parameters
				maxiter=1000, maxevals=500, bounds=NULL,## scad parameters
				max_allowed_feat=NULL, n.threshold=50, ## pamr parameters
				maxRuns=300) { ## rf boruta parameters
	## check if correct method namings are passed
	stopifnot(all(fs.methods %in% c("pamr", "rf_boruta", "scad", "scad+L2", "1norm", "DrHSVM")))
	#stopifnot(require(ROCR))
		
	if(!file.exists(DIR))
		dir.create(DIR)
		
	results <- list()
	for(i in 1:length(fs.methods)) {
		if(fs.methods[i]=="pamr") {
        	paramsPAMR <- list(seed=seed, bstr=bstr, max_allowed_feat=max_allowed_feat, n.threshold=n.threshold, saveres=saveres, jitter=jitter)
			results[["pamr"]] <- bsPAMR(logX, groupings, DIR, paramsPAMR)
		} else if(fs.methods[i]=="rf_boruta") {
            paramsRFBORUTA <- list(seed=seed, bstr=bstr, maxRuns=maxRuns, saveres=saveres, jitter=jitter)
			results[["rf_boruta"]] <- bsRFBORUTA(logX, groupings, DIR, paramsRFBORUTA)
		} else { 
			#else if(fs.methods[i]=="scad") {
            paramsSCAD <- list(seed=seed, bstr=bstr, bounds=bounds, maxevals=maxevals, maxiter=maxiter, saveres=saveres, jitter=jitter, fs.method=fs.methods[i])
			results[[fs.methods[i]]] <- bsSCAD(logX, groupings, DIR, paramsSCAD)
		} 
	}

	results
}
