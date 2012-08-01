	doCV <- function(logX, groupings, ## data and grouping
				fs.methods=c("pamr","scad","rf_boruta"), ## method selection
				DIR="cv", ## output directory
				seed=123, ncv=5, repeats=10, jitter=FALSE,## general parameters
				maxiter=1000, maxevals=500, ## scad parameters
				max_allowed_feat=500, n.threshold=50, ## pamr parameters
				maxRuns=300) { ## rf boruta parameters
		## check if correct method namings are passed
		stopifnot(all(fs.methods %in% c("pamr", "rf_boruta", "scad", "scad+L2", "1norm", "DrHSVM")))
		#stopifnot(require(ROCR))
		
		if(!file.exists(DIR))
			dir.create(DIR)

		## run the CVs
		results <- list()
		for(i in 1:length(fs.methods)) {
			if(fs.methods[i]=="pamr") {
                paramsPAMR <- list(seed=seed, ncv=ncv, repeats=repeats, max_allowed_feat=max_allowed_feat, n.threshold=n.threshold, jitter=jitter)
				results[["pamr"]] <- cvPAMR(logX, groupings, DIR, paramsPAMR)
			} else if(fs.methods[i]=="rf_boruta") {
                paramsRFBORUTA <- list(seed=seed, ncv=ncv, repeats=repeats, maxRuns=maxRuns, jitter=jitter)
				results[["rf_boruta"]] <- cvRFBORUTA(logX, groupings, DIR, paramsRFBORUTA)
			} else {
				## any out of scad, scad+L2, DrHSVM or 1norm
				#	else if(fs.methods[i]=="scad") {
                paramsSCAD <- list(seed=seed, ncv=ncv, repeats=repeats, maxevals=maxevals, maxiter=maxiter, jitter=jitter, fs.method=fs.methods[i])
				results[["scad"]] <- cvSCAD(logX, groupings, DIR, paramsSCAD)
			}
		}
		results
	}
