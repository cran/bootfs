doBS <- function(logX, groupings, ## data and grouping
				fs.methods=c("pamr","gbm","rf"), ## method selection
				DIR=NULL, ## output directory
				seed=123, bstr=100, saveres=TRUE, jitter=FALSE, ## general parameters
				maxiter=1000, maxevals=500, bounds=NULL,## scad parameters
				max_allowed_feat=NULL, n.threshold=50, ## pamr parameters
				maxRuns=300,
				params=NULL) { ## rf boruta parameters
	## check if correct method namings are passed
	stopifnot(all(fs.methods %in% c("pamr", "rf_boruta", "rf", "scad", "scad+L2", "1norm", "DrHSVM", "gbm")))
	#stopifnot(require(ROCR))

	## check if applicable methods were requested for the type of classification problem
	for(i in 1:length(groupings)) {
		gx <- groupings[[i]]
                gxu <- unique(as.character(gx))
                gxu <- gxu[!is.na(gxu)]
                gxu <- gxu[!gxu=="NA"]
		if(length(gxu)>2) {
			if(!all(fs.methods %in% c("pamr", "rf_boruta", "gbm", "rf"))) {
				stop("ERROR: Some of the classifications are multi-class. Only pamr, rf_boruta and gbm support multi-class classification so far. Please adjust your setting for fs.methods.")
			}
		}
	}

	if(!is.null(DIR)) {
		if(!file.exists(DIR))
			dir.create(DIR)
	}
	
	##create a default parameter object if not given
	if(is.null(params)) {
		params <- control_params()
#~ 		params <- list(seed=seed,
#~ 				jitter=jitter, bstr=bstr, ## general parameters
#~ 				maxiter=maxiter, maxevals=maxevals, bounds=bounds, ## scad parameters
#~ 				max_allowed_feat=max_allowed_feat, n.threshold=n.threshold, ## pamr parameters
#~ 				maxRuns=maxRuns, ## RF parameters (Boruta),
#~ 				localImp=TRUE, rfimportance="MeanDecreaseGini", ## RF parameters
#~ 				ntree = 1000, ## GBM parameters, also RF
#~ 				shrinkage = 0.01, interaction.depth = 3,
#~ 				bag.fraction = 0.75, train.fraction = 0.75, 
#~ 				n.minobsinnode = 3, n.cores = 1, 
#~ 				verbose = TRUE, saveres=saveres
#~ 		)
	}

	results <- list()
	for(i in 1:length(fs.methods)) {
		params[["fs.method"]] <- fs.methods[i]
		if(fs.methods[i]=="pamr") {
			results[["pamr"]] <- bsPAMR(logX, groupings, DIR, params)
		} else if(fs.methods[i]=="rf_boruta") {
            #params[["fs.method"]] <- fs.methods[i]
			results[["rf_boruta"]] <- bsRFBORUTA(logX, groupings, DIR, params)
		} else if(fs.methods[i]=="rf") {
            #params[["fs.method"]] <- fs.methods[i]
			results[["rf"]] <- bsRFBORUTA(logX, groupings, DIR, params)
		} else if(fs.methods[i]=="gbm") {
			results[["gbm"]] <- bsGBM(logX, groupings, DIR, params)	
		} else { 
			results[[fs.methods[i]]] <- bsSCAD(logX, groupings, DIR, params)
		} 
	}

	results
}
