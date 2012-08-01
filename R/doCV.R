doCV <- function(logX, groupings, ## data and grouping
			fs.methods=c("pamr","gbm","rf"), ## method selection
			DIR=NULL, ## output directory
			seed=123, ncv=5, repeats=10, jitter=FALSE,## general parameters
			maxiter=1000, maxevals=500, ## scad parameters
			max_allowed_feat=500, n.threshold=50, ## pamr parameters
			maxRuns=300,
			params=NULL, saveres=FALSE) { ## rf boruta parameters
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


	if(is.null(params)) {
		##create a default parameter object
#~ 			params <- list(seed=123,
#~ 					ncv=5, repeats=10, jitter=FALSE,## general parameters
#~ 					maxiter=1000, maxevals=500, ## scad parameters
#~ 					max_allowed_feat=500, n.threshold=50, ## pamr parameters
#~ 					maxRuns=300, ## RF parameters
#~ 					ntree = 1000, ## GBM parameters
#~ 					shrinkage = 0.01, interaction.depth = 3,
#~ 					bag.fraction = 0.75, train.fraction = 0.75, 
#~ 					n.minobsinnode = 3, n.cores = 1, 
#~ 					verbose = TRUE
#~ 			)
#~ 		params <- list(seed=seed,
#~ 				ncv=ncv, repeats=repeats, jitter=jitter,## general parameters
#~ 				maxiter=maxiter, maxevals=maxevals, ## scad parameters
#~ 				max_allowed_feat=max_allowed_feat, n.threshold=n.threshold, ## pamr parameters
#~ 				maxRuns=maxRuns, ## RF parameters (Boruta),
#~ 				localImp=TRUE, rfimportance="MeanDecreaseGini", ## RF parameters
#~ 				ntree = 1000, ## GBM parameters, also RF
#~ 				shrinkage = 0.01, interaction.depth = 3,
#~ 				bag.fraction = 0.75, train.fraction = 0.75, 
#~ 				n.minobsinnode = 3, n.cores = 1, 
#~ 				verbose = TRUE, saveres=saveres
#~ 		)
		params <- control_params(saveres = ifelse(is.null(DIR), FALSE, TRUE))
	}

	if(!is.null(DIR)) {
		if(!file.exists(DIR))
			dir.create(DIR)
	}
	
	## run the CVs
	results <- list()
	for(i in 1:length(fs.methods)) {
		method <- fs.methods[i]
		params[["fs.method"]] <- method
		if(method=="pamr") {
			results[[method]] <- cvPAMR(logX, groupings, DIR, params)
		} else if(method=="rf_boruta") {
			results[[method]] <- cvRFBORUTA(logX, groupings, DIR, params)
		} else if(method=="rf") {
			results[[method]] <- cvRFBORUTA(logX, groupings, DIR, params)
		} else if(method=="gbm") {
			results[[method]] <- cvGBM(logX, groupings, DIR, params)
		} else {
			## any out of scad, scad+L2, DrHSVM or 1norm
			## TODO change the name of the results object to the actual method
			## can also be 1norm, DrHSVM etc.
			#results[["scad"]] <- cvSCAD(logX, groupings, DIR, params)
			results[[method]] <- cvSCAD(logX, groupings, DIR, params)
		}
	}
	results
}
