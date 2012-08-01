bsRFBORUTA <- function(logX, groupings, DIR=NULL, params=NULL) {
	## default parameters if not given
	if(is.null(params)) {
		#params <- list(seed=123, bstr=100, maxRuns=300, saveres=TRUE, jitter=FALSE, localImp=TRUE, rfimportance="MeanDecreaseAccuracy", ntree=1000)
		params <- control_params()
	}
	## introduce some minimal noise to make scaling etc. possible
	if(params$jitter) {
		logX <- jitter(logX)
	}

	# output directory
	fs.method <- params$fs.method #"rf_boruta"
	seed <- params$seed
	bstr <- params$bstr
	maxRuns <- params$maxRuns
	saveres <- params$saveres
	rfimportance <- params$rfimportance
	ntree <- params$ntree
	fs.method <- params$fs.method
	localImp <- params$localImp
	
	if(!is.null(DIR)) {
		SUBDIR <- paste(DIR,fs.method,sep="/")
		if(!file.exists(SUBDIR))
			dir.create(SUBDIR)
		fnames <- paste(SUBDIR, "/", names(groupings), ".pdf", sep="")
	} else {
		SUBDIR <- NULL
		fnames <- rep("-", length(groupings))
		saveres <- FALSE
	}	

	# grouping information
	X <- lapply(1:length(groupings), function(i,groupings,fnames) list(groupings[[i]], fnames[i]), groupings=groupings, fnames=fnames)
	names(X) <- names(groupings)
	
    ## use multicores if more than one group is to be classified
    useparallel <- length(grep("package:(parallel|multicore)", search())>0)
	if(length(X)>1 & useparallel) {
		rfs_bstr <- mclapply(X, rf_multi, datX=logX, maxRuns=maxRuns, seed=seed, bstr=bstr, rfimportance=rfimportance, ntree=ntree, fs.method=fs.method, localImp=localImp,mc.preschedule=TRUE, mc.cores=length(X))
	} else {
		rfs_bstr <- lapply(X, rf_multi, datX=logX, maxRuns=maxRuns, seed=seed, bstr=bstr, rfimportance=rfimportance, ntree=ntree, fs.method=fs.method, localImp=localImp)
	}
	
    ## set an attribute naming the method used
    attr(rfs_bstr, "fs.method") <- fs.method #"rf_boruta"

	if(saveres) {
		ig <- makeIG(rfs_bstr, SUBDIR, prob=0.975)
		save(rfs_bstr, ig, params, file=paste(SUBDIR, "RF_RData.RData", sep="/"))
	}

	invisible(rfs_bstr)
}

