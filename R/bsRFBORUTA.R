bsRFBORUTA <- function(logX, groupings, DIR, params=NULL) {
	## default parameters if not given
	if(is.null(params)) {
		params <- list(seed=123, bstr=100, maxRuns=300, saveres=TRUE, jitter=FALSE)
	}
	## introduce some minimal noise to make scaling etc. possible
	if(params$jitter) {
		logX <- jitter(logX)
	}

	# output directory
	fs.method <- "rf_boruta"
	seed <- params$seed
	bstr <- params$bstr
	maxRuns <- params$maxRuns
	saveres <- params$saveres
		
	SUBDIR <- paste(DIR,fs.method,sep="/")
	if(!file.exists(SUBDIR))
		dir.create(SUBDIR)
	# grouping information
	fnames <- paste(SUBDIR, "/", names(groupings), ".pdf", sep="")
	X <- lapply(1:length(groupings), function(i,groupings,fnames) list(groupings[[i]], fnames[i]), groupings=groupings, fnames=fnames)
	names(X) <- names(groupings)
	
    ## use multicores if more than one group is to be classified
    useparallel <- length(grep("package:(parallel|multicore)", search())>0)
	if(length(X)>1 & useparallel) {
		rfs_bstr <- mclapply(X, rf_multi, datX=logX, maxRuns=maxRuns, seed=seed, bstr=bstr, mc.preschedule=TRUE, mc.cores=length(X))
	} else {
		rfs_bstr <- lapply(X, rf_multi, datX=logX, maxRuns=maxRuns, seed=seed, bstr=bstr)
	}
	
    ## set an attribute naming the method used
    attr(rfs_bstr, "fs.method") <- "rf_boruta"

	ig <- makeIG(rfs_bstr, SUBDIR, prob=0.975)

	if(saveres) {
		save(rfs_bstr, ig, params, file=paste(SUBDIR, "RF_RData.RData", sep="/"))
	}

	invisible(rfs_bstr)
}

