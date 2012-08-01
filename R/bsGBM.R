bsGBM <- function(logX, groupings, DIR, params=NULL) {
	## default parameters if not given
	if(is.null(params)) {
		params <- list(seed=123, ncv=5, repeats=10, 
					ntree=1000, shrinkage=0.01, interaction.depth=3,
					bag.fraction=0.75, train.fraction=0.75,
					n.minobsinnode=3, n.cores=1, verbose=TRUE,
					jitter=FALSE, saveres=FALSE)
	}
	## introduce some minimal noise to make scaling etc. possible
	if(params$jitter) {
		logX <- jitter(logX)
	}

	# output directory
	fs.method <- "gbm"
	seed <- params$seed ##careful that this is not always the same, in doBS
	bstr <- params$bstr
	#maxRuns <- params$maxRuns
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
		rfs_bstr <- mclapply(X, gbm_multi, datX=logX, seed=seed, bstr=bstr, params=params, mc.preschedule=TRUE, mc.cores=length(X))
	} else {
		rfs_bstr <- lapply(X, gbm_multi, datX=logX, seed=seed, bstr=bstr, params=params)
	}
	
    ## set an attribute naming the method used
    attr(rfs_bstr, "fs.method") <- "gbm"

	ig <- makeIG(rfs_bstr, SUBDIR, prob=0.975)

	if(saveres) {
		save(rfs_bstr, ig, params, file=paste(SUBDIR, "RF_RData.RData", sep="/"))
	}

	invisible(rfs_bstr)
}

