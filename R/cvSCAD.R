cvSCAD <- function(logX, groupings, DIR, params=list(seed=123, ncv=5, repeats=10,maxiter=1000, maxevals=500, fs.method="scad")) {
	# output directory
	#fs.method <- "scad"
	fs.method <- params$fs.method
	seed <- params$seed
	ncv <- params$ncv
	repeats <- params$repeats
	maxiter <- params$maxiter
	maxevals <- params$maxevals
	jitter <- params$jitter
	
	## introduce some minimal noise to make scaling etc. possible
	if(jitter) {
		logX <- jitter(logX)
	}

	if(!is.null(DIR)) {
		SUBDIR <- paste(DIR,fs.method,sep="/")
		if(!file.exists(SUBDIR))
			dir.create(SUBDIR)
		fnames <- paste(SUBDIR, "/", names(groupings), ".pdf", sep="")
	} else {
		SUBDIR <- NULL
		fnames <- rep("-", length(groupings))
	}
	## grouping information
	X <- lapply(1:length(groupings), function(i,groupings,fnames) list(groupings[[i]], fnames[i]), groupings=groupings, fnames=fnames)
	names(X) <- names(groupings)

	## use multicores if more than one group is to be classified
	useparallel <- length(grep("package:(parallel|multicore)", search())>0)
	if(length(X)>1 & useparallel) {
		resSCAD <- mclapply(X, svmclass, logX=logX, ncv=ncv, repeats=repeats, maxiter=maxiter, maxevals=maxevals, fs.method=fs.method, seed=seed, mc.preschedule=TRUE, mc.cores=length(X))
	} else {
		resSCAD <- lapply(X, svmclass, logX=logX, ncv=ncv, repeats=repeats, maxiter=maxiter, maxevals=maxevals, fs.method=fs.method, seed=seed)
	}
	allpr <- colnames(logX)
	ffmat <- extract_feature_rankings(resSCAD, allpr, write=ifelse(is.null(SUBDIR), FALSE,  TRUE), DIR=SUBDIR)

	## extract the performance objects
	performance <- lapply(resSCAD, function(x) x$performance)
	names(performance) <- names(X)

	if(!is.null(SUBDIR)) {
		save(resSCAD, allpr, ffmat, X, SUBDIR, logX, ncv, repeats, maxiter, maxevals, fs.method, file=paste(SUBDIR, "env.RData", sep="/"))
	}

	list(res=resSCAD, featlist=ffmat, performance=performance)
}
