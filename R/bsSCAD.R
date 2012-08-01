bsSCAD <- function(logX, 
                    groupings,
                    DIR=NULL, 
                    params=NULL) {

                    #list(seed=123, bstr=100, maxiter=1000,
                     #           maxevals=500, bounds=NULL, saveres=TRUE,
                      #          jitter=FALSE, fs.method="scad")) {

	if(is.null(params)) {
		#params <- list(seed=123, ncv=5, repeats=10, 
		#			ntree=1000, shrinkage=0.01, interaction.depth=3,
		#			bag.fraction=0.75, train.fraction=0.75,
		#			n.minobsinnode=3, n.cores=1, verbose=TRUE,
		#			jitter=FALSE, saveres=FALSE)
		params <- control_params()
	}

	## introduce some minimal noise to make scaling etc. possible
	if(params$jitter) {
		logX <- jitter(logX)
	}

	# output directory
	fs.method <- params$fs.method
	seed <- params$seed
	bstr <- params$bstr
	bounds <- params$bounds
	maxiter <- params$maxiter
	maxevals <- params$maxevals
	saveres <- params$saveres

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
	        
	# assemble grouping information in one object
	X <- lapply(1:length(groupings), function(i,groupings,fnames) list(groupings[[i]], fnames[i]), groupings=groupings, fnames=fnames)
	names(X) <- names(groupings)

    ## use multicores if more than one group is to be classified
    useparallel <- length(grep("package:(parallel|multicore)", search())>0)
	if(length(X)>1 & useparallel) {
		scad_bstr <- mclapply(X, bstr_multi, datX=logX, bstr=bstr, seed=seed, fs.method=fs.method, bounds=bounds, maxiter=maxiter, maxevals=maxevals, mc.preschedule=TRUE, mc.cores=length(X))
	} else {
		scad_bstr <- lapply(X, bstr_multi, datX=logX, bstr=bstr, seed=seed, fs.method=fs.method, bounds=bounds, maxiter=maxiter, maxevals=maxevals)
	}

    ## set an attribute naming the method used
    attr(scad_bstr, "fs.method") <- fs.method


	if(saveres) {
	    ## draw an importance graph for the fs
		ig <- makeIG(scad_bstr, SUBDIR, prob=0.975)
		save(scad_bstr, ig, params, file=paste(SUBDIR, "SCAD_RData.RData", sep="/"))
	}
	#invisible(list(res=scad_bstr, bstr=bstr, seed=seed, bounds=bounds, maxiter=maxiter, maxeval=maxeval, adj=adj, allsignatures=allsignatures, allprots=allprots))
	invisible(scad_bstr)
}
