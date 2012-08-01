bsSCAD <- function(logX, 
                    groupings,
                    DIR, 
                    params=list(seed=123, bstr=100, maxiter=1000,
                                maxevals=500, bounds=NULL, saveres=TRUE,
                                jitter=FALSE, fs.method="scad")) {

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
		
	SUBDIR <- paste(DIR,fs.method,sep="/")
	if(!file.exists(SUBDIR))
		dir.create(SUBDIR)
        
	# assemble grouping information in one object
	fnames <- paste(SUBDIR, "/", names(groupings), ".pdf", sep="")
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

    ## draw an importance graph for the fs
	ig <- makeIG(scad_bstr, SUBDIR, prob=0.975)

	if(saveres) {
		save(scad_bstr, ig, params, file=paste(SUBDIR, "SCAD_RData.RData", sep="/"))
	}
	#invisible(list(res=scad_bstr, bstr=bstr, seed=seed, bounds=bounds, maxiter=maxiter, maxeval=maxeval, adj=adj, allsignatures=allsignatures, allprots=allprots))
	invisible(scad_bstr)
}
