bsSCAD <- function(logX, groupings, DIR, params=NULL) {
	## default parameters if not given
	if(is.null(params)) {
		params <- list(seed=123, bstr=100, maxiter=1000, maxevals=500,
			bounds=NULL, saveres=TRUE, jitter=FALSE)
	}
	## introduce some minimal noise to make scaling etc. possible
	if(params$jitter) {
		logX <- jitter(logX)
	}

	# output directory
	fs.method <- "scad"
	seed <- params$seed
	bstr <- params$bstr
	bounds <- params$bounds
	maxiter <- params$maxiter
	maxevals <- params$maxevals
	saveres <- params$saveres
		
	SUBDIR <- paste(DIR,fs.method,sep="/")
	if(!file.exists(SUBDIR))
		dir.create(SUBDIR)
	# grouping information
	fnames <- paste(SUBDIR, "/", names(groupings), ".pdf", sep="")
	X <- lapply(1:length(groupings), function(i,groupings,fnames) list(groupings[[i]], fnames[i]), groupings=groupings, fnames=fnames)
	names(X) <- names(groupings)

	if(length(X)>1) {
		scad_bstr <- mclapply(X, bstr_multi, datX=logX, bstr=bstr, seed=seed, fs.method=fs.method, bounds=bounds, maxiter=maxiter, maxevals=maxevals, mc.preschedule=TRUE, mc.cores=length(X))
	} else {
		scad_bstr <- lapply(X, bstr_multi, datX=logX, bstr=bstr, seed=seed, fs.method=fs.method, bounds=bounds, maxiter=maxiter, maxevals=maxevals)
	}

	ig <- makeIG(scad_bstr, SUBDIR)

	if(saveres) {
		save(scad_bstr, ig, params, file=paste(SUBDIR, "SCAD_RData.RData", sep="/"))
	}
	#invisible(list(res=scad_bstr, bstr=bstr, seed=seed, bounds=bounds, maxiter=maxiter, maxeval=maxeval, adj=adj, allsignatures=allsignatures, allprots=allprots))
	invisible(scad_bstr)
}
