bsPAMR <- function(logX, groupings, DIR, params=NULL) {
	## default parameters if not given
	if(is.null(params)) {
		params <- list(seed=123, bstr=100, ncv=5, max_allowed_feat=500, n.threshold=30, saveres=TRUE, jitter=FALSE)
	}

	## introduce some minimal noise to make scaling etc. possible
	if(params$jitter) {
		logX <- jitter(logX)
	}
	
	fs.method <- "pamr"
	seed <- params$seed
	bstr <- params$bstr
	ncv <- params$ncv
	max_allowed_feat <- params$max_allowed_feat
	n.threshold <- params$n.threshold
	saveres <- params$saveres
	
	SUBDIR <- paste(DIR,fs.method,sep="/")
	if(!file.exists(SUBDIR))
		dir.create(SUBDIR)
	# grouping information
	fnames <- paste(SUBDIR, "/", names(groupings), ".pdf", sep="")
	X <- lapply(1:length(groupings), function(i,groupings,fnames) list(groupings[[i]], fnames[i]), groupings=groupings, fnames=fnames)
	names(X) <- names(groupings)
	seedo <- seed # start seed value

	## do the pamr classification for all strata
    ## is rather quick, so parallel execution is not a major speed gain
    ## TODO: add parallel option
	pam_bstr <- list()
	for(i in 1:length(X)) {
		datX <- logX
		datY <- X[[i]][[1]]
		nasY <- which(is.na(datY))
		nasX <- which(apply(datX, 1, function(x) all(is.na(x))))
		nas <- unique(c(nasY, nasX))
		if(length(nas)>0) {
			datX <- datX[-nas,]
			datY <- datY[-nas]
		}
		pams <- list()
		seed <- seedo # start with the same seed for each classification
		## do the bootstrapping
		bind <- createResample(datY, bstr)

		for(rp in 1:bstr) {
			#stopifnot(require(pamr))
			print(rp)
			#if(!is.null(seed)) { ## increase and set seed for each iteration
				seed <- seed + 1
			#	set.seed(seed)
			#}
			## bootstrap data: keep proportions of 1 and -1 in all sets
			#dat_bstr <- select_bootstrap_data(datX, datY) #datX[sample(1:nrow(datX), nrow(datX),]
			dat_bstr <- list(datX=datX[bind[[rp]],], datY=datY[bind[[rp]]])
			rownames(dat_bstr$datX) <- paste(rownames(dat_bstr$datX), 1:nrow(dat_bstr$datX), sep=".")

			ypam <- dat_bstr[["datY"]]
			xpam <- t(dat_bstr[["datX"]])
			pamdat <- list(x=xpam, y=ypam)
			histtr <- pamr.train(pamdat, n.threshold=n.threshold)
			histcv <- pamr.cv(histtr, pamdat, nfold=ncv)
		#browser()
			tmin <- select_threshold(histcv, max_allowed_feat=max_allowed_feat)
			selected <- pamr.predict(histtr, pamdat$x, tmin, type="nonzero")
			selected_names <- rownames(pamdat$x)[selected]
			pams[[rp]] <- list(histtr=histtr, histcv=histcv, tmin=tmin, selected=selected, selected_names=selected_names)
		}
		pam_bstr[[names(X)[i]]] <- pams
	}

    ## set an attribute naming the method used
    attr(pam_bstr, "fs.method") <- "pamr"

	ig <- makeIG(pam_bstr, SUBDIR, prob=0.975)

	if(saveres) {
		save(pam_bstr, ig, params, file=paste(SUBDIR, "PAM_RData.RData", sep="/"))
	}
	invisible(pam_bstr)
}
