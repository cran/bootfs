bstr_multi <- function(X, datX, bstr, seed=123, fs.method="scad",
				bounds=NULL, maxiter=1000, maxevals=500) {
        datY <- X[[1]] # select the strat vector
        nasY <- which(is.na(datY))
        nasX <- which(apply(datX, 1, function(x) all(is.na(x))))
        nas <- unique(c(nasY, nasX))
        if(length(nas)>0) {
            datX <- datX[-nas,]
            datY <- datY[-nas]
        }
        if(is.null(bounds)) {
			bounds <- t(data.frame(log2lambda1=c(-10, 10)))
			colnames(bounds)<-c("lower", "upper")
		}
        
        scads <- list()
        for(rp in 1:bstr) {
            print(rp)
			#if(!is.null(seed)) { ## increase and set seed for each iteration
				seed <- seed + 1
			#	set.seed(seed)
			#}
            proceed <- TRUE
            while(proceed) {
				## bootstrap data: keep proportions of 1 and -1 in all sets
				dat_bstr <- select_bootstrap_data(datX, datY)
				ttt <- try(scad <- my.svm.fs(dat_bstr[[1]], y=dat_bstr[[2]], 				fs.method=fs.method, bounds=bounds, 
								cross.outer= 0, grid.search = "interval",  maxIter = maxiter, 
								inner.val.method = "cv", cross.inner= 5, maxevals=maxevals,
								seed=seed, parms.coding = "log2", show="none", verbose=FALSE )
							)
				if(class(ttt)!="try-error")
					proceed <- FALSE
			}
            scads[[rp]] <- scad
        }
        scads
    }
