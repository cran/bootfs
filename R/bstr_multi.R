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
        ## somehow this is the only way how these methods are going to work
        grid.search <- "interval"
        lambda1.scad <- lambda2.scad <- NULL
        bounds <- NULL

#~         if(fs.method %in% c("scad+L2", "DrHSVM")) {
#~ 			grid.search <- "discrete"
#~ 			#lambda1.scad <- c(seq(0.01 ,0.05, .01),  seq(0.1,0.5, 0.2), 1 ) 	
#~ 			#lambda1.scad <- lambda1.scad[2:3]
#~ 			lambda1.scad <- c(0.01, 0.05, 0.1, 0.25, 0.5, 1)
#~ 			bounds <- NULL
#~ 		} else {
#~ 			grid.search <- "interval"
#~ 			bounds <- t(data.frame(log2lambda1=c(-10, 10)))
#~ 			colnames(bounds) <- c("lower", "upper")
#~ 			lambda1.scad <- NULL
#~ 		}
   
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
				ttt <- try(scad <- my.svm.fs(dat_bstr[[1]], y=dat_bstr[[2]], 
								fs.method=fs.method, bounds=bounds,
								lambda1.set=lambda1.scad, lambda2.set=lambda2.scad,
								cross.outer= 0, grid.search = grid.search,  maxIter = maxiter, 
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
