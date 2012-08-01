rf_multi <- function(X, datX, maxRuns=500, seed=123, bstr=100) {
        datY <- X[[1]]
        nasY <- which(is.na(datY))
        nasX <- which(apply(datX, 1, function(x) all(is.na(x))))
        nas <- unique(c(nasY, nasX))
        if(length(nas)>0) {
            datX <- datX[-nas,]
            datY <- datY[-nas]
        }
        rfs <- list()
        for(rp in 1:bstr) {
            print(rp)
   			#if(!is.null(seed)) { ## increase and set seed for each iteration
				seed <- seed + 1
			#	set.seed(seed)
			#}
            ## bootstrap data: keep proportions of 1 and -1 in all sets
            dat_bstr <- select_bootstrap_data(datX, datY)
            bres <- Boruta(x=as.data.frame(dat_bstr$datX), y=factor(dat_bstr$datY), doTrace=2, maxRuns=maxRuns)
            selprobes <- gsub("`","",names(bres$finalDecision[which(bres$finalDecision!="Rejected")]))
            rfs[[rp]] <- list(bres=bres, selprobes=selprobes)
        }
        rfs
    }
