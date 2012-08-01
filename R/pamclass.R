pamclass <-
function(X, logX, nfold=5, n.threshold=30, seed=NULL, max_allowed_feat=NULL, repeats=1) {
		Y <- X[[1]]
		filename <- X[[2]]
		nas <- which(is.na(Y))
		if(length(nas) > 0) {
			ypam <- Y[-nas]
			xpam <- t(logX[-nas,])
		} else {
			ypam <- Y
			xpam <- t(logX)
		}
		pamdat <- list(x=xpam, y=ypam, max_allowed_feat=max_allowed_feat)
		pamret <- list()
		set.seed(seed)
		for(ri in 1:repeats) {
			pamret[[ri]] <- run_pam(pamdat, nfold=nfold, n.threshold=n.threshold, seed=seed)
		}
		pamret
	}
