select_cv_balanced <- function(x, yp, ncv) {
		clz <- which(yp==-1)
		clo <- which(yp==1)
		nz <- floor(length(clz)/ncv)
		no <- floor(length(clo)/ncv)
		nzs <- rep(nz, ncv)
		nos <- rep(no, ncv)
		if(sum(nzs)!=length(clz)) {
			nzs[length(nzs)] <- nzs[length(nzs)] + (length(clz) - max(cumsum(nzs)))
		}
		if(sum(nos)!=length(clo)) {
			nos[length(nos)] <- nos[length(nos)] + (length(clo) - max(cumsum(nos)))
		}
		ivecz <- permute(clz)
		iveco <- permute(clo)
		indz <- c(0,cumsum(nzs))
		indo <- c(0,cumsum(nos))

		ivec <- list() #NULL
		for(lb in 1:(length(indz)-1)) {
			ivtmp <- c(ivecz[(indz[lb]+1):indz[lb+1]],iveco[(indo[lb]+1):indo[lb+1]])
			print(ivtmp)
			ivec[[lb]] <- ivtmp #c(ivec, ivtmp)
		}
		ivec
	}
