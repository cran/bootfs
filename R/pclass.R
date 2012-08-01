pclass <- function(X, logX, ncv, repeats, maxiter=1000, maxevals=500, fs.method="scad", seed=123) {
		Y <- X[[1]]
		filename <- X[[2]]
		ret <- cv_penalizedSVM(logX, Y, ncv=ncv, repeats=repeats, filename=filename, maxiter=maxiter, maxevals=maxevals, fs.method=fs.method, seed=seed)
		ret
	}
