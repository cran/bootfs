select_threshold <-
function(histcv, max_allowed_feat=NULL){
	   ts <- histcv$threshold
		te <- histcv$error
		if(!is.null(max_allowed_feat)) {
			raus <- which(histcv$size>max_allowed_feat)
			if(length(raus>0)) {
				ts <- ts[-raus]
				te <- te[-raus]
			}
		}
		## select threshold
		tmin <- ts[min(which(te==min(te)))] ## prefer smallest feature number 
		tmin
	}
