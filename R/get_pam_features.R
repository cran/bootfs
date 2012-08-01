get_pam_features <-
function(x) {
		list(c(signif(x$minerr,digits=4), x$selected_names))
	}
