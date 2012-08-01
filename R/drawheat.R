drawheat <- function(thdat, groups=NULL, log=FALSE,
				mar=c(12,10), distfun=dist.eucsq,
				hclustfun=ward, cexCol=1, cexRow=1) {

	## original format has features in columns, samples in rows
	## the heatmaps seem to be nicer, when the features are in the
	## rows, though
	thdat <- t(thdat)
	
	if(is.null(groups)) {
		groups <- rep("none", ncol(thdat))
	}
	cvec <- matlab.like2(length(unique(groups)))
	names(cvec) <- unique(groups)
	ccols <- cvec[match(groups, names(cvec))]

	# cluster
	rclust <- hclustfun(dist.eucsq(thdat))
	cclust <- hclustfun(dist.eucsq(t(thdat)))
	ddr <- as.dendrogram(rclust)
	ddc <- as.dendrogram(cclust)

	## log the data?
	if(log) {
		thdat <- log2(thdat)
	}
	heatmap.2(thdat, Rowv=ddr, Colv=ddc, trace="none", scale="none", distfun=distfun, hclustfun=hclustfun, col=blue2green, ColSideColors=ccols, margins=mar, cexCol=cexCol, cexRow=cexRow)

	legend("topright", fill=cvec, legend=names(cvec))

}
