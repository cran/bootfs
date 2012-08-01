## get all signatures from the bootstrapping object
extractsignatures <- function(res_bstr, strat) {
    switch(attr(res_bstr, "fs.method"),
        'pamr' = allsignatures <- sapply(res_bstr[[strat]], function(x) x$selected_names, simplify=FALSE),
        'rf_boruta' = allsignatures <- sapply(res_bstr[[strat]], function(x) x$selprobes, simplify=FALSE),
        'rf' = allsignatures <- sapply(res_bstr[[strat]], function(x) x$selprobes, simplify=FALSE),
        'scad' = allsignatures <- sapply(res_bstr[[strat]], function(x) names(x$model$w), simplify=FALSE),
        'scad+L2' = allsignatures <- sapply(res_bstr[[strat]], function(x) names(x$model$w), simplify=FALSE),
        '1norm' = allsignatures <- sapply(res_bstr[[strat]], function(x) names(x$model$w), simplify=FALSE),
        'DrHSVM' = allsignatures <- sapply(res_bstr[[strat]], function(x) names(x$model$w), simplify=FALSE),
        'gbm' = allsignatures <- sapply(res_bstr[[strat]], function(x) x$selprobes, simplify=FALSE)
    )
    allsignatures
}
