purity = function(ct) {
    sum(apply(ct, 1, max)) / sum(ct)
}

f_measure = function(ct) {
    F_h_k = matrix(0, nrow=nrow(ct), ncol=ncol(ct))
    for(h in 1:ncol(ct)) {
        for(k in 1:nrow(ct)) {
            F_h_k[k, h] = (2 * ct[k, h]) / (sum(ct[k, ]) + sum(ct[, h]))
        }
    }

    F_h = matrix(0, ncol=ncol(ct))
    for(h in 1:ncol(ct)) {
        F_h = sum(ct[, h]) / sum(ct) * max(F_h_k[, h])
    }
    sum(F_h)
}