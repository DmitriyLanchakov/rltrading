"sigAND" <- function(label, data=mktdata, columns,  cross = FALSE) {
    ret_sig = NULL
    colNums <- rep(0, length(columns))
    for(i in 1:length(columns)) {
        colNums[i] <- match.names(columns[i], colnames(data))
    }
    ret_sig <- data[, colNums[1]]
    for(i in 2:length(colNums)) {
        ret_sig <- ret_sig & data[, colNums[i]]
    }
    ret_sig <- ret_sig*1
    if (isTRUE(cross)) 
        ret_sig <- diff(ret_sig) == 1
    colnames(ret_sig) <- label
    return(ret_sig)
}