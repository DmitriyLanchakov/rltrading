# Custom indicators

# ROC on SMA indicator
ROConSMA <- function(x, nSMA, nROC) {
    bar<-ROC(SMA(x, n=nSMA), n=nROC)
    if (!is.null(dim(bar))) {
        colnames(bar) <- paste("ROC", nSMA, nROC, sep = ".")
    }
    return(bar)
}

# My SMA makes ColNames just SMA.N instead of Close.SMA.N
MySMA<-function (x, n = 10, ...) 
{
    ma <- runMean(x, n)
    if (!is.null(dim(ma))) {
        colnames(ma) <- paste("SMA", n, sep = ".")
    }
    return(ma)
}
