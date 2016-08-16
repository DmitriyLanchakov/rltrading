"sigTimeStampSession"<-function (label, data = mktdata, startTimeStamp, endTimeStamp) 
{
    if (missing(label)) 
        label <- "timestamp"
    ret <- .xts(logical(nrow(data)), .index(data), dimnames = list(NULL, 
                                                                   label))
    if (is.character(startTimeStamp)) {
        time.str <- paste("T",startTimeStamp,"/T", endTimeStamp, sep = "")
        ret[time.str] <- TRUE
    }
    else {
        stop("don't know how to handle 'timestamp' of class ", 
             class(startTimeStamp))
    }
    return(ret)
}