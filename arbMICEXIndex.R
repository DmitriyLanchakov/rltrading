library(rusquant)
library(ggplot2)
library(data.table)

from<-as.Date("2015-11-01")
to<-Sys.Date()
period="5min"

micexdf<-read.csv("micex.csv", sep=";", header = TRUE, stringsAsFactors = FALSE)
symbols<-micexdf$SECSNAME#, "MICEX")
for (symb in symbols)
    getSymbols(symb, from=from, to=to, period=period, src='mfd',adjust=TRUE, auto.assign=TRUE)

symbols<-toupper(micexdf$SECSNAME)
symbObj<-lapply(symbols, get)
allSymb<-do.call(merge, symbObj)
allSymb<-data.table(datetime=as.POSIXct(index(allSymb)), allSymb)
names<-grep("Close",colnames(allSymb), value=TRUE)

corrSymb<-allSymb[,lapply(.SD, Delt), .SDcols=names]
setnames(corrSymb, symbols)

clnames<-as.character(melt(corrSymb[,lapply(.SD, FUN=function(x)sum(is.na(x)))])[value<100]$variable)
corrSymb<-corrSymb[,.SD,.SDcols=clnames]
corrSymb<-corrSymb[complete.cases(corrSymb)]



pairs(~., data=corrSymb)
corrSymb<-corrSymb[,cor(.SD)]
corrSymb

colnames(corrSymb)<-rownames(corrSymb)<-symbols
corrSymb
#mixecDT<-data.table(as.POSIXct(index(get("MICEX 10"))),get("MICEX 10"))

