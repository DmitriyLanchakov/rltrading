#''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
#' Построение истории по списку инструменту из  архивов сделок по фьючерсам и 
#' опционам с сайта Московской биржи
#' 
#' 
#' 2016-03-20 | rlukerin@gmail.com
#''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
#' Месяц	Код фьючерса
#' Январь	F
#' Февраль	G
#' Март	H
#' Апрель	J
#' Май	K
#' Июнь	M
#' Июль	N
#' Август	Q
#' Сентябрь	U
#' Октябрь	V
#' Ноябрь	X
#' Декабрь	Z
#' 

library(data.table)
options(digits.secs=3)
getSymbData<-function(symbol, fname){
    futFname<-gsub(".ZIP","ft.csv",gsub("FT","",toupper(fname)))
    unzip(fname, files=futFname)
    secDT<-fread(futFname, stringsAsFactors = FALSE)
    file.remove(futFname)
    secDT[code %in%symbol][, dat_time:=as.POSIXct(strptime(dat_time,format="%Y-%m-%d %H:%M:%OS"))][]
}


getSymbol.MOEX<-function(from=as.Date("2016-03-28"), to=as.Date("2016-04-05"),symbList=c("GZM5", "GZH5"), homeDir="~/repos/Data/MOEX/"){
    
    years<-year(from):year(to)    
    fileFilter<-paste("FT",format(seq.Date(from=from, to=to, by=1),"%y%m%d"),".zip",sep="")
    symbDT<-rbindlist(lapply(years,FUN=function(y){
        setwd(paste(homeDir,y,sep=""))
        fileList<-dir()[dir() %in% fileFilter]
        rbindlist(lapply(fileList,FUN=function(f)getSymbData(symbList,f)))}))
    
    symbDT
}
