#''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
#' Арбитраж Ри против синтетики миниМИКС + Си 
#' Данные: архив тиковых данных с сайта Московской биржи
#' 
#' #' 2016-04-06 | rlukerin@gmail.com
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

homeDir<-"~/repos/Data/MOEX/"
#setwd(homeDir)
#dirList<-dir()
#setwd(dirList[14])
#fileList<-dir()
getSymbData<-function(symbol, fname){
  futFname<-gsub(".ZIP","ft.csv",gsub("FT","",toupper(fname)))
  unzip(fname, files=futFname)
  secDT<-fread(futFname, stringsAsFactors = FALSE)
  file.remove(futFname)
  secDT[code %in%symbol][, dat_time:=as.POSIXct(strptime(dat_time,format="%Y-%m-%d %H:%M:%OS"))][]
}

symbMonth<-c("F", "G", "H","J","K","M","N","Q","U","V","X","Z")
symb<-"BR"

years<-2016:2016
from<-as.Date("2016-03-20")
to<-as.Date("2016-04-15")
#from<-as.Date("2016-01-15")
#to<-as.Date("2016-03-15")

fileFilter<-paste("FT",format(seq.Date(from=from, to=to, by=1),"%y%m%d"),".zip",sep="")


symbList<-unlist(lapply(years-2010, FUN=function(x)paste(symb,symbMonth,x, sep="")))
#symbList<-c("MXH6","MMH6","MXM6", "MMM6")
#symbList<-c("RIM6", "SiM6","MMM6", "SRM6", "GZM6","LKM6")
#symbList<-c("MMM6", "SRM6", "GZM6","LKM6")
symbList<-c("RIM6", "SiM6","MMM6")

symbDT<-rbindlist(lapply(years,FUN=function(y){
  setwd(paste(homeDir,y,sep=""))
  fileList<-dir()[dir() %in% fileFilter]
  rbindlist(lapply(fileList,FUN=function(f)getSymbData(symbList,f)))}))




  spreadDT<-symbDT[,median(price), by=.(code, format(dat_time,"%Y%m%d%H%M%S"))]
  spreadDT<-spreadDT[,.(.N,code, V1),by=format][N>1]
  spreadDT<-dcast.data.table(spreadDT,format~code, fun=mean, drop = FALSE, value.var = "V1")
  spreadDT[,datetime:=as.POSIXct(strptime(x=format,format="%Y%m%d%H%M%S"))]
  spreadDT<-spreadDT[complete.cases(spreadDT[,.SD,.SDcol=symbList]*0)]
  #spreadDT[,v1:=4.447*100*1000*MMM6/SiM6]
  
  spreadDT[,v1:=5*10*1000*10*get(symbList[3])/get(symbList[2])]

  modFit<-lm(get(symbList[1]) ~ v1-1, data=spreadDT)
  
  spreadDT[,synRI:=modFit$coefficients[1]*v1]
  spreadDT[,synRI:=19/3*v1]
  spreadDT[,spread:=get(symbList[1])-synRI]
  
  library(ggplot2)
  #ggplot(data=spreadDT[datetime>=as.POSIXct("2016-04-05") & datetime<as.POSIXct("2016-04-05")])+
  ggplot(data=spreadDT[spread > -400 & spread <  400])+
    geom_point(aes(x=as.POSIXct(strptime(x=paste("2016-04-01",format(datetime,"%H%M%S")),format="%Y-%m-%d %H%M%S")),y=SMA(spread,300),colour=format(datetime,"%Y-%m-%d")))+
    scale_y_continuous(breaks=seq(-400,400,10))+
    ggtitle("Day to Day spread value by time")+
    ylab("Spread RI ~ synRI")+
    xlab("Time")
  
  
  ggplot(data=spreadDT[spread > -400 & spread < 400])+
    geom_density(aes(spread,colour=format(datetime,"%Y-%m-%d")))
    
    
  
  summary(spreadDT[,get(symbList[1])-synRI])
  
#   
# # Synt MIX
#   modFit<-lm(get(symbList[1]) ~ get(symbList[3])-1, data=spreadDT)
#   
#   spreadDT[,synMIX:=rowSums(spreadDT[,lapply(3:3,FUN=function(x) modFit$coefficients[x-2]*get(symbList[x]))])]
#   #spreadDT[,synRI:=19/3*v1]
#   spreadDT[,spread:=get(symbList[1])-synMIX]
#   
#   library(ggplot2)
#   #ggplot(data=spreadDT[datetime>=as.POSIXct("2016-04-05") & datetime<as.POSIXct("2016-04-05")])+
#   ggplot(data=spreadDT[spread > -400 & spread <  400])+
#     geom_point(aes(x=as.POSIXct(strptime(x=paste("2016-04-01",format(datetime,"%H%M%S")),format="%Y-%m-%d %H%M%S")),y=SMA(spread,300),colour=format(datetime,"%Y-%m-%d")))+
#     scale_y_continuous(breaks=seq(-400,400,10))+
#     ggtitle("Day to Day spread value by time")+
#     ylab("Spread RI ~ synRI")+
#     xlab("Time")
  
  

  ggplot(data=spreadDT[spread > -400 & spread < 400])+
    geom_density(aes(spread,colour=format(datetime,"%Y-%m-%d")))
  
  
  
  summary(spreadDT[,spread])
    
#   
#   symbDT[, dtcheck:=dat_time]
#   setkey(symbDT, dat_time)
#   sprDT<-symbDT[code=="RIM6" & amount==1][symbDT[code=="MMM6" & amount ==19],roll=T,mult="last"]
#   sprDT<-symbDT[code=="SiM6" & amount==3][sprDT,,roll=T,mult="last"]
#   sprDT[,dtdif:=dat_time-dtcheck]
#   sprDT<-sprDT[as.numeric(dtdif)<1]
#   sprDT[,spread:=i.price-5*100*1000*19/3*i.price.1/price]
#   
#   
#   ggplot(data=sprDT[dat_time>=as.POSIXct("2016-04-04") & dat_time<as.POSIXct("2016-04-05")])+
#     geom_line(aes(x=dat_time,y=spread))+
#     geom_point(aes(x=dat_time,y=spread))+ scale_y_continuous(breaks=seq(-300,-200,10))
#   
#   
#   