# Lead Lag research by tick Data

library(data.table)
library(xts)
library(yuima)
library(ggplot2)
options(digits.secs=3)


 makeBidAskData<-function(fname)
 {
     print(paste(Sys.time(),"Start procesing bid/ask data from",fname))
     obDT<-fread(fname, sep=",", stringsAsFactors = FALSE)
     obDT[,`:=`(V2,NULL),]
     obDT[,`:=`(V9,NULL),]
     setnames(obDT,c("datetime","bidprice0","bidvolume0",
                     "bidprice1","bidvolume1",
                     "bidprice2","bidvolume2",
                     "askprice0","askvolume0",
                     "askprice1","askvolume1",
                     "askprice2","askvolume2"))
     dtFormat<-"%Y-%m-%d %H:%M:%OS"
     obDT[,datetime:=as.POSIXct(strptime(datetime,dtFormat))]
     # Filter 
     #' if best bid and best ask were not changed.
     #' if price=0
     
     obDT<-obDT[askprice0>0 &bidprice0>0 &  askprice1>0 & bidprice1>0 &  askprice2>0 & bidprice2>0]
     obDT<-obDT[askprice0>bidprice0]
     print(paste(Sys.time(),"Found",obDT[,.N]," bidasks within ",obDT[1,datetime],"-",obDT[.N,datetime]))
     #obDT<-obDT[shift(askprice0)!=askprice0 | shift(bidprice0)!=bidprice0]
     obDT<-obDT[askprice0>bidprice0]
 }
 
 
 findLeadLag<-function(dt=0.01, j=2, symbList)
 {
     
     symb1=symbList[1]
     symb2=symbList[2]
     
     dDT<-data.table(datetime=seq.POSIXt(from=as.POSIXct(paste(as.Date("2016-02-02"),"11:00:00.000")),
                                         to=as.POSIXct(paste(as.Date("2016-02-02"),"14:00:00.000")),by=dt))
     setkey(dDT,datetime)
     
     
     setwd(paste("~/repos/Data/research/regression/",symb1,sep=""))
     fileList<-dir()
     
     symb1DT<-rbindlist(lapply(grep("bid",fileList,value=T),FUN=makeBidAskData))
     symb1DT<-symb1DT[as.numeric(format(datetime,"%H")) %in% 11:14]
     setkey(symb1DT,datetime)
     
     symb1DT<-symb1DT[dDT,roll =T, mult="last"]
     
     
     symb1DT[,pricemid:=(askprice0*askvolume0+bidprice0*bidvolume0)/(askvolume0+bidvolume0)]
     symb1DT[,logRet:=rowSums(symb1DT[,lapply(1:j,FUN=function(x) symb1DT[,log(shift(pricemid,x, type="lead"))-log(pricemid)])]*exp(-0.5*(j:1)))]
     #symb1DT<-symb1DT[logRet!=0]
     #symb1DT<-symb1DT[,.SD[1],by=datetime]
     symb1.zoo<-xts(x=symb1DT[,.(logRet)], order.by=symb1DT[,datetime])
     
     setwd(paste("~/repos/Data/research/regression/",symb2,sep=""))
     fileList<-dir()
     
     symb2DT<-rbindlist(lapply(grep("bid",fileList,value=T),FUN=makeBidAskData))
     symb2DT<-symb2DT[datetime>=as.POSIXct(paste(as.Date("2016-02-01"),"10:00:00.000"))]
     setkey(symb2DT,datetime)
     symb2DT<-symb2DT[as.numeric(format(datetime,"%H")) %in% 11:14]
     
     symb2DT<-symb2DT[dDT,roll =T, mult="last"]
     
     symb2DT[,pricemid:=(askprice0*askvolume0+bidprice0*bidvolume0)/(askvolume0+bidvolume0)]
     symb2DT[,logRet:=rowSums(symb2DT[,lapply(1:j,FUN=function(x) symb2DT[,log(shift(pricemid,x, type="lead"))-log(pricemid)])]*exp(-0.5*(j:1)))]
     
     
     
     #symb2DT<-symb2DT[logRet!=0]
     #symb2DT<-symb2DT[,.SD[1],by=datetime]
     symb2.zoo<-xts(x=symb2DT[,.(logRet)], order.by=symb2DT[,datetime])
     
     #symbDT<-symb1DT[,.(datetime,symb1=logRet)][symb2DT[,.(datetime,symb2=logRet)]]
     #symbDT<-symbDT[complete.cases(symbDT)]
     #symbDT<-xts(x=symbDT[,.(symb1,symb2)],order.by=symbDT[,datetime])
     
     symbDT<-cbind(symb1.zoo,symb2.zoo)
     colnames(symbDT)<-c("symb1","symb2")
     symbDT$symb1[is.na(symbDT$symb1)]<-0
     symbDT$symb2[is.na(symbDT$symb2)]<-0
     #symbDT<-symbDT[complete.cases(symbDT)]
     #head(symbDT)
     #rm(dDT)
     rm(symb1DT)
     rm(symb2DT)
     
     rm(symb1.zoo)
     rm(symb2.zoo)
     #yuima <- setData(list( symbDT$logRet, symbDT$logRet.1))
     #yuima <- setData(list( symb1.zoo, symb2.zoo))
     
     G <- seq(-10*j*dt, 10*j*dt, by = dt)
     est <- llag(list( symbDT[1:100000]$symb1, symbDT[1:100000]$symb2), grid = G, ci = TRUE)

     #est <- llag(list( symb1.zoo[1:10000]$logRet, symb2.zoo[1:10000]$logRet), grid = G, ci = TRUE)
     
     ## The shape of the plotted cross-correlation is evidently bimodal,
     ## so there are likely two lead-lag parameters
     
     ## Lead-lag estimation by mllag
     
     mllag(est) # succeeds in detecting two lead-lag parameters
     
 }
 
 
 symbList<-c("RTS", "SBRF", "BR", "SI", "GAZ")
 symbList<-c("BRAP",  "BR")
 resLL<-lapply(combn(symbList,2, simplify = F),FUN=function(s)findLeadLag(symbList = s))
 
 lapply(1:length(resLL),FUN=function(i){
     symbPair<-toString(combn(symbList,2, simplify = F)[[i]])
     png(filename=paste(symbPair,".png",sep=""),width = 1024, height = 768)
     plot(resLL[[i]])
     title(paste("\n\n",symbPair,toString(data.table(resLL[[i]]$mlagcce$`(1,2)`)[order(-abs(correlation))][1])))
     dev.off()
 })
 