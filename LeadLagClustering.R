# Lead Lag research by tick Data

library(data.table)
library(xts)
library(yuima)
library(ggplot2)
options(digits.secs=3)

# Чтение стакана
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


# Шаг дискретизации
dt=0.01

# Период заглядывания в будущее
j=2

symbList<-c("BRAP",  "BR")
symb1=symbList[1]
symb2=symbList[2]

# Датасет для дискретизации
dDT<-data.table(datetime=seq.POSIXt(from=as.POSIXct(paste(as.Date("2016-02-02"),"11:00:00.000")),
                                    to=as.POSIXct(paste(as.Date("2016-02-02"),"14:00:00.000")),by=dt))
setkey(dDT,datetime)


setwd(paste("~/repos/Data/research/regression/",symb1,sep=""))
fileList<-dir()

symb1DT<-rbindlist(lapply(grep("bid",fileList,value=T),FUN=makeBidAskData))
symb1DT<-symb1DT[as.numeric(format(datetime,"%H")) %in% 11:17]
setkey(symb1DT,datetime)

# На каждый дискрет из dDT берем ближайшее последнее значение из symb1DT 
#symb1DT<-symb1DT[dDT,roll =T, mult="last"]

setwd(paste("~/repos/Data/research/regression/",symb2,sep=""))
fileList<-dir()

symb2DT<-rbindlist(lapply(grep("bid",fileList,value=T),FUN=makeBidAskData))
symb2DT<-symb2DT[datetime>=as.POSIXct(paste(as.Date("2016-02-01"),"10:00:00.000"))]
setkey(symb2DT,datetime)
symb2DT<-symb2DT[as.numeric(format(datetime,"%H")) %in% 11:17]

#symb2DT<-symb2DT[dDT,roll =T, mult="last"]

##########################TEST FOR CLUSTERING##########################################
# Средняя цена. Можно VWAP еще проверить
symb1DT[,pricemid:=log((askprice0+bidprice0)/2)]
symb2DT[,pricemid:=log((askprice0+bidprice0)/2)]

# Изменения цены в пунктах
symb2DT[,dprice:=(shift(pricemid,1, type="lag")-pricemid)]
symb1DT[,dprice:=(shift(pricemid,1, type="lag")-pricemid)]

smDT<-symb1DT[,.(datetime,symb1=dprice)][symb2DT[,.(datetime,symb2=dprice)],allow.cartesian=TRUE]
smDT<-melt.data.table(smDT,c("datetime"))
smDT[is.na(value),value:=0]
setkey(smDT,datetime)

smDT[variable=="symb1", cmvalue:=cumsum(value)]
smDT[variable=="symb2", cmvalue:=cumsum(value)]

#' График кумулятиваных разниц цен
#' Гипотеза: если симв1 ниже, то покупаем
#qplot(x=datetime,y=cmvalue,color=variable, data=smDT)


smDT[variable=="symb2" & 
         shift(variable,1, type="lag") == "symb2" &
         shift(variable,2, type="lag") == "symb2" &
         shift(variable,3, type="lag") == "symb2" &
         #shift(variable,1, type="lead") == "symb1" &
         shift(variable,4, type="lag") == "symb1" &
         (value+
              shift(value,1,type="lag")+
              shift(value,2,type="lag")+
              shift(value,3,type="lag")) > 0,pos:=1]

smDT[variable=="symb2" & 
         shift(variable,1, type="lag") == "symb2" &
         shift(variable,2, type="lag") == "symb2" &
         shift(variable,3, type="lag") == "symb2" &
         #shift(variable,1, type="lead") == "symb1" &
         shift(variable,4, type="lag") == "symb1" &
         (value+
              shift(value,1,type="lag")+
              shift(value,2,type="lag")+
              shift(value,3,type="lag")) < 0,pos:=-1]


smDT[shift(abs(pos)==1)]

smDT[shift(pos==1),CP:= cumsum(value)]
smDT[shift(pos==-1),CP:= cumsum(abs(value))]

smDT[,pos1:=shift(pos,1,type="lag")]

qplot(x=datetime, y=CP,geom = c("line", "point"), color=factor(pos1), data=smDT[shift(abs(pos)==1)])
qplot(x=datetime, y=cumsum(abs(value)),geom = c("line", "point"), data=smDT[shift(abs(pos)==1)])


####################################################################

# Buy at ask and Sell at Bid HOLD POSITION 10 events
cpDT<-rbindlist(lapply(smDT[shift(abs(pos)==1)][,datetime], FUN=function(i)symb1DT[datetime>=i,][1]))

cpDT<-cpDT[complete.cases(cpDT)]
cpDT[pos1==-1,value:=(-log(bidprice0)+log(shift(askprice0,10,type="lag")))]
cpDT[pos1==1,value:=(log(askprice0)-log(shift(bidprice0,10,type="lag")))]

cpDT<-cpDT[!is.na(value)]
cpDT[pos1==-1,value:=cumsum(value)]
cpDT[pos1==1,value:=cumsum(value)]
qplot(x=datetime,y=value,color=factor(pos1),data=cpDT)


# Buy at Bid and sell at Ask HOLD POSITION 10 events
cpDT<-symb1DT[smDT[shift(abs(pos)==1)], mult="last"]
cpDT[pos1==-1,value:=(-log(askprice0)+log(shift(bidprice0,10,type="lag")))]
cpDT[pos1==1,value:=(log(bidprice0)-log(shift(askprice0,10,type="lag")))]

cpDT<-cpDT[!is.na(value)]
cpDT[pos1==-1,value:=cumsum(value)]
cpDT[pos1==1,value:=cumsum(value)]
qplot(x=datetime,y=value,color=factor(pos1),data=cpDT)


# Mid Price HOLD POSITION 10 events
cpDT<-symb1DT[smDT[shift(abs(pos)==1)], mult="last"]
cpDT[pos1==-1,value:=(-log(pricemid)+log(shift(pricemid,2,type="lag")))]
cpDT[pos1==1,value:=(log(pricemid)-log(shift(pricemid,2,type="lag")))]

cpDT<-cpDT[!is.na(value)]
cpDT[pos1==-1,value:=cumsum(value)]
cpDT[pos1==1,value:=cumsum(value)]
qplot(x=datetime,y=value,color=factor(pos1),data=cpDT)


# Mid Price. Sell / Buy each signal.
cpDT<-symb1DT[smDT[shift(abs(pos)==1)], mult="last"]
cpDT[,value:=pos1*log(pricemid)]
cpDT[,value:=value-shift(value)]
cpDT<-cpDT[!is.na(value)]
cpDT[,value:=cumsum(value)]
qplot(x=datetime,y=value,geom = c("point"),color=factor(pos1),data=cpDT)


