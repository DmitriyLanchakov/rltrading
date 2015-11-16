##VPIN

#install.packages('fasttime',repos='http://www.rforge.net/', type="source")
#install.packages("data.table")
require(data.table); 
require(fasttime); 
require(plyr)

# Now we have an xts data frame called 'stock' with a DateTime index and... 
# two columns: Price and Volume
# Vbucket=Number of volume buckets in an average volume day (Vbucket=50)
VPIN=function(stock,Vbucket=50, period="seconds") {
    stock$dP1=diff(stock[,'Price'],lag=1,diff=1,na.pad=TRUE)
    ends=endpoints(stock,period)
    timeDF=period.apply(stock[,'dP1'],INDEX=ends,FUN=sum)
    timeDF$Volume=period.apply(stock[,'Volume'],INDEX=ends,FUN=sum)
    Vbar=mean(period.apply(timeDF[,'Volume'],INDEX=endpoints(timeDF,'hours'),
                           FUN=sum))/Vbucket
    timeDF$Vfrac=timeDF[,'Volume']/Vbar
    timeDF$CumVfrac=cumsum(timeDF[,'Vfrac'])
    timeDF$Next=(timeDF[,'CumVfrac']-floor(timeDF[,'CumVfrac']))/timeDF[,'Vfrac']
    timeDF[timeDF[,'Next']<1,'Next']=0
    timeDF$Previous=lag(timeDF[,'dP1'])*lag(timeDF[,'Next'])
    timeDF$dP2=(1-timeDF[,'Next'])*timeDF[,'dP1'] + timeDF[,'Previous']
    timeDF$Vtick=floor(timeDF[,'CumVfrac'])
    timeDF[,'Vtick']=timeDF[,'Vtick']-diff(timeDF[,'Vtick']); timeDF[1,'Vtick']=0
    timeDF=as.data.frame(timeDF); timeDF[,'DateTime']=row.names(timeDF)
    
    timeDF=ddply(as.data.frame(timeDF),.(Vtick),last)
    
    timeDF=as.xts(timeDF[,c('Volume','dP2','Vtick')],
                  order.by=fastPOSIXct(timeDF$DateTime,tz='GMT'))
    timeDF[1,'dP2']=0
    
    timeDF$sigma=rollapply(timeDF[,'dP2'],Vbucket,FUN=sd,fill=NA)
    
    timeDF$sigma=na.fill(timeDF$sigma,"extend")
    timeDF$Vbuy=Vbar*pnorm(timeDF[,'dP2']/timeDF[,'sigma'])
    timeDF$Vsell=Vbar-timeDF[,'Vbuy']
    timeDF$OI=abs(timeDF[,'Vsell']-timeDF[,'Vbuy'])
    timeDF$VPIN=rollapply(timeDF[,'OI'],Vbucket,sum)/(Vbar*Vbucket)
    timeDF=timeDF[,c('VPIN')]
    return(timeDF)
}

fname<-"g:/TRADE/Data/research/"
setwd(fname)
fname<-c("tickorderbookSI07072015.RData")
load(fname) 

if(!is.numeric(df[,2]))
    for(i in 2 :ncol(df))
        df[,i]<-as.numeric(levels(df[,i]))[df[,i]]

#Discrete data
dfts<-xts(x=df[,-1],order.by=df$datetime, unique=FALSE)
dfts<-dfts[!is.na(index(dfts))]


dfts<-dfts[, c("price", "volume")]
colnames(dfts)<-c("Price", "Volume")
out=VPIN(dfts,50)

vv<-merge(dfts, out)
vv$VPIN<-na.fill(vv$VPIN,"extend")
dfsec<-endpoints(dfts, "seconds")
vvsec<-vv[dfsec,]
vvsec$delta<-diff(vvsec$Price)

qplot(x=VPIN, y=abs(delta), data=vvsec, color=VPIN)


