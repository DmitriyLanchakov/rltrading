library(rusquant)
library(dplyr)
library(ggplot2)
library(tidyr)

source('G:/TRADE/R/rltrading/sandBox/spread.R')
# 
# from<-as.Date("2015-01-26")
# to<-as.Date("2015-01-31")
# period="1min"
# lot=1000

# fname<-"g:/TRADE/SAZAN/ru.sazan.trader.smartcom.quote/ru.sazan.trader.smartcom.quote/bin/Debug/Bar.csv"
# quoteData<-read.csv(fname, header=TRUE)
# 
# symbols<-names(table(quoteData$Symbol))
# 
# assign(symbols[1],xts(x=quoteData[quoteData$Symbol==symbols[1],
#                                  c("Open","High", "Low", "Close", "Volume")],
#                       order.by=as.POSIXct(strptime(quoteData[quoteData$Symbol==symbols[1],
#                                                             "DateTime"],"%d/%m/%Y %H:%M:%S")))) 
# assign(symbols[2],xts(x=quoteData[quoteData$Symbol==symbols[2],
#                                c("Open","High", "Low", "Close", "Volume")],
#                       order.by=as.POSIXct(strptime(quoteData[quoteData$Symbol==symbols[2],
#                                                             "DateTime"],"%d/%m/%Y %H:%M:%S")))) 
# rm(quoteData)

from<-as.Date("2015-04-30")
to<-as.Date("2015-04-30")

period<-"1min"
lot<-1/1000

symbols<-c("USDRUB_TOM",
    "SiM5 (06.2015)"       
    #"SiU5 (09.2015)"
           #"USDRUB_TOD"
           )

dateExp<-c(as.POSIXct("2015-05-04 12:30:00"),
           as.POSIXct("2015-06-15 12:30:00"))
rate<-0.115

options(digits.secs=0)
for(s in symbols)
    getSymbols(s, from=from, to=to, period=period, src='mfd',adjust=TRUE, auto.assign=TRUE)
symbols<-toupper(symbols)





df<-getRateSpread(symbols = symbols, rate,dateExp,lot)
tail(df[,c("datetime","fut", "asset", "spread")])

#df<-getStatSpread(symbols, rate,dateExp,lot)

#df<-getStatSpread(symbols, smaPeriod=14)


df2<-df %>% 
    select(datetime, fut, futfair) %>%
    gather(symbol, price, c(fut, futfair))
#lm(fut~stock, data=df)

library(grid)
qp<-list()
i<-0
# i<-i+1
# qp[[i]]<-qplot(x=datetime,
#                y=price, 
#                color=symbol, 
#                data=df2, 
#                geom="line")
i<-i+1
qp[[i]]<-qplot(x=datetime,
               y=spread,
               data=df, 
               geom="line",
               colour=futvol,
              # size=futvol,
               alpha=I(0.8))+geom_hline(yintercept=0)
qp[[i]]<-qp[[i]]+geom_line(y=2*df$spreadsd, colour=I(10))#geom_hline(yintercept=sd(df$spread, na.rm=TRUE), colour=I(10))
qp[[i]]<-qp[[i]]+geom_line(y=-2*df$spreadsd, colour=I(10))#geom_hline(yintercept=-sd(df$spread, na.rm=TRUE), colour=I(10))

i<-i+1
qp[[i]]<-qplot(x=factor(format(datetime, "%d/%m-%Hh")),
               y=spread,
               data=df,
               geom="violin")+geom_hline(yintercept=0)
i<-i+1
qp[[i]]<-qplot(x=factor(format(datetime, "%m%d%H%M")),
               y=ratefair,
               data=df,
               geom="violin")+geom_hline(yintercept=0)


pushViewport(viewport(layout = grid.layout(length(qp), 1)))
for(i in 1:length(qp))
    print(qp[[i]], vp = viewport(layout.pos.row = i, layout.pos.col = 1))
rm(qp)

