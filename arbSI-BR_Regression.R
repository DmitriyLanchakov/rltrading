#''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
#' Зависимость между ценами  на нефть и доллар/рубль
#' 
#' 2016-04-07 | rlukerin@gmail.com
#' 
#''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

library(rusquant)
library(ggplot2)
require(scales)
symbols<-c("@Si",
           "@BR"
           )

from<-"2016-01-01"
to<-Sys.Date()
period<-"day" # "1min"
for(s in symbols)
    getSymbols(s, from=from, to=to, period=period, src='mfd',adjust=TRUE, auto.assign=TRUE)

symbols<-toupper(symbols)

df<-merge.xts(Cl(get(symbols[1])),Cl(get(symbols[2])),Vo(get(symbols[2])))
colnames(df)<-c("USDRUB", "OIL", "VOLUSDRUB")
yearmonth<-as.numeric(format(index(df), "%y%m"))
df$USDRUB<-df$USDRUB/1000
qplot(OIL, USDRUB, 
      alpha=0.5,color=factor(yearmonth),data=df,
      #facets= weekNum~.,
      geom=c("point", "smooth"),
      main="USDRUB / OIL", 
      xlab="OIL", ylab="USDRUB") + scale_x_continuous(breaks=pretty_breaks(n=20)) + scale_y_continuous(breaks=pretty_breaks(n=20))

summary(lm(USDRUB~factor(yearmonth)+(OIL), df))
