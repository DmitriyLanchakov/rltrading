library("rusquant")
source('G:/TRADE/R/rltrading/sandBox/STATARB.R', echo=TRUE)
from<-as.Date("2015-01-20")
to<-as.Date("2015-01-26")
numdays=as.numeric(to-from)
period="1min"
#TESTING PAIRS


#SRH5 (03.2015)
#SPH5 (03.2015)
#SiH (03.2015)



symbols<-c("SiH5 (03.2015)","USDRUB_TOM")
StatArb(symbols, startDate, numdays, period)

symbols<-c("SRH5 (03.2015)","SPH5 (03.2015)")
StatArb(symbols, startDate, numdays, period)

symbols<-c("SiH5 (03.2015)","SPH5 (03.2015)")
StatArb(symbols, startDate, numdays, period)
