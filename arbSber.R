library(rusquant)
library(urca)
library(ggplot2)
source('G:/TRADE/R/rltrading/sandBox/spread.R', echo=TRUE)

from<-as.Date("2015-02-12")
to<-as.Date("2015-02-13")
period="1min"

symbols<-c("SRH5 (03.2015)",
           #"USDRUB_TOD",
           "Сбербанк")

symbols<-c("SiH5 (03.2015)",
           #"USDRUB_TOD",
           "USDRUB_TOM")

for(s in symbols)
    getSymbols(s, from=from, to=to, period=period, src='mfd',adjust=TRUE, auto.assign=TRUE)
symbols<-toupper(symbols)


dateExp<-as.POSIXct("2015-03-16 12:00:00")
rate<-0.11

df<-getStatSpread(symbols, dateExp = dateExp,rate=rate, ratio=100)
qplot(x=datetime ,y=fairrate, data=df, geom="line")


symbols=cbind(Cl(get(symbols[1])),Cl(get(symbols[2])))
symbA=as.numeric(symbols[,1])
lotB=100
symbB=as.numeric(symbols[,2])*lotB

retA=as.numeric(Delt(symbA,type="log"))
retB=as.numeric(Delt(symbB,type="log"))

test<-ur.kpss(retA)
summary(test)

test<-ur.kpss(retB)
summary(test)

regression <- lm(retA ~ retB)
regression <- lm(symbA ~ symbB)
betaRet <- as.numeric(coef(regression)[1])
spread <- symbA - betaRet*symbB
hist(spread)

test<-ur.kpss(spread)
summary(test)

mu<-mean(spread, na.rm=TRUE)
sigma<-sd(spread, na.rm=TRUE)
x<-seq(mu-5*sigma, mu+5*sigma, length=nrow(spread))

hist(spread, breaks=100, cex.main=0.8, prob=TRUE)
lines(x,y=dnorm(x, mu, sigma))