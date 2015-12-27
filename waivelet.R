library(ggplot2)
excert<-leftLeg[DateTimeSymb>as.POSIXct("2015-09-18")&DateTimeSymb<as.POSIXct("2015-09-19")]
patern<-excert[DateTimeSymb>as.POSIXct("2015-09-18 14:00:00")&
                    DateTimeSymb<as.POSIXct("2015-09-18 15:00:00")]
ggplot()+
    geom_point(aes(DateTimeSymb,Close), data=excert)+
    geom_point(aes(DateTimeSymb,Close), data=patern, color="lightcoral")

ccf(excert$Close, patern$Close, lag=1000)

library(biwavelet)
# Sample time-series
noise1 <- cbind(1:100, rnorm(100))
noise2 <- cbind(1:100, rnorm(100))

dataW<-excert[,.(patern=patern[.I, Close],signal=Close),by=1:nrow(excert)]
dataW[is.na(patern),patern:=0]
# Cross-wavelet
xwt_noise12 <- xwt(dataW[,.(.I,signal)], dataW[,.(.I,patern)])

# Make room to the right for the color bar
par(oma = c(0, 0, 0, 1), mar = c(5, 4, 4, 5) + 0.1)
plot(xwt_noise12, plot.cb = TRUE, plot.phase = TRUE,
     main = "Cross wavelet power and phase difference (arrows)")





###############################################################################
# Load Systematic Investor Toolbox (SIT)
# http://systematicinvestor.wordpress.com/systematic-investor-toolbox/
###############################################################################
source("https://raw.githubusercontent.com/systematicinvestor/SIT/master/R/bt.test.r")

#*****************************************************************
# Load historical data
#****************************************************************** 
library(quantmod)   
tickers = 'SPY'

data = getSymbols(tickers, src = 'yahoo', from = '1950-01-01', auto.assign = F)

#*****************************************************************
# Euclidean distance, one to one mapping
#****************************************************************** 
obj = bt.matching.find(Cl(data), normalize.fn = normalize.mean, dist.fn = 'dist.euclidean', plot=T)

matches = bt.matching.overlay(obj, plot.index=1:90, plot=T)

layout(1:2)
matches = bt.matching.overlay(obj, plot=T, layout=T)
bt.matching.overlay.table(obj, matches, plot=T, layout=T)
