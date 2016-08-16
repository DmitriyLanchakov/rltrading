#Orderbook animation
oopt <- animation::ani.options(interval = 0.1)

obrderbookFrame<-function(datadf, start, timeframe=10){
    downlimit<-start
    uplimit<-start+timeframe
    if(nrow(filter(datadf,datetime>downlimit & datetime<uplimit))>0)
        print(orderbookggplot(filter(datadf,datetime>downlimit & datetime<uplimit)))
    
}

orderbookAll <- function(datadf, from, to, i=1) {
    #datadf<-dplyr::filter(datadf,datetime>from & datetime<to) 
    lapply(seq(from, to, by = i), function(x) {
        obrderbookFrame(datadf,x)        
    })
}


# Density VolDelta with price direction
histFrame<-function(obdf,bidaskspreadLimit=10, voldeltaLow=50, voldeltaUp=500, priceLag=50){    
    obdf$pricenext<-lag(obdf$price, priceLag)
    obdf<-mutate(obdf,
                 pricediff=pricenext-price,
                 direction=factor(sign(pricediff),levels=c(-1,0,1),
                                  labels=c("down", "flat","up"))
    )
    print(qplot(voldelta,
                color=direction,
                fill=direction, 
                alpha=I(0.7), 
                stat="bin",
                binwidth = 10,
                facets = bidaskspread~direction,
                data=filter(obdf, 
                            bidaskspread<=bidaskspreadLimit, 
                            abs(voldelta)>=voldeltaLow, 
                            abs(voldelta)<=voldeltaUp))+
              geom_vline(xintercept = voldeltaLow,alpha =I(0.7))+
              geom_vline(xintercept = -voldeltaLow,alpha =I(0.7))+
              ggtitle(paste(paste(min(obdf$datetime), max(obdf$datetime), sep=" :: "),
                            paste("VolDelta threshold",voldeltaLow, sep=" : "),
                            paste("BidAskSpread threshold",bidaskspreadLimit, sep=" : "),
                            paste("Price lag",priceLag, sep=" : "),
                            sep="\n"))
          
    )
}

histVolDelta<-function(obdf, i=1, threshold=300){
    lapply(seq(0, threshold, by = i), function(x) {
        histFrame(obdf, priceLag=50, voldeltaLow=x,voldeltaUp=threshold)        
    }
    )
}

histPriceLag<-function(obdf, i=1, threshold=300){
    lapply(seq(0, threshold, by = i), function(x) {
        histFrame(obdf, priceLag=x, voldeltaLow=50,voldeltaUp=threshold)        
    }
    )
}

plotAllStrategies<-function(TT, SS){
    if(length(TT)>1 && length(SS)==1)
        lapply(TT, function(x)
            PlotStrategies(x,SS))
    if(length(SS)>1 && length(TT)==1)
        lapply(TT, function(x)
            PlotStrategies(TT,x))
}
