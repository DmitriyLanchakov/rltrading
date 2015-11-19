library(rusquant)
library(ggplot2)
library(data.table)
library(grid)

# Calandar Spread (FRA) tests
plotSpread<-function(symbs,
                     from="2015-09-16",
                     to=Sys.Date(),
                     period="1min",
                     abs=FALSE){
    for (symb in symbs)
        getSymbols(symb, from=from, to=to, period=period, src='mfd',adjust=TRUE, auto.assign=TRUE)
    
    symbs<-toupper(symbs)
    leftLeg<-data.table(DateTimeSymb=as.POSIXct(index(get(symbs[1]))),
                        as.data.frame(get(symbs[1]), stringsAsFactors=FALSE))
    
    rightLeg<-data.table(DateTimeSymb=as.POSIXct(index(get(symbs[2]))),
                         as.data.frame(get(symbs[2]), stringsAsFactors=FALSE))
    
    setkey(leftLeg,DateTimeSymb)
    setkey(rightLeg,DateTimeSymb)
    
    spreadDT<-leftLeg[rightLeg]
    coefBeta<-as.numeric(symbs[3])
    spreadDT[,spread:=(i.Close/(Close*coefBeta)-1)*100*(!abs) + (i.Close-(Close*coefBeta))*(abs)]
    
    qpi<-qplot(#DateTimeSymb,
               format(DateTimeSymb, "%m%d"),
               y=spread, 
               data=spreadDT, 
               alpha=0.05,
               geom = c("violin", "jitter"), 
               #geom=c("point", "smooth"),
               xlab="Dates",
               ylab="",
               main=paste(paste(symbs[1:2],collapse = "-"),
                          paste(paste(names(summary(spreadDT$spread)),
                                      summary(spreadDT$spread), 
                                      sep="="),
                                collapse=" / "),
                          sep="\n"))
    return(qpi)
    
    
    # 
    # qplot(DateTimeSymb,(1-Close/i.Close)*100,data=leftLeg[rightLeg],
    #       color=i.Volume)
}

symbs<-data.frame(c("SiZ5 (12.2015)","SiH6 (03.2016)",1),
                  c("RIZ5 (12.2015)","RIH6 (03.2016)",1),
                  #c("MXZ5 (12.2015)","MXH6 (03.2016)",1),
                  c("SRZ5 (12.2015)","SRH6 (03.2016)",1),
                  #c("GZZ5 (12.2015)","GZH6 (03.2016)",1),
                  c("MMZ5 (12.2015)","MXZ5 (12.2015)",100),
                  stringsAsFactors = FALSE)

qp<-lapply(symbs,plotSpread,from="2015-11-02",  abs=FALSE)
pushViewport(viewport(layout = grid.layout(length(qp), 1)))
for(i in 1:length(qp))
    print(qp[[i]], vp = viewport(layout.pos.row = i, layout.pos.col = 1))
rm(qp)


