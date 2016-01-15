library(rusquant)
library(ggplot2)
library(data.table)
library(grid)

# Calandar Spread (FRA) tests
plotSpread<-function(symbs,
                     from="2015-11-01",
                     to=Sys.Date(),
                     period="1min",
                     abs=TRUE){
    for (symb in symbs[1:2])
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
               format(DateTimeSymb, "%y%m%d"),
               y=spread, 
               data=spreadDT, 
               alpha=0.05,
               geom = c("violin", "jitter"), 
               #geom=c("point", "smooth"),
               xlab="Dates",
               ylab="",
               color=Volume,
               main=paste(paste(symbs[1:2],collapse = "-"),
                          paste(paste(names(summary(spreadDT$spread)),
                                      summary(spreadDT$spread), 
                                      sep="="),
                                collapse=" / "),
                          sep="\n"))+theme(legend.position = "none")
    return(qpi)
    
    
    # 
    # qplot(DateTimeSymb,(1-Close/i.Close)*100,data=leftLeg[rightLeg],
    #       color=i.Volume)
}

# symbs<-data.frame(c("SiZ4 (12.2014)","SiH5 (03.2015)",1),
#                   c("RIZ4 (12.2014)","RIH5 (03.2015)",1),
#                   c("MXZ4 (12.2014)","MXH5 (03.2015)",1),
#                   c("SRZ4 (12.2014)","SRH5 (03.2015)",1),
#                   c("GZZ4 (12.2014)","GZH5 (03.2015)",1),
#                   c("LKZ4 (12.2014)","LKH5 (03.2015)",1),
#                   c("RNZ4 (12.2014)","RNH5 (03.2015)",1),
#                   c("VBZ4 (12.2014)","VBH5 (03.2015)",1),
#                   #c("MMZ4 (12.2014)","MMH5 (03.2015)",1),
#                   
#                   #c("MMZ5 (12.2015)","MXZ5 (12.2015)",100),
#                   stringsAsFactors = FALSE)
# 
# qp<-lapply(symbs,plotSpread,from="2014-09-14", to="2014-12-16",  abs=TRUE)
# pushViewport(viewport(layout = grid.layout(length(qp), 1)))
# for(i in 1:length(qp))
#     print(qp[[i]], vp = viewport(layout.pos.row = i, layout.pos.col = 1))
# dev.copy(device=png, filename = "arbcalendar1214-0315.png", 
#          width = 1200, height = 2400) 
# dev.off()
# rm(qp)
# 
# symbs<-data.frame(c("SiH5 (03.2015)","SiM5 (06.2015)",1),
#                   c("RIH5 (03.2015)","RIM5 (06.2015)",1),
#                   c("MXH5 (03.2015)","MXM5 (06.2015)",1),
#                   c("SRH5 (03.2015)","SRM5 (06.2015)",1),
#                   c("GZH5 (03.2015)","GZM5 (06.2015)",1),
#                   c("LKH5 (03.2015)","LKM5 (06.2015)",1),
#                   c("RNH5 (03.2015)","RNM5 (06.2015)",1),
#                   c("VBH5 (03.2015)","VBM5 (06.2015)",1),
#                   #c("MMZ4 (12.2014)","MMH5 (03.2015)",1),
#                   
#                   #c("MMZ5 (12.2015)","MXZ5 (12.2015)",100),
#                   stringsAsFactors = FALSE)
# 
# qp<-lapply(symbs,plotSpread,from="2014-12-14", to="2015-03-16",  abs=TRUE)
# pushViewport(viewport(layout = grid.layout(length(qp), 1)))
# for(i in 1:length(qp))
#     print(qp[[i]], vp = viewport(layout.pos.row = i, layout.pos.col = 1))
# dev.copy(device=png, filename = "arbcalendar0315-0615.png", 
#          width = 1200, height = 2400) 
# dev.off()
# rm(qp)
# 
# 
# symbs<-data.frame(c("SiM5 (06.2015)","SiU5 (09.2015)",1),
#                   c("RIM5 (06.2015)","RIU5 (09.2015)",1),
#                   c("MXM5 (06.2015)","MXU5 (09.2015)",1),
#                   c("SRM5 (06.2015)","SRU5 (09.2015)",1),
#                   c("GZM5 (06.2015)","GZU5 (09.2015)",1),
#                   c("LKM5 (06.2015)","LKU5 (09.2015)",1),
#                   c("RNM5 (06.2015)","RNU5 (09.2015)",1),
#                   c("VBM5 (06.2015)","VBU5 (09.2015)",1),
#                   #c("MMZ4 (12.2014)","MMH5 (03.2015)",1),
#                   
#                   #c("MMZ5 (12.2015)","MXZ5 (12.2015)",100),
#                   stringsAsFactors = FALSE)
# 
# qp<-lapply(symbs,plotSpread,from="2015-03-14", to="2015-06-16",  abs=TRUE)
# pushViewport(viewport(layout = grid.layout(length(qp), 1)))
# for(i in 1:length(qp))
#     print(qp[[i]], vp = viewport(layout.pos.row = i, layout.pos.col = 1))
# dev.copy(device=png, filename = "arbcalendar0615-0915.png", 
#          width = 1200, height = 2400) 
# dev.off()
# rm(qp)
# 
# symbs<-data.frame(c("SiU5 (09.2015)","SiZ5 (12.2015)",1),
#                   c("RIU5 (09.2015)","RIZ5 (12.2015)",1),
#                   c("MXU5 (09.2015)","MXZ5 (12.2015)",1),
#                   c("SRU5 (09.2015)","SRZ5 (12.2015)",1),
#                   c("GZU5 (09.2015)","GZZ5 (12.2015)",1),
#                   c("LKU5 (09.2015)","LKZ5 (12.2015)",1),
#                   c("RNU5 (09.2015)","RNZ5 (12.2015)",1),
#                   c("VBU5 (09.2015)","VBZ5 (12.2015)",1),
#                   #c("MMZ4 (12.2014)","MMH5 (03.2015)",1),
#                   
#                   #c("MMZ5 (12.2015)","MXZ5 (12.2015)",100),
#                   stringsAsFactors = FALSE)
# 
# qp<-lapply(symbs,plotSpread,from="2015-03-14", to="2015-09-16",  abs=TRUE)
# pushViewport(viewport(layout = grid.layout(length(qp), 1)))
# for(i in 1:length(qp))
#     print(qp[[i]], vp = viewport(layout.pos.row = i, layout.pos.col = 1))
# dev.copy(device=png, filename = "arbcalendar0915-1215.png", 
#          width = 1200, height = 2400) 
# dev.off()
# rm(qp)


# symbs<-data.frame(c("SiZ5 (12.2015)","SiH6 (03.2016)",1),
#                   c("RIZ5 (12.2015)","RIH6 (03.2016)",1),
#                   c("MXZ5 (12.2015)","MXH6 (03.2016)",1),
#                   c("SRZ5 (12.2015)","SRH6 (03.2016)",1),
#                   c("GZZ5 (12.2015)","GZH6 (03.2016)",1),
#                   c("LKZ5 (12.2015)","LKH6 (03.2016)",1),
#                   c("RNZ5 (12.2015)","RNH6 (03.2016)",1),
#                   c("VBZ5 (12.2015)","VBH6 (03.2016)",1),
#                   c("MMZ5 (12.2015)","MMH6 (03.2016)",1),
#                   
#                   #c("MMZ5 (12.2015)","MXZ5 (12.2015)",100),
#                   stringsAsFactors = FALSE)
# 
# qp<-lapply(symbs,plotSpread,from="2015-09-14", to="2015-12-15",  abs=FALSE)
# pushViewport(viewport(layout = grid.layout(length(qp), 1)))
# for(i in 1:length(qp))
#     print(qp[[i]], vp = viewport(layout.pos.row = i, layout.pos.col = 1))
# dev.copy(device=png, filename = "arbcalendar1215-0316.png", 
#          width = 1200, height = 2400) 
# dev.off()
# rm(qp)


# symbs<-data.frame(c("SiH6 (03.2016)","SiM6 (06.2016)",1),
#                   c("RIH6 (03.2016)","RIM6 (06.2016)",1),
#                   #c("MXH6 (03.2016)","MXM6 (06.2016)",1),
#                   c("SRH6 (03.2016)","SRM6 (06.2016)",1),
#                   c("GZH6 (03.2016)","GZM6 (06.2016)",1),
#                   #c("LKH6 (03.2016)","LKM6 (06.2016)",1),
#                   #c("RNH6 (03.2016)","RNM6 (06.2016)",1),
#                   #c("VBH6 (03.2016)","VBM6 (06.2016)",1),
#                   #c("MMH6 (03.2016)","MMM6 (06.2016)",1),
#                   
#                   #c("MMZ5 (12.2015)","MXZ5 (12.2015)",100),
#                   stringsAsFactors = FALSE)
# 
# qp<-lapply(symbs,plotSpread,from="2015-11-01", to="2015-12-15",  abs=FALSE)
# pushViewport(viewport(layout = grid.layout(length(qp), 1)))
# for(i in 1:length(qp))
#     print(qp[[i]], vp = viewport(layout.pos.row = i, layout.pos.col = 1))
# dev.copy(device=png, filename = "arbcalendar0316-0616.png", 
#          width = 1200, height = 2400) 
# dev.off()
rm(qp)

symbs<-data.frame(c("SiH6 (03.2016)","SiM6 (06.2016)",1),
                  c("RIH6 (03.2016)","RIM6 (06.2016)",1),
                  c("MXH6 (03.2016)","MXM6 (06.2016)",1),
                  c("Сбербанк","SRH6 (03.2016)",100),
                  c("SRH6 (03.2016)","SRM6 (06.2016)",1),
                  c("GZH6 (03.2016)","GZM6 (06.2016)",1),
                  #c("LKH6 (03.2016)","LKM6 (06.2016)",1),
                  #c("RNH6 (03.2016)","RNM6 (06.2016)",1),
                  #c("VBH6 (03.2016)","VBM6 (06.2016)",1),
                  #c("MMH6 (03.2016)","MMM6 (06.2016)",1),
                  
                  #c("MMZ5 (12.2015)","MXZ5 (12.2015)",100),
                  stringsAsFactors = FALSE)

qp<-lapply(symbs,plotSpread,from="2016-01-01", to="2016-01-24",  abs=FALSE)
pushViewport(viewport(layout = grid.layout(length(qp), 1)))
for(i in 1:length(qp))
    print(qp[[i]], vp = viewport(layout.pos.row = i, layout.pos.col = 1))
dev.copy(device=png, filename = "arbcalendar0316-0616.png", 
         width = 1200, height = 2400) 
dev.off()
rm(qp)
