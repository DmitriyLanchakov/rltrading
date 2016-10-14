library(jsonlite)
library(httr)
library(rusquant)
library(data.table)
getInitPos<-function(traderId=83225, startDate="2016-09-16"){
    resp<-POST(url = "investor.moex.com/ru/statistics/2016/portfolio.aspx/GetPortfolioData", 
               encode = "json", 
               body = list('traderId'=paste0(traderId),'date'=paste0(startDate),'tableId'=6 ), timeout(10))
    stop_for_status(resp)
    #str(resp$headers)
    json<-content(resp, "text")
    validate(json)
    
    initPos <- fromJSON(fromJSON(txt = json)$d)
    initPos$start_pos<-getStartPos(initPos$pos)
    if(!is.null(dim(initPos)))
        initPos
    else getInitPos(82806)[1,] # костыль так как у некоторых пользоватлей таблица пустая совсем
}

getStartPos<-function(ipos){
    sapply(ipos, FUN=function(x){
        res<-as.numeric(strsplit(gsub(")","",gsub(" ","",x)),split="(", fixed=T)[[1]])
        res[is.na(res)]<-0
        res[1]-res[2]
    })
}

resday <- read.table("ftp://ftp.moex.com/pub/info/stats_contest/2016/result_day.csv",
                     header = TRUE
                     ,sep = ";", 
                     dec = ".",
                     quote = "",
                     as.is = TRUE,
                     stringsAsFactors = F,
                     fileEncoding = "CP1251",
                     skipNul= TRUE
)

resday2<-resday[resday$count_deal>0,]

resday2<-unique(resday2[,c("nik", "trader_id", "date_start")])

resday2<-resday2[1:30,]
startTime<-Sys.time()
qntyDF<- do.call(rbind.data.frame,
                 lapply(1:nrow(resday2),
                        FUN=function(i) { 
                            print(i)
                            cbind(resday2[i,],
                                  getInitPos(resday2[i,"trader_id"],
                                             paste(strptime(resday2[i,"date_start"], format = "%d.%m.%Y")))[,c("seccode","start_pos")])}))
Sys.time()-startTime


symbolsMOEX<-loadStockListMoex()
data(tickers)

tickers<-data.table(tickers)
tickers[grepl("Si",V4),]