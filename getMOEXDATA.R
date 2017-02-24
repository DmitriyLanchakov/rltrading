#''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
#' Скачивание архивов сделок по фьючерсам и опционам с сайта Московской биржи
#' С 15-Апреля-2016 открытый доступ к архивам закрыт
#' 2016-03-17 | rlukerin@gmail.com
#''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

#install.packages("RCurl")
library(RCurl)
library(XML)

bdown=function(url, file){
  if(file.exists(file)){
    print(paste(file, "exists"))
    return("File exits")
    
  }
  else{
    f = CFILE(file, mode="wb")
    a = curlPerform(url = url, writedata = f@ref, noprogress=FALSE)
    print(url)
    close(f)
    return(a)
    
  }
}


getMOEXDATA<-function(year){
  homeDir<-"~/repo/Data/MOEX/"
  setwd(homeDir)
  dir.create(paste(year,sep=""))
  setwd(paste(homeDir,year,sep=""))
  #http://ftp.moex.com/pub/info/stats/history/F/2017/
  urlMOEX<-paste("http://ftp.moex.com/pub/info/stats/history/F/",year,"/",sep="")
#  fileNames<-getURL(url = urlMOEX,ftp.use.epsv = FALSE,dirlistonly = TRUE)
#  fileNames<-strsplit(fileNames,"\r*\n")[[1]]
  
  doc <- htmlParse(urlMOEX)
  fileNames <- xpathSApply(doc, "//a/@href")
  free(doc)
  
  fileNames<-sapply(fileNames,FUN=function(x)as.character(strsplit(x,"/")[[1]][8]))
  lapply(fileNames[grepl("FT",toupper(fileNames))],
         FUN=function(x)bdown(paste(urlMOEX,x,sep=""),x))
}

lapply(2016:2016,FUN = getMOEXDATA)




