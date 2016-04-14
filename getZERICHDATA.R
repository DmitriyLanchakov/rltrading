#''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
#' Скачивание архивов полного ордерорга по самым ликвидным фьючерсам 
#' 2016-03-24 | rlukerin@gmail.com
#''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

#install.packages("RCurl")
library(RCurl)

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


getMOEXDATA<-function(date){
  urlMOEX<-paste("ftp://athistory.zerich.com/",date,"/",sep="")
  if(url.exists(urlMOEX))
  {

    homeDir<-"~/repos/Data/QSH/"
    setwd(homeDir)
    dir.create(paste(date,sep=""))
    setwd(paste(homeDir,date,sep=""))
    
    fileNames<-getURL(url = urlMOEX,ftp.use.epsv = FALSE,dirlistonly = TRUE)
    fileNames<-strsplit(fileNames,"\r\n")[[1]]
    
    lapply(fileNames,
           FUN=function(x)bdown(paste(urlMOEX,x,sep=""),x))
    
  }
}
startDate<-as.Date("2016-03-05")
startDate<-as.Date("2016-03-24")
endDate<-Sys.Date()

lapply(seq.Date(from=startDate,to=endDate, by=1),getMOEXDATA)