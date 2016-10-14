library(data.table)
fname<-"~/repo/Data/research/SBRF/deals"
fname<-dir(fname)
dealsDT<-fread(fname, stringsAsFactors = F, header=FALSE)

dealsDT[, V3:=gsub("DT=","",V3)]
dealsDT[, V4:=as.numeric(gsub("Price=","",V4))]
dealsDT[, V5:=as.numeric(gsub("Volume=","",V5))]
dealsDT[, V6:=as.numeric(gsub("Fee=$","",V6, fixed=T))]


dealsDT[, V3:=as.POSIXct(strptime(V3,"%d.%m.%Y %H:%M:%OS"))]

# 2016-09-15 19:16:00.000;SiZ6;-2;66152.00000
dealsDT[, Symb:="SRU6"]
write.table(x=dealsDT[,.(V3,Symb, V5, V4, V6)], file=paste("LCHI",fname), sep =";", col.names = F,row.names = F, quote=F )