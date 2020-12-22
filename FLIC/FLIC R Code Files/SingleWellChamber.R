## Private Class Methods for FLIC Class V2
source("DFM.R")
source("ParametersClass.R")
source("CommonChamber.R")
require(stats)


BinFeedingData.Well.Licks<-function(dfm,well,binsize.min,range=c(0,0)){
  tmp<-FeedingData.Licks(dfm,range)
  cname=paste("W",well,sep="")
  
  tmp<-tmp[,c("Minutes",cname)]
  
  m.min<-min(tmp$Minutes)
  m.max<-max(tmp$Minutes)
  
  y<-seq(m.min,m.max,by=binsize.min)
  if(y[length(y)]<m.max)
    y<-c(y,m.max)
  
  z<-cut(tmp$Minutes,y,include.lowest=TRUE)
  
  r.min<-aggregate(tmp$Minutes~z,FUN=mean)
  r.A<-aggregate(tmp[,cname]~z,FUN=sum)
  
  results<-data.frame(r.min,r.A[,2])
  names(results)<-c("Interval","Min","SumLicks")
  results
}

BinFeedingData.Licks<-function(dfm,binsize.min,range=c(0,0)){
  result<-BinFeedingData.Well.Licks(dfm,1,binsize.min,range)

  for(i in 2:12) {
    cname=paste("W",i,sep="")
    tmp<-BinFeedingData.Well.Licks(dfm,i,binsize.min,range)
    result<-data.frame(result,tmp$SumLicks)
  }
  names(result)<-c("Interval","Min",paste("W",1:12,sep=""))
  result  
}

BinFeedingData.Well.Events<-function(dfm,well,binsize.min,range=c(0,0)){
  tmp<-FeedingData.Events(dfm,range)
  cname=paste("W",well,sep="")
  
  tmp<-tmp[,c("Minutes",cname)]
  ## Remember that Event data include duration, but we aren't interested
  ## in that.  Set values >0 to 1.
  tmp[tmp[,cname]>1,cname]<-1
  
  m.min<-min(tmp$Minutes)
  m.max<-max(tmp$Minutes)
  
  y<-seq(m.min,m.max,by=binsize.min)
  if(y[length(y)]<m.max)
    y<-c(y,m.max)
  
  z<-cut(tmp$Minutes,y,include.lowest=TRUE)
  
  r.min<-aggregate(tmp$Minutes~z,FUN=mean)
  r.A<-aggregate(tmp[,cname]~z,FUN=sum)
  
  results<-data.frame(r.min,r.A[,2])
  names(results)<-c("Interval","Min","SumEvents")
  results
}

BinFeedingData.Events<-function(dfm,binsize.min,range=c(0,0)){
  result<-BinFeedingData.Well.Events(dfm,1,binsize.min,range)
  
  for(i in 2:12) {
    cname=paste("W",i,sep="")
    tmp<-BinFeedingData.Well.Licks(dfm,i,binsize.min,range)
    result<-data.frame(result,tmp$SumLicks)
  }
  names(result)<-c("Interval","Min",paste("W",1:12,sep=""))
  result  
}

OutputBinnedFeeding.Monitors<-function(monitors,parameters,binsize.min,range=c(0,0)){
  for(j in monitors){
    ##print(paste("Outputting Interval Data for DFM ",j,".",sep=""))
    ##flush.console()
    monitor<-j
    dfm<-DFMClass(monitor,parameters)
    tmp2<-BinFeedingData.Licks(dfm,binsize.min,range)
    if(j==monitors[1]){
      result<-tmp2
    }
    else {
      result<-rbind(result,tmp2)
    }
  }
  filename<-paste("BinnedFeeding_DFM",monitors[1],"_",monitors[length(monitors)],".csv",sep="") 
  write.csv(result,file=filename,row.names=FALSE)  
}
OutputBinnedEvents.Monitors<-function(monitors,parameters,binsize.min,range=c(0,0)){
  for(j in monitors){
    ##print(paste("Outputting Interval Data for DFM ",j,".",sep=""))
    ##flush.console()
    monitor<-j
    dfm<-DFMClass(monitor,parameters)
    tmp2<-BinFeedingData.Events(dfm,binsize.min,range)
    if(j==monitors[1]){
      result<-tmp2
    }
    else {
      result<-rbind(result,tmp2)
    }
  }
  filename<-paste("BinnedEvents_DFM",monitors[1],"_",monitors[length(monitors)],".csv",sep="") 
  write.csv(result,file=filename,row.names=FALSE)  
}

RawDataPlot.SingleWell<-function(dfm,chamber,range=c(0,0)){ 
  if(dfm$Parameters$Chamber.Size!=1)
    stop("This function is for one-chamber DFM only")  
  rd<-RawData(dfm,range)
  wellA<-dfm$Parameters$Chamber.Sets[chamber,1]
  wellB<-dfm$Parameters$Chamber.Sets[chamber+1,1] 
  
  nameA<-paste("W",wellA,sep="")
  nameB<-paste("W",wellB,sep="")
  
  dA<-rd[,nameA]
  dB<-rd[,nameB]
  ymax<-max(c(dA,dB))
  ymin<-min(c(dA,dB))
  plot(rd$Minutes,dA,xlab = "Minutes", ylab="Signal",ylim=c(ymin,ymax),type="l")
  lines(rd$Minutes,dB,col=2)  
  title(paste("DFM:",dfm$ID," W:",wellA,"&",wellB,"- Raw Data"))
}

BaselineDataPlot.SingleWell<-function(dfm,chamber,range=c(0,0)){ 
  if(dfm$Parameters$Chamber.Size!=1)
    stop("This function is for one-chamber DFM only")
  
  rd<-BaselinedData(dfm,range)
  wellA<-dfm$Parameters$Chamber.Sets[chamber,1]
  wellB<-dfm$Parameters$Chamber.Sets[chamber+1,1] 
  
  nameA<-paste("W",wellA,sep="")
  nameB<-paste("W",wellB,sep="")
  
  dA<-rd[,nameA]
  dB<-rd[,nameB]
  ymax<-max(c(dA,dB))
  ymin<-min(c(dA,dB))
  plot(rd$Minutes,dA,xlab = "Minutes", ylab="Signal",ylim=c(ymin,ymax),type="l")
  lines(rd$Minutes,dB,col=2)  
  title(paste("DFM:",dfm$ID," W:",wellA,"&",wellB,"- Baselined Data"))
}

Feeding.ThresholdPlots.SingleWell<-function(dfm,chamber,range=c(0,0)){ 
  if(dfm$Parameters$Chamber.Size!=1)
    stop("This function is for one-chamber DFM only")
  
  rd<-BaselinedData(dfm)
  wellA<-dfm$Parameters$Chamber.Sets[chamber,1]
  wellB<-dfm$Parameters$Chamber.Sets[chamber+1,1] 
  
  nameA<-paste("W",wellA,sep="")
  nameB<-paste("W",wellB,sep="")
  
  dA<-rd[,nameA]
  dB<-rd[,nameB]
  
  tA<-dfm$Thresholds[[nameA]]
  tB<-dfm$Thresholds[[nameB]]
  
  x<-rd$Minutes
  
  if(sum(range)!=0) {
    tA<-tA[rd$Minutes>range[1] & rd$Minutes<range[2],]
    tB<-tB[rd$Minutes>range[1] & rd$Minutes<range[2],]
    dA<-dA[rd$Minutes>range[1] & rd$Minutes<range[2]]
    dB<-dB[rd$Minutes>range[1] & rd$Minutes<range[2]]
    x<-x[x>range[1] & x<range[2]]
  }
  
  newData<-data.frame(x,dA,tA$FeedingMin,tA$FeedingMax)
  names(newData)<-c("x","a","ta","tb")
  
  
  
  ymax<-max(c(newData$a))
  ymin<-min(c(newData$a))
  plot(newData$x,newData$a,xlab = "Minutes", ylab="Signal",ylim=c(ymin,ymax),type="l")
  lines(newData$x,newData$ta,col=2,lty=2)    
  lines(newData$x,newData$tb,col=2,lty=2)  
  title(paste("DFM:",dfm$ID," Well:",wellA))
  
  
  newData<-data.frame(x,dB,tB$FeedingMin,tB$FeedingMax)
  names(newData)<-c("x","b","ta","tb")
  
  if(sum(range)!=0) {
    newData<- newData[(newData$x>range[1]) & (newData$x<range[2]),]
  }
  
  ymax<-max(c(newData$b))
  ymin<-min(c(newData$b))
  plot(newData$x,newData$b,xlab = "Minutes", ylab="Signal",ylim=c(ymin,ymax),type="l")
  lines(newData$x,newData$tb,col=2,lty=2)  
  lines(newData$x,newData$ta,col=2,lty=2)  
  title(paste("DFM:",dfm$ID," Well:",wellB))
}