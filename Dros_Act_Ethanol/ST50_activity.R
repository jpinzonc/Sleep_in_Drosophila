# Clean up R environment - This is not necesary, but I like to do it. 
rm(list = ls()) 

# Load require libraries
library(dplyr)
library(zoo)
library(ggplot2)
library(cpm)

# Set Working directory
setwd("/Users/jpinzon/Desktop/git/Dros_Act_Ethanol")
dir("/Users/jpinzon/Desktop/git/Dros_Act_Ethanol")
setwd("/Users/jpinzon/Desktop/git/Dros_Act_Ethanol/60610")
dir()
getwd()


data_Set = function(path){
  setwd(path)
  file_list <- list.files(path)
  for (file in file_list){
    # if the merged dataset does exist, append to it
    if (exists("dataset")){
      temp_dataset <-read.csv(file, sep='\t', header=F)
      cols1=c(11:13,15:17)
      temp_dataset=temp_dataset[,cols1]  
      names(temp_dataset) <- paste(names(temp_dataset),substr(file,8,9), sep = "")
      dataset<-cbind(dataset, temp_dataset)
      rm(temp_dataset)
    }
    # if the merged dataset doesn't exist, create it
    if (!exists("dataset")){
      dataset=read.csv(file, sep='\t', header=F)
      cols=c(1:3,11:13,15:17)
      dataset=dataset[,cols]
      names(dataset) <- c(names(dataset[1:3]),paste(names(dataset)[4:9],substr(file,8,9),  sep = ""))
    }
  }
  dataset
}


tras_df=function(data_Set, a, t){
  if(missing(t)) {t=0} else {t=t}
  if (a == "A"){#Air in first three
    data_al=data_Set%>%mutate(OneAir=V1126+V1127+V1128, TwoAir=V1226+V1227+V1228, ThreeAir=V1326+V1327+V1328,
                             OneE=V1526+V1527+V1528, TwoE=V1626+V1627+V1628, ThreeE=V1726+V1727+V1728, date=paste(V2,V3,sep="-"))%>%
      select(V1, date, OneAir, TwoAir, ThreeAir, OneE, TwoE, ThreeE)}
  if (a == "E"){#Alcohol in first three
    data_al=data_Set%>%mutate(OneE=V1126+V1127+V1128, TwoE=V1226+V1227+V1228, ThreeE=V1326+V1327+V1328,
                             OneAir=V1526+V1527+V1528, TwoAir=V1626+V1627+V1628, ThreeAir=V1726+V1727+V1728, date=paste(V2,V3,sep="-"))%>%
      select(V1, date, OneAir, TwoAir, ThreeAir, OneE, TwoE, ThreeE)}
  data_al$date=as.POSIXct(data_al$date,format="%d %b %y-%H:%M:%OS")
  data_al=data_al%>%mutate(ttime=difftime(date, date[1], units=c("mins")))%>%filter(ttime>t)%>%mutate(time=ttime-ttime[1])
  data_al=data_al%>%mutate(Air=(OneAir+TwoAir+ThreeAir)/3, Eth=(OneE+TwoE+ThreeE)/3)
  data_al=data_al%>%mutate(rm_e=rollmean(data_al$Eth , 10, align = "right", fill = NA, na.pad=T), rm_a=rollmean(data_al$Air , 10, align = "right", fill = NA, na.pad=T))
  data_al
}


a=(tras_df(data_Set("/Users/jpinzon/Desktop/git/Dros_Act_Ethanol/60610"), "A", 2))
b=(tras_df(data_Set("/Users/jpinzon/Desktop/git/Dros_Act_Ethanol/60624"), "A", 18))
head(b)
every_5a=na.omit(a%>%filter(as.numeric(time)%%5==0))
every_5b=na.omit(b%>%filter(as.numeric(time)%%5==0))


plot(a$time, a$Eth, col="blue", lwd=0.5, cex=0.5,type="l", xlab="Time (mins)", ylab="Activity (mean No. of crosses)")
lines(a$time, a$Air,col="red", cex=0.5, lwd=0.5, lty=1)
lines(b$time, b$Eth,col="green", cex=0.5, lwd=0.5 )
lines(b$time, b$Air,col="brown", cex=0.5, lwd=0.5, lty=3)

lines(every_5a$time, every_5a$rm_e, col="red")
lines(every_5a$time, every_5a$rm_a,col="black")
lines(every_5b$time, every_5b$rm_e, col="red")
lines(every_5b$time, every_5b$rm_a,col="black")

legend(40,20, c("140:10","80:70", "Air1","Air2"), bty = "n",cex=1, pt.cex = 0.4,inset=10,text.width=5,
       lty=c(1,1,1,1), # gives the legend appropriate symbols (lines)
       lwd=c(1,1), col=c("blue","green", "red", "brown" ))

abline(v=as.numeric(min((a%>%filter(time>10 & Eth==0))$time)/2), col="orange")
text(as.numeric(min((a%>%filter(time>2 & Eth==0))$time)/2)+4, 20, 
     paste("ST50", as.numeric(min((a%>%filter(time>2 & Eth==0))$time)/2), sep=" ~"),
     col = "orange", cex=0.5) 

min(b$Eth)
abline(v=as.numeric(min((b%>%filter(time>0 & Eth<1))$time)/2), col="yellow")
text(as.numeric(min((b%>%filter(time>0 & Eth<0.76))$time)/2)+4, 20, 
     paste("ST50", as.numeric(min((b%>%filter(time>0 & Eth<.76))$time)/2), sep=" ~"),
     col = "orange", cex=0.5) 



x=as.numeric(a$time)
y=a$Eth

lo <- loess(y~x)
plot(x,y, type="l")
xl <- seq(min(x),max(x), (max(x) - min(x))/1000)
lines(xl, predict(lo,xl), col='red', lwd=2)

scatter.smooth(x, y)


qplot(x,y, geom='smooth', span =0.5)


detectChangePointBatch(every_5$rm_e, "Student", lambda=NA, alpha=0.005)






every_5=na.omit(data_al%>%filter(as.numeric(time)%%5==0))
