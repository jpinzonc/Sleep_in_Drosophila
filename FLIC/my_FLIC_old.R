library(dplyr)
library(lubridate)
library(tidyr)
library(grid)
library(gridExtra)
library(ggplot2)
library(gtable)
library(cowplot) 
library(xts)
library(RColorBrewer)
library(scatterplot3d)
library(strucchange)
library(zoo)

read_data_flic = function(file){
  a = read.csv(file, header=T)
  options(digits.secs = 3)
  a$ntime= as.POSIXct(strptime(paste(a$Date,a$Time,a$MSec,sep="."), tz="America/Chicago", format="%m/%d/%Y.%H:%M:%OS"))
  b=as.data.frame(a%>%mutate(position=Sample)%>%dplyr::select(position, ntime, W1,W2,W3,W4,W5,W6,W7,W8,W9,W10,W11,W12))
  b
}

# Average amplitude per columns:
baseLineCal=function(raw_df){
  c=raw_df%>%summarise_each(funs(mean),-ntime,-position)
  # Total average
  cAverage=c%>%mutate(aver=rowMeans(.), Stdev=sd(.))
  baseLine=(cAverage[,13])#-cAverage[,14])
  baseLine
}


# Moving Average df correction for baseline every 5 mins
# 1500 are ~5 mins 
dfm_base_ma<-function(df, threshold){
  df=df%>%mutate_each(funs(replace(., .!=0, (.-(1+rollmean(x = ., 1500, align = "right", fill = NA, na.pad=T))))),-ntime,-position)%>%
    mutate_each(funs(replace(.,.<threshold,0)),-ntime, -position)
  df=na.omit(df)
  df
}

#Data with out baseline (threshold)
dfm_base<-function(df,Ampl){
  df%>%mutate_each(funs(.-Ampl),-ntime, -position)%>%
    mutate_each(funs(replace(.,.<0,0)),-ntime, -position)
}

new_df = function(df,w){ #This function creates the Events
  W=(paste("W",w,sep=""))
  p="position"
  t="ntime"
  rfa=df%>%select_(p,t,W)%>%filter_(paste(W,">",0))%>%arrange_(p)%>%
    mutate(Events = as.numeric(cumsum(c(1, diff(position) !=1))))
  as.data.frame(rfa)
}

# Area under the curve
areaEvent= function(df,w, ns){ #Determine the area of each Event
  if(missing(ns)) {ns=1} else {ns=ns}
  df1 = new_df(df,w)
  W=as.name(colnames(df1[3]))#paste("W",w,sep="")
  area = df1%>%arrange(ntime)%>%group_by(Events) %>%
    filter(n()>ns)%>%summarise(area=sum(diff(as.numeric(ntime))*rollmean(W,2)))
  area
}

# Maximum amplitude
amplitudeEvent=function(df,w, ns){ # determine the maximum amplitude for each Event
  if(missing(ns)) {ns=1} else {ns=ns}
  df1 = new_df(df,w)
  W=as.name(colnames(df1[3]))#paste("W",w,sep="")
  ampli = df1%>% group_by(Events) %>%filter(n()>ns)%>%summarize(amplitude=max(W)) 
  ampli
}

#Lenght in seconds:
lengthEvent=function(df,w, ns){ #Calculates the length in time(secs) of each event
  if(missing(ns)) {ns=1} else {ns=ns}
  df1 = new_df(df,w)
  W=as.name(colnames(df1[3]))#paste("W",w,sep="")
  length_t =df1 %>%group_by(Events) %>% filter(n()>ns)%>% summarise(start_pos = min(ntime), end_pos = max(ntime))%>%
    mutate(diff_time=as.numeric(end_pos-start_pos)) %>% dplyr::select(Events,diff_time)
  length_t
}

# Area, amplitude and length together
allEvents = function(df){ # creates a df with amplitude, area and length for all wells in one DFM
  nameL=as.list(colnames((df%>%select(matches('^W')))[,colSums(df%>%select(matches('^W'))) > 0]))
  count=1
  if (length(nameL)!=12){
    warning("One or more wells, do not report signal. ** Check this list for wells with data:", immediate. = T)
    print(as.character(nameL))
  }
  allEvents=""
  for (i in 1:length(nameL)){
    w=as.numeric(noquote(substring(nameL[i],2,3)))
    uno=merge(lengthEvent(df,w),(merge(areaEvent(df,w),amplitudeEvent(df,w))))
    uno=uno%>%mutate(well=paste("W",w, sep=""))
    allEvents=rbind(allEvents,uno)
    count=count+1
  }
  allEvents=allEvents%>%mutate(area=as.numeric(area),amplitude=as.numeric(amplitude),diff_time=as.numeric(diff_time))
  allEvents=na.omit(allEvents)
  allEvents
}

initialScatPlot=function(fdata){ # Generates 4 scattered plots combining Area, time and Amplitude
  par(mfrow=c(2,2), cex=0.5, cex.axis=0.9, cex.lab=1, bg="white")
  plot(fdata$area,fdata$amplitude, xlab="Area", ylab = "Amplitude")
  plot(fdata$area,fdata$diff_time, xlab="Area", ylab = "Time(secs)")
  plot(fdata$amplitude,fdata$diff_time, xlab="Amplitude", ylab = "Time(secs)")
  plot(fdata$diff_time,fdata$amplitude , xlab="Time(secs)", ylab = "Amplitude")
}

# 3d PLOTS
graph_3d = function(data, v, t){
  if(missing(t)) {t="h"} else {t=t}
  if(missing(v)) {v=1} else {v=v}
  if (v==1) {par(mfrow=c(2,2), bg="white",cex=0.5, cex.axis=0.5, cex.lab=0.5)} 
  else {par(mfrow=c(3,1), bg="white",cex=0.5, cex.axis=0.5, cex.lab=0.5)}
  sp1=scatterplot3d(x=data$area,y=data$amplitude,z=data$diff_time, 
                    color = data$colorCode, pch=data$pch,type =t,
                    xlim=c(0,max(data$area)), ylim=c(0,max(data$amplitude)), zlim=c(0,max(data$diff_time)))
  sp2=scatterplot3d(x=data$area,y=data$diff_time,z=data$amplitude, 
                    color = data$colorCode, pch=data$pch,type =t,
                    xlim=c(0,max(data$area)), ylim=c(0,max(data$diff_time)), zlim=c(0,max(data$amplitude)))
  sp3=scatterplot3d(x=data$diff_time,y=data$area,z=data$amplitude, 
                    color = data$colorCode, pch=data$pch,type =t,
                    xlim=c(0,max(data$diff_time)), ylim=c(0,max(data$area)), zlim=c(0,max(data$amplitude)))
}

## Breakpoints
breakPoints_data = function(value.ts,x){ #prints a set of values associted with the breakpoints in the distribution
  for (i in (1:length(x))){
    c = i
    a=x[i]
    b = value.ts[(a-2):(a+2)]
    cat(sprintf("\"%f\"\"%f\"\"%f\"\n", c, a, b))
    }
}

#PI_index calculation
PI_calc=function(df1){
  j=colnames(df1)[2]
  names=c("ONE","TWO")
  colnames(df1)=names
  ad={}
  w=1
  for (i in 1:6){
    W1=as.name(paste("W",w,sep = ""))
    C=as.name(paste("C",i,sep=""))
    W2=as.name(paste("W",w+1,sep=""))
    total_optA = df1$TWO[df1$ONE==paste(W1)]
    if (length(total_optA)==0){total_optA = 0}else{total_optA=total_optA}
    total_optB = df1$TWO[df1$ONE==paste(W2)]
    if (length(total_optB)==0){total_optB = 0}else{total_optB=total_optB}
    PI = signif((total_optA - total_optB)/(total_optA + total_optB), digits = 3)
    ad[paste(C)]=PI
    w=w+2
  }
  ad=as.data.frame(ad)
  colnames(ad) <- j
  ad
}


mergeDF=function(list){# function to merge dataframes from a list of dataframes
  p=list[[1]]
  for (i in 2:length(list)){
    q=list[[i]]
    p=merge(p,q,all=T)
  }
  p[is.na(p)]=""
  p
}

all_PI=function(df, n){ # Function to determine the PI (Against the compount in A) on the Counts
  #df should be graphEvent 
  # n=1 is for area, n=2 is for amplitude, n=3 is for time
  if (n==1){n="area"
  a=area.ts[vv_area[1]] 
  b=area.ts[vv_area[2]]} 
  if (n==2){n="amplitude"
  a=amplitude.ts[vv_amplitude[1]] 
  b=amplitude.ts[vv_amplitude[2]]} 
  if(n==3){n="diff_time"
  a=time.ts[vv_time[1]] 
  b=time.ts[vv_time[2]]} 
  if (is.na(b)){warning("There is only one breakpoint in your distribution")}
  all_fi=df%>%group_by(well)%>%summarise(all=n())
  blips =df%>%filter_(paste(n,"<",a))%>%group_by(well)%>%summarize(blips=n())
  tastes=df%>%filter_(paste(n,"<",b))%>%filter_(paste(n,">",a))%>%group_by(well)%>%summarize(tastes=n())
  licks =df%>%filter_(paste(n,">",b))%>%group_by(well)%>%summarize(licks=n())
  mylist=list(all_fi,blips,licks,tastes)
  dur=mergeDF(mylist)
  dur=dur%>%mutate(order=as.numeric(gsub("W","", well)))%>%arrange(order)%>%select(-order)
  resPI=(list(PIs=(cbind.data.frame(PI_calc(licks),PI_calc(tastes),PI_calc(blips),PI_calc(all_fi))),
              Touches=(dur)))
  names(resPI[[1]])=paste(n,colnames(resPI[[1]]),sep="_")
  names(resPI[[2]])=paste(n,colnames(resPI[[2]]),sep="_")
  resPI
}

PI_fil_area=function(df1){ # PI using the area under the curve to filter the date
  # by touches, tastes and licks. 
  total = df1%>%group_by(well)%>%summarize(total_dur=sum(diff_time), total_counts=n())
  licks  =df1%>%filter(area>80)%>%group_by(well)%>%summarize(licks_duration=sum(diff_time), licks_counts=n())
  tastes =df1%>%filter(area>10 & area <=80)%>%group_by(well)%>%summarize(tastes_duration=sum(diff_time), tastes_counts=n())
  touches=df1%>%filter(area<=10 & amplitude <=80)%>%group_by(well)%>%summarize(touches_duration=sum(diff_time), touches_counts=n())
  mylist=list(total,licks,tastes, touches)
  dur=mergeDF(mylist)
  dur=dur%>%mutate(order=as.numeric(gsub("W","", well)))%>%arrange(order)%>%select(-order)
  resPI=list(PIs=(cbind.data.frame(PI_calc(total),PI_calc(licks),PI_calc(tastes),PI_calc(touches))),
             Durations=(dur))
  names(resPI[[1]])=paste(colnames(resPI[[1]]),sep="_")
  names(resPI[[2]])=paste(colnames(resPI[[2]]),sep="_")
  resPI
}

############################################################
############################################################
############################################################
############################################################
############################################################

# Set the path to the directory  you want:
setwd("/Users/jpinzon/Documents/UTSW/Projects/Jmjc Project/03 Feeding preference/00 FLIC/60425m1")
# Open the file of interest
DFM="DFM_2.csv"
raw_df = read_data_flic(DFM)

# Remove the baseline. 
# 1 Using average of all data:
#baseLine=baseLineCal(raw_df)
#df=dfm_base(raw_df,baseLine)

# 2. Using moving average
# Set to ~5 mins
df=dfm_base_ma(raw_df, 0)

#Calculate Area, amplitude and time of all events in the DFM
graphEvent=allEvents(df)
initialScatPlot(graphEvent)

# Saving the data.frame graphEvent into a file (change the name accordingly)
#write.csv(graphEvent, file = "suc340-suc340E15-stv5h_60502_ma.csv")

# Filter data
# Using quatiles:
prob = c(0.01,0.99)
l_T=quantile(as.numeric(graphEvent$diff_time), prob = prob, na.rm=T)
l_A=quantile(graphEvent$area, prob = prob, na.rm=T)
l_Am=quantile(graphEvent$amplitude, prob = prob, na.rm=T)

graphEvent2=graphEvent%>%filter(diff_time<=l_T[[2]], area<=l_A[[2]], amplitude>l_Am[[1]])
initialScatPlot(graphEvent2)

# Manual filter:
# Wxploring the whole data set and deciding the limits:
head(as.data.frame(graphEvent%>%arrange(desc(as.numeric(diff_time)))),20)
head(as.data.frame(graphEvent%>%arrange(desc(as.numeric(area)))),10)
head(as.data.frame(graphEvent%>%arrange((as.numeric(amplitude)))),50)

# Change values here:
graphEvent2=graphEvent%>%filter(diff_time<100)
graphEvent2=graphEvent2%>%filter(as.numeric(area)<200)
graphEvent2=graphEvent2%>%filter(amplitude>0.87)

initialScatPlot(graphEvent2)

# Determine PI on time, filtering the events into licks (area > 80), tastes (areas between 10 and 80) 
# and touches (area less than 10 and amplitude less than 80) 

# Unfiltered data
PI_fil_area(graphEvent)

# Filtered data
PI_fil_area(graphEvent2)

total_G =graphEvent%>%group_by(well)
lick_G  =graphEvent%>%filter(amplitude>60)%>%group_by(well)
tastes_G=graphEvent%>%filter(amplitude>20 & amplitude <=60)%>%group_by(well)
touch_G =graphEvent%>%filter(amplitude<=20)%>%group_by(well)

nrow(lick_G)+nrow(tastes_G)+nrow(touch_G)==nrow(graphEvent)

scatterplot3d(x=graphEvent$amplitude, y = graphEvent$diff_time, z= graphEvent$area)


# Create columns for color and pch codes
graphEvent2$colorCode <- ifelse(graphEvent2$amplitude>60,"green",
                               ifelse(graphEvent2$amplitude<20, "red", 
                                "blue"))
graphEvent2$pch <- ifelse(graphEvent2$amplitude>60,1,
                         ifelse(graphEvent2$amplitude<20, 2, 
                                3)) 
# Make the 3d plots:
graph_3d(graphEvent2,1,"h")




plot(touch_G$diff_time, touch_G$amplitude, col = "red", ylim=c(0,400), xlim=c(0,8), pch=1)
points(lick_G$diff_time, lick_G$amplitude, col="blue", pch=2)
points(tastes_G$diff_time, tastes_G$amplitude, pch =4)

###################################
## PLOTING ALL EVENTS SEPARATELY
# Use the df data frame, and add the well to be analyzed. 
df1=new_df(df,1)
df2=new_df(df,2)
df3=new_df(df,3)
df4=new_df(df,4)
df5=new_df(df,5)
df6=new_df(df,6)
df7=new_df(df,7)
df8=new_df(df,8)
df9=new_df(df,9)
df10=new_df(df,10)
df11=new_df(df,11)
df12=new_df(df,12)

# No sense plot with all events one after each other
limY=c(0,400)
h=50
i=100
par(mfrow=c(6,2), cex=0.5, cex.axis=0.9, cex.lab=0.9, bg="white", tcl=-0.1)
#par(mfrow=c(2,1), cex=0.5, cex.axis=0.9, cex.lab=0.9, bg="white", tcl=-0.1)

plot(df1$Events,df1$W1, type="h", main="A", ylim=limY)
abline(h=h, col="red")
abline(h=i, col="blue")
plot(df2$Events,df2$W2, type="h", main="B", ylim=limY)
abline(h=h, col="red")
abline(h=i, col="blue")


#par(mfrow=c(2,1), cex=0.5, cex.axis=0.9, cex.lab=0.9, bg="white", tcl=-0.1)
plot(df3$Events,df3$W3, type="h", main="A", ylim=limY)
abline(h=h, col="red")
abline(h=i, col="blue")
plot(df4$Events,df4$W4, type="h", main="B", ylim=limY)
abline(h=h, col="red")
abline(h=i, col="blue")
#par(mfrow=c(2,1), cex=0.5, cex.axis=0.9, cex.lab=0.9, bg="white", tcl=-0.1)
plot(df5$Events,df5$W5, type="h", main="A", ylim=limY)
abline(h=h, col="red")
abline(h=i, col="blue")
plot(df6$Events,df6$W6, type="h", main="B", ylim=limY)
abline(h=h, col="red")
abline(h=i, col="blue")
#par(mfrow=c(2,1), cex=0.5, cex.axis=0.9, cex.lab=0.9, bg="white", tcl=-0.1)
plot(df7$Events,df7$W7, type="h", main="A", ylim=limY)
abline(h=h, col="red")
abline(h=i, col="blue")
plot(df8$Events,df8$W8, type="h", main="B", ylim=limY)
abline(h=h, col="red")
abline(h=i, col="blue")

#par(mfrow=c(2,1), cex=0.5, cex.axis=0.9, cex.lab=0.9, bg="white", tcl=-0.1)
plot(df9$Events,df9$W9, type="h", main="A", ylim=limY)
abline(h=h, col="red")
abline(h=i, col="blue")

plot(df10$Events,df10$W10, type="h", main="B", ylim=limY)
abline(h=h, col="red")
abline(h=i, col="blue")


#par(mfrow=c(2,1), cex=0.5, cex.axis=0.9, cex.lab=0.9, bg="white", tcl=-0.1)
plot(df11$Events,df11$W11, type="h", main="A", ylim=limY)
abline(h=h, col="red")
abline(h=i, col="blue")
plot(df12$Events,df12$W12, type="h", main="B", ylim=limY)
abline(h=h, col="red")
abline(h=i, col="blue")



plot(((df10$ntime-df10$ntime[1])/3600),df10$W10, type="h", main="B", ylim=limY)

abline(h=h, col="red")
abline(h=i, col="blue")

# counts the number of "observations" on each event
nrow(df1 %>% group_by(Events)%>%summarise(count=n()))
nrow(df2 %>% group_by(Events)%>%summarise(count=n()))
nrow(df3 %>% group_by(Events)%>%summarise(count=n()))
nrow(df4 %>% group_by(Events)%>%summarise(count=n()))
nrow(df5 %>% group_by(Events)%>%summarise(count=n()))
nrow(df6 %>% group_by(Events)%>%summarise(count=n()))
nrow(df7 %>% group_by(Events)%>%summarise(count=n()))
nrow(df8 %>% group_by(Events)%>%summarise(count=n()))
nrow(df9 %>% group_by(Events)%>%summarise(count=n()))
nrow(df10 %>% group_by(Events)%>%summarise(count=n()))
nrow(df11 %>% group_by(Events)%>%summarise(count=n()))
nrow(df12 %>% group_by(Events)%>%summarise(count=n()))

# Calculate the time of each observation from the first one on each event, on events that have 2  or more observations. 
rfa1=df1%>%group_by(Events)%>%filter(n()>2)%>%arrange(ntime)%>%
  mutate(time_from_uno = ntime-min(ntime))
rfa2=df2%>%group_by(Events)%>%filter(n()>2)%>%arrange(ntime)%>%
  mutate(time_from_uno = ntime-min(ntime))



rfa11=df11%>%group_by(Events)%>%filter(n()>2)%>%arrange(ntime)%>%
  mutate(time_from_uno = ntime-min(ntime))
rfa12=df12%>%group_by(Events)%>%filter(n()>1)%>%arrange(ntime)%>%
  mutate(time_from_uno = ntime-min(ntime))

unique(rfa11$Events)
unique(rfa12$Events)

head(as.data.frame(rfa11%>%filter(Events==1)%>%mutate(totaltime=ntime-raw_df$ntime[1])), 2000)
head(as.data.frame(rfa12%>%filter(Events==4)%>%mutate(totaltime=ntime-raw_df$ntime[1])), 2000)

raw_df%>%mutate(uno=ntime-ntime[1])

View(rfa12)

# Change the ylim with the max # of rows
max(rfa1$W1)
# PLOT EACH EVENT
par(mfrow=c(4,2), cex=0.5, cex.axis=0.9, cex.lab=0.9, bg="white", tcl=-0.1)
for (i in (unique(rfa1$Events))){
  rfa2=(rfa1%>%filter(Events==i))
  plot(rfa2$time_from_uno,rfa2$W3, type="l", col="black",  ann = T, axes=T, ylim=c(0,30), labels = T, tck=0)
  # title(xlab = "Loss", cex.lab = 1.5,
  #      line = 4.5)
}

par(mfrow=c(2,3), cex=0.4, cex.axis=0.9)
e_df11=df11%>%filter(Events<=7)
for (i in (unique(e_df11$Events))){
  nombre=paste("W11(Suc)_Event",i,sep="_")
  rfa2=(e_df11%>%filter(Events==i))%>%mutate(time=ntime-ntime[1])
  plot(rfa2$time,rfa2$W11, type="l", col="black",  ann = T, axes=T, ylim=c(0,200), labels = T, tck=0, main=nombre)
}

par(mfrow=c(4,3), cex=0.4, cex.axis=0.9)
e_df12=df12%>%filter(Events<=12)
for (i in (unique(e_df12$Events))){
  nombre=paste("W12(EtOH)_Event",i,sep="_")
  rfa2=(e_df12%>%filter(Events==i))%>%mutate(time=ntime-ntime[1])
  plot(rfa2$time,rfa2$W12, type="l", col="black",  ann = T, axes=T, ylim=c(0,200), labels = T, tck=0, 
       main=nombre)
}

df12



# Asingle event, change the numeber
rfa27=(rfa%>%filter(Events==1))
View(rfa27)
plot(rfa27$time_from_uno,rfa27$W1, type="l", col="black",  ann = T, axes=T, ylim=c(0,205), labels = T, tck=0)

###################################

### PI USING BREAKPOINTS
### Determine breakpoints in the data
###
# Generate graphs with the distributions to get a visual 
# Sort the data by value
y= graphEvent
par(mfrow=c(3,2), cex=0.5, cex.lab=0.8, cex.axis=0.8)
# by Area
area_dist=y%>%arrange((area))%>%mutate(pos=row_number())
plot(area_dist$pos,(area_dist$area))
plot(area_dist$pos,log((area_dist$area)))
# by Amplitude
ampl_dist=y%>%arrange(amplitude)%>%mutate(pos=row_number())
plot(ampl_dist$pos,ampl_dist$amplitude)
plot(ampl_dist$pos,log(ampl_dist$amplitude))
# by Time
t_dist=y%>%arrange(diff_time)%>%mutate(pos=row_number())
plot(t_dist$pos,t_dist$diff_time)
plot(t_dist$pos,log(t_dist$diff_time))

# Detemrmine the optimal breakpoints in each distribution
# break=2 can be change to other values if desired 
area.ts<- ts((area_dist$area))
vv_area=breakpoints(area.ts~1,breaks=2)$breakpoints
amplitude.ts<- ts((ampl_dist$amplitude))
vv_amplitude=breakpoints(amplitude.ts~1,breaks=2)$breakpoints
time.ts<- ts((t_dist$diff_time))
vv_time=breakpoints(time.ts~1,breaks=2)$breakpoints


auno=time.ts[vv_area[[1]]]
ados=time.ts[vv_area[[2]]]

par(mfrow=c(3,1), cex=0.5, cex.lab=0.8, cex.axis=0.8)
plot(t_dist$pos[t_dist$diff_time<auno], t_dist$diff_time[t_dist$diff_time<auno], ylim=c(0,8))
plot(t_dist$pos[t_dist$diff_time>auno & t_dist$diff_time<ados], t_dist$diff_time[t_dist$diff_time>auno & t_dist$diff_time<ados], ylim=c(0,8))
plot(t_dist$pos[t_dist$diff_time>ados], t_dist$diff_time[t_dist$diff_time>ados], ylim=c(0,8))

# These printouts will give an idea of the actual values. 
breakPoints_data(area.ts,vv_area)
breakPoints_data(amplitude.ts,vv_amplitude)
breakPoints_data(time.ts,vv_time)

# Test the breakpoints
# May or may not match the point selected above

fs.test <- Fstats(area_dist$area ~1)
ff=breakpoints(fs.test)
ff$breakpoints
ff
plot(fs.test)

head(y)
lines(breakpoints(fs.test))
arrange(y,diff_time)

# PI by area
PI_area=all_PI(y,1)
# PI by amplitude
PI_amplitude=all_PI(graphEvent2,2)
# PI by time
PI_time=all_PI(graphEvent2,3)

PI_area[[1]]
PI_amplitude[[1]]
PI_time[[1]]

head(y)
y%>%group_by(well)%>%summarise_each(funs(mean))
class(y$Events)

# Create columns for color and pch codes
graphEvent2$colorCode <- ifelse(graphEvent2$well=="W1" | graphEvent2$well=="W2" | graphEvent2$well=="W3" | graphEvent2$well=="W4" | graphEvent2$well=="W5" | graphEvent2$well=="W6","green",
                                "blue") 
graphEvent2$pch <- ifelse(graphEvent2$well=="W1" | graphEvent2$well=="W2" | graphEvent2$well=="W3" | graphEvent2$well=="W4" | graphEvent2$well=="W5" | graphEvent2$well=="W6",19,
                          15) 
# Make the 3d plots:
graph_3d(graphEvent2,1,"h")

### Additional options can be added here:
with(graphEvent2, {
  s3d<-scatterplot3d(diff_time, area,  amplitude,  # x axis, y axis and z axis
                     color = colorCode, pch=pch, type = "h",
                     xlim=c(0,1000), ylim=c(0,1000), zlim=c(0,1000)
                     #main="3-D DFM"
  )
  s3d.coords <- s3d$xyz.convert(diff_time, area,  amplitude) # convert 3D coords to 2D projection
  text(s3d.coords$x, s3d.coords$y,          # x and y coordinates
       labels=row.names(graphEvent2),       # text to plot
       cex=.5, pos=4)                       # shrink text 50% and place to right of points)
})



#############################################
library("changepoint")
y=graphEvent
z=y%>%dplyr::select(area)%>%arrange(area)
z=as.numeric(na.omit(z$area))


ansmeanvar=cpt.meanvar(z, pen.value=0.001, param.estimates=T)
plot(ansmeanvar,cpt.width=1)
cpts(ansmeanvar)

print(ansmeanvar)


# change in variance
#set.seed(1)
#x=c(rnorm(100,0,1),rnorm(100,0,10))
ansvar=cpt.var(z)
plot(ansvar)
print(ansvar) # identifies 1 changepoint at 100

z[1633,]
z[1696,]
z[1739,]

# change in mean
ansmean=cpt.mean(z)
plot(ansmean,cpt.col='blue')
print(ansmean)

# change in mean and variance
ansmeanvar=cpt.meanvar(z)
plot(ansmeanvar,cpt.width=3)
print(ansmeanvar)


cpts(ansvar)

plot(row_number(z),z)

value.ts = (z)
plot(value.ts)

mvalue = cpt.meanvar(value.ts, method="SegNeigh", Q=10, pen.value = 0.05, penalty="Manual")

vv=(cpts(mvalue))
vv
plot(mvalue)

value.ts[(vv[9]-2):(vv[9]+2)]


### SINGLE 3dPLot
rf <- colorRampPalette(rev(brewer.pal(11,'RdYlBu')))
r <- rf(nrow(graphEvent2))

with(graphEvent2, {
  scatterplot3d(area, diff_time, amplitude,  # x axis, y axis and z axis
                color = r ,pch=19,
                main="3-D DFM")
})

raw_df$ntime[4]-raw_df$ntime[5]

raw_df%>%select(ntime)%>%arrange(ntime)%>%mutate(diff=lead(ntime)-(ntime))
y=graphEvent

PI_Adrian=function(df1){ # PI using an "arbitrary" measure of time combined with an area
  #licks are events that last more or equal to 600msec (0.6sec) 
  #tastes are event than last less than 400msec (0.4sec)
  # if duration is between 400 and 600 msec, events are considered licks if their area is > 15, other wise are tastes
  df=df1%>%filter(diff_time>0.2)
  licks=df%>%filter(diff_time>0.6 | (diff_time<0.6 & diff_time>0.4 & area>15))%>%group_by(well)%>%summarize(licks=n())
  tastes=df%>%filter((diff_time<0.6 & diff_time>0.4 & area<15 | diff_time<0.4))%>%group_by(well)%>%summarize(tastes=n())
  mylist=list(licks,tastes)
  resPI=list(PIs=(cbind.data.frame(PI_calc(licks),PI_calc(tastes))),
              Events_count=(mergeDF(mylist)))
  names(resPI[[1]])=paste(colnames(resPI[[1]]),sep="_")
  names(resPI[[2]])=paste(colnames(resPI[[2]]),sep="_")
  resPI
}




#To order the data by the column with the wells names
i$Durations%>%mutate(order=as.numeric(gsub("W","", well)))%>%arrange(order)%>%select(-order)
  
group_by(solution)
i$Touches%>%arrange(mixedsort(well))

i$Touches$solution <- ifelse(i$Touches$well=="W1" | i$Touches$well=="W2" | i$Touches$well=="W3" | i$Touches$well=="W4" | 
                               i$Touches$well=="W5" |i$Touches$well=="W6","sol1","sol2") 

i$Touches%>%mutate(new_well=paste(substring(well,1,1),substring(well,2,4),sep="-"))%>%arrange(desc(new_well))
  
i$Touches%>%mutate(well1=replace(well,substring(well,3,3)=="",paste(substring(well,1,1),"0",substring(well,2,2))))%>%
  arrange(well1) 

i$Events_count%>%mutate(well1=replace(well, substring(well,3,3)=="",paste("W",substring(lead(well),1,2),sep="")))

i
length(as.name(i$Touches$well[[2]]))

library(gtools)
%>%
  arrange(well1)

i$Events_count%>%mutate(w=length(well))
substring(i$Touches$well, 3,3)

as.factor(w[[,1]])
?split
split(w)
n <- 10; nn <- 100

g <- factor(round(n * runif(n * nn)))
x <- rnorm(n * nn) + sqrt(as.numeric(g))
xg <- split(x, g)



mixedsort(i$PIs)

i=PI_area

#### DOES NOT WORK YET!!
ordered_df_well = function(PI_df, wellcolname){
  #wellcolname=noquote(wellcolname)
  print (wellcolname)
  i$Events_count%>%mutate(order=as.numeric(gsub("W","", as.name(wellcolname))))%>%arrange(order)
}
ordered_df_well(PI_area,"area_well")
i=PI_time
PI_time$PIs
i$Touches%>%mutate(order=as.numeric(gsub("W","", diff_time_well)))%>%arrange(order)




