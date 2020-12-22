# Clean up R environment - This is not necesary, but I like to do it. 
rm(list = ls()) 

# LOAD FUNCTIONS FROM myFLIC.R
setwd("/Users/jpinzon/Documents/UTSW/Projects/Jmjc Project/03 Feeding preference/00 FLIC")
source("myFLIC.R")
############################################################
############################################################
############################################################
############################################################

# FAST RUN:
# Change the Threshold appropiately (e.g. change 0 to other number such as 1)
# 1. Set the path:
# 1.a. Manually:
path_to_files="/Users/jpinzon/Documents/UTSW/Projects/Jmjc Project/03 Feeding preference/00 FLIC/60707m1/"
# 1.b. Using the select folder option:
path_to_files=select_folder()
# 2. Get the PI's:
# 2.a. Based on number of events (e.g. counts)
sum_PI(path_to_files,0,1,1)
# 2.b. Based on the duration of the events
sum_PI(path_to_files,0,2,1)
# 2.c. Get duration and counts for each well on each DFM
sum_PI(path_to_files,0,3,1)
sum_PI(path_to_files,0,3,4)
# STEP BY STEP - looks into individual DFM's
# NOT ALL FUNCTIONS ARE COMPLETEL
# 1. Set the path to the directory you want:
setwd("/Users/jpinzon/Documents/UTSW/Projects/Jmjc Project/03 Feeding preference/00 FLIC/60414m1")
# 2. Open the DFM file of interest
DFM="DFM_3.csv"
raw_df = read_data_flic(DFM)

# 3. Remove the baseline. 
# 3.a. Using average of all data:
#baseLine=baseLineCal(raw_df)
#df=dfm_base(raw_df,baseLine)

# 3.b. Using moving average
# Set to ~5 mins
df=dfm_base_ma(raw_df, 0)

# Select only the first 30 mins of the run (a.k.a. 1800 sec)
df1=df%>%arrange(ntime)%>%filter(ntime-ntime[1]<1800)

# Calculate Area, amplitude and time of all events in the DFM
# This data frame can be used to calculate the PI's
graphEvent= allEvents(df)

initialScatPlot(graphEvent)

# Saving the data.frame graphEvent into a file (change the name accordingly)
#write.csv(graphEvent, file = "suc340-suc340E15-stv5h_60502_ma.csv")

# You can further filter the data 
# Using quatiles:
prob = c(0.01,0.99)
l_T=quantile(as.numeric(graphEvent$diff_time), prob = prob, na.rm=T)
l_A=quantile(graphEvent$area, prob = prob, na.rm=T)
l_Am=quantile(graphEvent$amplitude, prob = prob, na.rm=T)

graphEvent2=graphEvent%>%filter(diff_time<=l_T[[2]], area<=l_A[[2]], amplitude>l_Am[[1]])
initialScatPlot(graphEvent2)

# OR a manual filter:
# Exploring the whole data set and deciding the limits:
head(as.data.frame(graphEvent%>%arrange(desc(as.numeric(diff_time)))),20)
head(as.data.frame(graphEvent%>%arrange(desc(as.numeric(area)))),10)
head(as.data.frame(graphEvent%>%arrange((as.numeric(amplitude)))),50)

# Change values here:
graphEvent2=graphEvent%>%filter(diff_time<100)
graphEvent2=graphEvent2%>%filter(as.numeric(area)<200)
graphEvent2=graphEvent2%>%filter(amplitude>0.87)

# Determine PI based on duration by separating the events into licks (area > 80), tastes (areas between 10 and 80) 
# and touches (area less than 10 and amplitude less than 80) 
# Unfiltered data
PI_fil_area(graphEvent)
# Filtered data
PI_fil_area(graphEvent2)

# Repeat for other DFM's. 

###################################################################################
#### WORK IN PROGRESS!!!!

###################################
# Determine the total time of the test for each observation:
# use the df created with dfm_base_ma or dfm_base
df$time=as.numeric(difftime(df$ntime,df$ntime[1], units="mins"))
# Bin the data in four to correspond for each 30 mins of the test
df$bin=cut(df$time, 4, labels=c("0-0.5","0.5-1","1-1.5","1.5-2"))
# Sum the number observations with higher values than the thershold - not equal to events.
df%>%group_by(bin)%>%summarise_each(funs(Sum=sum(.>0)),-position,-ntime, -time)%>%select(-bin)
# Sum the previous data by columns
colSums(df%>%group_by(bin)%>%summarise_each(funs(Sum=sum(.>0)),-position,-ntime, -time)%>%select(-bin))

# The following two do sort of the same but had some other features. 
df%>%group_by(bin)%>%select(W1)%>%summarize(total = n(), larger = sum(W1>0), ratio = larger / total)

for (i in 1:length(names(df%>%select(matches('^W'))))){
  a=df%>%select(matches('^W'))%>%select(i)%>%filter(.!=0)
  print(names(a))
  print(count(a)[[1]])
}

###################################
# 3D plots of the data distribution
scatterplot3d(x=graphEvent$amplitude, y = graphEvent$diff_time, z= graphEvent$area)
# Create columns for color and pch codes
graphEvent2$colorCode <- ifelse(graphEvent2$amplitude>60,"green",
                               ifelse(graphEvent2$amplitude<20, "red", 
                                "blue"))
graphEvent2$pch <- ifelse(graphEvent2$amplitude>60,1,
                         ifelse(graphEvent2$amplitude<20, 2, 
                                3)) 
# Make the 3d plots:
graph_3d(graphEvent2)

###################################
## PLOTING INDIVIDUAL EVENTS EACH ON A SINGLE GRAPH 
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
limY=c(0,200)
h= 10 # approximate limits between touches and tastes, these are amplitudes and the limit is set in area
i= 80 # approximate limits between licks and tastes

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

# count the number of "observations" on each event
df1 %>% group_by(Events)%>%summarise(obs.=n())
df2 %>% group_by(Events)%>%summarise(obs.=n())
df3 %>% group_by(Events)%>%summarise(obs.=n())
df4 %>% group_by(Events)%>%summarise(obs.=n())
df5 %>% group_by(Events)%>%summarise(obs.=n())
df6 %>% group_by(Events)%>%summarise(obs.=n())
df7 %>% group_by(Events)%>%summarise(obs.=n())
df8 %>% group_by(Events)%>%summarise(obs.=n())
df9 %>% group_by(Events)%>%summarise(obs.=n())
df10 %>% group_by(Events)%>%summarise(obs.=n())
df11 %>% group_by(Events)%>%summarise(obs.=n())
df12 %>% group_by(Events)%>%summarise(obs.=n())

# Calculate the time of each observation from the first one on each event, on events that have 2  or more observations. 
rfa1=df3%>%group_by(Events)%>%filter(n()>2)%>%arrange(ntime)%>%
  mutate(time_from_uno = ntime-min(ntime))
rfa2=df4%>%group_by(Events)%>%filter(n()>2)%>%arrange(ntime)%>%
  mutate(time_from_uno = ntime-min(ntime))


rfa11=df11%>%group_by(Events)%>%filter(n()>2)%>%arrange(ntime)%>%
  mutate(time_from_uno = ntime-min(ntime))
rfa12=df12%>%group_by(Events)%>%filter(n()>1)%>%arrange(ntime)%>%
  mutate(time_from_uno = ntime-min(ntime))

unique(rfa1$Events)
unique(rfa2$Events)

head(as.data.frame(rfa1%>%filter(Events==1)%>%mutate(totaltime=ntime-raw_df$ntime[1])), 2000)
head(as.data.frame(rfa2%>%filter(Events==4)%>%mutate(totaltime=ntime-raw_df$ntime[1])), 2000)

# Change the ylim with the max # of rows
max(rfa1$W3)
# PLOT EACH EVENT
par(mfrow=c(4,2), cex=0.5, cex.axis=0.9, cex.lab=0.9, bg="white", tcl=-0.1)
for (i in (unique(rfa1$Events))){
  rfa2=(rfa1%>%filter(Events==i))
  plot(rfa2$time_from_uno,rfa2$W3, type="l", col="black",  ann = T, axes=T, ylim=c(0,205), labels = T, tck=0)
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

# Ploting a single event. Just change the number after the "==", and the df (rfa1)
sinEve=(rfa1%>%filter(Events==1))
plot(sinEve$time_from_uno,sinEve$W1, type="l", col="black",  ann = T, axes=T, ylim=c(0,205), labels = T, tck=0)

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

## SINGLE 3dPLot
rf <- colorRampPalette(rev(brewer.pal(11,'RdYlBu')))
r <- rf(nrow(graphEvent2))

with(graphEvent2, {
  scatterplot3d(area, diff_time, amplitude,  # x axis, y axis and z axis
                color = r ,pch=19,
                main="3-D DFM")
})

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
