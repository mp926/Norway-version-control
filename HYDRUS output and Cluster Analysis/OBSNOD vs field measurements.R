# Output analyzer for HYDRUS OBSNOD output vs Skuterud IISV field measurments for 2016

# Load in the data from the OBSNOD files for each realization ------------------------------------------------

path<-("C:\\Users\\Matt\\Documents\\Norway\\HYDRUS and ParaView files\\HYDRUS results with kriged scaling factor maps\\")
filename<-list()
data<-list()
data.dry<-list()

for (i in 1:50){
  filename[[i]]<-paste("ObsNod 5 timesteps kriging map ", as.character(i),".out",sep="")



data[[i]]<-read.delim(paste(path,filename[[i]],sep=""),header=TRUE, skip=4, sep="",stringsAsFactors=FALSE) 
data[[i]]$time<-as.numeric(data[[i]]$time)

col.idx<-c(seq(2,61,3),seq(3,61,3))
data[[i]]<-data.frame(data[[i]][,1],data[[i]][,col.idx])
data.dry[[i]]<-data.frame(data[[i]][145:150,])
data[[i]]<-data.frame(data[[i]][2:144,])
 
colnames<-c(paste("h",seq(1,20,1),sep=""),paste("theta",seq(1,20,1),sep=""))

names(data[[i]])<-c("days",colnames) # Add names to data frame
names(data.dry[[i]])<-c("days",colnames)

}

# Load in the measurement data -------------------------------------------------------------------------------

library(readxl)

path<-"C:\\Users\\Matt\\Documents\\Norway\\Field experiment\\Spring 2016 field data\\"

df.VWC<-read_excel(paste(path,"VWC spring 2016.xlsx", sep=""), sheet="Data after initial storm", col_names=TRUE)

df.cal<-read_excel(paste(path,"VWC spring 2016.xlsx", sep=""), sheet="Density Corrected VWC", skip=85, col_names=FALSE)
names(df.cal)<-names(df.VWC)

require(xts)

#VWC

df.xts.vwc <- xts(df.VWC[,2:21], as.POSIXct(df.VWC$Timestamp),
                  format="%d/%m/%Y %H:%M:%S")

means <- period.apply(df.xts.vwc, endpoints(df.xts.vwc, "days"), mean) # DAILY AVERAGE

align.time.down = function(x,n){index(x) = index(x)-n; align.time(x,n) }
vwc.day <- align.time.down(means, 60*60)    

# calibrated VWC

df.xts.cal <- xts(df.cal[,2:21], as.POSIXct(df.cal$Timestamp),
                  format="%d/%m/%Y %H:%M:%S")

means <- period.apply(df.xts.cal, endpoints(df.xts.cal, "days"), mean)

align.time.down = function(x,n){ index(x) = index(x)-n; align.time(x,n) }
vwc.cal.day <- align.time.down(means, 60*60)    

# get the time series 

df.evap<-read_excel("C:\\Users\\Matt\\Documents\\Norway\\Field experiment\\Weather data\\MET complete dataset\\Skuterud evaporation calculations.xlsx",
                    sheet="ET (fixed)", col_names=TRUE)

df.xts.atmo <- xts(df.evap[,c(4,5,16)], as.POSIXct(df.evap$Date),
                   format="%d/%m/%Y %H:%M")

means <- period.apply(df.xts.atmo, endpoints(df.xts.atmo, "days"), mean) # DAILY AVERAGE

align.time.down = function(x,n){ index(x) = index(x)-n; align.time(x,n) }
atmo.day <- align.time.down(means, 60*60)    

atmo.day<-atmo.day*24 # Convert to cm/d





# find difference between laboratory and field measured th_s to scale the modeled data ----------- 

require(R.matlab)

dat<-readMat("C:\\Users\\Matt\\Documents\\Norway\\Water retention\\ku-pF\\Mingming corrected retentions Oct 2019\\Laboratory porosity.mat")
dat<-dat[[1]]
dat<-unlist(dat)
lab<-data.frame(as.character(c(dat[seq(1,242,by=2)])),as.numeric(c(dat[seq(2,242,by=2)])))
lab<-lab[84:121,] # remove the 2015 data since it is not needed
names(lab)<-c("core","porosity")
lab$sensor=c(1,10,11,12,14,15,16,19,2,20,3,5,6,8,17,7,2,6,
             20,5,19,15,15,5,6,13,2,6,6,8,16,20,3,20,15,2,6,10)
lab.ct<-lab[1:16,]
attach(lab.ct)
lab.ct<-lab.ct[order(sensor),]
detach(lab.ct)


fths.meas<-apply(vwc.day,2,max)
lths.meas<-data.frame(lab.ct$porosity,lab.ct$sensor)
names(lths.meas)<-c("porosity","sensor")
deep.porosity<-mean(as.numeric(lab.ct$porosity[c(6,14)])) 
# take the mean value between the two 90 cm cores, 
# since there were no other cores measured at that depth

lths.meas[17:20,]<-c(rep(deep.porosity,times=4),c(4,9,13,18))
attach(lths.meas)
lths.meas<-lths.meas[order(sensor),]
detach(lths.meas)
lths.meas$porosity<-lths.meas$porosity*100

diff<-lths.meas$porosity-fths.meas

vwc.sc<-matrix(nrow=143,ncol=20)
for(i in 1:20){
  vwc.sc[,i]<-m.model[,i+20]-diff[i]
}

vwc.sc<-as.data.frame(vwc.sc)


# Correlation in porosities between field and TDR-measured
# for BD of lower cores, average the two 90 cm cores, and use for all deep cores
deep.bd<-mean(c(1.60,1.51))
corr.por<-data.frame(lab=lths.meas$porosity,field=fths.meas,sensor=seq(1,20,1),
                     depth=c(10,40,60,92,25,33,93,67,132,25,45,58,
                             90,25,45,58,95,94,10,10),
                     depth.ind=c(1,3,3,4,1,2,4,3,5,1,3,3,4,1,3,3,4,4,1,1),
                     BD=c(1.32,1.65,1.69,deep.bd,1.37,1.33,1.51,1.52,deep.bd,
                     1.47,1.64,1.67,deep.bd,1.22,1.67,1.69,1.60,deep.bd,1.34,1.30))

require(ggplot2)

g<-ggplot(data=corr.por, aes(x=lab, y=field, group=depth)) +
  geom_point(aes(color=BD, shape=factor(depth.ind)), size=4)+
  geom_abline(slope=1,intercept=0, lwd=1.2)+
  xlim(0,100)+
  ylim(0,100)+
  xlab(expression("Laboratory measured" ~ theta[s] ~ "(%)")) +
  ylab(expression("Field (TDR) measured" ~ theta[s] ~ "(%)"))

g + scale_shape_discrete(name  ="Depth (-cm)",
                         breaks=c(1,2,3,4,5),
                         labels=c("10-25","25-40","40-70","70-95","95+"),
                         solid=TRUE) + 
  scale_colour_gradient(name = "Bulk Density (g/cc)",
                        low = "grey", high = "black") +
  theme_bw() + theme(axis.title=element_text(size=14), axis.text=element_text(size=12),
                     legend.title=element_text(size=12),legend.text=element_text(size=12))


# Pressure potential 

df.press<-read_excel(paste(path,"pressure potential spring-fall2016.xlsx", sep=""), sheet="Data", col_names=TRUE)

df.xts.press <- xts(df.press[,2:21], as.POSIXct(df.press$Timestamp),
                    format="%d/%m/%Y %H:%M:%S")

means.hour <- period.apply(df.xts.press, endpoints(df.xts.press, "hours"), mean) #HOURLY AVERAGE

means <- period.apply(df.xts.press, endpoints(df.xts.press, "days"), mean) #DAILY AVERAGE

align.time.down = function(x,n){ index(x) = index(x)-n; align.time(x,n) }
press.hour<-align.time.down(means.hour, 60*60)
press.day <- align.time.down(means, 60*60)
press.day = press.day * 10 # convert to cm 


# Compare the model output to the measurements -------------------------------------------------------------


# find the mean and standard deviation of the model ouput time series (n=50) for each observation node 

#dat.mat<-matrix(nrow=3402,ncol=50)
dat.mat<-matrix(nrow=143,ncol=50)
dat.mat.dry<-matrix(nrow=6,ncol=50)
m.model<-matrix(nrow=143,ncol=40)
m.model.dry<-matrix(nrow=6,ncol=40)
sd.model<-matrix(nrow=143,ncol=40)
sd.model.dry<-matrix(nrow=6,ncol=40)

for(j in 2:41){
  for(i in 1:50){
  dat.mat[,i]<-data[[i]][,j]
  dat.mat.dry[,i]<-data.dry[[i]][,j]
  m.model[,j-1]<-apply(dat.mat,1,mean)
  m.model.dry[,j-1]<-apply(dat.mat.dry,1,mean)
  sd.model[,j-1]<-apply(dat.mat,1,sd)
  sd.model.dry[,j-1]<-apply(dat.mat.dry,1,sd)
  }
}


m.model<-as.data.frame(m.model)
m.model.dry<-as.data.frame(m.model.dry)
sd.model<-as.data.frame(sd.model)
sd.model.dry<-as.data.frame(sd.model.dry)


m.model[,21:40]<-m.model[,21:40]*100 # convert VWC to percent
m.model.dry[,21:40]<-m.model.dry[,21:40]*100

sd.model[,21:40]<-sd.model[,21:40]*100 #convert VWC to percent
sd.model.dry[,21:40]<-sd.model.dry[,21:40]*100

sens.idx<-seq(1,20,1) # Sensor numbers for each column of data

#n<-NA

#for(i in 1:20){
#  for(j in 21:40){
#n[i]<-paste("sensor.h",as.character(sens.idx[i]),sep="")
#n[j]<-paste("sensor.th",as.character(sens.idx[j-20]),sep="")
#  }
#}

#names(m.model)<-n


#m.model<-xts(m.model,as.POSIXct(index(atmo.day)),
#             format="%d/%m/%Y %H:%M:%S")


# find SSE between the model output and the measurements for the entire profile. 
SSE<-list()

for(i in 1:50){
SSE[[i]]<-matrix(nrow=142,ncol=20)
}

for(k in 1:50){
  for(j in 1:20){
    for(i in 1:142){
    SSE[[k]][i,j]=(as.numeric(press.day[i,j])-m.model[i,j])^2    
    }
  }
}


SSE.all<-list()
for(i in 1:50){
SSE.all[[i]]<-sum(SSE[[i]])
SSE.all[[i]]<-format(SSE.all[[i]],scientific=TRUE)
}



# Calculate the Kling-Gupta efficiency (KGE) and plot the comparison between measurements and simulations: Original simulations ----------------------------------------------------------
library(hydroGOF)
library(ggplot2)


# Calculate KGE (Gupta et al. 2009)

df.xts.simvwc<-list()
for(i in 1:20){
df.xts.simvwc[[i]] <- xts(m.model[,i+20], as.POSIXct(index(vwc.day)),
                    format="%d/%m/%Y %H:%M:%S")
}

KGE.out<-list()

for(i in 1:20){
  
KGE.out[[i]]<-KGE(sim=df.xts.simvwc[[i]],obs=vwc.day[,i],s=c(1,1,1), na.rm=TRUE, method="2012", out.type="single")

}

KGE.out<-KGE.out[-9] # remove sensor 9 since NA value was produced (there was no standard deviation in the simulated data)
mean(unlist(KGE.out))  # mean value of KGE
sd(unlist(KGE.out))


## Calculate the Kling-Gupta efficiency (KGE) and plot the comparison between measurements and simulations: Simulations with parameter changes ----------------------------------------------------------

library(hydroGOF)
library(ggplot2)


# load in simulated data with L changes


path<-("C:\\Users\\Matt\\Documents\\Norway\\HYDRUS and ParaView files\\Norway 2016 Scaled IISV with plow pan and altered properties\\")
subdir<-c("Scaled props all data L=0.5",
          "Scaled props all data L=1",
          "Scaled props all data L=-1",
          "Scaled props all data L=-2",
          "Scaled props all data L=3",
          "Scaled props using only field data",
          "Scaled props all data topsoil subsoil K6",
          "Scaled props all data topsoil subsoil K6_lowered water table")
filename<-"Obsnod.out"  
mylist<-list()

for (i in 1:length(subdir)){

  mylist[[i]]<-read.delim(paste(path,subdir[i],"\\",filename,sep=""),header=TRUE, skip=4, sep="",stringsAsFactors=FALSE) 
  mylist[[i]]$time<-as.numeric(mylist[[i]]$time)
  
  col.idx<-c(seq(2,61,3),seq(3,61,3))
  mylist[[i]]<-data.frame(mylist[[i]][,1],mylist[[i]][,col.idx])
  mylist[[i]]<-data.frame(mylist[[i]][2:144,])
  
  colnames<-c(paste("h",seq(1,20,1),sep=""),paste("theta",seq(1,20,1),sep=""))
  
  names(mylist[[i]])<-c("days",colnames) # Add names to data frame
}

names(mylist)<-c("L0.5","L1","L-1","L-2","L3","fieldref","K6","K6WT")

for (i in 1:length(subdir)){
  mylist[[i]][,22:41]<-mylist[[i]][,22:41]*100  # convert VWC to percent
}

  

xts.simvwc<-vector(mode="list", length=length(subdir))

for(i in 1:length(subdir)){
  xts.simvwc[[i]]<-matrix(nrow=143,ncol=20)
}

for(i in 1:length(subdir)){
  for(j in 2:21){
  xts.simvwc[[i]][,j-1] <- mylist[[i]][,j+20]
  xts.simvwc[[i]]<-xts(xts.simvwc[[i]], as.POSIXct(index(vwc.day)),
        format="%d/%m/%Y %H:%M:%S")
  }
}

names(xts.simvwc)<-names(mylist)


KGE.out.adj<-vector(mode="list", length=length(subdir))
for(i in 1:length(subdir)){
  KGE.out.adj[[i]]<-matrix(nrow=1,ncol=20)
}

for(i in 1:length(subdir)){
  for(j in 1:20){
  KGE.out.adj[[i]][,j]<-KGE(sim=xts.simvwc[[i]][,j],obs=vwc.day[,j],s=c(1,1,1), na.rm=TRUE, method="2012", out.type="single")
  }
}

KGE.out.adj<-data.frame(t(KGE.out.adj[[4]]),t(KGE.out.adj[[3]]),t(KGE.out.adj[[1]]),t(KGE.out.adj[[2]]), unlist(KGE.out),t(KGE.out.adj[[5]]),t(KGE.out.adj[[6]]),t(KGE.out.adj[[7]]),t(KGE.out.adj[[8]]))

names(KGE.out.adj)<-c("L-2","L-1","L0.5","L1","L2.3","L3","fieldref","K6","K6WT")

#KGE.out.adj$L2.3 <- unlist(KGE.out)# add in KGE for the initial simulations (L=2.312)

KGE.out.adj<-KGE.out.adj[-9,] # remove sensor 9 (sd = 0, so KGE = NA)

apply(KGE.out.adj,2,mean)
apply(KGE.out.adj,2,sd)


# plot KGE values as a function of adjustment




# Plot simulated v. measured pressure potential and water content time series -------------------




# Pressure potential


# plot as time series

glist<-list()

for (i in 1:20) {
  message(i)
  glist[[i]] <- local({
    i <- i
    p1 <- ggplot()+
      geom_line(data=press.day, aes(x=0:142,y=as.numeric(press.day[,i]), color="Measured"), lwd=1.3)+
      geom_line(data=m.model,aes(x=data[[1]]$days,y=m.model[,i], color="L=2.3")) +
      geom_line(data=mylist$`L-2`, aes(x=days, y=mylist$`L-2`[,i+1], color="L=-2"))+
      geom_line(data=mylist$`L-1`, aes(x=days, y=mylist$`L-1`[,i+1], color="L=-1")) +  
      geom_line(data=mylist$L0.5, aes(x=days, y=mylist$L0.5[,i+1], color="L=0.5")) + 
      geom_line(data=mylist$L1, aes(x=days, y=mylist$L1[,i+1], color="L=1")) + 
      geom_line(data=mylist$L3, aes(x=days, y=mylist$L3[,i+1], color="L=3")) +
      geom_line(data=mylist$fieldref, aes(x=days, y=mylist$fieldref[,i+1], color="Field saturated water content"))+
      geom_line(data=mylist$K6, aes(x=days, y=mylist$K6[,i+1], color="K(-6)")) +
      geom_line(data=mylist$K6WT, aes(x=days, y=mylist$K6WT[,i+1], color="K(-6) and lowered water table")) +
      scale_color_manual(values=c("Measured"="black", "L=2.3"="red","L=-2"="blue","L=-1"="yellow","L=0.5"="brown","L=1"="green",
                                  "L=3"="orange", "Field saturated water content"="turquoise","K(-6)"="purple",
                                  "K(-6) and lowered water table" ="darkgrey")) +
      xlab("Time") +
      ylab(paste("Pressure potential (hPa)","sensor",as.character(sens.idx[i]),sep=" ")) +
      theme_bw() + theme(legend.title=element_blank())
    print(p1)
  })
}

dfmeas<-data.frame("days"=rep(seq(0,142,1), times=20),"data"=unlist(as.numeric(press.day[,i])),
                   "id"=rep(c("Sensor 1","Sensor 2","Sensor 3","Sensor 4","Sensor 5","Sensor 6","Sensor 7",
                              "Sensor 8","Sensor 9","Sensor 10","Sensor 11","Sensor 12","Sensor 13","Sensor 14",
                              "Sensor 15","Sensor 16","Sensor 17","Sensor 18","Sensor 19","Sensor 20"),each=143))
# plot as correlation plots 
  mydf<-data.frame("days"=rep(seq(0,142,1),times=(9*20)), "data"=c(
                                                                   unlist(m.model[,1:20]),
                                                                   unlist(mylist$`L-2`[,2:21]),
                                                                   unlist(mylist$`L-1`[,2:21]),
                                                                   unlist(mylist$L0.5[,2:21]),
                                                                   unlist(mylist$L1[,2:21]),
                                                                   unlist(mylist$L3[,2:21]),
                                                                   unlist(mylist$fieldref[,2:21]),
                                                                   unlist(mylist$K6[,2:21]),
                                                                   unlist(mylist$K6WT[,2:21])),
                 "id"=rep(c("Sensor 1","Sensor 2","Sensor 3","Sensor 4","Sensor 5","Sensor 6","Sensor 7",
                            "Sensor 8","Sensor 9","Sensor 10","Sensor 11","Sensor 12","Sensor 13","Sensor 14",
                            "Sensor 15","Sensor 16","Sensor 17","Sensor 18","Sensor 19","Sensor 20"),each=143, times=9),
                 "type"=rep(c("L=2.3","L=-2","L=-1","L=0.5","L=1","L=3",
                              "field saturated water content","K(-6)","K(-6) and lowered water table"), each=143*20))

  
mydf$id<-factor(mydf$id, levels=c("Sensor 1","Sensor 2","Sensor 3","Sensor 4","Sensor 5","Sensor 6","Sensor 7",
                          "Sensor 8","Sensor 9","Sensor 10","Sensor 11","Sensor 12","Sensor 13","Sensor 14",
                          "Sensor 15","Sensor 16","Sensor 17","Sensor 18","Sensor 19","Sensor 20"))  

mydf$type<-factor(mydf$type, levels=c("L=-2","L=-1","L=0.5","L=1","L=2.3","L=3",
                    "field saturated water content","K(-6)","K(-6) and lowered water table"))
     
g<-ggplot(mydf, aes(x=rep(log10(-dfmeas$data),times=9), y=log10(-data)))+
  geom_smooth(method="lm", se=FALSE, aes(color=type)) +
  facet_wrap(~id)+
  #geom_point(aes(shape=type, color=type), alpha=0.4, size=2) +
  geom_abline(slope=1,intercept=0, lty=2, lwd=1.4) +
  scale_shape_manual(values=c("L=2.3"=1,"L=-2"=1,"L=-1"=1,"L=0.5"=1,"L=1"=1,"L=3"=1,
                              "field saturated water content"=2, "K(-6)"=0, "K(-6) and lowered water table"=0))+
  scale_color_manual(values=c("L=2.3"="black","L=-2"="black","L=-1"="black","L=0.5"="black","L=1"="black","L=3"="black",
                              "field saturated water content"="green", "K(-6)"="blue", "K(-6) and lowered water table"="red"))+
  guides(colour = guide_legend(override.aes = list(alpha = 1)))+
  labs(x="Log measured pressure potential (-hPa)", y="Log predicted pressure potential (-hPa)") +
  lims(x=c(0,log10(3500)),y=c(0,log10(3500)))+
  theme_bw() + theme(legend.title=element_blank(), legend.text=element_text(size=13), axis.text=element_text(size=11),
                     axis.title=element_text(size=13), strip.text=element_text(size=13),
                     legend.position="bottom", panel.grid=element_blank())


# Volumetric water content

for(i in 1:20){
  
  print(ggplot()+
          geom_line(data=vwc.day, aes(x=index(vwc.day),y=as.numeric(vwc.day[,i])))+
          geom_line(data=m.model,aes(x=index(atmo.day),y=m.model[,i+20]),color="red") +
          geom_ribbon(aes(x=index(atmo.day),ymin=as.numeric(m.model[,i+20])-sd.model[,i+20],
                          ymax=as.numeric(m.model[,i+20])+sd.model[,i+20]),alpha=0.3) +
          #geom_line(data=vwc.cal.day, aes(x=index(vwc.cal.day),y=as.numeric(vwc.cal.day[,i])), col="blue")+
          xlab("Time") +
          ylab(paste("Volumetric Water Content","sensor",as.character(sens.idx[i]),sep=" ")))

}


for(i in 1:20){
  
  print(ggplot() +
         geom_point(data=m.model,aes(x=as.numeric(vwc.day[,i])/100, y=m.model[,i+20]/100)) +
         #geom_point(aes(x=(vwc.cal.day[,i]/100),y=(m.model[,i+20])/100),color="blue")+ # density-calibrated TDR
         #geom_point(aes(x=(as.numeric(vwc.day[,i])/100), y=vwc.sc[,i]/100),color="red")+ #red is the model output - the difference between the laboratory and field-measured saturated water content values
         geom_abline(slope=1,intercept=0)+
         xlim(0.25,0.5)+
         ylim(0.25,0.5)+
         xlab(paste("Measured volumetric Water Content","sensor",as.character(sens.idx[i]),sep=" ")) +
         ylab(paste("Modeled Volumetric Water Content","sensor",as.character(sens.idx[i],sep=""))))
  
   
}

# isolate some plots to show the effect of magnitude mismatch between measured and modeled VWC 
require(cowplot)

i=c(18,19,15)

p1<-ggplot()+
  geom_line(data=vwc.day, aes(x=index(vwc.day),y=as.numeric(vwc.day[,i[1]]/100)))+
  geom_line(data=m.model,aes(x=index(atmo.day),y=m.model[,i[1]+20]/100),color="red") +
  #geom_ribbon(aes(x=index(atmo.day),ymin=as.numeric(m.model[,i+20])-sd.model[,i+20],
  #                ymax=as.numeric(m.model[,i+20])+sd.model[,i+20]),alpha=0.3) +
  #geom_line(data=vwc.cal.day, aes(x=index(vwc.cal.day),y=as.numeric(vwc.cal.day[,i])), col="blue")+
  xlab("Time") +
  ylab(paste("Volumetric Water Content","sensor",as.character(sens.idx[i[1]]),sep=" ")) +
  ylim(0.28,0.45)+
  theme_bw()

p2<-ggplot()+
    geom_point(data=m.model,aes(x=as.numeric(vwc.day[,i[1]])/100, y=m.model[,i[1]+20]/100)) +
    #geom_point(aes(x=(vwc.cal.day[,i]/100),y=(m.model[,i+20])/100),color="blue")+ # density-calibrated TDR
    #geom_point(aes(x=(as.numeric(vwc.day[,i])/100), y=vwc.sc[,i]/100),color="red")+ #red is the model output - the difference between the laboratory and field-measured saturated water content values
    geom_abline(slope=1,intercept=0)+
    xlim(0.25,0.5)+
    ylim(0.25,0.5)+
    xlab("Measured Volumetric Water Content") +
    ylab("Simualated Volumetric Water Content") +
  theme_bw()

p3<-ggplot()+
  geom_line(data=vwc.day, aes(x=index(vwc.day),y=as.numeric(vwc.day[,i[2]]/100)))+
  geom_line(data=m.model,aes(x=index(atmo.day),y=m.model[,i[2]+20]/100),color="red") +
  #geom_ribbon(aes(x=index(atmo.day),ymin=as.numeric(m.model[,i+20])-sd.model[,i+20],
  #                ymax=as.numeric(m.model[,i+20])+sd.model[,i+20]),alpha=0.3) +
  #geom_line(data=vwc.cal.day, aes(x=index(vwc.cal.day),y=as.numeric(vwc.cal.day[,i])), col="blue")+
  xlab("Time") +
  ylab(paste("Volumetric Water Content","sensor",as.character(sens.idx[i[2]]),sep=" ")) +
  ylim(0.28,0.45)+
  theme_bw()

p4<-ggplot()+
  geom_point(data=m.model,aes(x=as.numeric(vwc.day[,i[2]])/100, y=m.model[,i[2]+20]/100)) +
  #geom_point(aes(x=(vwc.cal.day[,i]/100),y=(m.model[,i+20])/100),color="blue")+ # density-calibrated TDR
  #geom_point(aes(x=(as.numeric(vwc.day[,i])/100), y=vwc.sc[,i]/100),color="red")+ #red is the model output - the difference between the laboratory and field-measured saturated water content values
  geom_abline(slope=1,intercept=0)+
  xlim(0.25,0.5)+
  ylim(0.25,0.5)+
  xlab("Measured Volumetric Water Content") +
  ylab("Simulated Volumetric Water Content") +
  theme_bw()

p5<-ggplot()+
  geom_line(data=vwc.day, aes(x=index(vwc.day),y=as.numeric(vwc.day[,i[3]]/100)))+
  geom_line(data=m.model,aes(x=index(atmo.day),y=m.model[,i[3]+20]/100),color="red") +
  #geom_ribbon(aes(x=index(atmo.day),ymin=as.numeric(m.model[,i+20])-sd.model[,i+20],
  #                ymax=as.numeric(m.model[,i+20])+sd.model[,i+20]),alpha=0.3) +
  #geom_line(data=vwc.cal.day, aes(x=index(vwc.cal.day),y=as.numeric(vwc.cal.day[,i])), col="blue")+
  xlab("Time") +
  ylab(paste("Volumetric Water Content","sensor",as.character(sens.idx[i[3]]),sep=" ")) +
  ylim(0.28,0.45)+
  theme_bw()

p6<-ggplot()+
  geom_point(data=m.model,aes(x=as.numeric(vwc.day[,i[3]])/100, y=m.model[,i[3]+20]/100)) +
  #geom_point(aes(x=(vwc.cal.day[,i]/100),y=(m.model[,i+20])/100),color="blue")+ # density-calibrated TDR
  #geom_point(aes(x=(as.numeric(vwc.day[,i])/100), y=vwc.sc[,i]/100),color="red")+ #red is the model output - the difference between the laboratory and field-measured saturated water content values
  geom_abline(slope=1,intercept=0)+
  xlim(0.25,0.5)+
  ylim(0.25,0.5)+
  xlab("Measured Volumetric Water Content") +
  ylab("Simulated Volumetric Water Content") +
  theme_bw()

plot_grid(p3,p4,p5,p6,p1,p2,ncol=2,
          labels=c("10 cm depth below surface","","45 cm depth below surface","", "94 cm depth below surface"),
          label_size=10, label_x=0.001, label_y=1.01, scale=0.9)


# No correlation figures 

plot_grid(p3,p5,p1,ncol=1,
          labels=c("10 cm depth","45 cm depth", "94 cm depth"),
          label_size=10, label_x=0.1, label_y=1.01, scale=0.9)



################ Drying behavior

for(i in 1:20){
  
  print(ggplot()+
          #geom_line(data=press.day, aes(x=index(atmo.day),y=as.numeric(press.day[,i])))+
          geom_line(data=m.model.dry,aes(x=144:148,y=m.model[,sens.idx[i]]),color="red") +
          geom_ribbon(aes(x=index(atmo.day),ymin=as.numeric(m.model[,sens.idx[i]])-sd.model[,sens.idx[i]],
                          ymax=as.numeric(m.model[,sens.idx[i]])+sd.model[,sens.idx[i]]))+  
          xlab("Time") +
          ylab(paste("Pressure potential (hPa)","sensor",as.character(sens.idx[i]),sep=" ")))
  
  #print(ggplot() +
  #       geom_point(data=m.model,aes_string(x=as.numeric(press.day[,i]), y=paste("sensor.h",as.character(sens.idx[i]),sep=""))) +
  #       xlab(paste("Pressure potential (hPa)","sensor",as.character(sens.idx[i]),sep=" ")) +
  #       ylab(paste("modeled pressure potential","sensor",as.character(sens.idx[i],sep=""))))
  
}


# Volumetric water content

for(i in 1:20){
  
  print(ggplot()+
          geom_line(data=vwc.day, aes(x=index(vwc.day),y=as.numeric(vwc.day[,i])))+
          geom_line(data=m.model,aes(x=index(atmo.day),y=m.model[,i+20]),color="red") +
          geom_ribbon(aes(x=index(atmo.day),ymin=as.numeric(m.model[,i+20])-sd.model[,i+20],
                          ymax=as.numeric(m.model[,i+20])+sd.model[,i+20]),alpha=0.3) +
          xlab("Time") +
          ylab(paste("Volumetric Water Content","sensor",as.character(sens.idx[i]),sep=" ")))
  
}







