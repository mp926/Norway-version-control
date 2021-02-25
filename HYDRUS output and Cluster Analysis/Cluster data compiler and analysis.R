# Compiler code for the clustered VWC output 

# Contents: 
# Data extraction
# Data cleaning and restructuring
# Conversion to HYDRUS input file for volume comparison
# Saving new versions of data in more manageable form

## DATA EXTRACTION --------------------------------------

# path to save R data to
setwd("C:\\Users\\Matt\\Documents\\Norway\\Norway-version-control\\HYDRUS output and Cluster Analysis")


#path where cluster output is located
path<-("C:\\Users\\Matt\\Documents\\Norway\\Ruth clusters\\2016 experiment\\VWC clusters\\")

daysn<-c(0,"3_33",144,147,150) # the datasets have different notation for 3.33 days 
days<-c(0,3.33,144,147,150)
quantile<-c(5,95)

data.compiler <- function(path,type="norm"){
  if(type=="norm"){
    data<-read.csv(paste(path,norm.file,sep=""),header=TRUE,sep=",")
  } else if (type=="mean"){
    data<-read.csv(paste(path,mean.file,sep=""),header=TRUE,sep=",")
  } else if (type=="quant"){
    data<-read.csv(paste(path,quant.file,sep=""),header=TRUE,sep=",")
  }
  return(data)
}


# Normal 50 realization data set 

out<-matrix(nrow=38519,ncol=50)
cluster.map<-list()

for (j in 1:5){
  for(i in 1:50){
norm.file<-paste("LISA_VWC HYDRUS map 5 timesteps kriging map ",i,"_",daysn[j],"d.csv",sep="")    
data.norm<-data.compiler(path,type="norm")
coords<-data.norm[,1:3]
fdr<-data.norm$fdr
out[,i]<-fdr
cluster.map[[j]]<-out
  }
}

names(cluster.map)<-c("0 days","3.33 days","144 days","147 days","150 days")

save(cluster.map,file="50 realizations clustered.Rdata")


# Mean data set 

cluster.map<-list()

for (j in 1:5){
    mean.file<-paste("LISA_mean data_",days[j]," days mean.csv",sep="")    
    data.mean<-data.compiler(path,type="mean")
    coords<-data.mean[,1:3]
    fdr<-data.mean$fdr
    out<-fdr
    cluster.map[[j]]<-out
}

names(cluster.map)<-c("0 days","3.33 days","144 days","147 days","150 days")

save(cluster.map,file="Mean cluster data.RData")
save(coords,file="Mean cluster data coords.RData")

# Quantile data set 

out<-matrix(nrow=38519,ncol=2)
cluster.map<-list()

for(j in 1:5){
  for (k in 1:2){
  quant.file<-paste("LISA_mean data_",days[j], " days " ,quantile[k],"pc.csv",sep="")    
  data.quant<-data.compiler(path,type="quant")
  coords<-data.quant[,1:3]
  fdr<-data.quant$fdr
  out[,k]<-fdr
  cluster.map[[j]]<-out
  }
}

names(cluster.map)<-c("0 days","3.33 days","144 days","147 days","150 days")

save(cluster.map,file="Quantile cluster data.RData")



## DATA CLEANING AND RESTRUCTURING ----------------


# Load in the needed data

setwd("C:\\Users\\Matt\\Documents\\Norway\\Norway-version-control\\HYDRUS output and Cluster Analysis")

load("Mean cluster data.Rdata")
#load("50 realizations clustered.Rdata")

HH.idx<-sapply(cluster.map,function(y) which(y==4)) # apply function "which" to list 
LL.idx<-sapply(cluster.map,function(y) which(y==1))
NS.idx<-sapply(cluster.map,function(y) which(y==0))

for(i in 1:5){   # Convert the FDR values to arbitrary material numbers. HH=1, LL=2, NS=3

cluster.map[[i]][HH.idx[[i]]]<-1
cluster.map[[i]][LL.idx[[i]]]<-2
cluster.map[[i]][NS.idx[[i]]]<-3

}

# Georeference ERT coordinates to VWC coordinates
ERT<-read.csv(paste(path,"ClusterDataSpring2016ERT_78_79_58removed-LISA_ISO.csv",sep=""),header=TRUE,sep=",")
ERT[,1:3]<-ERT[,1:3]*100
ERT.coords<-ERT[,c(1:3)]

write.table(ERT.coords, file="C:\\Users\\Matt\\Documents\\Norway\\Norway-version-control\\HYDRUS output and Cluster Analysis\\ERT 2016 coordinates.txt", 
            col.names=FALSE, row.names=FALSE, quote=FALSE, sep=" ")

VWC<-read.csv(paste(path,subdir,"LISA_mean data_150 days mean.csv",sep=""),header=TRUE,sep=",")
VWC.coords<-VWC[,c(1:3)]

write.table(VWC.coords, file="C:\\Users\\Matt\\Documents\\Norway\\Norway-version-control\\HYDRUS output and Cluster Analysis\\VWC 2016 coordinates.txt",
            col.names=FALSE, row.names=FALSE, quote=FALSE, sep=" ")

## WRITE THE HYDRUS IMPORT FILES -----------------------------

library(tidyr)

setwd("C:\\Users\\Matt\\Documents\\Norway\\Ruth clusters\\2016 experiment\\HYDRUS cluster imports")

coords<-read.table("coords.txt",header=TRUE)
nrow=38519
days<-c(0,3.33,144,147,150)

head<-"OBJECT=INDEXES_AT_POINTS"
imp<-read.table("IISV_import.txt",skip=1,nrow=38519)
tail<-";"

for(j in 1:5){
  if(is.null(ncol(cluster.map[[1]]))==TRUE){
    imp[,4]<-cluster.map[[j]] 
    
    out<-rbind(head,imp,tail)
    filename<-paste("VWC mean clusters_",days[j],"days",sep="")
    write.table(out,paste(filename,".txt",sep=""),quote=FALSE,row.names=FALSE,col.names=FALSE)
  }else{
    for(i in 1:ncol(cluster.map[[1]])){
      imp[,4]<-cluster.map[[j]][,i] 
      
      out<-rbind(head,imp,tail)
      filename<-paste("VWC clusters",i,"_",days[j],"days",sep="")
      write.table(out,paste(filename,".txt",sep=""),quote=FALSE,row.names=FALSE,col.names=FALSE)
    }
    
    
}
    }





## Volume Analysis ---------------------------

setwd("C:\\Users\\Matt\\Documents\\Norway\\Ruth clusters\\2016 experiment")
# import volume data 
library(readxl)

df.vol<-read_excel("Cluster volumes.xlsx",col_names=TRUE)

df.mean<-df.vol[grep("mean",df.vol$label),] #extract mean data for later use

df.vol<-df.vol[-grep("mean",df.vol$label),] #remove the mean data from the main data.frame to make calculations easier 

strings<-c("t1","t2","t3","t4","t5")
idx<-list()
t.list<-list() #list of data broken up by time step
m.vol<-data.frame(HH=rep(NA,times=5),LL=rep(NA,times=5),NS=rep(NA,times=5),
                  row.names=c("0 days","3.33 days","144 days","147 days","150 days"))
sd.vol<-data.frame(HH=rep(NA,times=5),LL=rep(NA,times=5),NS=rep(NA,times=5),
                   row.names=c("0 days","3.33 days","144 days","147 days","150 days"))

for(i in 1:5){
idx[[i]]<-grep(strings[i],df.vol$label)
t.list[[i]]<-as.data.frame(df.vol[idx[[i]],])
m.vol[i,]<-apply(t.list[[i]][,2:4],2,mean)
sd.vol[i,]<-apply(t.list[[i]][,2:4],2,sd)
}


# plot the difference between resistivity cluster and VWC cluster per timestep

library(ggplot2)
library(cowplot)

df.plot<-data.frame(data=c(m.vol$HH/max(m.vol$HH),m.vol$LL/max(m.vol$LL)),
                    labels=c(rep("VWC HH",times=5),rep("VWC LL",times=5)),
                    wc=rep(c(0.40,0.39,0.38,0.365,0.35),times=2))



df.plot2<-data.frame(rel.diff=c((abs((m.vol$HH-df.vol$LL[1])/max(c(m.vol$HH,df.vol$LL[1])))),
                                (abs((m.vol$LL-df.vol$HH[1])/max(c(m.vol$LL,df.vol$HH[1])))),
                                (abs((m.vol$HH-df.vol$HH[1])/max(c(m.vol$HH,df.vol$HH[1])))),
                                (abs((m.vol$LL-df.vol$LL[1])/max(c(m.vol$LL,df.vol$LL[1]))))),
                                  labels=c(rep("(VWC HH, ERT LL)",times=5),
                                  rep("(VWC LL, ERT HH)",times=5),
                                  rep("(VWC HH, ERT HH)",times=5),
                                  rep("(VWC LL, ERT LL)", times=5)),
                     wc=rep(c(0.40,0.39,0.38,0.365,0.35),times=4))
df.plot2$labels <- factor(df.plot2$labels, levels = c("(VWC HH, ERT HH)", "(VWC LL, ERT LL)", "(VWC HH, ERT LL)", "(VWC LL, ERT HH)"))



g<-ggplot(df.plot) +
  geom_path(aes(x=wc,y=data),lwd=1.3)+
  facet_wrap(~labels)+
  xlab(expression(paste("Mean volumetric water content","  ", (m^3 ~ m^-3))))+
  ylab(expression(paste("Normalized mean volume","  ",(m^3))))+
  theme_bw()+theme(axis.text=element_text(size=13),axis.title=element_text(size=13),
                   strip.text=element_text(size=13))



g2<-ggplot(df.plot2) +
  geom_path(aes(x=wc,y=rel.diff),lwd=1.3)+
  facet_wrap(~labels)+
  xlab(expression(paste("Mean volumetric water content","  ", (m^3 ~ m^-3))))+
  ylab(expression(paste("Relative difference(X,Y) [-]")))+
  theme_bw()+theme(axis.text=element_text(size=13),axis.title=element_text(size=13),
                   strip.text=element_text(size=13))

scaleFUN <- function(x) sprintf("%.3f", x)
g2<- g2+scale_y_continuous(labels=scaleFUN)

plot_grid(g,g2,nrow=2,labels=c("a.","b."))



## Percent total area and Perimeter ------------------------------------------------------------------------------

# Create xyz data files to convert to images in imageJ (these will be used to compile a 3D image stack later)
setwd("C:\\Users\\Matt\\Documents\\Norway\\Norway-version-control\\HYDRUS output and Cluster Analysis")

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# import data
load("Mean cluster data.RData")
load("Mean cluster data coords.RData")
load("ERT clustered.Rdata")

#add xyz coordinates to cluster values
dat0<-cbind(coords,cluster.map[[1]])
dat3<-cbind(coords,cluster.map[[2]])
dat144<-cbind(coords,cluster.map[[3]])
dat147<-cbind(coords,cluster.map[[4]])
dat150<-cbind(coords,cluster.map[[5]])
datERT<-ERT


#order resulting data.frame as a function of z value
attach(dat0)
dat0<-dat0[order(z),]
detach(dat0)

attach(dat3)
dat3<-dat3[order(z),]
detach(dat3)

attach(dat144)
dat144<-dat144[order(z),]
detach(dat144)

attach(dat147)
dat147<-dat147[order(z),]
detach(dat147)

attach(dat150)
dat150<-dat150[order(z),]
detach(dat150)

attach(datERT)
datERT<-datERT[order(z),]
detach(datERT)

#isolate unique z values (n=6994 images)
#z.idx<-unique(dat1$z)



#class z values into 5 cm increments for managability (n=32 images for image stack)
inc<-seq(from=0,to=-160,by=-5)

mylist0<-list()

for(i in 1:(length(inc)-1)){
  mylist0[[i]]<-dat0[which(dat0$z<=inc[i] & dat0$z>=inc[i+1]),]
  mylist0[[i]]<-mylist0[[i]][,-3] # remove z coordinates to write as 2D images 
  names(mylist0[[i]])<-c("x","y","cluster")
}

mylist3<-list()

for(i in 1:(length(inc)-1)){
  mylist3[[i]]<-dat3[which(dat3$z<=inc[i] & dat3$z>=inc[i+1]),]
  mylist3[[i]]<-mylist3[[i]][,-3] # remove z coordinates to write as 2D images 
  names(mylist3[[i]])<-c("x","y","cluster")
}


mylist144<-list()

for(i in 1:(length(inc)-1)){
  mylist144[[i]]<-dat144[which(dat144$z<=inc[i] & dat144$z>=inc[i+1]),]
  mylist144[[i]]<-mylist144[[i]][,-3] # remove z coordinates to write as 2D images 
  names(mylist144[[i]])<-c("x","y","cluster")
}

mylist147<-list()

for(i in 1:(length(inc)-1)){
  mylist147[[i]]<-dat147[which(dat147$z<=inc[i] & dat147$z>=inc[i+1]),]
  mylist147[[i]]<-mylist147[[i]][,-3] # remove z coordinates to write as 2D images 
  names(mylist147[[i]])<-c("x","y","cluster")
}

mylist150<-list()

for(i in 1:(length(inc)-1)){
  mylist150[[i]]<-dat150[which(dat150$z<=inc[i] & dat150$z>=inc[i+1]),]
  mylist150[[i]]<-mylist150[[i]][,-3] # remove z coordinates to write as 2D images 
  names(mylist150[[i]])<-c("x","y","cluster")
}


mylistERT<-list()

for(i in 1:(length(inc)-1)){
  mylistERT[[i]]<-datERT[which(datERT$z<=inc[i] & datERT$z>=inc[i+1]),]
  mylistERT[[i]]<-mylistERT[[i]][,-3] # remove z coordinates to write as 2D images 
  names(mylistERT[[i]])<-c("x","y","cluster")
}



# Convert list components to images by converting x and y data from continuous to discrete 
#test<-list()
#x.inc<-seq(0,200,by=5)
#y.inc<-seq(0,100,by=5)

#for(k in 1:length(mylist)){
#  test[[k]]<-matrix(nrow=length(x.inc)-1,ncol=length(y.inc)-1)
#  for(i in 1:length(x.inc)-1){
#      x.idx<-which(mylist[[k]]$x>=x.inc[i] & mylist[[k]]$x<=x.inc[i+1])
#  ans.x<-mylist[[k]][x.idx,]
#    test[[k]][i,round(ans.x$y)]<-ans.x$cluster
#  }
#}



# write xyz files to convert to images in imageJ
setwd(paste("C:\\Users\\Matt\\Documents\\Norway\\Norway-version-control\\",
      "HYDRUS output and Cluster Analysis\\image import files",sep=""))
name.idx<-c("0 days","3.33 days","144 days","147 days","150 days","ERT")
for(j in 6){
for(i in 1:(length(inc)-1)){
filename<-paste("imageJ cluster import",name.idx[j], "depth",inc[i],"to",inc[i+1],"cm",sep=" ")
if(j==1){
write.table(mylist0[[i]],file=paste(filename,".txt",sep=""),quote=FALSE,row.names=FALSE,col.names=FALSE)
} else if(j==2){
write.table(mylist3[[i]],file=paste(filename,".txt",sep=""),quote=FALSE,row.names=FALSE,col.names=FALSE)
} else if(j==3){
write.table(mylist144[[i]],file=paste(filename,".txt",sep=""),quote=FALSE,row.names=FALSE,col.names=FALSE)
} else if(j==4){
write.table(mylist147[[i]],file=paste(filename,".txt",sep=""),quote=FALSE,row.names=FALSE,col.names=FALSE)
  }else if (j==5){
    write.table(mylist150[[i]],file=paste(filename,".txt",sep=""),quote=FALSE,row.names=FALSE,col.names=FALSE)
  } else if (j==6){
    write.table(mylistERT[[i]],file=paste(filename,".txt",sep=""),quote=FALSE,row.names=FALSE,col.names=FALSE)
}
  }
}

# Read in the summarized imageJ analysis 
require(readxl)

mydataij<-read_excel("C:\\Users\\Matt\\Documents\\Norway\\Norway-version-control\\HYDRUS output and Cluster Analysis\\image stacks\\Summarized output ALL.xlsx",
                     sheet=NULL,col_names=TRUE)


#mydataij$dataset<-factor(mydataij$dataset, 
#      levels=c("ERT","VWC 0 days", "VWC 3.33 days", "VWC 144 days","VWC 147 days","VWC 150 days"))

# convert timesteps to mean water content labels
idx0<-which(mydataij$dataset=='VWC 0 days')
idx3<-which(mydataij$dataset=='VWC 3.33 days')
idx144<-which(mydataij$dataset=='VWC 144 days')
idx147<-which(mydataij$dataset=='VWC 147 days')
idx150<-which(mydataij$dataset=='VWC 150 days')

mydataij$dataset[idx0]<-"VWC 0.40"
mydataij$dataset[idx3]<-"VWC 0.39"
mydataij$dataset[idx144]<-"VWC 0.38"
mydataij$dataset[idx147]<-"VWC 0.37"
mydataij$dataset[idx150]<-"VWC 0.35"



# remove NS
NS.idx<-which(mydataij$cluster=="NS")
mydataij<-mydataij[-NS.idx,]


vwc.id<-seq(from = 1, to =160, by = 32)

mydataij$vwc <- c(rep(NA, times=64), rep(plotdf$vwc[1:32],times=2), rep(plotdf$vwc[33:64], times=2),
                  rep(plotdf$vwc[65:96], times=2), rep(plotdf$vwc[97:128], times=2), 
                  rep(plotdf$vwc[129:160], times=2)) # ADD VWC w.r.t. DEPTH FROM LAST SECTION

mydataij$dataset = factor(mydataij$dataset, levels=c("ERT","VWC 0.40","VWC 0.39","VWC 0.38", "VWC 0.37","VWC 0.35"))


# plot the perimeter and percent area w.r.t. depth
require(ggplot2)

pct<-ggplot(mydataij, aes(y=depth,color=cluster)) +
  facet_wrap(~ dataset) + #labeller= label_bquote(cols= .(dataset) ~ m^3 ~ m^-3)) +
  geom_path(aes(x=pct.area), lwd=1) +
  xlab("Percent total area") +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = -32, ymax = -25, fill = "grey", alpha = .3, color = NA) +
  ylab("Depth (cm)") +
  labs(color = "Cluster") +
  theme_bw() + theme(strip.text=element_text(size=12),axis.text=element_text(size=12),axis.title=element_text(size=13))
  

# plot without ERT 
require(ggplot2)

mydataij.vwc<-mydataij[which(mydataij$dataset!="ERT"),]

pct.vwc<-ggplot(mydataij.vwc, aes(y=depth,color=cluster)) +
  facet_wrap(~ dataset) + #labeller= label_bquote(cols= .(dataset) ~ m^3 ~ m^-3)) +
  geom_path(aes(x=pct.area), lwd=1) +
  xlab("Percent total area") +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = -32, ymax = -25, fill = "brown", alpha = .4, color = NA) +
  ylab("Depth (cm)") +
  labs(color = "Cluster") +
  theme_bw() + theme(strip.text=element_text(size=12),axis.text=element_text(size=12),axis.title=element_text(size=13),
                     legend.text=element_text(size=14),legend.title=element_text(size=14), legend.position="bottom")


# save legend to extract and put outside the plots
leg.vwc<-get_legend(pct.vwc)

pct.vwc<-pct.vwc + theme(legend.position="none")

# Percent area represented by clusters in general as a function of depth
library(data.table)

# MEAN AREA OF HH AND LL, RESPECTIVELY


ERT<-mydataij[which(mydataij$dataset=="ERT"),]
VWC0<-mydataij[which(mydataij$dataset=="VWC 0.40"),]
VWC3<-mydataij[which(mydataij$dataset=="VWC 0.39"),]
VWC144<-mydataij[which(mydataij$dataset=="VWC 0.38"),]
VWC147<-mydataij[which(mydataij$dataset=="VWC 0.37"),]
VWC150<-mydataij[which(mydataij$dataset=="VWC 0.35"),]


diff.HH<-matrix(nrow=192, ncol=5)

diff.HH[,1]<-ERT$pct.area[which(ERT$cluster=="HH")] - VWC0$pct.area[which(VWC0$cluster=="HH")]
diff.HH[,2]<-ERT$pct.area[which(ERT$cluster=="HH")] - VWC3$pct.area[which(VWC0$cluster=="HH")]
diff.HH[,3]<-ERT$pct.area[which(ERT$cluster=="HH")] - VWC144$pct.area[which(VWC0$cluster=="HH")]
diff.HH[,4]<-ERT$pct.area[which(ERT$cluster=="HH")] - VWC147$pct.area[which(VWC0$cluster=="HH")]
diff.HH[,5]<-ERT$pct.area[which(ERT$cluster=="HH")] - VWC150$pct.area[which(VWC0$cluster=="HH")]

diff.LL<-matrix(nrow=192, ncol=5)

diff.LL[,1]<-ERT$pct.area[which(ERT$cluster=="LL")] - VWC0$pct.area[which(VWC0$cluster=="LL")]
diff.LL[,2]<-ERT$pct.area[which(ERT$cluster=="LL")] - VWC3$pct.area[which(VWC0$cluster=="LL")]
diff.LL[,3]<-ERT$pct.area[which(ERT$cluster=="LL")] - VWC144$pct.area[which(VWC0$cluster=="LL")]
diff.LL[,4]<-ERT$pct.area[which(ERT$cluster=="LL")] - VWC147$pct.area[which(VWC0$cluster=="LL")]
diff.LL[,5]<-ERT$pct.area[which(ERT$cluster=="LL")] - VWC150$pct.area[which(VWC0$cluster=="LL")]


diff.depth<-data.frame(diff=c(diff.HH[,1],diff.HH[,2],diff.HH[,3],diff.HH[,4],diff.HH[,5],
                              diff.LL[,1],diff.LL[,2],diff.LL[,3],diff.LL[,4],diff.LL[,5]),
                       depth=rep(ERT$depth,times=5), cluster=c(rep("HH",times=192*5),rep("LL",times=192*5)),
                       dataset=rep(c("VWC 0.40","VWC 0.39","VWC 0.38","VWC 0.37","VWC 0.35"), each=192, times=2))

attach(diff.depth)

diff.depth <- diff.depth[order(-depth),]

detach(diff.depth)

m0HH<-abs(mean(diff.depth$diff[which(diff.depth$cluster=="HH" & diff.depth$dataset=="VWC 0.40")]))
m0LL<-abs(mean(diff.depth$diff[which(diff.depth$cluster=="LL" & diff.depth$dataset=="VWC 0.40")]))
m3HH<-abs(mean(diff.depth$diff[which(diff.depth$cluster=="HH" & diff.depth$dataset=="VWC 0.39")]))
m3LL<-abs(mean(diff.depth$diff[which(diff.depth$cluster=="LL" & diff.depth$dataset=="VWC 0.39")]))
m144HH<-abs(mean(diff.depth$diff[which(diff.depth$cluster=="HH" & diff.depth$dataset=="VWC 0.38")]))
m144LL<-abs(mean(diff.depth$diff[which(diff.depth$cluster=="LL" & diff.depth$dataset=="VWC 0.38")]))
m147HH<-abs(mean(diff.depth$diff[which(diff.depth$cluster=="HH" & diff.depth$dataset=="VWC 0.37")]))
m147LL<-abs(mean(diff.depth$diff[which(diff.depth$cluster=="LL" & diff.depth$dataset=="VWC 0.37")]))
m150HH<-abs(mean(diff.depth$diff[which(diff.depth$cluster=="HH" & diff.depth$dataset=="VWC 0.35")]))
m150LL<-abs(mean(diff.depth$diff[which(diff.depth$cluster=="LL" & diff.depth$dataset=="VWC 0.35")]))

g<-ggplot(diff.depth, aes(x=diff, y=depth, color=cluster)) +
  facet_wrap(~ dataset) +
  geom_path() +
  geom_vline(data=md, aes(xintercept=m), lwd=1.3, color="red", lty=2) +


# TOTAL AREA

attach(mydataij)

newdf <- mydataij[order(-depth),]
col<-as.data.frame(cbind(newdf$area, newdf$pct.area))

detach(mydataij)



tot.area<-setDT(col)[, as.list(colSums(.SD)), by = gl(ceiling(384/2), 2, 384)]
names(tot.area)<-c("gl","area","pct.area")

tot.area$depth<-rep(unique(newdf$depth),each=6)
tot.area$dataset<-rep(unique(newdf$dataset),times=32)
ERT.id<-which(tot.area$dataset=="ERT")
ERT.df<-tot.area[ERT.id]
tot.area<-tot.area[-ERT.id]

tot<-ggplot(tot.area, aes(y=depth)) +
  facet_wrap(~ dataset, labeller= label_bquote(cols= .(dataset) ~ m^3 ~ m^-3)) +
  geom_path(aes(x=pct.area), lwd=1) +
  annotate(geom='path', x=ERT.df$pct.area,y=ERT.df$depth, color="red", lwd=1.1) +
  xlab("Percent total area") +
  ylab("Depth (cm)") +
  theme_bw() + theme(strip.text=element_text(size=12),axis.text=element_text(size=12),axis.title=element_text(size=13))


#Diff. in total percent area btwn ERT and VWC w.r.t. depth

depth.id<-unique(tot.area$depth)
dat.id<-unique(tot.area$dataset)
AD<-list()

for(i in 1:length(ERT.id)){
AD[[i]]<-(tot.area$pct.area[which(tot.area$depth==depth.id[i])] - ERT.df$pct.area[i])
}

AD<-unlist(AD)
AD<-data.frame(abs.diff=AD, depth=tot.area$depth, dataset=tot.area$dataset)
m0<-mean(AD$abs.diff[which(AD$dataset==dat.id[1])])
m3<-mean(AD$abs.diff[which(AD$dataset==dat.id[2])])
m144<-mean(AD$abs.diff[which(AD$dataset==dat.id[3])])
m147<-mean(AD$abs.diff[which(AD$dataset==dat.id[4])])
m150<-mean(AD$abs.diff[which(AD$dataset==dat.id[5])])
sd0<-sd(AD$abs.diff[which(AD$dataset==dat.id[1])])
sd3<-sd(AD$abs.diff[which(AD$dataset==dat.id[2])])
sd144<-sd(AD$abs.diff[which(AD$dataset==dat.id[3])])
sd147<-sd(AD$abs.diff[which(AD$dataset==dat.id[4])])
sd150<-sd(AD$abs.diff[which(AD$dataset==dat.id[5])])

mdata<-data.frame(m=c(m0,m3,m144,m147,m150),sdlow=c(m0-sd0,m3-sd3,m144-sd144,m147-sd147,m150-sd150),
                  sdhigh=c(m0+sd0,m3+sd3,m144+sd144,m147+sd147,m150+sd150),dataset=dat.id)

mdata.HHLL<-data.frame(m=c(m0HH,m3HH,m144HH,m147HH,m150HH,m0LL,m3LL,m144LL,m147LL,m150LL),
                       cluster=rep(c("HH","LL"), each=5))


ad<-ggplot(AD, aes(y=depth)) +
  facet_wrap(~ dataset, scales="free_y", labeller= label_bquote(cols= .(as.character(dataset)) ~ m^3 ~ m^-3)) +
  geom_rect(data=mdata, aes(x = NULL,y = NULL,xmin=sdlow, xmax=sdhigh, 
                                 ymin=-Inf, ymax=Inf), 
                                 alpha=0.5,fill="grey")+ 
  geom_path(aes(x=abs.diff), lwd=1) +
  geom_vline(data=mdata, aes(xintercept=m), lwd=1.3, color="red", lty=2) +
  xlab("Difference in percent area occupied by ERT and VWC clusters") +
  ylab("Depth (cm)") +
  theme_bw() + theme(strip.text=element_text(size=12),axis.text=element_text(size=12),axis.title=element_text(size=13))

# Make a figure of mean cluster representation as a function of water content

(ggplot(mdata, aes(x=c(0.40,0.39,0.38,0.37,0.35), y=m)) +
    geom_point(size=3)+
    geom_line(lty=2, lwd=1)+
    geom_line(data=mdata.HHLL, aes(x=rep(c(0.40,0.39,0.38,0.37,0.35), times=2), y=m, color=cluster)) +
    xlab(expression(paste("Mean volumetric water content"," ", (m^3 ~ m^-3)))) +
    ylab("Mean difference (%)") +
    theme_bw() + theme(axis.text=element_text(size=14), axis.title=element_text(size=14)) 
  )


## Variograms -----------------

require(gstat)
require(sp)
# load in the VWC and Resistivity data used for clustering (before clusters)
path<-("C:\\Users\\Matt\\Documents\\Norway\\Ruth clusters\\2016 experiment\\")
subdir<-("VWC clusters\\")

ERT<-read.csv(paste(path,"ClusterDataSpring2016ERT_78_79_58removed-LISA_ISO.csv",sep=""),header=TRUE,sep=",")
ERT[,1:3]<-ERT[,1:3]*100
ERT<-ERT[,c(1:3,8)]

# Change ERT FDR INDICES TO REFLECT INVERSE RELATIONSHIP TO WATER CONTENT
HH.idx<-which(ERT$fdr==4)
LL.idx<-which(ERT$fdr==1)
NS.idx<-which(ERT$fdr==0)

ERT$fdr[HH.idx]<-1 # HH to LL 
ERT$fdr[LL.idx]<-4 # LL to HH  


coordinates(ERT)=~x+y+z
g.ERT=gstat(NULL,"log resistivity", log.resistivity~1, ERT)
#g.ERT=gstat(g.ERT, "lisa log resistivity", lisa.log.resistivity~1, ERT) #LISA statistic 
g.ERT=gstat(g.ERT, "ERT HH", log.resistivity~1, ERT[which(ERT$fdr==4),])
g.ERT=gstat(g.ERT, "ERT LL", log.resistivity~1, ERT[which(ERT$fdr==1),])
g.ERT=gstat(g.ERT, "ERT NS", log.resistivity~1, ERT[which(ERT$fdr==0),])


v.ERT = variogram(g.ERT)

# Save output variograms (since they are large and the calculation takes a long time)
setwd("C:\\Users\\Matt\\Documents\\Norway\\Norway-version-control\\HYDRUS output and Cluster Analysis")
save(v.ERT,file="ERT variograms with clusters.RData")

library(data.table)

VWC<-read.csv(paste(path,subdir,"LISA_mean data_150 days mean.csv",sep=""),header=TRUE,sep=",")
VWC<-VWC[,c(1:5,8)]

coordinates(VWC)=~x+y+z
g.VWC=gstat(NULL,"VWC", vwc~1, VWC)
#g.VWC=gstat(g.VWC, "lisa vwc", lisa.vwc~1, VWC)
g.VWC=gstat(g.VWC, "VWC HH", vwc~1, VWC[which(VWC$fdr==4),])
g.VWC=gstat(g.VWC, "VWC LL", vwc~1, VWC[which(VWC$fdr==1),])
g.VWC=gstat(g.VWC, "VWC NS", vwc~1, VWC[which(VWC$fdr==0),])

v.vwc = variogram(g.VWC) #calculates the variograms and cross variograms for all objects in "g"
plot(v.vwc)

# LL variogram has number of points decrease at 48 cm 

save(v.vwc,file="VWC variograms with clusters.RData")


# Raw data comparison --

g=gstat(NULL,"log resistivity", log.resistivity~1, ERT)
g=gstat(g, "vwc", vwc~1, VWC)
v.raw<-variogram(g)
plot(v.raw)

save(v.raw,file="vwc vs. ERT raw data variograms.RData")
load("vwc vs. ERT raw data variograms.RData")


#                                       Clustered indices comparison ---


g=gstat(NULL,"ERT", fdr~1, ERT)
g=gstat(g,"VWC", fdr~1, VWC)
v.clusters<-variogram(g, cross=FALSE,pseudo=-1, verbose=TRUE, covariogram=FALSE)
plot(v.clusters)


g = gstat(g, model = vgm(psill=1, "Sph", range=40, nugget=2.2),fill.all=TRUE)
g.fit = fit.lmc(v.clusters, g, fit.ranges=TRUE)
g.fit
plot(v.clusters, g.fit)

g.fitERT = fit.variogram(v.clusters[which(v.clusters$id=="ERT"),],model=vgm(psill=1, "Sph", range=80, nugget=0))
g.fit = fit.variogram(v.clusters[which(v.clusters$id=="VWC"),],model=vgm(psill=1, "Sph", range=80, nugget=0))


save(v.clusters,file="mean vwc 0 days vs. ERT cluster variograms.RData")
load("mean vwc 0 days vs. ERT cluster variograms.RData")


#Create a data.frame with the output data for all of the variograms


var.parameters=data.frame(np=v.clusters$np,dist=v.clusters$dist,gamma=v.clusters$gamma,id=v.clusters$id)
var.parameters=var.parameters[16:45,] # cut off cross-variograms
load("mean vwc 3.33 days vs. ERT cluster variograms.RData")
var.parameters<-rbind(var.parameters,v.clusters[which(v.clusters$id=="VWC"),c(1:3,6)])
load("mean vwc 144 days vs. ERT cluster variograms.RData")
var.parameters<-rbind(var.parameters,v.clusters[which(v.clusters$id=="VWC"),c(1:3,6)])
load("mean vwc 147 days vs. ERT cluster variograms.RData")
var.parameters<-rbind(var.parameters,v.clusters[which(v.clusters$id=="VWC"),c(1:3,6)])
load("mean vwc 150 days vs. ERT cluster variograms.RData")
var.parameters<-rbind(var.parameters,v.clusters[which(v.clusters$id=="VWC"),c(1:3,6)])
var.parameters$id=c(rep("VWC 0 days", times=15),rep("ERT", times=15),rep("VWC 3.33 days", times=15), rep("VWC 144 days", times=15),
                    rep("VWC 147 days", times=15), rep("VWC 150 days", times=15))
var.parameters$id<-factor(var.parameters$id, levels=c("ERT","VWC 0 days", "VWC 3.33 days", "VWC 144 days","VWC 147 days","VWC 150 days"))

# create the variogram models to be used for later plotting (variogram parameters obtained from fit.lmc)
vgm.ert<-vgm(psill=2.592, "Sph", range=85.64, nugget=0.049)
vgm.vwc0<-vgm(psill=2.442, "Sph", range=88.70, nugget=0.0816)
vgm.vwc3<-vgm(psill=2.73, "Sph", range=89.52, nugget=0.133)
vgm.vwc144<-vgm(psill=2.46, "Sph", range=82.30, nugget=0.028)
vgm.vwc147<-vgm(psill=2.49, "Sph", range=95.57, nugget=0.027)
vgm.vwc150<-vgm(psill=2.80, "Sph", range=102.72, nugget=0)


vmodel.lines<-list()
vmodel.lines[[1]]<-variogramLine(vgm.ert,maxdist=100,n=50)
vmodel.lines[[2]]<-variogramLine(vgm.vwc0,maxdist=100,n=50)
vmodel.lines[[3]]<-variogramLine(vgm.vwc3,maxdist=100,n=50)
vmodel.lines[[4]]<-variogramLine(vgm.vwc144,maxdist=100,n=50)
vmodel.lines[[5]]<-variogramLine(vgm.vwc147,maxdist=100,n=50)
vmodel.lines[[6]]<-variogramLine(vgm.vwc150,maxdist=100,n=50)

require(data.table)

vmod<-rbindlist(vmodel.lines)
names(vmod)<-c("dist","gamma")
vmod$id<-c(rep("ERT", times=50),rep("VWC 0 days", times=50),rep("VWC 3.33 days", times=50), rep("VWC 144 days", times=50),
             rep("VWC 147 days", times=50), rep("VWC 150 days", times=50))
vmod$id<-factor(vmod$id, levels=c("ERT","VWC 0 days", "VWC 3.33 days", "VWC 144 days","VWC 147 days","VWC 150 days"))

require(ggplot2)

g<-ggplot(var.parameters, aes(x=dist, y=gamma))+
  facet_wrap(~id) +
  geom_point(pch=1, size =3) +
  geom_line(vmod,mapping=aes(x=dist,y=gamma), color="red")+
  xlab("Distance (cm)") +
  ylab(expression(paste("Semi-variance  ",gamma))) +
  theme_bw() + theme(axis.text=element_text(size=13), axis.title=element_text(size=13), strip.text=element_text(size=12))



#                   Try rearranging the index labels to prove indpendence of cluster spatial variance from ---
#                   classification. Just changing the label/classification does not change the spatial variance
#                   of the clusters since the clustering is based on the water content, which does not care about 
#                   whether or not it is in the HH or LL cluster. Does the variogram range increase as a function of
#                   drying, regardless of cluster index arrangement?


VWC0<-read.csv(paste(path,subdir,"LISA_mean data_0 days mean.csv",sep=""),header=TRUE,sep=",")
VWC0<-VWC0[,c(1:5,8)]

VWC3<-read.csv(paste(path,subdir,"LISA_mean data_3.33 days mean.csv",sep=""),header=TRUE,sep=",")
VWC3<-VWC3[,c(1:5,8)]

VWC144<-read.csv(paste(path,subdir,"LISA_mean data_144 days mean.csv",sep=""),header=TRUE,sep=",")
VWC144<-VWC144[,c(1:5,8)]

VWC147<-read.csv(paste(path,subdir,"LISA_mean data_147 days mean.csv",sep=""),header=TRUE,sep=",")
VWC147<-VWC147[,c(1:5,8)]

VWC150<-read.csv(paste(path,subdir,"LISA_mean data_150 days mean.csv",sep=""),header=TRUE,sep=",")
VWC150<-VWC150[,c(1:5,8)]

HH.idx0<-which(VWC0$fdr==4)
LL.idx0<-which(VWC0$fdr==1)
NS.idx0<-which(VWC0$fdr==0)

HH.idx3<-which(VWC3$fdr==4)
LL.idx3<-which(VWC3$fdr==1)
NS.idx3<-which(VWC3$fdr==0)

HH.idx144<-which(VWC144$fdr==4)
LL.idx144<-which(VWC144$fdr==1)
NS.idx144<-which(VWC144$fdr==0)

HH.idx147<-which(VWC147$fdr==4)
LL.idx147<-which(VWC147$fdr==1)
NS.idx147<-which(VWC147$fdr==0)

HH.idx150<-which(VWC150$fdr==4)
LL.idx150<-which(VWC150$fdr==1)
NS.idx150<-which(VWC150$fdr==0)

VWC0$fdr[HH.idx0]<-1 # HH to LL 
VWC0$fdr[LL.idx0]<-0 # LL to NS  
VWC0$fdr[NS.idx0]<-4 # NS to HH

VWC3$fdr[HH.idx3]<-1  
VWC3$fdr[LL.idx3]<-0 
VWC3$fdr[NS.idx3]<-4 

VWC144$fdr[HH.idx144]<-1  
VWC144$fdr[LL.idx144]<-0 
VWC144$fdr[NS.idx144]<-4 

VWC147$fdr[HH.idx147]<-1  
VWC147$fdr[LL.idx147]<-0 
VWC147$fdr[NS.idx147]<-4 

VWC150$fdr[HH.idx150]<-1 
VWC150$fdr[LL.idx150]<-0 
VWC150$fdr[NS.idx150]<-4 

coordinates(VWC0)=~x+y+z
coordinates(VWC3)=~x+y+z
coordinates(VWC144)=~x+y+z
coordinates(VWC147)=~x+y+z
coordinates(VWC150)=~x+y+z


g=gstat(NULL,"ERT", fdr~1, ERT)
g=gstat(g,"VWC0", fdr~1, VWC0)
g=gstat(g, "VWC3", fdr~1, VWC3)
g=gstat(g, "VWC144", fdr~1, VWC144)
g=gstat(g, "VWC147", fdr~1, VWC147)
g=gstat(g, "VWC150", fdr~1, VWC150)
v.clusters<-variogram(g, cross=TRUE,pseudo=-1, verbose=TRUE, covariogram=FALSE)
plot(v.clusters)


g = gstat(g, model = vgm(psill=1, "Sph", range=80, nugget=0),fill.all=TRUE)
g.fit = fit.lmc(v.clusters, g, fit.ranges=TRUE)
g.fit
plot(v.clusters, g.fit)



# g0=gstat(NULL,"ERT", fdr~1, ERT)
# g0=gstat(g0,"VWC0", fdr~1, VWC0)
# v.clusters0<-variogram(g0, cross=FALSE,pseudo=-1, verbose=TRUE, covariogram=FALSE)
# plot(v.clusters0)
# 
# g0 = gstat(g0, model = vgm(psill=1, "Sph", range=40, nugget=2.2),fill.all=TRUE)
# g0.fit = fit.lmc(v.clusters0, g0, fit.ranges=TRUE)
# g0.fit
# plot(v.clusters0, g.fit0)
# 
# g.fit0 = fit.variogram(v.clusters0[which(v.clusters0$id=="VWC0"),],model=vgm(psill=1, "Sph", range=80, nugget=0))


save(v.clusters,file="mean vwc all days vs. ERT cluster variograms _ LL and HH indices reversed.RData")
load("mean vwc all days vs. ERT cluster variograms _ LL and HH indices reversed.RData")



load("mean vwc 0 days vs. ERT cluster variograms.RData")


# Create a data.frame with the output data for all of the variograms
var.parameters=data.frame(np=v.clusters$np,dist=v.clusters$dist,gamma=v.clusters$gamma,id=v.clusters$id)
var.parameters=var.parameters[16:45,] # cut off cross-variograms
load("mean vwc 3.33 days vs. ERT cluster variograms.RData")
var.parameters<-rbind(var.parameters,v.clusters[which(v.clusters$id=="VWC"),c(1:3,6)])
load("mean vwc 144 days vs. ERT cluster variograms.RData")
var.parameters<-rbind(var.parameters,v.clusters[which(v.clusters$id=="VWC"),c(1:3,6)])
load("mean vwc 147 days vs. ERT cluster variograms.RData")
var.parameters<-rbind(var.parameters,v.clusters[which(v.clusters$id=="VWC"),c(1:3,6)])
load("mean vwc 150 days vs. ERT cluster variograms.RData")
var.parameters<-rbind(var.parameters,v.clusters[which(v.clusters$id=="VWC"),c(1:3,6)])
var.parameters$id=c(rep("VWC 0 days", times=15),rep("ERT", times=15),rep("VWC 3.33 days", times=15), rep("VWC 144 days", times=15),
                    rep("VWC 147 days", times=15), rep("VWC 150 days", times=15))
var.parameters$id<-factor(var.parameters$id, levels=c("ERT","VWC 0 days", "VWC 3.33 days", "VWC 144 days","VWC 147 days","VWC 150 days"))

# create the variogram models to be used for later plotting (variogram parameters obtained from fit.lmc)
vgm.ert<-vgm(psill=2.592, "Sph", range=85.64, nugget=0.049)
vgm.vwc0<-vgm(psill=2.442, "Sph", range=88.70, nugget=0.0816)
vgm.vwc3<-vgm(psill=2.73, "Sph", range=89.52, nugget=0.133)
vgm.vwc144<-vgm(psill=2.46, "Sph", range=82.30, nugget=0.028)
vgm.vwc147<-vgm(psill=2.49, "Sph", range=95.57, nugget=0.027)
vgm.vwc150<-vgm(psill=2.80, "Sph", range=102.72, nugget=0)


vmodel.lines<-list()
vmodel.lines[[1]]<-variogramLine(vgm.ert,maxdist=100,n=50)
vmodel.lines[[2]]<-variogramLine(vgm.vwc0,maxdist=100,n=50)
vmodel.lines[[3]]<-variogramLine(vgm.vwc3,maxdist=100,n=50)
vmodel.lines[[4]]<-variogramLine(vgm.vwc144,maxdist=100,n=50)
vmodel.lines[[5]]<-variogramLine(vgm.vwc147,maxdist=100,n=50)
vmodel.lines[[6]]<-variogramLine(vgm.vwc150,maxdist=100,n=50)

require(data.table)

vmod<-rbindlist(vmodel.lines)
names(vmod)<-c("dist","gamma")
vmod$id<-c(rep("ERT", times=50),rep("VWC 0 days", times=50),rep("VWC 3.33 days", times=50), rep("VWC 144 days", times=50),
           rep("VWC 147 days", times=50), rep("VWC 150 days", times=50))
vmod$id<-factor(vmod$id, levels=c("ERT","VWC 0 days", "VWC 3.33 days", "VWC 144 days","VWC 147 days","VWC 150 days"))

require(ggplot2)

g<-ggplot(var.parameters, aes(x=dist, y=gamma))+
  facet_wrap(~id) +
  geom_point(pch=1, size =3) +
  geom_line(vmod,mapping=aes(x=dist,y=gamma), color="red")+
  xlab("Distance (cm)") +
  ylab(expression(paste("Semi-variance  ",gamma))) +
  theme_bw() + theme(axis.text=element_text(size=13), axis.title=element_text(size=13), strip.text=element_text(size=12))







# calculate co-dispersion (does not work with these cross-variograms, since they are flat)
vzy<-v.clusters$gamma[which(v.clusters$id=="ERT.VWC")]/sqrt((v.clusters$gamma[which(v.clusters$id=="ERT")]*
                                                               v.clusters$gamma[which(v.clusters$id=="VWC")]))


# Determine whether or not specific parameters are significantly different from the mean through t.test

# All variables were checked for normality with shapiro.test (shapiro-wilk test for normality) and were normally distributed
sill<-c(2.592,2.442,2.73,2.46,2.49,2.80)
range<-c(85.64,88.70,89.52,82.30,95.57,102.72)
nugget<-c(0.049,0.0816,0.133,0.028,0.027,0)

t.sill<-list()
t.range<-list()
t.nug<-list()

for(i in 1:6){
  t.sill[[i]]<-t.test(sill,rep(sill[i],times=6))
  t.range[[i]]<-t.test(range,rep(range[i],times=6))
  t.nug[[i]]<-t.test(nugget,rep(nugget[i],times=6))
}

# Indicator variograms ---------------------------------------------------------------------

#convert cluster data to binary indicators

#ERT
ERT.HH<-ERT
ERT.LL<-ERT
ERT.NS<-ERT
ERT.HH$log.resistivity[which(ERT.HH$fdr==4)]<-1
ERT.HH$log.resistivity[which(ERT.HH$fdr!=4)]<-0
ERT.LL$log.resistivity[which(ERT.LL$fdr==1)]<-1
ERT.LL$log.resistivity[which(ERT.LL$fdr!=1)]<-0
ERT.NS$log.resistivity[which(ERT.NS$fdr==0)]<-1
ERT.NS$log.resistivity[which(ERT.NS$fdr!=0)]<-0

g.ERT.ind=gstat(NULL,"ERT.HH", log.resistivity~1, ERT.HH)
g.ERT.ind=gstat(g.ERT.ind,"ERT.LL", log.resistivity~1, ERT.LL)
g.ERT.ind=gstat(g.ERT.ind,"ERT.NS", log.resistivity~1, ERT.NS)

v.ert.ind<-variogram(g.ERT.ind)
plot(v.ert.ind)

save(v.ert.ind,file="ERT indicator variograms with clusters.RData")
load("ERT indicator variograms with clusters.RData")

#VWC
VWC.HH<-VWC
VWC.LL<-VWC
VWC.NS<-VWC
VWC.HH$vwc[which(VWC.HH$fdr==4)]<-1
VWC.HH$vwc[which(VWC.HH$fdr!=4)]<-0
VWC.LL$vwc[which(VWC.LL$fdr==1)]<-1
VWC.LL$vwc[which(VWC.LL$fdr!=1)]<-0
VWC.NS$vwc[which(VWC.NS$fdr==0)]<-1
VWC.NS$vwc[which(VWC.NS$fdr!=0)]<-0

g.vwc.ind=gstat(NULL,"VWC.HH", vwc~1, VWC.HH)
g.vwc.ind=gstat(g.vwc.ind,"VWC.LL", vwc~1, VWC.LL)
g.vwc.ind=gstat(g.vwc.ind,"VWC.NS", vwc~1, VWC.NS)

v.vwc.ind<-variogram(g.vwc.ind)
plot(v.vwc.ind)

save(v.vwc.ind,file="VWC indicator variograms with clusters.RData")


# ERT indicators vs. VWC indicators --
require(gstat)

g.ertvwc.ind=gstat(NULL,"VWC.HH", vwc~1, VWC.HH)
g.ertvwc.ind=gstat(g.ertvwc.ind,"VWC.LL", vwc~1, VWC.LL)
g.ertvwc.ind=gstat(g.ertvwc.ind,"VWC.NS", vwc~1, VWC.NS)
g.ertvwc.ind=gstat(g.ertvwc.ind,"ERT.HH", log.resistivity~1, ERT.HH)
g.ertvwc.ind=gstat(g.ertvwc.ind,"ERT.LL", log.resistivity~1, ERT.LL)
g.ertvwc.ind=gstat(g.ertvwc.ind,"ERT.NS", log.resistivity~1, ERT.NS)

v.ertvwc.ind<-variogram(g.ertvwc.ind)
plot(v.ertvwc.ind)

save(v.ertvwc.ind,file="mean VWC 150 days v. ERT indicator variograms clusters.RData")



# Fit the variograms simultaneously with linear model of coregionalization (LMC) 
# LMC is used for bivariate cross-variogram fitting 

# Load the needed data
setwd("C:\\Users\\Matt\\Documents\\Norway\\Norway-version-control\\HYDRUS output and Cluster Analysis")
load("mean VWC 0 days v. ERT indicator variograms clusters.RData")

g.ertvwc.ind = gstat(g.ertvwc.ind, model =vgm(psill=0.15, "Exp", range=30),fill.all=TRUE)
g.fit = fit.lmc(v.ertvwc.ind, g.ertvwc.ind, fit.ranges=TRUE)
g.fit
plot(v.ertvwc.ind, g.fit)


# Mean VWC as a function of depth -----------------------------------------
library(data.table)

path<-("C:\\Users\\Matt\\Documents\\Norway\\Ruth clusters\\2016 experiment\\")
subdir<-("VWC clusters\\")

VWC0<-read.csv(paste(path,subdir,"LISA_mean data_0 days mean.csv",sep=""),header=TRUE,sep=",")
VWC3<-read.csv(paste(path,subdir,"LISA_mean data_3.33 days mean.csv",sep=""),header=TRUE,sep=",")
VWC144<-read.csv(paste(path,subdir,"LISA_mean data_144 days mean.csv",sep=""),header=TRUE,sep=",")
VWC147<-read.csv(paste(path,subdir,"LISA_mean data_147 days mean.csv",sep=""),header=TRUE,sep=",")
VWC150<-read.csv(paste(path,subdir,"LISA_mean data_150 days mean.csv",sep=""),header=TRUE,sep=",")

attach(VWC0)
VWC0 <- VWC0[order(-z),]
detach(VWC0)

attach(VWC3)
VWC3 <- VWC3[order(-z),]
detach(VWC3)

attach(VWC144)
VWC144 <- VWC144[order(-z),]
detach(VWC144)

attach(VWC147)
VWC147 <- VWC147[order(-z),]
detach(VWC147)

attach(VWC150)
VWC150 <- VWC150[order(-z),]
detach(VWC150)

id<-seq(0,-160,-5)
id<-id[-33]

out<-matrix(nrow=length(id), ncol=5)

for(i in 1:length(id)){
out[i,1]<-mean(VWC0$vwc[which(VWC0$z < id[i] & VWC0$z > id[i+1])])  
out[i,2]<-mean(VWC3$vwc[which(VWC3$z < id[i] & VWC3$z > id[i+1])]) 
out[i,3]<-mean(VWC144$vwc[which(VWC144$z < id[i] & VWC144$z > id[i+1])]) 
out[i,4]<-mean(VWC147$vwc[which(VWC147$z < id[i] & VWC147$z > id[i+1])]) 
out[i,5]<-mean(VWC150$vwc[which(VWC150$z < id[i] & VWC150$z > id[i+1])]) 
}


plotdf<-data.frame(vwc=c(out[,1],out[,2],out[,3],out[,4],out[,5]),depth=rep(id, times=5),
                   dataset=rep(c("VWC 0.40", "VWC 0.39", "VWC 0.38", "VWC 0.37", "VWC 0.35"), each=length(id)))

#pan <- data.frame( x = c(-Inf, Inf), y = -33, pan = factor(50))

plotdf$dataset = factor(plotdf$dataset, levels=c("VWC 0.40","VWC 0.39","VWC 0.38","VWC 0.37","VWC 0.35"))

vwc.d<-ggplot(plotdf,aes(x=vwc,y=depth)) +
  facet_wrap(~ dataset) +
  geom_path(lwd=1.2) +
  #geom_line(aes(x,y,linetype=pan), pan, lwd=1.5, color="brown") +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = -32, ymax = -25, fill = "brown", alpha = .4, color = NA) +
  xlab(expression(paste("Volumetric water content"," ", (m^3 ~ m^-3)))) +
  ylab("Depth (cm)") +
  scale_x_continuous(breaks=c(0.3,0.35,0.40)) +
  #scale_linetype_manual(labels = "plow pan", values = 1) +
  theme_bw() + theme(axis.text=element_text(size=13), axis.title=element_text(size=13),
                     strip.text=element_text(size=13))

require(cowplot)

plot_grid(plot_grid(vwc.d,pct.vwc,ncol=1, labels=c("a.","b.")),plot_grid(leg.vwc),
         ncol=1, rel_heights=c(1,0.1))
