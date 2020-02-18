# Read in the data
require(R.matlab)

path<-"C:\\Users\\Matt\\Documents\\Norway\\Water retention\\ku-pF\\Mingming corrected retentions Oct 2019"

mydata<-readMat(paste(path,"\\SKUWRC_kupF_PPE_WP4C_corrected.mat", sep=""))

# extract needed data


data<-list()
h.cm<-list()
theta.v<-list()

for (i in 1:(length(mydata$SKUWRC)/length(mydata$SKUWRC[,,1]))){

     data[[i]]<-mydata$SKUWRC[,,i][c(2,10,14)]
     h.cm[[i]]<-data[[i]]$h.cm
     theta.v[[i]]<-data[[i]]$theta.v
}

# Extract only lab data to check water retention curves -------------------

h.cm.lab<-list()
theta.v.lab<-list()
  for (i in 1:121){
  h.cm.lab[[i]]<-h.cm[[i]]
  theta.v.lab[[i]]<-theta.v[[i]]
}

# Write lab water retentions to file (if needed)

setwd(path)

h.cm.lab<-unlist(h.cm.lab)
theta.v.lab<-unlist(theta.v.lab)

write.table(h.cm.lab,"h lab only.txt")
write.table(theta.v.lab,"theta lab only.txt")

# Extract all data to data frame -------------------



mydata.par<-readMat(paste(path,"\\VG parmeters SKU lab and field corrected pF PPE WP4C.mat", sep=""))

par<-list()


for (i in 1:(length(mydata$SKUWRC)/length(mydata$SKUWRC[,,1]))){
  
 par[[i]]<-mydata.par$WRCPara[,,i]$vGx
  
}

df.par<-data.frame(t(data.frame(par)))
row.names(df.par)<-c(sapply(data,"[[",1))
colnames(df.par)<-c("thr","ths","alpha","n")


require(readxl)
Ks<-read_excel("C:\\Users\\Matt\\Documents\\Norway\\Ksat and K-6\\Compiled Ksat data - Attila Aug 2019.xlsx",
               sheet="Compiled", col_names=TRUE)


df.par$Ks<-Ks$`Ksat (cm/d)`
df.par$tau<-rep(0.5,times=130)



# exclude Ks values that are less than 1 cm/d (low outliers)
# idx<-which(Ks[,2]>1)
# df.par$Ks[idx]<-Ks$`Ksat (cm/d)`[idx]
# df.par$tau[idx]<-0.5


# Apply function Conductivity.R to determine conductivity curves -----------------
source("C:\\Users\\Matt\\Documents\\Clustering ERT paper\\Paper 2 - Geometry and Structure\\Submission codes and data\\Conductivity.R")

K<-list()
#K10<-1
#K30<-1
#K100<-1

for (i in 1:length(df.par$thr)){
  K[[i]]<-Conductivity(df.par[i,],h.cm[[i]])
  #K10[i]<-K[[i]][which(h.cm[[i]]==10)]
  #K30[i]<-K[[i]][which(h.cm[[i]]==30)]
  #K100[i]<-K[[i]][which(h.cm[[i]]==100)]
}



# Apply function Vogel.scale to determine scaling factors and scaled retention data  -------------
path<-("C:\\Users\\Matt\\Documents\\Norway\\Water retention\\Vogel scaling")

source(paste(path,"\\vscale.R",sep=""))


scaled<-Vogel.scale(h.cm,theta.v,K,df.par$Ks,df.par$ths,df.par$thr)


# Create animated figure of scaling process 

h.cm.plot<-h.cm # REMEMBER to "reset" the plot every time by re-applying these starting points
th.plot<-theta.v
hsc.plot<-list()
thsc.plot<-list()


tiff("scaled%02d.tiff")
for (i in 1:130) {
  h.cm.plot<-h.cm.plot[-1]
  th.plot<-th.plot[-1]
  hsc.plot[[i]]<-scaled$h.sc[[i]]
  thsc.plot[[i]]<-scaled$theta.sc[[i]]
  if (i!=130){
  par(mar=c(5,5,2,2))
  plot(log10(unlist(h.cm.plot)),unlist(th.plot), xlim=c(0,6.5),ylim=c(0,0.5), cex=1.3,
       xlab=expression(log10(h)), ylab=expression(theta), cex.axis=1.5, cex.lab=1.7)
  points(log10(unlist(hsc.plot)),unlist(thsc.plot), col="red")
  text(6,0.45, paste("i =", i, sep=" "), cex=1.5, col="red")
  } else {
    par(mar=c(5,5,2,2))
    plot(log10(unlist(hsc.plot)),unlist(thsc.plot), xlim=c(0,6.5),ylim=c(0,0.5),col="red",
         xlab=(expression(h/alpha[h])), ylab=(expression(theta/alpha[theta])), cex.axis=1.5, cex.lab=1.4)
    text(6,0.45, paste("i =", i, sep=" "), cex=1.5, col="red")
  }
  
}


dev.off()

# Create a .gif movie of the scaling procedure

library(magick)
frames<- paste0("scaled",sprintf('%02d',1:130),".tiff") # create a call to all images

m <- image_read(frames)
m <- image_animate(m, fps=10, loop=1)
image_write(m, "VSCALE.gif")



# Create static figures for the manuscript


plot(log10(unlist(scaled[[2]])),unlist(scaled[[3]]),
              xlab="scaled pressure potential (pF)",
              ylab="scaled VWC", col="red")


plot(log10(unlist(h.cm)),log10(unlist(K)),
     xlab="pressure potential (pF)",
     ylab="Hydraulic conductivity (log10 cm/d)")

plot(log10(unlist(scaled[[2]])), log10(unlist(scaled[[4]])),
     xlab="scaled pressure potential (pF)",
     ylab="scaled Hydraulic conductivity (log10 cm/d)")


scaled.th.lab<-unlist(scaled[[3]][1:122])
scaled.h.lab<-unlist(scaled[[2]][1:122])

scaled.th<-unlist(scaled[[3]])
scaled.h<-unlist(scaled[[2]])
scaled.K<-unlist(scaled[[4]])

write.table(scaled.th, "C:\\Users\\Matt\\Documents\\Norway\\Water retention\\Vogel scaling\\scaled theta.txt", sep="\t")
write.table(scaled.h, "C:\\Users\\Matt\\Documents\\Norway\\Water retention\\Vogel scaling\\scaled h.txt", sep="\t")
write.table(scaled.K, "C:\\Users\\Matt\\Documents\\Norway\\Water retention\\Vogel scaling\\scaled K.txt", sep="\t")
write.table(scaled.th.lab,"C:\\Users\\Matt\\Documents\\Norway\\Water retention\\Vogel scaling\\scaled theta lab.txt", sep="\t")
write.table(scaled.h.lab,"C:\\Users\\Matt\\Documents\\Norway\\Water retention\\Vogel scaling\\scaled h lab.txt", sep="\t")
#write.table(scaled.hKfit, "C:\\Users\\Matt\\Documents\\Norway\\Water retention\\Vogel scaling\\scaled hKfit.txt", sep="\t")

write.table(scaled$scaling.factors, "C:\\Users\\Matt\\Documents\\Norway\\Water retention\\Vogel scaling\\scaling factors.txt", sep="\t")



#check for normality
shapiro.test(log10(scaled$scaling.factors$a.h))
shapiro.test(scaled$scaling.factors$a.theta)

require(ggpubr)

ggqqplot(log10(scaled$scaling.factors$a.h))
ggqqplot(scaled$scaling.factors$a.theta)

# For fitting conductivity curve 
#scaled.hKfit<-unlist(h.scplot)
#scaled.Kfit<-unlist(K.scplot)


