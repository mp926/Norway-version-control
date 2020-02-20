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
         xlab=(expression(log10(h/alpha[h]))), ylab=(expression(theta/alpha[theta])), cex.axis=1.5, cex.lab=1.4)
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



## Create 2D plot showing how scaling factors are implmeneted in HYDRUS -------------------------------
top<-readMat("C:\\Users\\Matt\\Documents\\Norway\\Water retention\\Vogel scaling\\HYDRUS_top_layer_SFTH.mat")
top.coords<-data.frame(top$top[,1:2]) # Z is not needed since it is only the top layer 
top.coords$SFTH<-top$top[,4]

names(top.coords)<-c("x","y")

library(plotly)

p <- plot_ly(
  type="contour",
  x = top.coords$x,
  y = top.coords$y, 
  z = top.SFTH, 
  zauto=FALSE,
  zmin=0.913,
  zmax=1.079,
  colorscale = list(c(0,0.0909,0.1818,0.2727,0.3636,0.4545,0.5454,0.6363,0.7272,0.8181,0.9090,1),
                    c("rgb(0,0,230)", "rgb(125,125,255)","rgb(62,183,255)","rgb(0,244,244)",
                      "rgb(0,228,136)","rgb(0,252,0)","rgb(252,252,0)","rgb(208,208,0)",
                      "rgb(252,132,0)","rgb(252,0,0)","rgb(188,0,0)","rgb(188,0,0)")),
  autocontour = F,
  contours = list(
    start = 0.913,
    end = 1.079,
    size = 0.015
  )
)

p


# reference curve parameters and model line
ref.curve<-c(7.52E-8,0.3866,8.1052E-4,1.3723) #thr ths alpha n
# Determine the number of random samples that will be drawn from the resulting output (to conserve memory)
r.samp<-sample(1:dim(top.coords)[1],100,replace=FALSE)
h=seq(from=0, to=10000, length.out=length(r.samp)) # create h to be the same length as the drawn samples (for plotting consistency)

th.h=(1/(1+(ref.curve[3]*h)^ref.curve[4])^(1-1/ref.curve[4])) *((ref.curve[2]-ref.curve[1])+ref.curve[1])


h.sc<-matrix(nrow=dim(top.coords)[1],ncol=length(r.samp))
th.sc<-matrix(nrow=dim(top.coords)[1],ncol=length(r.samp))
for (i in 1:dim(top.coords)[1]){
  h.sc[i,]<-h*top.SFTH[i]
  th.sc[i,]<-th.h*top.SFTH[i]
}



plotdf<-data.frame("SF"=rep(top.coords$SFTH[r.samp],times=length(r.samp)),"order"=rep(top.coords$order[r.samp],times=length(r.samp)),"x"=c(log10(h.sc[r.samp,])),"y"=c(th.sc[r.samp,]))

require(ggplot2)
require(scales)

b<-c(0.913,0.928,0.943,0.958,0.973,0.988,1.003,1.019,1.034,1.049,1.064,1.079)
colors<-c(rgb(0,0,230,max=255), rgb(125,125,255,max=255),rgb(62,183,255,max=255),rgb(0,244,244,max=255),
          rgb(0,228,136,max=255),rgb(0,252,0,max=255),rgb(252,252,0,max=255),rgb(208,208,0,max=255),
          rgb(252,132,0,max=255),rgb(252,0,0,max=255),rgb(188,0,0,max=255),rgb(188,0,0,max=255))

g<-ggplot() +
    geom_point(data=plotdf,aes(x = x, y = y, color = SF))+
    xlim(1.5,4) +
    xlab(expression(log10(h)*alpha[h])) +
    ylab(expression(theta*alpha[theta])) +
    scale_color_gradientn(colors=colors, breaks=b, limits=c(0.913,1.079))+
    geom_line(aes(x=log10(h),y=th.h),lwd=1.3) +
    theme_bw() + theme(axis.title=element_text(size=14), axis.text = element_text(size=13))


