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

# h.cm.lab<-list()
# theta.v.lab<-list()
#   for (i in 1:121){
#   h.cm.lab[[i]]<-h.cm[[i]]
#   theta.v.lab[[i]]<-theta.v[[i]]
# }
# 
# # Write lab water retentions to file (if needed)
# 
# setwd(path)
# 
# h.cm.lab<-unlist(h.cm.lab)
# theta.v.lab<-unlist(theta.v.lab)
# 
# write.table(h.cm.lab,"h lab only.txt")
# write.table(theta.v.lab,"theta lab only.txt")

# Extract all data to data frame -------------------



mydata.par<-readMat(paste(path,"\\VG parmeters SKU lab and field corrected pF PPE WP4C with bimodal.mat", sep=""))

vGpar<-list()
vGBpar<-list()
model.rmse<-list()

for (i in 1:(length(mydata$SKUWRC)/length(mydata$SKUWRC[,,1]))){
  
 vGpar[[i]]<-mydata.par$WRCPara[,,i]$vGx
 vGBpar[[i]]<-mydata.par$WRCPara[,,i]$vGBx
 model.rmse[[i]]<-mydata.par$WRCPara[,,i]$RMSE[7:8]
  
}



df.par<-data.frame(t(data.frame(vGpar)),t(data.frame(vGBpar)))
row.names(df.par)<-c(sapply(data,"[[",1))
colnames(df.par)<-c("thrvG","thsvG","alpha","n","thrvGB","thsvGB","alpha1","n1","w2","alpha2","n2")

# Create a data.frame with model fitting statistics, including AICc

fit.stat<-data.frame("rmse.vgb"=unlist(model.rmse)[c(FALSE,TRUE)],"rmse.vgu"=unlist(model.rmse)[c(TRUE, FALSE)])
fit.stat$n=c(20,19,20,21,21,19,21,21,21,21,21,20,21,20,20,20,20,20,20,20,21,21,21,19,21,20,20,21,21,20,21,21,21,20,
             20,21,19,20,20,20,22,21,21,20,20,21,21,21,20,22,21,20,22,21,19,21,22,21,21,20,21,21,21,22,21,21,21,21,
             21,22,22,19,20,20,21,19,20,21,20,20,20,20,20,20,22,21,21,21,21,22,21,21,20,21,21,21,21,21,21,21,22,21,
             22,21,20,21,21,21,21,21,22,21,21,21,22,21,21,21,21,21,21,2444,3864,1211,2766,3082,2665,1740,3415,3366)
fit.stat$sse.vgb=(fit.stat$rmse.vgb)^2*fit.stat$n
fit.stat$sse.vgu=(fit.stat$rmse.vgu)^2*fit.stat$n

#Calculate AICc for each model fit 

for (i in 1:130){
if(fit.stat$n[i]/7 <= 40){
fit.stat$AICc.vgb[i]= fit.stat$n[i]*log(fit.stat$sse.vgb[i]/fit.stat$n[i]) + 2*7 + (2*7*(7+1))/(n-7-1) #corrected Akaike criterion for small sample sizes, k = 7 for vgb
} else {
  fit.stat$AICc.vgb[i]= fit.stat$n[i]*log(fit.stat$sse.vgb[i]/fit.stat$n[i]) + 2*7
  }
if(fit.stat$n[i]/4 <= 40){
  fit.stat$AICc.vgu[i]= fit.stat$n[i]*log(fit.stat$sse.vgu[i]/fit.stat$n[i]) + 2*4 + (2*4*(4+1))/(n-4-1) # k = 4 for vgu
} else {
  fit.stat$AICc.vgu[i]= fit.stat$n[i]*log(fit.stat$sse.vgu[i]/fit.stat$n[i]) + 2*4
  }
}
 

# idx<-which(fit.stat$AICc.vgb>fit.stat$AICc.vgu) # identify which WRCs are better fit by vG1980 based on AICc



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

for (i in 1:length(df.par$thrvGB)){
  K[[i]]<-Conductivity(df.par[i,c(3,4,12,13)],h.cm[[i]])
  #K10[i]<-K[[i]][which(h.cm[[i]]==10)]
  #K30[i]<-K[[i]][which(h.cm[[i]]==30)]
  #K100[i]<-K[[i]][which(h.cm[[i]]==100)]
}



# Apply function Vogel.scale to determine scaling factors and scaled retention data  -------------
path<-("C:\\Users\\Matt\\Documents\\Norway\\Water retention\\Vogel scaling")

source(paste(path,"\\vscale.R",sep=""))


scaled<-Vogel.scale(h.cm,theta.v,K,df.par$Ks,c(df.par$thsvGB),c(df.par$thrvGB))


# Create animated figure of scaling process ---------------------------------------------- 

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

# create the .gif image

library(magick)
frames<- paste0("scaled",sprintf('%02d',1:130),".tiff") # create a call to all images

m <- image_read(frames)
m <- image_animate(m, fps=10, loop=1)
image_write(m, "VSCALE.gif")



# Create static figures for the manuscript



# Compare scaled plots of Unimodal vs. Bimodal fits

# par(mfrow=c(2,2))
# hist(scaled$scaling.factors$a.theta[idx], main="vG 1980 SFTH")
# hist(scaled$scaling.factors$a.theta[-idx], main="Durner 1994 SFTH")
# hist(scaled$scaling.factors$a.h[idx], main="vG 1980 SFH")
# hist(scaled$scaling.factors$a.h[-idx], main="Durner 1994 SFH")

par(mfrow=c(2,2)) # plot the unscaled, scaled, back-transformed correlation

btransh<-list() # back transform the data 
btransth<-list()
for (i in 1:130){
  btransh[[i]]<-scaled$h.sc[[i]]*scaled$scaling.factors$a.h[i]
  btransth[[i]]<-(scaled$theta.sc[[i]]*scaled$scaling.factors$a.theta[i])+df.par$thrvGB[i]
}

lm.h<-lm(log10(unlist(btransh)+1)~log10(unlist(h.cm)+1)) 
lm.th<-lm(unlist(btransth)~unlist(theta.v))


plot(log10(unlist(h.cm)),unlist(theta.v),
     xlab="pressure potential (pF)",
     ylab="VWC", col="black",
     main="Unscaled data")


plot(log10(unlist(scaled[[2]])),unlist(scaled[[3]]),
              xlab="scaled pressure potential (pF)",
              ylab="scaled VWC", col="black",
              main="Scaled data")

plot(log10(unlist(btransh)),log10(unlist(h.cm)),
  xlab="pressure potential * scaling factor",
  ylab="pressure potential unscaled", col="black",
  main="Back-transformed h")
  abline(lm.h, col="red")
  
  plot(unlist(btransth),unlist(theta.v),
       xlab="VWC scaled * scaling factor + theta_r",
       ylab="VWC unscaled", col="black", 
       main="Back-transformed theta")
  abline(lm.th, col="red")




# plot the unscaled and scaled hydraulic conductivity 


plot(log10(unlist(h.cm)),log10(unlist(K)),
     xlab="pressure potential (pF)",
     ylab="Hydraulic conductivity (log10 cm/d)")

plot(log10(unlist(scaled[[2]])), log10(unlist(scaled[[4]])),
     xlab="scaled pressure potential (pF)",
     ylab="scaled Hydraulic conductivity (log10 cm/d)")


# scaled.th.lab<-unlist(scaled[[3]][1:122])
# scaled.h.lab<-unlist(scaled[[2]][1:122])

scaled.th<-unlist(scaled[[3]])
scaled.h<-unlist(scaled[[2]])
scaled.K<-unlist(scaled[[4]])

write.table(scaled.th, "C:\\Users\\Matt\\Documents\\Norway\\Water retention\\Vogel scaling\\scaled theta bimodal.txt", sep="\t")
write.table(scaled.h, "C:\\Users\\Matt\\Documents\\Norway\\Water retention\\Vogel scaling\\scaled h bimodal.txt", sep="\t")
write.table(scaled.K, "C:\\Users\\Matt\\Documents\\Norway\\Water retention\\Vogel scaling\\scaled K bimodal.txt", sep="\t")
#write.table(scaled.th.lab,"C:\\Users\\Matt\\Documents\\Norway\\Water retention\\Vogel scaling\\scaled theta lab.txt", sep="\t")
#write.table(scaled.h.lab,"C:\\Users\\Matt\\Documents\\Norway\\Water retention\\Vogel scaling\\scaled h lab.txt", sep="\t")
#write.table(scaled.hKfit, "C:\\Users\\Matt\\Documents\\Norway\\Water retention\\Vogel scaling\\scaled hKfit.txt", sep="\t")

write.table(scaled$scaling.factors, "C:\\Users\\Matt\\Documents\\Norway\\Water retention\\Vogel scaling\\scaling factors bimodal.txt", sep="\t")



#check for normality in the 2016 scailing factors 
hist(log10(scaled$scaling.factors$a.h[80:130]))
hist(scaled$scaling.factors$a.theta[80:130])
shapiro.test(log10(scaled$scaling.factors$a.h[80:130]))
shapiro.test(scaled$scaling.factors$a.theta[80:130])

require(ggpubr)

ggqqplot(log10(scaled$scaling.factors$a.h[80:130]), conf.int=TRUE, conf.int.level=0.95)
ggqqplot(scaled$scaling.factors$a.theta[80:130], conf.int=TRUE, conf.int.level=0.95) 



## Create 2D plot showing how scaling factors are implemeneted in HYDRUS -------------------------------
top.th<-readMat("C:\\Users\\Matt\\Documents\\Norway\\Water retention\\Vogel scaling\\HYDRUS_top_layer_SFTH.mat")
top.h<-readMat("C:\\Users\\Matt\\Documents\\Norway\\Water retention\\Vogel scaling\\HYDRUS_top_layer_SFH.mat")
top.coords<-data.frame(top.th$top[,1:2]) # Z is not needed since it is only the top layer 
top.coords$SFTH<-top.th$top[,4]
top.coords$SFH<-top.h$top.h[,4]

names(top.coords)<-c("x","y","SFTH","SFH")

require(plotly)

p1 <- plot_ly(
  type="contour",
  x = top.coords$x,
  y = top.coords$y, 
  z = top.coords$SFTH, 
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


p2 <- plot_ly(
  type="contour",
  x = top.coords$x,
  y = top.coords$y, 
  z = top.coords$SFH, 
  showscale=FALSE,
  zauto=TRUE,
  zmin=0.8459,
  zmax=1.0825,
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



fig<-subplot(p1,p2)
fig %>% layout(annotations = list(
  list(x=0.2, y=1.05,text="SFTH", showarrow=F, xref='paper', yref='paper'),
  list(x=0.78, y=1.05, text="SFH", showarrow=F, xref='paper', yref='paper')
))



# reference curve parameters and model line
ref.curve<-c(0.0016,0.4034,0.0309,2.5058,0.9319,5.5595e-4,1.4701) #thr ths alpha1 n1 w2 alpha2 n2
# Determine the number of random samples that will be drawn from the resulting output (to conserve memory)
r.samp<-sample(1:dim(top.coords)[1],200,replace=FALSE)
h=seq(from=0, to=6, length.out=length(r.samp)) # create h to be the same length as the drawn samples (for plotting consistency)

# create a truncated normal distribution based on bounds of theta.r values
require(truncnorm)

thr.dist<-rtruncnorm(length(r.samp), a=-max(df.par$thrvGB), b=max(df.par$thrvGB), 
                     mean = mean(df.par$thrvGB), sd=sd(df.par$thrvGB))
thr.rand<-sample(thr.dist,length(r.samp),replace=TRUE)

th.h=(((1-ref.curve[5])*(1./(1+(ref.curve[3]*10^h)^ref.curve[4]))^(1-1/ref.curve[4]))+
        (ref.curve[5]*(1./(1+(ref.curve[6]*10^h)^ref.curve[7]))^(1-1/ref.curve[7])))*((ref.curve[2]-ref.curve[1])+ref.curve[1])

h.sc<-matrix(nrow=dim(top.coords)[1],ncol=length(r.samp))
th.sc<-matrix(nrow=dim(top.coords)[1],ncol=length(r.samp))
for (i in 1:dim(top.coords)[1]){
  h.sc[i,]<-(10^h)*top.coords$SFH[i]
  th.sc[i,]<-0.0016 + ((th.h-0.0016)*top.coords$SFTH[i]) 
  th.sc[i,]<-th.sc[i,] + thr.rand[i]  # random theta_r value for each 
  # This is how HYDRUS implements the scaling factor for theta (technical manual p. 50)
}



plotdf<-data.frame("SFTH"=rep(top.coords$SFTH[r.samp],times=length(r.samp)),"SFH"=rep(top.coords$SFH[r.samp],times=length(r.samp)),"x"=c(log10(h.sc[r.samp,])),"y"=c(th.sc[r.samp,]),
                   "idx"=rep(seq(1,length(r.samp)),times=length(r.samp)))

require(ggplot2)
require(scales)

b<-seq(from=0.8459, to=1.0825, length.out=10)
colors<-c(rgb(0,0,230,max=255), rgb(125,125,255,max=255),rgb(62,183,255,max=255),rgb(0,244,244,max=255),
          rgb(0,228,136,max=255),rgb(0,252,0,max=255),rgb(252,252,0,max=255),rgb(208,208,0,max=255),
          rgb(252,132,0,max=255),rgb(252,0,0,max=255),rgb(188,0,0,max=255),rgb(188,0,0,max=255))

g<-ggplot() +
    geom_point(data=plotdf,aes(x = x, y = y, color = idx))+
    xlim(0,6) +
    xlab(expression(log10(h^"*"*alpha[h]))) +
    ylab(expression(theta^"*"*alpha[theta])) +
    #scale_color_gradientn(colors=colors, breaks=b, limits=c(0.8459,1.0825), name="SF")+
    geom_line(aes(x=h,y=th.h),lwd=1.3) +
    theme_bw() + theme(axis.title=element_text(size=14), axis.text = element_text(size=13))
g

