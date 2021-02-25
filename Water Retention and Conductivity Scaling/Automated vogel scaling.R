# Read in the data
require(R.matlab)

path<-"C:\\Users\\Matt\\Documents\\Norway\\Water retention\\ku-pF\\Mingming corrected retentions Oct 2019"

mydata<-readMat(paste(path,"\\SKUWRC_kupF_PPE_WP4C_corrected.mat", sep=""))

# extract needed data


data<-list()
h.cm<-list()
theta.v<-list()
depth.cor<-list()

for (i in 1:(length(mydata$SKUWRC)/length(mydata$SKUWRC[,,1]))){

     data[[i]]<-mydata$SKUWRC[,,i][c(2,10,14,15)]
     h.cm[[i]]<-data[[i]]$h.cm
     theta.v[[i]]<-data[[i]]$theta.v
     depth.cor[[i]]<-data[[i]]$depth
}

depth.cor<-unlist(depth.cor) # depths of all core samples taken from the field

# Extract only lab and field data to check water retention curves -------------------

  h.cm.lab<-list()
  h.cm.field<-list()
  theta.v.lab<-list()
  theta.v.field<-list()
    for (i in 1:83){
    h.cm.lab[[i]]<-h.cm[[i]]
    theta.v.lab[[i]]<-theta.v[[i]]
    }
  
  for (i in 122:130){
    h.cm.field[[i-121]]<-h.cm[[i]]
    theta.v.field[[i-121]]<-theta.v[[i]]
  }

# # Write lab water retentions to file (if needed)
#  
#  setwd(path)
#  
#  lab<-cbind(unlist(h.cm.lab),unlist(theta.v.lab))
#   
#  write.table(lab,"WRC lab only.txt",quote=FALSE, col.names=FALSE,row.names=FALSE)

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
fit.stat$AICc.vgb[i]= fit.stat$n[i]*log(fit.stat$sse.vgb[i]/fit.stat$n[i]) + 2*7 + (2*7*(7+1))/(fit.stat$n-7-1) #corrected Akaike criterion for small sample sizes, k = 7 for vgb
} else {
  fit.stat$AICc.vgb[i]= fit.stat$n[i]*log(fit.stat$sse.vgb[i]/fit.stat$n[i]) + 2*7
  }
if(fit.stat$n[i]/4 <= 40){
  fit.stat$AICc.vgu[i]= fit.stat$n[i]*log(fit.stat$sse.vgu[i]/fit.stat$n[i]) + 2*4 + (2*4*(4+1))/(fit.stat$n[i]-4-1) # k = 4 for vgu
} else {
  fit.stat$AICc.vgu[i]= fit.stat$n[i]*log(fit.stat$sse.vgu[i]/fit.stat$n[i]) + 2*4
  }
}
 

#idx<-which(fit.stat$AICc.vgb>fit.stat$AICc.vgu) # identify which WRCs are better fit by vG1980 based on AICc



require(readxl)
Ks<-read_excel("C:\\Users\\Matt\\Documents\\Norway\\Ksat and K-6\\Compiled Ksat data - Attila Aug 2019.xlsx",
               sheet="Compiled", col_names=TRUE)


df.par$Ks<-Ks$`Ksat (cm/d)`
df.par$tau<-rep(0.5,times=130)



# exclude Ks values that are less than 1 cm/d (low outliers)
# idx<-which(Ks[,2]>1)
# df.par$Ks[idx]<-Ks$`Ksat (cm/d)`[idx]
# df.par$tau[idx]<-0.5


# Apply function "Ku" from package "Soil HyP" to determine conductivity curves -----------------
require(SoilHyP)

#modify data.frame to work with SoilHyP format
par<-data.frame(df.par[,5:13])
names(par)<-c("thr","ths","alfa","n","w2","alfa2","n2","Ks","tau")


K<-list()


for (i in 1:length(df.par$thrvGB)){
  K[[i]]<-Ku(h.cm[[i]],FUN.shp="vGM",par.shp=par[i,],modality="bi",suc.negativ=FALSE)
}

# If only lab or field values are wanted 
K.lab<-list()
h.cm.lab<-list()
theta.v.lab<-list()

K.field<-list()
h.cm.field<-list()
theta.v.field<-list()

for(i in 1:121){
h.cm.lab[[i]]<-h.cm[[i]]
theta.v.lab[[i]]<-theta.v[[i]]
K.lab[[i]] <- K[[i]]
}

for(i in 122:130){
  h.cm.field[[i-121]]<-h.cm[[i]]
  theta.v.field[[i-121]]<-theta.v[[i]]
  K.field[[i-121]] <- K[[i]]
}


# plot lab-only data
g<-ggplot() +
  geom_point(aes(x=log10(unlist(h.cm.lab)),y=unlist(theta.v.lab)), pch=21, size=2) +
  xlab("log |h| (cm)") +
  ylab(expression(paste("Volumetric Water Content ", ~(cm^3 ~ cm^-3)))) +
  theme_bw() + theme(axis.text=element_text(size=14), axis.title=element_text(size=14))

# plot field-only data

g<-ggplot() +
  geom_point(aes(x=log10(unlist(h.cm.field)),y=unlist(theta.v.field)), pch=21, size=2) +
  xlab("log |h| (cm)") +
  ylab(expression(paste("Volumetric Water Content ", ~(cm^3 ~ cm^-3)))) +
  theme_bw() + theme(axis.text=element_text(size=14), axis.title=element_text(size=14))

# Apply function Vogel.scale to determine scaling factors and scaled retention data  -------------
path<-("C:\\Users\\Matt\\Documents\\Norway\\Water retention\\Vogel scaling")

source(paste(path,"\\vscale.R",sep=""))


scaled<-Vogel.scale(h.cm,theta.v,K,df.par$Ks,c(df.par$thsvGB),c(df.par$thrvGB))

scaled.lab<-Vogel.scale(h.cm.lab,theta.v.lab,K.lab,df.par$Ks[1:121],c(df.par$thsvGB[1:121]),c(df.par$thrvGB[1:121]))

scaled.field<-Vogel.scale(h.cm.field,theta.v.field,K.field,df.par$Ks[122:130],c(df.par$thsvGB[122:130]),c(df.par$thrvGB[122:130]))

# Fit the scaled water retention and hydraulic conductivity with SoilHyP (WARNING! THIS TAKES ABOUT 30 MIN. CHECK THE SAVED FILE)

ans <- fitSHP(obs = list(th = unlist(scaled$theta.sc), K = unlist(scaled$K.sc)),
              suc = list(th = unlist(scaled$h.sc)+1, K = unlist(scaled$h.sc)+1),
              FUN.shp = 'vg',
              modality = 'bi',
              par.shp = NULL,
              fit = 'both',
              weighting = 'var',
              log = c('alfa', 'n', 'ks','alfa2','n2'),
              control = list(ncomplex = 15, reltol = 1e-07,tolsteps = 6),
              suc.negativ = FALSE,
              integral = FALSE,
              log_Ku=TRUE
)
ref.par<-data.frame(ans$par)

# LAB ONLY

ans.lab <- fitSHP(obs = list(th = unlist(scaled.lab$theta.sc), K = unlist(scaled.lab$K.sc)),
              suc = list(th = unlist(scaled.lab$h.sc)+1, K = unlist(scaled.lab$h.sc)+1),
              FUN.shp = 'vg',
              modality = 'bi',
              par.shp = NULL,
              fit = 'both',
              weighting = 'var',
              log = c('alfa', 'n', 'ks','alfa2','n2'),
              control = list(ncomplex = 15, reltol = 1e-07,tolsteps = 6),
              suc.negativ = FALSE,
              integral = FALSE,
              log_Ku=TRUE
)
ref.par<-data.frame(ans.lab$par)

# FIELD ONLY

ans.field <- fitSHP(obs = list(th = unlist(scaled.field$theta.sc), K = unlist(scaled.field$K.sc)),
                  suc = list(th = unlist(scaled.field$h.sc)+1, K = unlist(scaled.field$h.sc)+1),
                  FUN.shp = 'vg',
                  modality = 'bi',
                  par.shp = NULL,
                  fit = 'both',
                  weighting = 'var',
                  log = c('alfa', 'n', 'ks','alfa2','n2'),
                  control = list(ncomplex = 15, reltol = 1e-07,tolsteps = 6),
                  suc.negativ = FALSE,
                  integral = FALSE,
                  log_Ku=TRUE
)
ref.par<-data.frame(ans.field$par)




# Save fitting output to avoid long fitting times
save(ans, file="WRC fit output.RData")
save(ans.lab, file="WRC fit output lab only.RData")
save(ans.field, file="WRC fit output field only.RData")

# Save model parameters to file

path<-("C:\\Users\\Matt\\Documents\\Norway\\Norway-version-control\\Water Retention and Conductivity Scaling")
write.table(round(ref.par,digits=3),
            file=paste(path,"\\Reference curve parameters bimodal K and WRC.txt", sep=""),
            append=FALSE,quote=FALSE,col.names=TRUE,row.names=FALSE)



# load in data (if needed)
load('C:\\Users\\Matt\\Documents\\Norway\\Water retention\\ku-pF\\Mingming corrected retentions Oct 2019\\WRC fit output.RData')



# get model prediction lines for the scaled data to add to later plots
model.lines<-predict(ans,suc=10^seq(-1,6.5,by=0.25),suc.negativ=FALSE)
model.lines.lab<-predict(ans.lab,suc=10^seq(0,6.5,by=0.25),suc.negativ=FALSE)

mod.lines.all<-rbind(model.lines,model.lines.lab)
mod.lines.all$label<-c(rep("All data", times=27),rep("Laboratory only", times=27))


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



# Create static figures for the manuscript -----------------------------



# Compare scaled plots of Unimodal vs. Bimodal fits

#par(mfrow=c(2,2))
#hist(scaled$scaling.factors$a.theta[idx], main="vG 1980 SFTH")
#hist(scaled$scaling.factors$a.theta[-idx], main="Durner 1994 SFTH")
#hist(scaled$scaling.factors$a.h[idx], main="vG 1980 SFH")
#hist(scaled$scaling.factors$a.h[-idx], main="Durner 1994 SFH")

par(mfrow=c(1,2)) # plot the unscaled, scaled, back-transformed correlation

btransh<-list() # back transform the data 
btransth<-list()
for (i in 1:130){
  btransh[[i]]<-scaled$h.sc[[i]]*scaled$scaling.factors$a.h[i]
  btransth[[i]]<-(scaled$theta.sc[[i]]*scaled$scaling.factors$a.theta[i])+df.par$thrvGB[i]
}

lm.h<-lm(log10(unlist(btransh)+1)~log10(unlist(h.cm)+1)) 
lm.th<-lm(unlist(btransth)~unlist(theta.v))


require(ggplot2)

idx<-lapply(h.cm,length) # length of each water retention curve
idx<-unlist(idx)
idx.lab<-sum(idx[1:121])
idx.field<-sum(idx[122:130])
plotdf<-data.frame(h=unlist(h.cm),theta=unlist(theta.v), label=c(rep("Laboratory", times=idx.lab), rep("Field",times=idx.field)),
                   depth=rep(depth.cor,times=idx))
idx.labapp<-which(plotdf$h>1000) # laboratory appended data to field data
plotdf$label[idx.labapp]<-"Laboratory"


# plot just the water retentions for cores at 10 cm depth or less

top<-plotdf[which(plotdf$depth>=-10 & plotdf$label=="Laboratory"),] # n = 24 core samples
top$depth=as.character(top$depth) 

g<-ggplot(top, aes(x=log10(h), y=theta)) +
  geom_point(pch=21, size=3) +
  xlab("log |h| (cm)")+
  ylab(expression('Volumetric water content' ~ ' ' ~ (cm^3 * cm^-3))) +
  annotate("text", x = 5.8, y = 0.55, label = "N = 24", size=8) +
  theme_bw() +
  theme(axis.text=element_text(size=14),axis.title=element_text(size=12),
        legend.position=c(.8,.9), legend.title=element_blank(), legend.text=element_text(size=14))


# Plot the unscaled water retention and hydraulic conductivity 

idxs<-lapply(scaled[[2]],length)
idxs<-unlist(idxs)
idxs.lab<-sum(idxs[1:121])
idxs.field<-sum(idxs[122:130])
plotdfs<-data.frame(scaledh=unlist(scaled[[2]]),scaledtheta=unlist(scaled[[3]]), label=c(rep("Laboratory", times=idx.lab), rep("Field",times=idx.field)))
idxs.labapp<-which(plotdfs$scaledh>1000) # laboratory appended data to field data
plotdfs$label[idxs.labapp]<-"Laboratory"

g1<-ggplot(plotdf,aes(x=log10(h), y=theta,color=label, shape=label))+
  geom_point(size=3)+
  scale_shape_manual(values=c(21,22))+
  xlab("Pressure potential (pF)")+
  ylab(expression('Volumetric water content' ~ ' ' ~ (cm^3 * cm^-3))) +
  scale_color_brewer(palette="Dark2") +
  theme_bw() +
  theme(axis.text=element_text(size=12),axis.title=element_text(size=12),
                     legend.position=c(.8,.9), legend.title=element_blank(), legend.text=element_text(size=14))

g2<-ggplot(plotdfs,aes(x=log10(scaledh), y=scaledtheta,color=label))+
  geom_point(aes(shape=label),size=3) +
  scale_shape_manual(values=c(21,22))+
  geom_line(data=model.lines, aes(x=log10(-suc),y=SWC), color="black", lwd=1.3)+
  scale_color_brewer(palette="Dark2") +
  #geom_line(data=mod.lines.all, aes(x=log10(-suc),y=SWC,color=label), lwd=1.3)+
  # scale_color_manual(values=cols)+
  xlab("Pressure potential (pF)")+
  ylab(expression('Scaled Volumetric water content' ~ ' ' ~ (cm^3 * cm^-3))) +
  theme_bw() + theme(axis.text=element_text(size=12),axis.title=element_text(size=12)) +
               theme(legend.position="none")

# IF TWO LINES ARE USED
#cols <- c("Laboratory and Field" = "red", "Laboratory only" = "blue")
#p + scale_colour_manual(values = cols)




# plot(log10(unlist(btransh)),log10(unlist(h.cm)),
#   xlab="pressure potential * scaling factor",
#   ylab="pressure potential unscaled", col="black",
#   main="Back-transformed h")
#   abline(lm.h, col="red")
#   
#   plot(unlist(btransth),unlist(theta.v),
#        xlab="VWC scaled * scaling factor + theta_r",
#        ylab="VWC unscaled", col="black", 
#        main="Back-transformed theta")
#   abline(lm.th, col="red")




# plot the unscaled and scaled hydraulic conductivity 

idx<-lapply(K,length)
idx<-unlist(idx)
idx.lab<-sum(idx[1:121])
idx.field<-sum(idx[122:130])
plotdf<-data.frame(h=unlist(h.cm),K=unlist(K), label=c(rep("Laboratory", times=idx.lab), rep("Field",times=idx.field)))  

idxs<-lapply(scaled[[4]],length)
idxs<-unlist(idxs)
idxs.lab<-sum(idxs[1:121])
idxs.field<-sum(idxs[122:130])
plotdfs<-data.frame(scaledh=unlist(scaled[[2]]),scaledK=unlist(scaled[[4]]), label=c(rep("Laboratory", times=idxs.lab), rep("Field",times=idxs.field)))


g3<-ggplot(plotdf,aes(x=log10(h), y=log10(K) ,color=label, shape=label))+
    geom_point(size=3)+
    scale_shape_manual(values=c(21,22))+
     xlab("Pressure potential (pF)")+
     ylab(expression('Hydraulic conductivity' ~ ' ' ~ '(' ~ log[10] ~ 'cm/d)'))+
     scale_color_brewer(palette="Dark2") +
      theme_bw() + theme(axis.text=element_text(size=14),axis.title=element_text(size=12),
                         legend.position="none")

g4<-ggplot(plotdfs,aes(x=log10(scaledh), y=log10(scaledK) ,color=label))+
  geom_point(aes(shape=label), size=3) +
  scale_shape_manual(values=c(21,22))+
  geom_line(data=model.lines, aes(x=log10(-suc),y=log10(Ku)),color="black",lwd=1.3)+
  scale_color_brewer(palette="Dark2") +
  #geom_line(aes(x=log10(-model.lines.lab$suc),y=log10(model.lines.lab$Ku)),color="blue",lwd=1.3)+
      xlab("Scaled Pressure potential (pF)")+
      ylab(expression('Scaled hydraulic conductivity' ~ ' ' ~ '(' ~ log[10] ~ 'cm/d)'))+
      theme_bw() + theme(axis.text=element_text(size=14),axis.title=element_text(size=12), 
                         legend.position="none")


# Create a single plot with all scaled and unscaled data

cowplot::plot_grid(
  cowplot::plot_grid(
    g1,g2,g3,g4, labels=c("a.","b.","c.","d.")))
#cowplot::get_legend(g2 + theme(legend.position = "bottom", legend.text=element_text(size=14), legend.title=element_blank())),ncol=1, rel_heights=c(.99, 0.01))



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



## Create plots showing the measured water retentions to the kriged,scaled water retentions -----------------------

# import the data with georeferenced scaling factors for the cores and sensors
library(R.matlab)

# Scaling factors derived using Vogel et al. (1991)
SF.orig<-read.table("C:\\Users\\Matt\\Documents\\Norway\\Norway-version-control\\Water Retention and Conductivity Scaling\\scaling factors bimodal.txt")
SFH.orig<-SF.orig$a.h[84:130]
SFTH.orig<-SF.orig$a.theta[84:130]

# Scaling factors after kriging

mydata<-readMat("C:/Users/Matt/Documents/Norway/Norway-version-control/Water Retention and Conductivity Scaling/2016_georef_scalingfactors.mat")

SFH<-data.frame(c(mydata$cores.SFH,mydata$sensors.SFH),c(rep("core",times=38),rep("sensor",times=20)))
idx=c(4,6,7,8,9,11,12,13,16,17,18) # sensors NOT used to derive scaling factors
SFH<-SFH[-(38+idx),]
names(SFH)<-c("sf","type")

SFTH<-data.frame(c(mydata$cores.SFTH,mydata$sensors.SFTH),c(rep("core",times=38),rep("sensor",times=20)))
SFTH<-SFTH[-(38+idx),]
names(SFTH)<-c("sf","type")

# load in the reference curve parameters and predict the water retention curve for the reference curve

load('C:\\Users\\Matt\\Documents\\Norway\\Water retention\\ku-pF\\Mingming corrected retentions Oct 2019\\WRC fit output.RData')
# get model prediction lines for the scaled data to add to later plots
model.lines<-list()
h<-list()
th<-list()
for (i in 1:47){
model.lines[[i]]<-predict(ans,suc=h.cm[[i+83]],suc.negativ=FALSE)
}

# multiply each value of h and theta on the reference curve by the associated scaling factors
for(i in 1:47){
h[[i]]<-model.lines[[i]]$suc
th[[i]]<-model.lines[[i]]$SWC
}

sc.h<-list()
sc.th<-list()
  
for(i in 1:nrow(SFH)){
  sc.h[[i]]<-h[[i]]*SFH[i,1]
  sc.th[[i]]<-th[[i]]*SFTH[i,1]
}

# isolate theta_s
th.s.sc<-list()
for(i in 1:38){
  th.s.sc[[i]]<-sc.th[[i]][1]
}


# Compare to measured data (load in data at the top of this script and extract 2016 datapoints)

l.idx<-lapply(h.cm,length)
l.idx<-unlist(l.idx[84:130])
core.id<-paste("core",as.character(seq(1,38,1)))
sensors.id<-paste("sensor",as.character(c(1,2,3,5,10,14,15,19,20)))

# isolate th_s
th.s.meas<-list()
for(i in 1:38){
  th.s.meas[[i]]<-theta.v[[i+83]][1]
}

# compare difference between th_s for measured vs. scaled and kriged 
diff<-unlist(th.s.meas)-unlist(th.s.sc)
diff<-data.frame("th_s difference"=diff,"id"=core.id)

dat.2016<-data.frame("h"=unlist(h.cm[c(84:130)]),"theta"=unlist(theta.v[c(84:130)]),"id"=c(rep(core.id,l.idx[1:38]),rep(sensors.id,l.idx[39:47])))
# re-arrange "id" in the right order
dat.2016$id<-factor(dat.2016$id, levels=c(core.id,sensors.id))


plotdf<-data.frame("sch"= -unlist(sc.h), "scth" = unlist(sc.th), "id"=c(rep(core.id,l.idx[1:38]),rep(sensors.id,l.idx[39:47])))
plotdf$id<-factor(plotdf$id, levels=c(core.id,sensors.id))


library(ggplot2)

g<-ggplot() +
  facet_wrap(~id)+
    geom_point(data=dat.2016,aes(x=log10(h), y=theta, color="Measured")) +
    geom_line(data=plotdf, aes(x=log10(sch), y=scth, color="Scaled"), lwd=1.4) +
    scale_color_manual(values=c("Measured"= "black","Scaled" = "red")) +
    labs(x="log pressure potential (cm)", y="Volumetric water content")+
  theme_bw() +
  theme(legend.title=element_blank(), legend.position=c(0.9,0.05),
        legend.background=element_blank(), legend.text=element_text(size=13),
        strip.text=element_text(size=12), axis.text=element_text(size=13),axis.title=element_text(size=13)) 


# Calculate the RMSE for each of the measured vs. scaled retentions
library(Metrics)
err<-list()
for(i in 1:38){
err[[i]]<-rmse(dat.2016$theta[which(dat.2016$id==core.id[i])],plotdf$scth[which(plotdf$id==core.id[i])])
}
for(j in 39:47){
err[[j]]<-rmse(dat.2016$theta[which(dat.2016$id==sensors.id[j-38])],plotdf$scth[which(plotdf$id==sensors.id[j-38])])
}
err<-unlist(err)

# plot RMSE w.r.t depth and cluster


depth<-c(7.5,-22.5,-42.5,-60,-22.5,-42.5,-60,-7.5,-42.5,-7.5,-60,-22.5,-35,-67,-92.5,-92.5,-35,-35,-7.5,-7.5,-22.5,
-60.5,-42.5,-22.5,-42.5,-92.5,-42.5,-22.5,-32,-60.5,-80,-7.5,-60.5,-22.5,-60.5,-32,-42.5,-7.5,-10,-40,-60,-25,-25,-25,-45,-10,-10)

cluster<-c("NS","HH","HH","NS","HH","HH","NS","NS","NS","HH","LL","HH","HH","NS","NS",
           "LL","NS","NS","HH","HH","NS","NS","HH","HH","HH","NS","NS","HH","NS","NS","NS",
           "HH","NS","HH","NS","NS","NS","HH","NS","NS","LL","NS","HH","HH","HH","NS","HH")
dist<-c(1.0973,1.0332,0.8383,0.8560,0.9606,0.9831,1.4005,0.8594,0.9916,1.0998,0.8925,0.5488,1.3953,
        1.0799,0.8471,0.6212,1.3177,0.9042,1.5816,0.8604,0.8474,1.0771,1.0245,0.8979,0.9245,1.1710,
        0.6541,1.1151,0.8207,1.0290,0.5960,0.5288,0.3692,0.9694,0.8077,1.4367,1.1838,0.9084,0.7370,
        0.9487,0.8807,1.0084,0.8507,0.8508,0.4865,0.7256,1.0551)
w2.param<-c(0.588,0.278,0.957,0.856,0.621,0.654,0.944,0.641,0.495,0.632,0.890,0.558,0.708,0.901,0.617,0.836,
          0.656,0.656,0.547,0.639,0.704,0.865,0.599,0.545,0.821,0.829,0.843,0.991,0.662,0.904,0.806,0.594,
          0.923,0.813,0.999,0.415,0.983,0.587,0.960,0.966,0.995,0.926,0.884,0.921,0.955,0.883,0.868)
w1.param<-1-w2.param

errplot<-data.frame("err"=err, "depth"= depth, "ERTcluster"=cluster, "dist"= dist, "w2"=w2.param, "w1"=w1.param) # dist = distance between kriging grid point and coordinates



g2<-ggplot(errplot, aes(x=ERTcluster, y=err)) +
    geom_boxplot(varwidth=TRUE,fill="grey") + # boxes are drawn with widths proportional to the square-roots of the number of observations in the groups (thinner = less observations)
    labs(x="ERT cluster", y="Root mean square error meas WRC v. scaled WRC") +
  theme_bw() + theme(axis.text=element_text(size=13),axis.title=element_text(size=13))

g3<-ggplot(errplot, aes(x=dist, y=err, color=cluster, shape=cluster))+
  geom_point(size=2) +
  labs(x="Distance between measured point and kriging grid node (cm)", y="Root mean square error")+
  theme_bw() + theme(legend.position=c(0.9,0.9), legend.title=element_blank(), legend.background=element_blank(),
                     axis.text=element_text(size=13), axis.title=element_text(size=13))

g4<-ggplot(errplot, aes(x=err,y=depth, color=cluster, shape=cluster))+
  geom_point(size=2) +
  labs(x="Root mean square error", y="Depth from surface (cm)") +
  theme_bw() + theme(legend.position=c(0.9,0.9), legend.title=element_blank(), legend.background=element_blank(),
                     axis.text=element_text(size=13), axis.title=element_text(size=13))

SF.comp<-data.frame("SF before kriging"=c(SFH.orig,SFTH.orig), "SF after kriging"=c(SFH$sf,SFTH$sf), "id"=rep(c(core.id,sensors.id),times=2),
                    "type"=rep(c("Pressure potential scaling factor","Water content scaling factor"),each=47),
                    "depth"=rep(depth,times=2))
SF.diff<-data.frame("diff"= c( (abs(SFH.orig-SFH$sf)/((SFH.orig + SFH$sf)/2)*100), (abs(SFTH.orig-SFTH$sf)/((SFTH.orig + SFTH$sf)/2)*100)), 
                    "RMSE" = rep(errplot$err, times=2), "type"=rep(c("Pressure potential scaling factor", "Water content scaling factor"), each=47))

g5<-ggplot(SF.comp, aes(x=SF.before.kriging, y=SF.after.kriging))+
  facet_wrap(~type)+
  geom_point(pch=21,size=2) +
  lims(y=c(0,3.5), x=c(0,3.5)) +
  geom_abline(slope=1,intercept=0, lwd=1.2) +
  labs(x="Scaling factors before kriging", y="Scaling factors after kriging") +
  theme_bw() + theme(axis.text=element_text(size=13),axis.title=element_text(size=13),strip.text=element_text(size=13))

g6<-ggplot(errplot, aes(x=w1, y=err, color=cluster, shape=cluster))+
  geom_point(size=2)+
  labs(x="w1 parameter", y="Root mean square error") +
  theme_bw() + theme(axis.text=element_text(size=13),axis.title=element_text(size=13),legend.title=element_blank(),
                     legend.background=element_blank(), legend.position=c(0.9,0.9))

g7<-ggplot(SF.diff, aes(x=RMSE, y=diff)) +
  facet_wrap(~type) +
  geom_point(pch=21, size=2) +
  labs(x="Root mean square error", y="Percentage change after kriging") +
  theme_bw() + theme(axis.text=element_text(size=13),axis.title=element_text(size=13), strip.text=element_text(size=13))
  

cowplot::plot_grid(g2,g3,g4,g6, ncol=2, labels=c("a.","b.","c.","d."))

cowplot::plot_grid(g5,g7,nrow=2, labels=c("a.","b."))

## Create 2D contour plot showing how scaling factors are implemeneted in HYDRUS -------------------------------
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



# reference curve parameters and model line ------------------------
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
  th.sc[i,]<-ref.curve[1] + ((th.h-ref.curve[1])*top.coords$SFTH[i]) 
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

