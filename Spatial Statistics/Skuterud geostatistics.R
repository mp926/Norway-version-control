library(gstat)
library(sp)
library(ggplot2)
library(dplyr)


## LOAD IN ENVIRONMENT DATA (FOR CONSISTENCY AND NO FURTHER RANDOM SAMPLING) ----------------

# NS maintained
cwd<-("C:\\Users\\Matt\\Documents\\Norway\\Norway-version-control")
path<-paste(cwd,"/Spatial Statistics", sep="")
setwd(path)
load(paste(path,"/SKUVarioNSmaintained.RData", sep=""))


# NS not maintained

load(paste(path,"/SKUVarioNSnotmaintained.RData", sep=""))



## LOAD IN THE DATA IF ENVIRONMENT DATA IS NOT AVAILABLE (or if needed) --------------
 


path<-("C:\\Users\\Matt\\Documents\\Norway\\SGeMS files")

# Scaling factors

df<-data.frame(read.table(paste(path,"\\Skuterud bimodal scaling factors K outliers removed April 30 2020.dat", sep=""), sep="", skip=8))

require(readxl)
Ks<-read_excel("C:\\Users\\Matt\\Documents\\Norway\\Ksat and K-6\\Compiled Ksat data - Attila Aug 2019.xlsx",
               sheet="Compiled", col_names=TRUE)


df$Ks<-Ks$`Ksat (cm/d)` 


names(df)<-c("x","y","z","sfh","sfth","sfK","Ks")

# if you want to use the previously selected random index
cwd<-("C:\\Users\\Matt\\Documents\\Norway\\Norway-version-control")
path<-paste(cwd,"/Spatial Statistics", sep="")
setwd(path)
load(paste(path,"/random index selection.RData", sep=""))

# if new random selection is desired
rand.id<-sample(seq(1,1000,1),50,replace=FALSE) #Take a random sampling of 50 variograms


# Bootstrap the variograms for SFTheta SFh and Ks by removing a random spatial data point  ---------------------------------------------------
# 
# #Set up the bootstrapping
# 
# n=1000 #Draw 50 random variogram model parameters from near the mean value for each parameter after 1000 runs 
# r.data<-length(df)*0.1 # percent of data to be removed from the base dataframe
# rand.id=matrix(data=NA,nrow=r.data,ncol=n)
# df.adj<-list()
# coords.adj<-list()
# vth<-list()
# vh<-list()
# vK<-list()
# init.rangeth<-data.frame()
# init.rangeh<-data.frame()
# init.rangeK<-data.frame()
# vth.fitE<-list()
# vth.fitS<-list()
# vh.fit<-list()
# vK.fit<-list()
# SSEth.Sph<-data.frame()
# SSEth.Exp<-data.frame()
# SSEh<-data.frame()
# SSEK<-data.frame()
# vmodel.linesth<-list()
# vmodel.linesh<-list()
# vmodel.linesK<-list()
# 
# # Perform the bootstrapping
# 
# for(i in 1:n){
# rand.id[,i]=sample(1:130,r.data,replace=TRUE)
# df.adj[[i]]<-df[-rand.id[,i],] # adjusted data.frame with data removed
# coordinates(df.adj[[i]])<- ~x+y+z #convert each data.frame to geoObject for gstat
# 
# 
# vth[[i]]<-variogram(sfth~1,df.adj[[i]], width=13, cutoff=60) #calculate the variograms for each iteration
# vh[[i]]<-variogram(sfh~1,df.adj[[i]], width=13, cutoff=60)
# #vK[[i]]<-variogram(Ks~1,df.adj[[i]], cutoff=60)
# ridxth<-first((which(vth[[i]]$gamma>(var(df.adj[[i]]$sfth)-(var(df.adj[[i]]$sfth)*0.15)))))
# ridxh<-first((which(vh[[i]]$gamma>(var(df.adj[[i]]$sfh)-(var(df.adj[[i]]$sfh)*0.15)))))
# #ridxK<-first((which(vK[[i]]$gamma[2:length(vK[[i]]$gamma)]>(var(df.adj[[i]]$Ks)-(var(df.adj[[i]]$Ks)*0.15))))) 
# # The above range idx finds the lowest gamma value that is larger than the variance (sill) - 15% of the variance
# # ridxK is modified because the first value is extremely high, so we skip it
# init.rangeth[i,1]<-vth[[i]][ridxth,]$dist #This pulls out the initial guess for range
# init.rangeh[i,1]<-vh[[i]][ridxh,]$dist
# #init.rangeK[i,1]<-vK[[i]][ridxK,]$dist
# 
# vth.fitE[[i]]<-fit.variogram(vth[[i]], vgm(psill=var(df.adj[[i]]$sfth), "Exp", range=init.rangeth[i,1],
#                                         nugget=vth[[i]]$gamma[1]/1.2)) #fit each variogram
# 
# vth.fitS[[i]]<-fit.variogram(vth[[i]], vgm(psill=var(df.adj[[i]]$sfth), "Sph", range=init.rangeth[i,1],
#                                            nugget=vth[[i]]$gamma[1]/1.2)) 
# 
# vh.fit[[i]]<-fit.variogram(vh[[i]], vgm(psill=var(df.adj[[i]]$sfh), "Sph", range=init.rangeh[i,1],
#                                        nugget=vh[[i]]$gamma[1]/1.2))
# 
# #vK.fit[[i]]<-fit.variogram(vK[[i]], vgm(psill=var(df.adj[[i]]$Ks), "Lin", range=init.rangeK[i,1],
#                                         #nugget=vK[[i]]$gamma[1]/1.2),fit.method=6) # fitted with OLS
# 
# SSEth.Exp[i,1]<-attr(vth.fitE[[i]],"SSErr") #obtain the sum square errors of each fitted variogram
# SSEth.Sph[i,1]<-attr(vth.fitS[[i]], "SSErr")
# SSEh[i,1]<-attr(vh.fit[[i]],"SSErr")
# #SSEK[i,1]<-attr(vh.fit[[i]],"SSErr")
# 
# vmodel.linesth[[i]]<-variogramLine(vth.fitE[[i]],maxdist=60,n=200) # simulate model line output for plotting
# vmodel.linesh[[i]]<-variogramLine(vh.fit[[i]],maxdist=60,n=200)
# #vmodel.linesK[[i]]<-variogramLine(vK.fit[[i]],maxdist=100,n=200)
# }
# 
# # compare correlation between the Exponential and Spherical fit for theta
# plot(SSEth.Exp[,1],SSEth.Sph[,1])
# abline(0,1)
# 
# 
# # Plot the bootstrapped variogram model confidence intervals
# 
# 
# dist.th<-vmodel.linesth[[1]]$dist # The distance values are constant
# dist.h<-vmodel.linesh[[1]]$dist
# #dist.K<-vmodel.linesK[[1]]$dist
# 
# vals.th<-matrix(nrow=n,ncol=200)
# vals.h<-matrix(nrow=n,ncol=200)
# #vals.K<-matrix(nrow=n,ncol=200)
# error90th<-1
# error95th<-1
# error99th<-1
# error90h<-1
# error95h<-1
# error99h<-1
# #error90K<-1
# #error95K<-1
# #error99K<-1
# 
# for (i in 1:n){
#   for (j in 1:length(vmodel.linesth[[i]]$dist)){
#   
#   vals.th[i,]<-vmodel.linesth[[i]]$gamma
#   vals.h[i,]<-vmodel.linesh[[i]]$gamma
#   #vals.K[i,]<-vmodel.linesK[[i]]$gamma 
#  error90th[j] <- qnorm(0.950)*sd(vals.th[,j])/sqrt(n) # 90% confidence interval calculation
#  error95th[j] <- qnorm(0.975)*sd(vals.th[,j])/sqrt(n) # 95% confidence interval calculation
#  error99th[j] <- qnorm(0.995)*sd(vals.th[,j])/sqrt(n) # 99% confidence interval calculation
#  error90h[j] <- qnorm(0.950)*sd(vals.h[,j])/sqrt(n)
#  error95h[j] <- qnorm(0.975)*sd(vals.h[,j])/sqrt(n)
#  error99h[j] <- qnorm(0.995)*sd(vals.h[,j])/sqrt(n)
#  # error90K[j] <- qnorm(0.950)*sd(vals.K[,j])/sqrt(n)
#  # error95K[j] <- qnorm(0.975)*sd(vals.K[,j])/sqrt(n) 
#  # error99K[j] <- qnorm(0.995)*sd(vals.K[,j])/sqrt(n) 
#   
#   
#   }
# }
# 
# 
# coordinates(df)<- ~x+y+z  # This transforms the data.frame. If you want to use df again, you must reload the data.frame into the environment
# vth.orig=variogram(sfth~1,df, width=13, cutoff=60) #cutoff = distance where np first decreases
# vth.orig.fit<-fit.variogram(vth.orig,vgm(psill=0.06,"Exp",range=45, nugget=0.10),fit.ranges=FALSE)
# origmodel.lines.th<-variogramLine(vth.orig.fit,maxdist=60,n=200)
# 
# vh.orig=variogram(sfh~1,df, width=13, cutoff=60) #cutoff = distance where np first decreases
# vh.orig.fit<-fit.variogram(vh.orig,vgm(psill=0.12,"Sph",range=20, nugget=0.08))
# origmodel.lines.h<-variogramLine(vh.orig.fit,maxdist=60,n=200)
# 
# #vK.orig=variogram(Ks~1,df, cutoff=60) #cutoff = distance where np first decreases
# #vK.orig.fit<-fit.variogram(vK.orig,vgm(psill=100000,"Exp",range=1, nugget=300000), fit.method=6) #OLS
# #origmodel.lines.K<-variogramLine(vK.orig.fit,maxdist=100,n=200)
# 
# 
# 
# plot(vth.orig$dist,vth.orig$gamma, xlim=c(0,60),ylim=c(0,0.019), pch=16,
#      ylab=expression(gamma), xlab="lag distance (cm)", main=expression("Variogram" ~ SF[theta]))
# lines(origmodel.lines.th$dist,origmodel.lines.th$gamma, col="red")
# lines(dist.th,origmodel.lines.th$gamma-error90th, lty=2, lwd=2)
# lines(dist.th,origmodel.lines.th$gamma+error90th, lty=2, lwd=2)
# lines(dist.th,origmodel.lines.th$gamma-error95th, lty=5, lwd=2)
# lines(dist.th,origmodel.lines.th$gamma+error95th, lty=5, lwd=2)
# lines(dist.th,origmodel.lines.th$gamma-error99th, lty=6, lwd=2)
# lines(dist.th,origmodel.lines.th$gamma+error99th, lty=6, lwd=2)
# legend("bottomright", legend=c("90% C.I.", "95% C.I.", "99% C.I."), lty=c(2,5,6))
# 
# 
#  
# plot(vh.orig$dist,vh.orig$gamma, xlim=c(0,60),ylim=c(0,0.25), pch=16,
#      ylab=expression(gamma), xlab="lag distance (cm)", main=expression("Variogram" ~ SF[h]))
# lines(origmodel.lines.h$dist,origmodel.lines.h$gamma, col="red")
# lines(dist.h,origmodel.lines.h$gamma-error90h, lty=2, lwd=2)
# lines(dist.h,origmodel.lines.h$gamma+error90h, lty=2, lwd=2)
# lines(dist.h,origmodel.lines.h$gamma-error95h, lty=5, lwd=2)
# lines(dist.h,origmodel.lines.h$gamma+error95h, lty=5, lwd=2)
# lines(dist.h,origmodel.lines.h$gamma-error99h, lty=6, lwd=2)
# lines(dist.h,origmodel.lines.h$gamma+error99h, lty=6, lwd=2)
# legend("bottomright", legend=c("90% C.I.", "95% C.I.", "99% C.I."), lty=c(2,5,6))
# 
# # plot(vK.orig$dist,vK.orig$gamma, pch=16, ylim=c(0,1000000),
# #      ylab=expression(gamma), xlab="lag distance (cm)", main=expression("Variogram" ~ K[s] ~ "[cm/d]"))
# # lines(origmodel.lines.K$dist,origmodel.lines.K$gamma, col="red")
# # lines(dist.K,origmodel.lines.K$gamma-error90K, lty=2, lwd=2)
# # lines(dist.K,origmodel.lines.K$gamma+error90K, lty=2, lwd=2)
# # lines(dist.K,origmodel.lines.K$gamma-error95K, lty=5, lwd=2)
# # lines(dist.K,origmodel.lines.K$gamma+error95K, lty=5, lwd=2)
# # lines(dist.K,origmodel.lines.K$gamma-error99K, lty=6, lwd=2)
# # lines(dist.K,origmodel.lines.K$gamma+error99K, lty=6, lwd=2)
# # legend("topright", legend=c("90% C.I.", "95% C.I.", "99% C.I."), lty=c(2,5,6))
# 
# 
# 
# # Histograms of gamma values from all simulations
# 
# hist(vals.th)
# hist(vals.h)
# 
# 
# 
# # save the global environment in case this specific bootstrap output is needed again 
# cwd<-("C:\\Users\\Matt\\Documents\\Norway\\Norway-version-control")
# path<-paste(cwd,"/Spatial Statistics", sep="")
# setwd(path)
# save.image(file="SKUvarioEnvironment.RData")




# Add random error to the variograms by bootstrapping the gamma values only (n:S ratio not maintained) -----

# We will simulate variogram models, based on a fitting of the original data,
# and then increase the nugget parameter by a random amount taken from a uniform distribution

# SFTHETA --

n=1000 # 1000 realizations 
coordinates(df)<- ~x+y+z  # This transforms the data.frame. If you want to use df again, you must reload the data.frame into the environment

# fit the original variogram data with the model with lowest SSE 
vth.orig=variogram(sfth~1,df, width=13, cutoff=60) #cutoff = distance where np first decreases
vth.orig.fit<-list()
mods<-c("Nug","Lin","Sph","Exp","Gau")
SSE.orig<-matrix(nrow=length(mods), ncol=2)
for (i in 1:length(mods)){
  if (mods[i]=="Nug"){
vth.orig.fit[[i]]<-fit.variogram(vth.orig,vgm(psill=0.06,mods[i],range=0, nugget=0.10),fit.ranges=FALSE)
  }else{
    vth.orig.fit[[i]]<-fit.variogram(vth.orig,vgm(psill=0.06,mods[i],range=45, nugget=0.10),fit.ranges=FALSE)
  }
  SSE.orig[i,1]<-attr(vth.orig.fit[[i]],"SSE")
  SSE.orig[i,2]<-levels(vth.orig.fit[[i]]$model)[vth.orig.fit[[i]]$model[2]]
}

best.fit.th<-c(min(SSE.orig[,1]),SSE.orig[which.min(SSE.orig[,1]),2])
fit.id.th<-which.min(SSE.orig[,1])
print(best.fit.th)

origmodel.lines.th<-variogramLine(vth.orig.fit[[fit.id.th]],maxdist=60,n=100) # create a line of the variogram model for plotting


# Extract the variogram model parameters from the original fit
orig.sill=vth.orig.fit[[fit.id.th]]$psill[2] + vth.orig.fit[[fit.id.th]]$psill[1]
orig.nug=vth.orig.fit[[fit.id.th]]$psill[1]
orig.rng=vth.orig.fit[[fit.id.th]]$range[2]

ns=vth.orig.fit[[fit.id.th]]$psill[1]/(vth.orig.fit[[fit.id.th]]$psill[2]+vth.orig.fit[[fit.id.th]]$psill[1])


# Add random error to the nugget value for n realizations pulled from a uniform distribution, with replacement (bootstrapping)

rand.err<-sample(runif(1000,min=0.9,max=1.1),replace=TRUE)

rand.nug<-matrix(nrow=n)
vth.rand<-list()
vth.modlines<-list()
ns.rand<-data.frame()
for (i in 1:n){
  rand.nug[i,]<-orig.nug*rand.err[i]
  vth.rand[[i]]<-vgm(psill=orig.sill-rand.nug[i,],mods[fit.id.th],range=orig.rng,nugget=rand.nug[i,]) #total sill must be equal to original
  vth.modlines[[i]]<-variogramLine(vth.rand[[i]],maxdist=60,n=100)
  ns.rand[i,1]<-vth.rand[[i]]$psill[1]/(vth.rand[[i]]$psill[2] + vth.rand[[i]]$psill[1])
}



require(ggplot2) # Plot the resulting variograms
require(data.table)


dfmod<-matrix(nrow=n*100,ncol=2)
for(i in 1:n){
  dfmod[,1]<-rep(t(vth.modlines[[1]]$dist), times=n)
  dfmod[seq(1,100000,100)[i]:seq(100,100000,100)[i],2]<-t(vth.modlines[[i]]$gamma)
}

dfmod<-as.data.frame(dfmod)
names(dfmod)<-c("dist","gamma")
dfmod$samp<-rep(paste("sample",as.character(seq(1,n,1))),each=100)


modlist = list()
for (i in 1:length(rand.id)) {
  modlist[[i]] <- subset(dfmod, samp==paste("sample", rand.id[i], sep=" "))
}
dfmodrand<-rbindlist(modlist) 

g1<-ggplot(vth.orig,aes(x=dist,y=gamma)) +  # plot the variograms
  geom_point() +
  geom_line(origmodel.lines.th, mapping=aes(x=dist,y=gamma), color="black", lwd=1.5) +
  geom_line(dfmodrand,mapping=aes(x=dist,y=gamma,color=samp)) +
  xlab("Distance (cm)") +
  ylab(expression(gamma)) +
  ggtitle("Random nugget SFth") +
  theme_bw() + theme(axis.text=element_text(size=14), axis.title=element_text(size=14),
                     legend.position="")

g1




# SFH ---

vh.orig=variogram(sfh~1,df, width=13, cutoff=60) #cutoff = distance where np first decreases
vh.orig.fit<-list()
mods<-c("Nug","Lin","Sph","Exp","Gau")
SSE.orig<-matrix(nrow=length(mods), ncol=2)

for (i in 1:length(mods)){
  if (mods[i]=="Nug"){
    vh.orig.fit[[i]]<-fit.variogram(vh.orig,vgm(psill=0.12,mods[i],range=0, nugget=0.08))
  }else{
    vh.orig.fit[[i]]<-fit.variogram(vh.orig,vgm(psill=0.12,mods[i],range=20, nugget=0.08))
  }
  SSE.orig[i,1]<-attr(vh.orig.fit[[i]],"SSE")
  SSE.orig[i,2]<-levels(vh.orig.fit[[i]]$model)[vh.orig.fit[[i]]$model[2]]
}

best.fit.h<-c(min(SSE.orig[,1]),SSE.orig[which.min(SSE.orig[,1]),2])
fit.id.h<-which.min(SSE.orig[,1])
print(best.fit.h)

origmodel.lines.h<-variogramLine(vh.orig.fit[[fit.id.h]],maxdist=60,n=100) # create a line of the variogram model for plotting


# Extract the variogram model parameters from the original fit
orig.sill=vh.orig.fit[[fit.id.h]]$psill[2] + vh.orig.fit[[fit.id.h]]$psill[1]
orig.nug=vh.orig.fit[[fit.id.h]]$psill[1]
orig.rng=vh.orig.fit[[fit.id.h]]$range[2]

ns=vh.orig.fit[[fit.id.h]]$psill[1]/(vh.orig.fit[[fit.id.h]]$psill[2]+vh.orig.fit[[fit.id.h]]$psill[1])


rand.nug<-matrix(nrow=n)
vh.rand<-list()
vh.modlines<-list()
ns.rand<-data.frame()
for (i in 1:n){
  rand.nug[i,]<-orig.nug*rand.err[i]
  vh.rand[[i]]<-vgm(psill=orig.sill-rand.nug[i,],mods[fit.id.h],range=orig.rng,nugget=rand.nug[i,]) #total sill must be equal to original
  vh.modlines[[i]]<-variogramLine(vh.rand[[i]],maxdist=60,n=100)
  ns.rand[i,1]<-vh.rand[[i]]$psill[1]/(vh.rand[[i]]$psill[2] + vh.rand[[i]]$psill[1])
}


dfmod<-matrix(nrow=n*100,ncol=2)
for(i in 1:n){
  dfmod[,1]<-rep(t(vh.modlines[[1]]$dist), times=n)
  dfmod[seq(1,100000,100)[i]:seq(100,100000,100)[i],2]<-t(vh.modlines[[i]]$gamma)
}

dfmod<-as.data.frame(dfmod)
names(dfmod)<-c("dist","gamma")
dfmod$samp<-rep(paste("sample",as.character(seq(1,n,1))),each=100)


modlist = list()
for (i in 1:length(rand.id)) {
  modlist[[i]] <- subset(dfmod, samp==paste("sample", rand.id[i], sep=" "))
}
dfmodrand<-rbindlist(modlist) 

g2<-ggplot(vh.orig,aes(x=dist,y=gamma)) +  # plot the variograms
  geom_point() +
  geom_line(origmodel.lines.h, mapping=aes(x=dist,y=gamma), color="black", lwd=1.5) +
  geom_line(dfmodrand,mapping=aes(x=dist,y=gamma,color=samp)) +
  xlab("Distance (cm)") +
  ylab(expression(gamma)) +
  ggtitle("Random nugget SFh") +
  theme_bw() + theme(axis.text=element_text(size=14), axis.title=element_text(size=14),
                     legend.position="")

g2


#combine plots

require(ggpubr)

ggarrange(g1 + theme(legend.position=""),g2 + theme(legend.position=""), labels=c("A","B"), ncol=2, nrow=1)


# save the global environment so that you have access to all randomized components

cwd<-("C:\\Users\\Matt\\Documents\\Norway\\Norway-version-control")
 path<-paste(cwd,"/Spatial Statistics", sep="")
 setwd(path)
 save.image(file="SKUVarioNSnotmaintained.RData")



# Add random error to the variograms with increased nugget (maintain n:s ratio) ---------------------

# --------------------------------- water content ---

rand.err<-sample(runif(1000,min=1,max=8),replace=TRUE) # Do bootstrap sampling for error from a uniform distribution
coordinates(df)<- ~x+y+z  # This transforms the data.frame. If you want to use df again, you must reload the data.frame into the environment
vth.orig=variogram(sfth~1,df, width=13, cutoff=60) #cutoff = distance where np first decreases
vth.orig.fit<-fit.variogram(vth.orig,vgm(psill=0.06,"Exp",range=45, nugget=0.10),fit.ranges=FALSE)
origmodel.lines.th<-variogramLine(vth.orig.fit,maxdist=60,n=200) # create a line of the variogram model for plotting
origmodel.lines.th$type=rep("None",times=200)

ns=vth.orig.fit$psill[1]/(vth.orig.fit$psill[2]+vth.orig.fit$psill[1])

n=1000
rand.err<-sample(runif(1000,min=1,max=8),replace=TRUE) # Do bootstrap sampling for error from a uniform distribution

gam.err=matrix(nrow=length(rand.err),ncol=length(vth.orig$gamma))
for (i in 1:n){
  gam.err[i,]<-vth.orig$gamma*rand.err[i]
}


# Change the variograms to accomodate the new error values 
vth.err<-list()
for (i in 1:n){
  vth.err[[i]]<-vth.orig
  vth.err[[i]]$gamma=gam.err[i,]
}


vth.err.fit<-list()
#vth.err.mod<-list()
error.lines.th<-list()
ns.err<-matrix(ncol=n)
for (i in 1:n){
  vth.err.fit[[i]]<-fit.variogram(vth.err[[i]], vgm(psill=max(vth.err[[i]]$gamma),"Exp",range=45, nugget=max(vth.err[[i]]$gamma)*ns), fit.ranges=FALSE)
  #vth.err.mod[[i]]<-vgm(psill=vth.err.fit[[i]]$psill[2]-nug,"Exp",range=45,nugget=vth.err.fit[[i]]$psill[1])
  error.lines.th[[i]]<-variogramLine(vth.err.fit[[i]],maxdist=60,n=200)
  ns.err[i]<-vth.err.fit[[i]]$psill[1]/(vth.err.fit[[i]]$psill[2]+vth.err.fit[[i]]$psill[1])
}


error.lines.th<-as.data.frame(error.lines.th)
require(data.table)
m<-melt(error.lines.th) # melt the variogram line data
vgmline.data<-subset(m, grepl("gamma", variable)) # subset the gamma values
vgmline.data$dist=rep(error.lines.th$dist, times=n) # add the repeated distance values
vgmline.data$samp=rep(paste("sample", seq(1,n,1)),each=200)


vario.data<-data.frame(dist=rep(vth.orig$dist,times=n+1), gamma=c(vth.orig$gamma,t(gam.err)),
                       samp=c(rep("orig",times=5),rep(paste("sample", seq(1,n,1)),each=5)))


datalist = list()
modlist = list()
for (i in 1:length(rand.id)) {
  datalist[[i]] <- subset(vario.data, samp==paste("sample", rand.id[i], sep=" "))
  modlist[[i]] <- subset(vgmline.data, samp==paste("sample", rand.id[i], sep=" "))
}
dfrand<-rbindlist(datalist)
dfmodrand<-rbindlist(modlist)



g1<-ggplot(dfrand,aes(x=dist,y=gamma, color=samp)) +  # plot the variograms
  geom_point() +
  geom_line(data=dfmodrand,aes(x=dist,y=value, color=samp)) +
  xlab("Distance (cm)") +
  ylab(expression(gamma)) +
  ggtitle("N:S maintained SFth") +
  theme_bw() + theme(axis.text=element_text(size=14), axis.title=element_text(size=14),
                     legend.position="")
g1

# -------------------------- pressure potential ---

vh.orig=variogram(sfh~1,df, width=13, cutoff=60) #cutoff = distance where np first decreases
vh.orig.fit<-fit.variogram(vh.orig,vgm(psill=0.12,"Sph",range=20, nugget=0.08))
origmodel.lines.h<-variogramLine(vh.orig.fit,maxdist=60,n=200)
ns=vh.orig.fit$psill[1]/(vh.orig.fit$psill[2]+vh.orig.fit$psill[1])


gam.err=matrix(nrow=length(rand.err),ncol=length(vh.orig$gamma))
for (i in 1:n){
  gam.err[i,]<-vh.orig$gamma*rand.err[i]
}


# Change the variograms to accomodate the new error values 
vh.err<-list()
for (i in 1:n){
  vh.err[[i]]<-vh.orig
  vh.err[[i]]$gamma=gam.err[i,]
}


vh.err.fit<-list()
vh.err.mod<-list()
error.lines.h<-list()
ns.err<-matrix(ncol=n)
for (i in 1:n){
  vh.err.fit[[i]]<-fit.variogram(vh.err[[i]], vgm(psill=max(vh.err[[i]]$gamma),"Sph",range=20))
  nug=max(vh.err.fit[[i]]$psill)*ns
  vh.err.mod[[i]]<-vgm(psill=vh.err.fit[[i]]$psill-nug,"Sph",range=20,nugget=nug)
  error.lines.h[[i]]<-variogramLine(vh.err.mod[[i]],maxdist=60,n=200)
  ns.err[i]<-vh.err.mod[[i]]$psill[1]/(vh.err.mod[[i]]$psill[2]+vh.err.mod[[i]]$psill[1])
}


error.lines.h<-as.data.frame(error.lines.h)
m<-melt(error.lines.h) # melt the variogram line data
vgmline.data<-subset(m, grepl("gamma", variable)) # subset the gamma values
vgmline.data$dist=rep(error.lines.h$dist, times=n) # add the repeated distance values
vgmline.data$samp=rep(paste("sample", seq(1,n,1)),each=200)


vario.data<-data.frame(dist=rep(vh.orig$dist,times=n+1), gamma=c(vh.orig$gamma,t(gam.err)),
                       samp=c(rep("orig",times=5),rep(paste("sample", seq(1,n,1)),each=5)))


datalist = list()
modlist = list()
for (i in 1:length(rand.id)) {
  datalist[[i]] <- subset(vario.data, samp==paste("sample", rand.id[i], sep=" "))
  modlist[[i]] <- subset(vgmline.data, samp==paste("sample", rand.id[i], sep=" "))
}
dfrand<-rbindlist(datalist)
dfmodrand<-rbindlist(modlist)



g2<-ggplot(dfrand,aes(x=dist,y=gamma, color=samp)) +  # plot the variograms
  geom_point() +
  geom_line(data=dfmodrand,aes(x=dist,y=value, color=samp)) +
  xlab("Distance (cm)") +
  ylab(expression(gamma)) +
  ggtitle("N:S maintained SFh") +
  theme_bw() + theme(axis.text=element_text(size=14), axis.title=element_text(size=14),
                     legend.position="")
g2




# plot both figures on the same page


ggarrange(g1 + theme(legend.position=""),g2 + theme(legend.position=""), labels=c("A","B"), ncol=2, nrow=1)


# save the global environment so that you have access to all randomized components

cwd<-("C:\\Users\\Matt\\Documents\\Norway\\Norway-version-control")
path<-paste(cwd,"/Spatial Statistics", sep="")
setwd(path)
save.image(file="SKUVarioNSmaintained.RData")



# PERFORM GLOBAL ORDINARY KRIGING ON THE DATASETS ---------------------------

# LOAD IN RESULTS THAT WERE ALREADY CALCULATED

load("C:\\Users\\Matt\\Documents\\Norway\\Norway-version-control\\Spatial Statistics\\myobjects")




# IF YOU WANT TO START FROM THE BEGINNING

path<-("C:\\Users\\Matt\\Documents\\Norway\\SGeMS files")

df<-data.frame(read.table(paste(path,"\\Skuterud bimodal scaling factors K outliers removed April 30 2020.dat", sep=""), sep="", skip=8))

require(readxl)
Ks<-read_excel("C:\\Users\\Matt\\Documents\\Norway\\Ksat and K-6\\Compiled Ksat data - Attila Aug 2019.xlsx",
               sheet="Compiled", col_names=TRUE)


df$Ks<-Ks$`Ksat (cm/d)` 


names(df)<-c("x","y","z","sfh","sfth","sfK","Ks")



# Extract the data to be kriged and change the y dimension 
df.sku2016<-df[84:130,]
df.sku2016$y<-df.sku2016$y-200
coordinates(df.sku2016)= ~x+y+z

# Create an empty Krig grid

range.yz <- seq(from = 0, to = 100, length.out = 50) # Define the grid dimensions and cell size
range.x <-seq(from = 0, to = 200, length.out = 100)
grid3D <- expand.grid(x = range.x, y = range.yz, z = range.yz) # create the grid 
gridded(grid3D) = ~x+y+z



# FOR N:S MAINTAINED


res3D<-list()
for (i in 1:length(rand.id)){
  if(i==1){
res3D[[i]] <- krige(formula = sfth ~ 1, df.sku2016, grid3D, model = vth.orig.fit,nsim=0,maxdist=Inf,nmax=Inf,nmin=0) 
  } else {
    res3D[[i]] <- krige(formula = sfth ~ 1, df.sku2016, grid3D, model = vth.err.fit[[rand.id[i-1]]],nsim=0, maxdist=Inf,nmin=0)
  }
}


est3D<-as.data.frame(res3D)


require(lattice)


levelplot(var1.pred ~ x + y | z, as.data.frame(res3D),
          main=paste("Original data fitting", "mod=Exp", ""))

levelplot(var1.pred.1 ~ x + y | z, as.data.frame(res3D),
          main=paste("Realization", as.character(rand.id[1]), "error", 
                     as.character(round(rand.err[rand.id[1]] *100)),"%",sep=" "))
levelplot(var1.pred.2 ~ x + y | z, as.data.frame(res3D),
          main=paste("Realization", as.character(rand.id[2]), "error", 
                     as.character(round(rand.err[rand.id[2]] *100)),"%",sep=" "))
levelplot(var1.pred.3 ~ x + y | z, as.data.frame(res3D),
          main=paste("Realization", as.character(rand.id[3]), "error", 
                     as.character(round(rand.err[rand.id[3]] *100)),"%",sep=" "))
levelplot(var1.pred.4 ~ x + y | z, as.data.frame(res3D),
          main=paste("Realization", as.character(rand.id[4]), "error", 
                     as.character(round(rand.err[rand.id[4]] *100)),"%",sep=" "))
levelplot(var1.pred.5 ~ x + y | z, as.data.frame(res3D),
          main=paste("Realization", as.character(rand.id[5]), "error", 
                     as.character(round(rand.err[rand.id[5]] *100)),"%",sep=" "))



# FOR N:S NOT MAINTAINED. 

# SFTH

res3D<-list()
for (i in 1:length(rand.id)){
  if(i==1){
    res3D[[i]] <- krige(formula = sfth ~ 1, df.sku2016, grid3D, model = vth.orig.fit[[fit.id.th]],nsim=0,maxdist=Inf,nmax=Inf,nmin=0) 
  } else {
    res3D[[i]] <- krige(formula = sfth ~ 1, df.sku2016, grid3D, model = vth.rand[[rand.id[i-1]]],nsim=0, maxdist=Inf,nmin=0)
  }
}


est3D.vth<-as.data.frame(res3D)

# SFH

res3D<-list()
for (i in 1:length(rand.id)){
  if(i==1){
    res3D[[i]] <- krige(formula = sfh ~ 1, df.sku2016, grid3D, model = vh.orig.fit[[fit.id.h]],nsim=0,maxdist=Inf,nmax=Inf,nmin=0) 
  } else {
    res3D[[i]] <- krige(formula = sfh ~ 1, df.sku2016, grid3D, model = vh.rand[[rand.id[i-1]]],nsim=0, maxdist=Inf,nmin=0)
  }
}


est3D.vh<-as.data.frame(res3D)


# Determine the max and min of each kriging grid output, as well as the global min and max for all realizations
pred.idx<-seq(from=4,to=250,by=5)
sim.min.vh<-apply(est3D.vh[,pred.idx],2,min)
sim.max.vh<-apply(est3D.vh[,pred.idx],2,max)
sim.min.vh[1] # the first position is the original model fitting
sim.max.vh[1]

sim.min.vth<-apply(est3D.vth[,pred.idx],2,min)
sim.max.vth<-apply(est3D.vth[,pred.idx],2,max)
sim.min.vth[1]
sim.max.vth[1]

gl.minmax<-data.frame(vth=c(min(sim.min.vth),max(sim.max.vth)),vh=c(min(sim.min.vh),max(sim.max.vh))) #global min and max for all kriging outputs
rownames(gl.minmax)<-c("global min","global max")

save(list=c("est3D.vth","est3D.vh"),file="krig maps", compress="bzip2")


require(lattice)
require(cowplot)

levelplot(var1.pred ~ x + y | z, as.data.frame(res3D),
main=paste("Original data fitting", "mod=Gaus", ""))


layout(matrix(c(1,2,3,4),2))

hist(est3D.vth$var1.pred.1, main=paste("Realization", as.character(rand.id[1]),"SFTH"), 
     xlab="Kriging prediction", 
     col="gray")
text(1.05,30000, paste("range =", as.character(round(min(est3D.vth$var1.pred.1),digits=3)), as.character(round(max(est3D.vth$var1.pred.1),digits=3))))
hist(est3D.vth$var1.pred.2,main=paste("Realization", as.character(rand.id[2]),"SFTH"),
     xlab="Kriging prediction",
     col="gray")
text(1.05,30000, paste("range =", as.character(round(min(est3D.vth$var1.pred.2),digits=3)), as.character(round(max(est3D.vth$var1.pred.2),digits=3))))
hist(est3D.vth$var1.pred.3,main=paste("Realization", as.character(rand.id[3]),"SFTH"),
     xlab="Kriging prediction",
     col="gray")
text(1.05,30000, paste("range =", as.character(round(min(est3D.vth$var1.pred.3),digits=3)), as.character(round(max(est3D.vth$var1.pred.3),digits=3))))
hist(est3D.vth$var1.pred.4,main=paste("Realization", as.character(rand.id[4]),"SFTH"),
     xlab="Kriging prediction",
     col="gray")
text(1.05,30000, paste("range =", as.character(round(min(est3D.vth$var1.pred.4),digits=3)), as.character(round(max(est3D.vth$var1.pred.4),digits=3))))



layout(matrix(c(1,2,3,4),2))

hist(est3D.vh$var1.pred.1, main=paste("Realization", as.character(rand.id[1]),"SFH"), 
     xlab="Kriging prediction", 
     col="gray")
text(1.75,50000, paste("range =", as.character(round(min(est3D.vh$var1.pred.1),digits=3)), as.character(round(max(est3D.vh$var1.pred.1),digits=3))))
hist(est3D.vh$var1.pred.2,main=paste("Realization", as.character(rand.id[2]),"SFH"),
     xlab="Kriging prediction",
     col="gray")
text(1.75,50000, paste("range =", as.character(round(min(est3D.vh$var1.pred.2),digits=3)), as.character(round(max(est3D.vh$var1.pred.2),digits=3))))
hist(est3D.vh$var1.pred.3,main=paste("Realization", as.character(rand.id[3]),"SFH"),
     xlab="Kriging prediction",
     col="gray")
text(1.75,50000, paste("range =", as.character(round(min(est3D.vh$var1.pred.3),digits=3)), as.character(round(max(est3D.vh$var1.pred.3),digits=3))))
hist(est3D.vh$var1.pred.4,main=paste("Realization", as.character(rand.id[4]),"SFH"),
     xlab="Kriging prediction",
     col="gray")
text(1.75,50000, paste("range =", as.character(round(min(est3D.vh$var1.pred.4),digits=3)), as.character(round(max(est3D.vh$var1.pred.4),digits=3))))





# Clay content
clay<-data.frame(read.table(paste(path,"\\clay content points.dat", sep=""), sep="", skip=6))

names(clay)<-c("x","y","z","cl")
coordinates(clay) = ~x+y+z


vc=variogram(cl~1,clay, width=13, cutoff=60)
plot(vc)

vc.fit<-fit.variogram(vc,vgm(psill=15,"Sph",range=25, nugget=18))
plot(vc,vc.fit)

vc.fit



