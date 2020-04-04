library(gstat)
library(sp)
library(ggplot2)
library(dplyr)


path<-("C:\\Users\\Matt\\Documents\\Norway\\SGeMS files")

# Scaling factors

df<-data.frame(read.table(paste(path,"\\Skuterud scaling factors K outliers removed feb 8 2020.dat", sep=""), sep="", skip=8))

require(readxl)
Ks<-read_excel("C:\\Users\\Matt\\Documents\\Norway\\Ksat and K-6\\Compiled Ksat data - Attila Aug 2019.xlsx",
               sheet="Compiled", col_names=TRUE)


df$Ks<-Ks$`Ksat (cm/d)` 


names(df)<-c("x","y","z","sfh","sfth","sfK","Ks")


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


# SFTHETA --

n=1000 # 1000 runs 
coordinates(df)<- ~x+y+z  # This transforms the data.frame. If you want to use df again, you must reload the data.frame into the environment
vth.orig=variogram(sfth~1,df, width=13, cutoff=60) #cutoff = distance where np first decreases
vth.orig.fit<-fit.variogram(vth.orig,vgm(psill=0.06,"Exp",range=45, nugget=0.10),fit.ranges=FALSE)
origmodel.lines.th<-variogramLine(vth.orig.fit,maxdist=60,n=200) # create a line of the variogram model for plotting
origmodel.lines.th$type=rep("None",times=200)

gam.err=matrix(nrow=n,ncol=length(vth.orig$gamma))
rand.err=matrix(nrow=n,ncol=length(vth.orig$gamma))
vth<-list()
vth.fit<-list()
SSEth.mod<-data.frame()
vmodel.linesth<-list()
mod.id<-c("Sph","Exp","Lin")
mod.opt<-data.frame()
nug<-matrix(nrow=n)
for (i in 1:n){
  rand.err[i,]<-sample(runif(1000,min=0.95,max=1.05),length(vth.orig$gamma),replace=TRUE) # Do bootstrap sampling for error from a uniform distribution 
  gam.err[i,]<-vth.orig$gamma*rand.err[i,] # Add error to the gamma for 1000 variograms
  vth[[i]]<-vth.orig
  vth[[i]]$gamma=gam.err[i,]# Substitute the gammas with error into the original variogram
  nug[i]=vth[[i]]$gamma[which.min(vth[[i]]$gamma)]
  
  for (j in 1:length(mod.id)){
  mod<-fit.variogram(vth[[i]], vgm(psill=vth[[i]]$gamma[5]-nug[i], paste(mod.id[j]),
                                   nugget=nug[i], range=vth[[i]]$dist[which.max(vth[[i]]$gamma)])) #fit each variogram with 3 models
  SSEth.mod[i,j]<-attr(mod,"SSErr")
  }
  names(SSEth.mod)<-mod.id
  
  vth.fit[[i]]<-fit.variogram(vth[[i]], vgm(psill=vth[[i]]$gamma[5]-nug[i], model=names(which.min(SSEth.mod[i,])),
                                             nugget=nug[i], range=vth[[i]]$dist[which.max(vth[[i]]$gamma)])) #fit each variogram with the best model fit
  
  vmodel.linesth[[i]]<-variogramLine(vth.fit[[i]],maxdist=60,n=50) # simulate model line output for plotting  

  mod.opt[i,1]<-as.character(vth.fit[[i]]$model[2]) # optimal model type for each realization based on SSE 
}

# Figure out how many realizations use each model type
barplot(table(mod.opt))



require(ggplot2) # Plot the resulting variograms
require(data.table)

dfdat<-matrix(nrow=n*5,ncol=2)
dfmod<-matrix(nrow=n*50,ncol=3)
for(i in 1:n){
  dfdat[,1]<-rep(t(vth[[1]]$dist), times=n)
  dfdat[seq(1,5000,5)[i]:seq(5,5000,5)[i],2]<-t(vth[[i]]$gamma)
  dfmod[,1]<-rep(t(vmodel.linesth[[1]]$dist), times=n)
  dfmod[seq(1,50000,50)[i]:seq(50,50000,50)[i],2]<-t(vmodel.linesth[[i]]$gamma)
}

dfdat<-as.data.frame(dfdat)
names(dfdat)<-c("dist","gamma")
dfmod<-as.data.frame(dfmod)
names(dfmod)<-c("dist","gamma")
dfdat$samp<-rep(paste("sample",as.character(seq(1,n,1))),each=5)
dfmod$samp<-rep(paste("sample",as.character(seq(1,n,1))),each=50)

rand.id<-sample(seq(1,1000,1),50,replace=FALSE) #Take a random sampling of 50 variograms

datalist = list()
modlist = list()
for (i in 1:length(rand.id)) {
  datalist[[i]] <- subset(dfdat, samp==paste("sample", rand.id[i], sep=" "))
  modlist[[i]] <- subset(dfmod, samp==paste("sample", rand.id[i], sep=" "))
}
dfrand<-rbindlist(datalist)
dfmodrand<-rbindlist(modlist) 

g1<-ggplot(dfrand,aes(x=dist,y=gamma, color=samp)) +  # plot the variograms
  geom_point() +
  geom_line(data=dfmodrand,aes(x=dist,y=gamma, color=samp)) +
  xlab("Distance (cm)") +
  ylab(expression(gamma)) +
  ggtitle("Random gamma (sill constrained) SFth") +
  theme_bw() + theme(axis.text=element_text(size=14), axis.title=element_text(size=14),
                     legend.position="")

g1

# SFH --

n=1000 # 1000 runs 
vh.orig=variogram(sfh~1,df, width=13, cutoff=60) #cutoff = distance where np first decreases
vh.orig.fit<-fit.variogram(vh.orig,vgm(psill=0.12,"Sph",range=20, nugget=0.08))
origmodel.lines.h<-variogramLine(vh.orig.fit,maxdist=60,n=200)
origmodel.lines.th$type=rep("None",times=200)

gam.err=matrix(nrow=n,ncol=length(vh.orig$gamma)-1)
rand.err=matrix(nrow=n,ncol=length(vh.orig$gamma)-1)
vh<-list()
vh.fit<-list()
SSEh.Exp<-data.frame()
vmodel.linesh<-list()
nug<-matrix(nrow=n)
for (i in 1:n){
  rand.err[i,]<-sample(seq(from=0.9,to=1.1,by=0.025),length(vh.orig$gamma)-1,replace=TRUE) # change to uniform distribution
  gam.err[i,]<-vh.orig$gamma[1:4]*rand.err[i,] # Add error to the gamma for 1000 variograms
  vh[[i]]<-vh.orig
  vh[[i]]$gamma[1:4]=gam.err[i,]# Substitute the gammas with error into the original variogram
  nug[i]=vh[[i]]$gamma[which.min(vh[[i]]$gamma)]
  vh.fit[[i]]<-fit.variogram(vh[[i]], vgm(psill=vh[[i]]$gamma[5]-nug[i], "Sph",
                                             nugget=nug[i], range=vh[[i]]$dist[which.max(vh[[i]]$gamma)])) #fit each variogram
  SSEh.Exp[i,1]<-attr(vh.fit[[i]],"SSErr")
  vmodel.linesh[[i]]<-variogramLine(vh.fit[[i]],maxdist=60,n=50) # simulate model line output for plotting
}


dfdat<-matrix(nrow=n*5,ncol=2)
dfmod<-matrix(nrow=n*50,ncol=2)
for(i in 1:n){
  dfdat[,1]<-rep(t(vh[[1]]$dist), times=n)
  dfdat[seq(1,5000,5)[i]:seq(5,5000,5)[i],2]<-t(vh[[i]]$gamma)
  dfmod[,1]<-rep(t(vmodel.linesh[[1]]$dist), times=n)
  dfmod[seq(1,50000,50)[i]:seq(50,50000,50)[i],2]<-t(vmodel.linesh[[i]]$gamma)
}

dfdat<-as.data.frame(dfdat)
names(dfdat)<-c("dist","gamma")
dfmod<-as.data.frame(dfmod)
names(dfmod)<-c("dist","gamma")
dfdat$samp<-rep(paste("sample",as.character(seq(1,n,1))),each=5)
dfmod$samp<-rep(paste("sample",as.character(seq(1,n,1))),each=50)


datalist = list()
modlist = list()
for (i in 1:length(rand.id)) {
  datalist[[i]] <- subset(dfdat, samp==paste("sample", rand.id[i], sep=" "))
  modlist[[i]] <- subset(dfmod, samp==paste("sample", rand.id[i], sep=" "))
}
dfrand<-rbindlist(datalist)
dfmodrand<-rbindlist(modlist) 

g2<-ggplot(dfrand,aes(x=dist,y=gamma, color=samp)) +  # plot the variograms
  geom_point() +
  geom_line(data=dfmodrand,aes(x=dist,y=gamma, color=samp)) +
  xlab("Distance (cm)") +
  ylab(expression(gamma)) +
  ggtitle("Random gamma (sill constrained) SFh") +
  theme_bw() + theme(axis.text=element_text(size=14), axis.title=element_text(size=14),
                     legend.position="")

g2


#combine plots

require(ggpubr)

ggarrange(g1 + theme(legend.position=""),g2 + theme(legend.position=""), labels=c("A","B"), ncol=2, nrow=1)




# Add random error to the variograms with increased nugget (maintain n:s ratio) ---------------------

# --------------------------------- water content ---

#err<-c(1.02,1.05,1.1,1.2)
err<-c(2,4,6,8) # Try more extreme values to see what happens 
coordinates(df)<- ~x+y+z  # This transforms the data.frame. If you want to use df again, you must reload the data.frame into the environment
vth.orig=variogram(sfth~1,df, width=13, cutoff=60) #cutoff = distance where np first decreases
vth.orig.fit<-fit.variogram(vth.orig,vgm(psill=0.06,"Exp",range=45, nugget=0.10),fit.ranges=FALSE)
origmodel.lines.th<-variogramLine(vth.orig.fit,maxdist=60,n=200) # create a line of the variogram model for plotting
origmodel.lines.th$type=rep("None",times=200)

ns=vth.orig.fit$psill[1]/(vth.orig.fit$psill[2]+vth.orig.fit$psill[1])


gam.err=matrix(nrow=length(err),ncol=length(vth.orig$gamma))
for (i in 1:4){
gam.err[i,]<-vth.orig$gamma*err[i]
}


# Change the variograms to accomodate the new error values 
vth.err<-list()
for (i in 1:4){
vth.err[[i]]<-vth.orig
vth.err[[i]]$gamma=gam.err[i,]
}


vth.err.fit<-list()
#vth.err.mod<-list()
error.lines.th<-list()
ns.err<-matrix(ncol=4)
for (i in 1:4){
  vth.err.fit[[i]]<-fit.variogram(vth.err[[i]], vgm(psill=max(vth.err[[i]]$gamma),"Exp",range=45, nugget=max(vth.err[[i]]$gamma)*ns), fit.ranges=FALSE)
  #vth.err.mod[[i]]<-vgm(psill=vth.err.fit[[i]]$psill[2]-nug,"Exp",range=45,nugget=vth.err.fit[[i]]$psill[1])
  error.lines.th[[i]]<-variogramLine(vth.err.fit[[i]],maxdist=60,n=200)
  ns.err[i]<-vth.err.mod[[i]]$psill[1]/(vth.err.mod[[i]]$psill[2]+vth.err.mod[[i]]$psill[1])
}


error.lines.th<-as.data.frame(error.lines.th)
require(data.table)
m<-melt(error.lines.th) # melt the variogram line data
vgmline.data<-subset(m, grepl("gamma", variable)) # subset the gamma values
vgmline.data$dist=rep(error.lines.th$dist, times=4) # add the repeated distance values
vgmline.data$type=rep(c("200%","400%","600%","800%"),each=200)


vario.data<-data.frame(dist=rep(vth.orig$dist,times=5), gamma=c(vth.orig$gamma,t(gam.err)), 
                       type=rep(c("orig","200%","400%","600%","800%"),each=5))


g1<-ggplot(data=vario.data, aes(x=dist,y=gamma, color=type)) +
  geom_point(shape=20, size=3) +
  geom_line(data=origmodel.lines.th, aes(x=dist, y=gamma), color="magenta") +
  geom_line(data=vgmline.data, aes(x=dist, y=value, color=type)) +
  xlab('Lag distance (cm)') +
  ylab(expression(gamma)) +
  scale_color_discrete(name="Added error",breaks=c("orig","200%","400%","600%","800%"), labels=c("None","200%","400%","600%","800%")) +
  theme_bw() + theme(axis.text=element_text(size=12), axis.title=element_text(size=14))

g1

# -------------------------- pressure potential ---

vh.orig=variogram(sfh~1,df, width=13, cutoff=60) #cutoff = distance where np first decreases
vh.orig.fit<-fit.variogram(vh.orig,vgm(psill=0.12,"Sph",range=20, nugget=0.08))
origmodel.lines.h<-variogramLine(vh.orig.fit,maxdist=60,n=200)
ns=vh.orig.fit$psill[1]/(vh.orig.fit$psill[2]+vh.orig.fit$psill[1])


gam.err=matrix(nrow=length(err),ncol=length(vth.orig$gamma))

for (i in 1:4){
  gam.err[i,]<-vh.orig$gamma*err[i]
}

vh.err<-list()
for (i in 1:4){
  vh.err[[i]]<-vh.orig
  vh.err[[i]]$gamma=gam.err[i,]
}

vh.err.fit<-list()
vh.err.mod<-list()
error.lines.h<-list()
ns.err<-matrix(ncol=4)
for (i in 1:4){
  vh.err.fit[[i]]<-fit.variogram(vh.err[[i]], vgm(psill=max(vh.err[[i]]$gamma),"Sph",range=20))
  nug=max(vh.err.fit[[i]]$psill)*ns
  vh.err.mod[[i]]<-vgm(psill=vh.err.fit[[i]]$psill-nug,"Sph",range=20,nugget=nug)
  error.lines.h[[i]]<-variogramLine(vh.err.mod[[i]],maxdist=60,n=200)
  ns.err[i]<-vh.err.mod[[i]]$psill[1]/(vh.err.mod[[i]]$psill[2]+vh.err.mod[[i]]$psill[1])
}


error.lines.h<-as.data.frame(error.lines.h)
m<-melt(error.lines.h) # melt the variogram line data
vgmline.data<-subset(m, grepl("gamma", variable)) # subset the gamma values
vgmline.data$dist=rep(error.lines.h$dist, times=4) # add the repeated distance values
vgmline.data$type=rep(c("2%","5%","10%","20%"),each=200)



vario.data<-data.frame(dist=rep(vh.orig$dist,times=5), gamma=c(vh.orig$gamma,t(gam.err)), 
                       type=rep(c("orig","2%","5%","10%","20%"),each=5))

g2<-ggplot(data=vario.data, aes(x=dist,y=gamma, color=type)) +
  geom_point(shape=20, size=3) +
  geom_line(data=origmodel.lines.h, aes(x=dist, y=gamma), color="magenta") +
  geom_line(data=vgmline.data, aes(x=dist, y=value, color=type)) +
  xlab('Lag distance (cm)') +
  ylab(expression(gamma)) +
  scale_color_discrete(name="Added error",breaks=c("orig","2%","5%","10%","20%"), labels=c("None","2%","5%","10%","20%")) +
  theme_bw() + theme(axis.text=element_text(size=12), axis.title=element_text(size=14))

g2


# plot both figures on the same page

require(ggpubr)

ggarrange(g1 + theme(legend.position="top"),g2 + theme(legend.position=), labels=c("A","B"), ncol=2, nrow=1)





# PERFORM GLOBAL ORDINARY KRIGING ON THE DATASETS ---------------------------


df<-data.frame(read.table(paste(path,"\\Skuterud scaling factors K outliers removed feb 8 2020.dat", sep=""), sep="", skip=8))

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

res3D<-list()
for (i in 1:length(rand.id)){
  if(i==1){
res3D[[i]] <- krige(formula = sfth ~ 1, df.sku2016, grid3D, model = vh.orig.fit,nsim=0,maxdist=Inf,nmax=Inf,nmin=0) 
  } else {
    res3D[[i]] <- krige(formula = sfth ~ 1, df.sku2016, grid3D, model = vh.fit[[rand.id[i-1]]],nsim=0, maxdist=Inf,nmin=0)
  }
}


est3D<-as.data.frame(res3D)

require(lattice)

l1<-levelplot(var1.pred ~ x + y | z, as.data.frame(res3D))
l2<-levelplot(var1.pred.1 ~ x.1 + y.1 | z.1, as.data.frame(res3D))
l3<-levelplot(var1.pred.2 ~ x.2 + y.2 | z.2, as.data.frame(res3D))
l4<-levelplot(var1.pred.3 ~ x.3 + y.3 | z.3, as.data.frame(res3D))
l5<-levelplot(var1.pred.4 ~ x.4 + y.4 | z.4, as.data.frame(res3D))
  
require(plot3D)
require(cowplot)

p1<-points3D(est3D$x,est3D$y,est3D$z,colvar=est3D$var1.pred,ticktype="detailed", theta=0, phi=215, bty="f", 
         pch=1)
p2<-points3D(est3D$x,est3D$y,est3D$z,colvar=est3D$var1.pred.1,ticktype="detailed", theta=0, phi=215, bty="f", 
             pch=1)
p3<-points3D(est3D$x,est3D$y,est3D$z,colvar=est3D$var1.pred.2,ticktype="detailed", theta=0, phi=215, bty="f", 
             pch=1)
p4<-points3D(est3D$x,est3D$y,est3D$z,colvar=est3D$var1.pred.3,ticktype="detailed", theta=0, phi=215, bty="f", 
             pch=1)
p5<-points3D(est3D$x,est3D$y,est3D$z,colvar=est3D$var1.pred.4,ticktype="detailed", theta=0, phi=215, bty="f", 
             pch=1)

layout(matrix(c(1,2,3,4),2))

points3D(est3D$x,est3D$y,est3D$z,colvar=est3D$var1.pred,ticktype="detailed", theta=0, phi=215, bty="f", 
         pch=1)
points3D(est3D$x,est3D$y,est3D$z,colvar=est3D$var1.pred.1,ticktype="detailed", theta=0, phi=215, bty="f", 
         pch=1)
points3D(est3D$x,est3D$y,est3D$z,colvar=est3D$var1.pred.2,ticktype="detailed", theta=0, phi=215, bty="f", 
         pch=1)
points3D(est3D$x,est3D$y,est3D$z,colvar=est3D$var1.pred.3,ticktype="detailed", theta=0, phi=215, bty="f", 
             pch=1)

# Clay content
clay<-data.frame(read.table(paste(path,"\\clay content points.dat", sep=""), sep="", skip=6))

names(clay)<-c("x","y","z","cl")
coordinates(clay) = ~x+y+z


vc=variogram(cl~1,clay, width=13, cutoff=60)
plot(vc)

vc.fit<-fit.variogram(vc,vgm(psill=15,"Sph",range=25, nugget=18))
plot(vc,vc.fit)

vc.fit



