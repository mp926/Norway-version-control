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


#If finding variograms of K10 derived from Automated vogel scaling. R
df$Ks<-Ks$`Ksat (cm/d)` 
#df$K10<-NA
#df$K30<-NA
#df$K100<-NA
#df$K10[1:121]<-K10
#df$K30[1:121]<-K30
#df$K100[1:121]<-K100


# Remove the NA values for K
dfK<-na.omit(df)

names(df)<-c("x","y","z","sfh","sfth","sfK","Ks")
names(dfK)<-c("x","y","z","sfh","sfth","sfK","Ks","K10","K30","K100")
#coordinates(dfK) = ~x+y+z


## BOOTSTRAP THE VARIOGRAMS for SFTheta SFh and Ks  ---------------------------------------------------

#Set up the bootstrapping

n=1000
rand.id=matrix(data=NA,nrow=26,ncol=n)
df.adj<-list()
coords.adj<-list()
vth<-list()
vh<-list()
vK<-list()
init.rangeth<-data.frame()
init.rangeh<-data.frame()
init.rangeK<-data.frame()
vth.fit<-list()
vh.fit<-list()
vK.fit<-list()
SSEth<-data.frame()
SSEh<-data.frame()
SSEK<-data.frame()
vmodel.linesth<-list()
vmodel.linesh<-list()
vmodel.linesK<-list()

# Perform the bootstrapping

for(i in 1:50){
rand.id[,i]=sample(1:130,26,replace=TRUE) # 10% of spatial data values removed for i=1...n simulations
df.adj[[i]]<-df[-rand.id[,i],] # adjusted data.frame with data removed
coordinates(df.adj[[i]])<- ~x+y+z #convert each data.frame to geoObject for gstat


vth[[i]]<-variogram(sfth~1,df.adj[[i]], width=13, cutoff=60) #calculate the variograms for each iteration
vh[[i]]<-variogram(sfh~1,df.adj[[i]], width=13, cutoff=60)
#vK[[i]]<-variogram(Ks~1,df.adj[[i]], cutoff=60)
ridxth<-first((which(vth[[i]]$gamma>(var(df.adj[[i]]$sfth)-(var(df.adj[[i]]$sfth)*0.15)))))
ridxh<-first((which(vh[[i]]$gamma>(var(df.adj[[i]]$sfh)-(var(df.adj[[i]]$sfh)*0.15)))))
#ridxK<-first((which(vK[[i]]$gamma[2:length(vK[[i]]$gamma)]>(var(df.adj[[i]]$Ks)-(var(df.adj[[i]]$Ks)*0.15))))) 
# The above range idx finds the lowest gamma value that is larger than the variance (sill) - 15% of the variance
# ridxK is modified because the first value is extremely high, so we skip it
init.rangeth[i,1]<-vth[[i]][ridxth,]$dist #This pulls out the initial guess for range
init.rangeh[i,1]<-vh[[i]][ridxh,]$dist
#init.rangeK[i,1]<-vK[[i]][ridxK,]$dist

vth.fit[[i]]<-fit.variogram(vth[[i]], vgm(psill=var(df.adj[[i]]$sfth), "Exp", range=init.rangeth[i,1],
                                        nugget=vth[[i]]$gamma[1]/1.2)) #fit each variogram


#SUMMARY STATISTICS FOR JUSTIFICATION OF MODEL SELECTION


vh.fit[[i]]<-fit.variogram(vh[[i]], vgm(psill=var(df.adj[[i]]$sfh), "Sph", range=init.rangeh[i,1],
                                        nugget=vh[[i]]$gamma[1]/1.2))
#vK.fit[[i]]<-fit.variogram(vK[[i]], vgm(psill=var(df.adj[[i]]$Ks), "Lin", range=init.rangeK[i,1],
                                        #nugget=vK[[i]]$gamma[1]/1.2),fit.method=6) # fitted with OLS

SSEth[i,1]<-attr(vth.fit[[i]],"SSErr") #obtain the sum square errors of each fitted variogram
SSEh[i,1]<-attr(vh.fit[[i]],"SSErr")
#SSEK[i,1]<-attr(vh.fit[[i]],"SSErr")

vmodel.linesth[[i]]<-variogramLine(vth.fit[[i]],maxdist=60,n=200) # simulate model line output for plotting
vmodel.linesh[[i]]<-variogramLine(vh.fit[[i]],maxdist=60,n=200)
#vmodel.linesK[[i]]<-variogramLine(vK.fit[[i]],maxdist=100,n=200)
}



# Plot the bootstrapped variogram model confidence intervals


dist.th<-vmodel.linesth[[1]]$dist # The distance values are constant
dist.h<-vmodel.linesh[[1]]$dist
dist.K<-vmodel.linesK[[1]]$dist

vals.th<-matrix(nrow=n,ncol=200)
vals.h<-matrix(nrow=n,ncol=200)
vals.K<-matrix(nrow=n,ncol=200)
error90th<-1
error95th<-1
error99th<-1
error90h<-1
error95h<-1
error99h<-1
error90K<-1
error95K<-1
error99K<-1

for (i in 1:n){
  for (j in 1:length(vmodel.linesth[[i]]$dist)){
  
  vals.th[i,]<-vmodel.linesth[[i]]$gamma
  vals.h[i,]<-vmodel.linesh[[i]]$gamma
  #vals.K[i,]<-vmodel.linesK[[i]]$gamma 
 error90th[j] <- qnorm(0.950)*sd(vals.th[,j])/sqrt(n) # 90% confidence interval calculation
 error95th[j] <- qnorm(0.975)*sd(vals.th[,j])/sqrt(n) # 95% confidence interval calculation
 error99th[j] <- qnorm(0.995)*sd(vals.th[,j])/sqrt(n) # 99% confidence interval calculation
 error90h[j] <- qnorm(0.950)*sd(vals.h[,j])/sqrt(n)
 error95h[j] <- qnorm(0.975)*sd(vals.h[,j])/sqrt(n)
 error99h[j] <- qnorm(0.995)*sd(vals.h[,j])/sqrt(n)
 # error90K[j] <- qnorm(0.950)*sd(vals.K[,j])/sqrt(n)
 # error95K[j] <- qnorm(0.975)*sd(vals.K[,j])/sqrt(n) 
 # error99K[j] <- qnorm(0.995)*sd(vals.K[,j])/sqrt(n) 
  
  
  }
}


coordinates(df)<- ~x+y+z  # This transforms the data.frame. If you want to use df again, you must reload the data.frame into the environment
vth.orig=variogram(sfth~1,df, width=13, cutoff=60) #cutoff = distance where np first decreases
vth.orig.fit<-fit.variogram(vth.orig,vgm(psill=0.06,"Exp",range=45, nugget=0.10),fit.ranges=FALSE)
origmodel.lines.th<-variogramLine(vth.orig.fit,maxdist=60,n=200)

vh.orig=variogram(sfh~1,df, width=13, cutoff=60) #cutoff = distance where np first decreases
vh.orig.fit<-fit.variogram(vh.orig,vgm(psill=0.12,"Sph",range=20, nugget=0.08))
origmodel.lines.h<-variogramLine(vh.orig.fit,maxdist=60,n=200)

vK.orig=variogram(Ks~1,df, cutoff=60) #cutoff = distance where np first decreases
vK.orig.fit<-fit.variogram(vK.orig,vgm(psill=100000,"Exp",range=1, nugget=300000), fit.method=6) #OLS
origmodel.lines.K<-variogramLine(vK.orig.fit,maxdist=100,n=200)



plot(vth.orig$dist,vth.orig$gamma, xlim=c(0,60),ylim=c(0,0.019), pch=16,
     ylab=expression(gamma), xlab="lag distance (cm)", main=expression("Variogram" ~ SF[theta]))
lines(origmodel.lines.th$dist,origmodel.lines.th$gamma, col="red")
lines(dist.th,origmodel.lines.th$gamma-error90th, lty=2, lwd=2)
lines(dist.th,origmodel.lines.th$gamma+error90th, lty=2, lwd=2)
lines(dist.th,origmodel.lines.th$gamma-error95th, lty=5, lwd=2)
lines(dist.th,origmodel.lines.th$gamma+error95th, lty=5, lwd=2)
lines(dist.th,origmodel.lines.th$gamma-error99th, lty=6, lwd=2)
lines(dist.th,origmodel.lines.th$gamma+error99th, lty=6, lwd=2)
legend("bottomright", legend=c("90% C.I.", "95% C.I.", "99% C.I."), lty=c(2,5,6))


 
plot(vh.orig$dist,vh.orig$gamma, xlim=c(0,60),ylim=c(0,0.25), pch=16,
     ylab=expression(gamma), xlab="lag distance (cm)", main=expression("Variogram" ~ SF[h]))
lines(origmodel.lines.h$dist,origmodel.lines.h$gamma, col="red")
lines(dist.h,origmodel.lines.h$gamma-error90h, lty=2, lwd=2)
lines(dist.h,origmodel.lines.h$gamma+error90h, lty=2, lwd=2)
lines(dist.h,origmodel.lines.h$gamma-error95h, lty=5, lwd=2)
lines(dist.h,origmodel.lines.h$gamma+error95h, lty=5, lwd=2)
lines(dist.h,origmodel.lines.h$gamma-error99h, lty=6, lwd=2)
lines(dist.h,origmodel.lines.h$gamma+error99h, lty=6, lwd=2)
legend("bottomright", legend=c("90% C.I.", "95% C.I.", "99% C.I."), lty=c(2,5,6))

plot(vK.orig$dist,vK.orig$gamma, pch=16, ylim=c(0,1000000),
     ylab=expression(gamma), xlab="lag distance (cm)", main=expression("Variogram" ~ K[s] ~ "[cm/d]"))
lines(origmodel.lines.K$dist,origmodel.lines.K$gamma, col="red")
lines(dist.K,origmodel.lines.K$gamma-error90K, lty=2, lwd=2)
lines(dist.K,origmodel.lines.K$gamma+error90K, lty=2, lwd=2)
lines(dist.K,origmodel.lines.K$gamma-error95K, lty=5, lwd=2)
lines(dist.K,origmodel.lines.K$gamma+error95K, lty=5, lwd=2)
lines(dist.K,origmodel.lines.K$gamma-error99K, lty=6, lwd=2)
lines(dist.K,origmodel.lines.K$gamma+error99K, lty=6, lwd=2)
legend("topright", legend=c("90% C.I.", "95% C.I.", "99% C.I."), lty=c(2,5,6))



# save the global environment in case this specific bootstrap output is needed again 
cwd<-getwd()
path<-paste(cwd,"/Spatial Statistics", sep="")
setwd(path)
save.image(file="SKUvarioEnvironment.RData")



# Repeat bootstrapping for SFK (with low outliers removed) ------------------------------------------------------

for(i in 1:50){
  rand.idK[,i]=sample(1:91,9,replace=FALSE) # 10% of spatial data values removed for i=1...n simulations
  df.adj[[i]]<-dfK[-rand.idK[,i],] # adjusted data.frame with data removed 
  coordinates(df.adj[[i]])<- ~x+y+z #convert each data.frame to geoObject for gstat
  
  
  vK[[i]]<-variogram(sfK~1,df.adj[[i]], width=13, cutoff=60) #calculate the variograms for each iteration
  ridxK<-first((which(vK[[i]]$gamma>(var(df.adj[[i]]$sfK)-(var(df.adj[[i]]$sfK)*0.15))))) 
  # The above range idx finds the lowest gamma value that is larger than the variance (sill) - 15% of the variance
  init.rangeK[i,1]<-vK[[i]][ridxK,]$dist #This pulls out the initial guess for range
  
  vK.fit[[i]]<-fit.variogram(vK[[i]], vgm(psill=var(df.adj[[i]]$sfK), "Exp", range=init.rangeK[i,1],
                                            nugget=vK[[i]]$gamma[1]/1.2)) #fit each variogram
  
  SSEK[i,1]<-attr(vK.fit[[i]],"SSErr") #obtain the sum square errors of each fitted variogram
  
  vmodel.linesK[[i]]<-variogramLine(vK.fit[[i]],maxdist=60,n=200) # simulate model line output for plotting
}



# Plot the bootstrapped variogram model confidence intervals


dist.K<-vmodel.linesK[[1]]$dist # The distance values are constant

vals.K<-matrix(nrow=n,ncol=200)
error90K<-1
error95K<-1
error99K<-1


for (i in 1:n){
  for (j in 1:length(vmodel.linesK[[i]]$dist)){
    
    vals.K[i,]<-vmodel.linesK[[i]]$gamma
    error90K[j] <- qnorm(0.950)*sd(vals.K[,j])/sqrt(n) # 90% confidence interval calculation 
    error95K[j] <- qnorm(0.975)*sd(vals.K[,j])/sqrt(n) # 95% confidence interval calculation 
    error99K[j] <- qnorm(0.995)*sd(vals.K[,j])/sqrt(n) # 99% confidence interval calculation 
    
  }
}


coordinates(dfK)<- ~x+y+z
vK.orig=variogram(sfK~1,dfK) #cutoff = distance where np first decreases
vK.orig.fit<-fit.variogram(vK.orig,vgm(psill=0.06,"Exp",range=45, nugget=0.10),fit.ranges=FALSE)
origmodel.lines.K<-variogramLine(vK.orig.fit,maxdist=60,n=200)


plot(vK.orig$dist,vK.orig$gamma, xlim=c(0,60),ylim=c(0,0.019), pch=16,
     ylab=expression(gamma), xlab="lag distance (cm)", main=expression("Variogram" ~ SF[K]))
lines(origmodel.lines.K$dist,origmodel.lines.K$gamma, col="red")
lines(dist.K,origmodel.lines.K$gamma-error90K, lty=2, lwd=2)
lines(dist.K,origmodel.lines.K$gamma+error90K, lty=2, lwd=2)
lines(dist.K,origmodel.lines.K$gamma-error95K, lty=5, lwd=2)
lines(dist.K,origmodel.lines.K$gamma+error95K, lty=5, lwd=2)
lines(dist.K,origmodel.lines.K$gamma-error99K, lty=6, lwd=2)
lines(dist.K,origmodel.lines.K$gamma+error99K, lty=6, lwd=2)
legend("bottomright", legend=c("90% C.I.", "95% C.I.", "99% C.I."), lty=c(2,5,6))








vK10=variogram(K10~x+y+z,dfK)
vK30=variogram(K30~x+y+z,dfK)
vK100=variogram(K100~x+y+z,dfK)

plot(vK10)
plot(vK30)
plot(vK100)



# Variograms for predicted Ks values
require(R.matlab)

meas.coords<-read.csv("measurement coords.csv")

mydata<-readMat("C:\\Users\\Matt\\Documents\\Norway\\Ksat and K-6\\Mingming code predicted Ks values.mat")
predK<-data.frame(mydata$ksPred)
names(predK)<-unlist(mydata$KsMethod)

coordinates(predK)<-meas.coords

v<-list()
for (i in 1:10){
  v[[i]]<-variogram(log10(predK[[i]]+0.0001)~1,predK)
}
names(v)<-unlist(mydata$KsMethod)


par(mar = c(1, 5, 2, 1))
par(mfrow=c(5,2))
for (i in 1:10){
  plot(v[[i]]$dist,v[[i]]$gamma)
  with(v[[i]][1:3,], text(gamma~dist, labels = np, pos = 4))
  text(95, max(v[[i]]$gamma), unlist(mydata$KsMethod[i]), font=2)




# Clay content
clay<-data.frame(read.table(paste(path,"\\clay content points.dat", sep=""), sep="", skip=6))

names(clay)<-c("x","y","z","cl")
coordinates(clay) = ~x+y+z


vc=variogram(cl~1,clay, width=13, cutoff=60)
plot(vc)

vc.fit<-fit.variogram(vc,vgm(psill=15,"Sph",range=25, nugget=18))
plot(vc,vc.fit)

vc.fit



}

