library(gstat)
library(sp)
library(ggplot2)
library(dplyr)


path<-("C:\\Users\\Matt\\Documents\\Norway\\SGeMS files")

# Scaling factors

df<-data.frame(read.table(paste(path,"\\Skuterud scaling factors K outliers removed feb 8 2020.dat", sep=""), sep="", skip=8))


#If finding variograms of K10 derived from Automated vogel scaling. R
df$K10<-NA
df$K30<-NA
df$K100<-NA
df$K10[1:121]<-K10
df$K30[1:121]<-K30
df$K100[1:121]<-K100

# Remove the NA values for K
dfK<-na.omit(df)

names(df)<-c("x","y","z","sfh","sfth","sfK")
#names(dfK)<-c("x","y","z","sfh","sfth","sfK","K10","K30","K100")
#coordinates(dfK) = ~x+y+z


## BOOTSTRAP THE VARIOGRAMS TO GET A BETTER REPRESENTATION OF SPATIAL VARIANCE -------------------------

#Set up the bootstrapping

n=50
id=matrix(data=NA,nrow=13,ncol=n)
df.adj<-list()
coords.adj<-list()
vth<-list()
init.range<-data.frame()
vth.fit<-list()
SSE<-data.frame()
vmodel.lines<-list()

# Perform the bootstrapping

for(i in 1:50){
id[,i]=sample(1:130,13,replace=FALSE) # 10% of spatial data values removed for i=1...n simulations
df.adj[[i]]<-df[-id[,i],] # adjusted data.frame with data removed 
coordinates(df.adj[[i]])<- ~x+y+z #convert each data.frame to geoObject for gstat


vth[[i]]<-variogram(sfth~1,df.adj[[i]], width=13, cutoff=60) #calculate the variograms for each iteration
ridx<-first((which(vth[[i]]$gamma>(var(df.adj[[i]]$sfth)-(var(df.adj[[i]]$sfth)*0.15))))) 
# The above range idx finds the lowest gamma value that is larger than the variance (sill) - 15% of the variance
init.range[i,1]<-vth[[i]][ridx,]$dist #This pulls out the initial guess for range

vth.fit[[i]]<-fit.variogram(vth[[i]], vgm(psill=var(df.adj[[i]]$sfth), "Sph", range=init.range[i,1],
                                          nugget=vth[[i]]$gamma[1]/1.2)) #fit each variogram
SSE[i,1]<-attr(vth.fit[[i]],"SSErr") #obtain the sum square errors of each fitted variogram
vmodel.lines[[i]]<-variogramLine(vth.fit[[i]],maxdist=60,n=200) # simulate model line output for plotting
}

# save the global environment in case this specific bootstrap output is needed again 
save.image(file="SKUvarioEnvironment.RData")


# Plot the bootstrapped variogram model confidence intervals


dist<-vmodel.lines[[1]]$dist # The distance values are constant

vals<-matrix(nrow=n,ncol=200)
error90<-1
error95<-1
error99<-1

for (i in 1:n){
  for (j in 1:length(vmodel.lines[[i]]$dist)){
  
  vals[i,]<-vmodel.lines[[i]]$gamma  
  error90[j] <- qnorm(0.950)*sd(vals[,j])/sqrt(n) # 90% confidence interval calculation 
  error95[j] <- qnorm(0.975)*sd(vals[,j])/sqrt(n) # 95% confidence interval calculation 
  error99[j] <- qnorm(0.995)*sd(vals[,j])/sqrt(n) # 99% confidence interval calculation 
  
  }
}


coordinates(df)<- ~x+y+z
vth.orig=variogram(sfth~1,df, width=13, cutoff=60) #cutoff = distance where np first decreases
vth.orig.fit<-fit.variogram(vth.orig,vgm(psill=0.12,"Sph",range=28, nugget=0.008),fit.ranges=FALSE)
origmodel.lines<-variogramLine(vth.orig.fit,maxdist=60,n=200)


plot(vth.orig$dist,vth.orig$gamma, ylim=c(0.010,0.019), pch=16)
lines(origmodel.lines$dist,origmodel.lines$gamma, col="red")
lines(dist,origmodel.lines$gamma-error90, lty=2, lwd=2)
lines(dist,origmodel.lines$gamma+error90, lty=2, lwd=2)
lines(dist,origmodel.lines$gamma-error95, lty=3, lwd=2)
lines(dist,origmodel.lines$gamma+error95, lty=3, lwd=2)
lines(dist,origmodel.lines$gamma-error99, lty=4, lwd=2)
lines(dist,origmodel.lines$gamma+error99, lty=4, lwd=2)




vh=variogram(sfh~1,df, width=13) #alpha=c(0,45,90,135) width=8
plot(vh)
vh60=variogram(sfh~1,df,width=13,cutoff=60)
plot(vh60)

vh.fit<-fit.variogram(vh60,vgm(psill=0.20,"Sph",range=20, nugget=NA))
plot(vh70,vh.fit)


vhns= vh.fit$psill[1]/vh.fit$psill[2]


vK=variogram(sfK~1,dfK) #alpha=c(0,45,90,135) width=8
plot(vK)

vK.fit<-fit.variogram(vK,vgm(psill=4,"Exp",range=10, nugget=0))
plot(vK,vK.fit)

vK.fit

vKns= vK.fit$psill[1]/vK.fit$psill[2]

#Variograms for conductivity data do not make sense, no matter what is done for them


vK10=variogram(K10~x+y+z,dfK)
vK30=variogram(K30~x+y+z,dfK)
vK100=variogram(K100~x+y+z,dfK)

plot(vK10)
plot(vK30)
plot(vK100)



# Clay content
clay<-data.frame(read.table(paste(path,"\\clay content points.dat", sep=""), sep="", skip=6))

names(clay)<-c("x","y","z","cl")
coordinates(clay) = ~x+y+z


vc=variogram(cl~1,clay, width=13, cutoff=60)
plot(vc)

vc.fit<-fit.variogram(vc,vgm(psill=15,"Sph",range=25, nugget=18))
plot(vc,vc.fit)

vc.fit


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
}

