library(gstat)
library(sp)



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
names(dfK)<-c("x","y","z","sfh","sfth","sfK","K10","K30","K100")
coordinates(df) = ~x+y+z
coordinates(dfK) = ~x+y+z


vth=variogram(sfth~1,df)
plot(vth)
vth60=variogram(sfth~1,df, width=13, cutoff=61) #cutoff = distance where np first decreases
plot(vth60)

vth4=variogram(sfth~1,df, width=13, cutoff=61, alpha=c(0,45,90,135))
plot(vth4)

vth.fit<-fit.variogram(vth60,vgm(psill=0.12,"Sph",range=28, nugget=0.008),fit.ranges=FALSE)
plot(vth60,vth.fit)

vth.fit

vthns= vth.fit$psill[1]/vth.fit$psill[2] #nugget:sill ratio 


vh=variogram(sfh~1,df, width=13) #alpha=c(0,45,90,135) width=8
plot(vh)
vh70=variogram(sfh~1,df,width=13,cutoff=60)
plot(vh70)

vh.fit<-fit.variogram(vh70,vgm(psill=0.20,"Sph",range=20, nugget=NA))
plot(vh70,vh.fit)

vh.fit

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
  v[[i]]<-variogram(predK[[i]]~1,predK)
}
names(v)<-unlist(mydata$KsMethod)


par(mar = c(1, 5, 2, 1))
par(mfrow=c(5,2))
for (i in 1:10){
  plot(v[[i]]$dist,v[[i]]$gamma)
  with(v[[i]][1:3,], text(gamma~dist, labels = np, pos = 4))
  text(95, max(v[[i]]$gamma/10), unlist(mydata$KsMethod[i]), font=2)
}

