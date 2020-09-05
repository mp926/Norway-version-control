# Quick code to create 3D scatterplot figure showing the sensor distribution over space

#load in sensor coordinates 
require(readxl)

mydata<-read_excel("C:\\Users\\Matt\\Documents\\Norway\\Field experiment\\Spring 2016 field data\\Sensor locations Spring-Fall2016.xlsx", 
                   sheet="position as f(depth)", col_names=FALSE, skip=3)

TDR.coords<-mydata[1:20,2:4]
TDR.coords[,3]<- -(TDR.coords[,3])
names(TDR.coords)<-c("x (cm)","y (cm)","z (cm)")
TDR.coords$`x (cm)`<-as.numeric(TDR.coords$`x (cm)`)

tens.coords<-mydata[1:20,9:11]
tens.coords[,3]<- -(tens.coords[,3])
names(tens.coords)<-c("x (cm)","y (cm)","z (cm)")

df<-rbind(TDR.coords,tens.coords)
df$ID<-factor(c(rep("TDR",times=20),rep("tens",times=20)))
colors<-c("#999999", "#E69F00")
colors<-colors[as.numeric(df$ID)]
  

require(scatterplot3d)
s3d<-scatterplot3d(df[,1:3], pch = 16,color=colors, box=FALSE, grid=TRUE,
                   cex.lab=1.2, cex.axis=1.1, lab.z=6,
                   scale.y=0.5)
legend(s3d$xyz.convert(-15, 20,25), legend = c("TDR probe", "Tensiometer"),
       col =  c("#999999", "#E69F00"), pch = 16, cex=1.1, bty="n")
