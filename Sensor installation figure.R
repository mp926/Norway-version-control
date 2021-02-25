# Quick code to create 3D scatterplot figure showing the sensor distribution over space

#load in sensor coordinates 
require(readxl)

mydata<-read_excel("C:\\Users\\Matt\\Documents\\Norway\\Field experiment\\Spring 2016 field data\\Sensor locations Spring-Fall2016.xlsx", 
                   sheet="position as f(depth)", col_names=FALSE, skip=3)

TDR.coords<-mydata[1:20,2:4]
TDR.coords[,3]<- -(TDR.coords[,3])
names(TDR.coords)<-c("x (m)","y (m)","z (m)")
TDR.coords$`x (m)`<-as.numeric(TDR.coords$`x (m)`)
TDR.coords<-TDR.coords/100

tens.coords<-mydata[1:20,9:11]
tens.coords[,3]<- -(tens.coords[,3])
names(tens.coords)<-c("x (m)","y (m)","z (m)")
tens.coords<-tens.coords/100

df<-rbind(TDR.coords,tens.coords)
df$ID<-factor(c(rep("TDR",times=20),rep("tens",times=20)))
colors<-c("#999999", "#E69F00")
colors<-colors[as.numeric(df$ID)]
  

require(scatterplot3d)
s3d<-scatterplot3d(df[,1:3], pch = 16,color=colors, box=FALSE, grid=TRUE,
                   cex.lab=1.2, cex.axis=1.1, lab.z=6,
                   scale.y=0.5, type="h")
legend(s3d$xyz.convert(0,0.05,0.25), legend = c("TDR probe", "Tensiometer"),
       col =  c("#999999", "#E69F00"), pch = 16, cex=1.1, bty="n")
