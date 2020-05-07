## PROGRAM FOR MODIFYING THE HYDRUS INPUT FILES WITH CUSTOM DOMAIN INPUTS (new dchange and scaling factors)

# Set the directory to location where hydrus input files are located

dir<-("C:\\Users\\Matt\\Documents\\Norway\\HYDRUS and ParaView files")
subdir<-("\\H3D2_IISV_import_TDR_scaled_100 cm depth_3DGeneral_No_geo_objects")

setwd(paste(dir,subdir,sep=""))

# Load in the kriging map outputs 

load("C:\\Users\\Matt\\Documents\\Norway\\Norway-version-control\\Spatial Statistics\\krig maps")

# Load in the coordinate indices taken from the k nearest neighbor searching between the hydrus 
# coordinates and the kriging map outputs 

require(R.matlab)

d.idx<-as.data.frame(readMat("C:\\Users\\Matt\\Documents\\Norway\\HYDRUS and ParaView files\\krigmap_index.mat"))


# Read in the file where the scaling factors are recorded 
# Axz = SFH, Dxz = SFTH

SFdom<-read.table("DOMAIN.DAT", skip=5, header=TRUE, fill=TRUE, nrows=52620) # Pull out the top portion of the file with the scaling factors
DOM<-read.table("DOMAIN.DAT", skip=52625, header=TRUE, fill=TRUE, nrows=Inf) # Extract the rest of the file
DOM[,11]<-NA # Add another column so that both the top and bottom portions have the same number of cols
names(DOM)<-names(SFdom) # Make the names between the two sections the same

pred<-seq(from=4,to=250,by=5) # The kringing prediction output is at every 5th column

Axz<-matrix(nrow=52620, ncol=50)  # Create a matrix with the scaling factors for h and th for all 50 realizations
Dxz<-matrix(nrow=52620, ncol=50)
for (j in 1:ncol(Axz)){
  for(i in 1:nrow(Axz)){
  Axz[i,j]<-est3D.vh[d.idx$k[i],pred[j]]
  Dxz[i,j]<-est3D.vth[d.idx$k[i],pred[j]]
  }
}

## Loop to modify the DOMAIN.DAT file and run HYDRUS ---------------------

for(i in 1:ncol(Axz)){

SFdom$Axz[1:nrow(Axz)]<-round(Axz[,1], digits=5) # Add the scaling factors to the data.frame to be written to text
SFdom$Dxz[1:nrow(Dxz)]<-round(Dxz[,1], digits=5)

SFdom <- rbind(SFdom, DOM) # Put the top and bottom sections together

write.table(SFdom,file="DOMAIN.DAT",append=FALSE, na="", quote=FALSE, row.names=FALSE, sep="   ") #Write to text file DOMAIN.DAT


}