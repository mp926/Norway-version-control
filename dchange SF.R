## PROGRAM FOR MODIFYING THE HYDRUS INPUT FILES WITH CUSTOM DOMAIN INPUTS (new dchange and scaling factors)

# Set the directory to location where hydrus input files are located

dir<-("C:\\Users\\Matt\\Documents\\Norway\\HYDRUS and ParaView files")
subdir<-("\\H3D2_IISV_import_TDR_scaled_100 cm depth_3DGeneral_No_geo_objects")

setwd(paste(dir,subdir,sep=""))

# Load in the kriging map outputs 

load("C:\\Users\\Matt\\Documents\\Norway\\Norway-version-control\\Spatial Statistics\\krig maps")

# Load in the coordinate indices taken from the k nearest neighbor searching between the hydrus 
# coordinates,the kriging map outputs and the coordinates for each node in the HYDRUS domain 

require(R.matlab)

d.idx<-as.data.frame(readMat("C:\\Users\\Matt\\Documents\\Norway\\HYDRUS and ParaView files\\krigmap_index.mat"))
coords<-as.data.frame(readMat("C:\\Users\\Matt\\Documents\\Norway\\HYDRUS and ParaView files\\nodal_coords.mat"))

# Read in the file where the scaling factors are recorded and parse the ASCII file
# Axz = SFH, Dxz = SFTH

head<-readLines("DOMAIN.DAT")
head<-data.frame(newcol=head[1:5])
SFdom<-read.table("DOMAIN.DAT", skip=5, header=FALSE, fill=TRUE, nrows=52621,stringsAsFactors=FALSE) # Pull out the top portion of the file with the scaling factors
DOM<-read.table("DOMAIN.DAT", skip=52626, header=FALSE, fill=TRUE, nrows=Inf,stringsAsFactors=FALSE) # Extract the rest of the file

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
library(tidyr) # needed for the "unite" function


nstep<-6 # number of time steps with outputs of water content (SELECTOR.IN file. 
          # t=0 is not shown in the file, so make nstep = # of timesteps in file + 1)

th.mapn<-matrix(nrow=nrow(d.idx),ncol=nstep)
th.idxn<-seq(from=2,to=(nrow(d.idx)+1)*nstep,by=nrow(d.idx)+1)
name.idx<-seq(from=1,to=(nrow(d.idx))*nstep,by=nrow(d.idx)+1)
out.path<-("C:\\Users\\Matt\\Documents\\Norway\\HYDRUS and ParaView files\\HYDRUS results with kriged scaling factor maps\\")


for(i in 1:ncol(Axz)){

th.map<-NA
  
SFdom[2:(nrow(Axz)+1),7]<-round(Axz[,i], digits=5) # Add the scaling factors to the data.frame to be written to text
SFdom[2:(nrow(Dxz)+1),9]<-round(Dxz[,i], digits=5)

NewDom<-unite(SFdom,newcol,sep=" ",remove=TRUE) #Reshape the data back to its original format
DOM<-unite(DOM,newcol,sep=" ", remove=TRUE) 
NewDom <- rbind(head,NewDom,DOM) # Put all sections together

write.table(NewDom,file="DOMAIN.DAT",append=FALSE, na=" ", col.names=FALSE, quote=FALSE, row.names=FALSE, sep="   ") #Write to text file DOMAIN.DAT

system2("h3d_calc.exe") #Run the HYDRUS solver executable file

th.map<-readBin("th.out",numeric(),n=(nrow(d.idx)+1)*nstep,size=4,endian="little") # extract all water content data
# IMPORTANT! To conserve space, th.map is overwritten every loop!

for(j in 1:nstep){ 
  th.mapn[,j]<-th.map[th.idxn[j]:(th.idxn[j]+nrow(d.idx)-1)] # Compile the output data based on time step
}

th.mapn<-as.data.frame(th.mapn)
th.mapn<-cbind(coords,th.mapn) # add coordinates to the data.frame
names(th.mapn)<-c("x","y","z",paste(as.character(th.map[name.idx]),"hours"))

write.csv(th.mapn,
          file=paste(out.path,"VWC HYDRUS map ",as.character(nstep)," timesteps kriging map ",as.character(i),".csv", sep=""),
          quote=FALSE,row.names=FALSE) # export a csv file with the data 

# Copy and rename the ObsNod.out file with observation node outputs to the same folder as the VWC maps
file.copy("ObsNod.out",to = out.path)
file.rename(from=paste(out.path,"ObsNod.out", sep=""), 
            to = paste(out.path,"ObsNod ",as.character(nstep)," timesteps kriging map ",as.character(i),".out", sep=""))


}