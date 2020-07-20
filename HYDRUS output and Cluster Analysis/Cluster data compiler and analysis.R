# Compiler code for the clustered VWC output 

# Contents: 
# Data extraction
# Data cleaning and restructuring
# Conversion to HYDRUS input file for volume comparison
# Saving new versions of data in more manageable form

## DATA EXTRACTION --------------------------------------

# path to save R data to
setwd("C:\\Users\\Matt\\Documents\\Norway\\Norway-version-control\\HYDRUS output and Cluster Analysis")


#path where cluster output is located
path<-("C:\\Users\\Matt\\Documents\\Norway\\Ruth clusters\\2016 experiment\\VWC clusters\\")

daysn<-c(0,"3_33",144,147,150) # the datasets have different notation for 3.33 days 
days<-c(0,3.33,144,147,150)
quantile<-c(5,95)

data.compiler <- function(path,type="norm"){
  if(type=="norm"){
    data<-read.csv(paste(path,norm.file,sep=""),header=TRUE,sep=",")
  } else if (type=="mean"){
    data<-read.csv(paste(path,mean.file,sep=""),header=TRUE,sep=",")
  } else if (type=="quant"){
    data<-read.csv(paste(path,quant.file,sep=""),header=TRUE,sep=",")
  }
  return(data)
}


# Normal 50 realization data set 

out<-matrix(nrow=38519,ncol=50)
cluster.map<-list()

for (j in 1:5){
  for(i in 1:50){
norm.file<-paste("LISA_VWC HYDRUS map 5 timesteps kriging map ",i,"_",daysn[j],"d.csv",sep="")    
data.norm<-data.compiler(path,type="norm")
coords<-data.norm[,1:3]
fdr<-data.norm$fdr
out[,i]<-fdr
cluster.map[[j]]<-out
  }
}

names(cluster.map)<-c("0 days","3.33 days","144 days","147 days","150 days")

save(cluster.map,file="50 realizations clustered.Rdata")


# Mean data set 

cluster.map<-list()

for (j in 1:5){
    mean.file<-paste("LISA_mean data_",days[j]," days mean.csv",sep="")    
    data.mean<-data.compiler(path,type="mean")
    coords<-data.mean[,1:3]
    fdr<-data.mean$fdr
    out<-fdr
    cluster.map[[j]]<-out
}

names(cluster.map)<-c("0 days","3.33 days","144 days","147 days","150 days")

save(cluster.map,file="Mean cluster data.RData")

# Quantile data set 

out<-matrix(nrow=38519,ncol=2)
cluster.map<-list()

for(j in 1:5){
  for (k in 1:2){
  quant.file<-paste("LISA_mean data_",days[j], " days " ,quantile[k],"pc.csv",sep="")    
  data.quant<-data.compiler(path,type="quant")
  coords<-data.quant[,1:3]
  fdr<-data.quant$fdr
  out[,k]<-fdr
  cluster.map[[j]]<-out
  }
}

names(cluster.map)<-c("0 days","3.33 days","144 days","147 days","150 days")

save(cluster.map,file="Quantile cluster data.RData")



## DATA CLEANING AND RESTRUCTURING ----------------


# Load in the needed data

setwd("C:\\Users\\Matt\\Documents\\Norway\\Norway-version-control\\HYDRUS output and Cluster Analysis")

load("Mean cluster data.Rdata")
#load("50 realizations clustered.Rdata")

HH.idx<-sapply(cluster.map,function(y) which(y==4)) # apply function "which" to list 
LL.idx<-sapply(cluster.map,function(y) which(y==1))
NS.idx<-sapply(cluster.map,function(y) which(y==0))

for(i in 1:5){   # Convert the FDR values to arbitrary material numbers. HH=1, LL=2, NS=3

cluster.map[[i]][HH.idx[[i]]]<-1
cluster.map[[i]][LL.idx[[i]]]<-2
cluster.map[[i]][NS.idx[[i]]]<-3

}

## WRITE THE HYDRUS IMPORT FILES -----------------------------

library(tidyr)

setwd("C:\\Users\\Matt\\Documents\\Norway\\Ruth clusters\\2016 experiment\\HYDRUS cluster imports")

coords<-read.table("coords.txt",header=TRUE)
nrow=38519
days<-c(0,3.33,144,147,150)

head<-"OBJECT=INDEXES_AT_POINTS"
imp<-read.table("IISV_import.txt",skip=1,nrow=38519)
tail<-";"

for(j in 1:5){
  if(is.null(ncol(cluster.map[[1]]))==TRUE){
    imp[,4]<-cluster.map[[j]] 
    
    out<-rbind(head,imp,tail)
    filename<-paste("VWC mean clusters_",days[j],"days",sep="")
    write.table(out,paste(filename,".txt",sep=""),quote=FALSE,row.names=FALSE,col.names=FALSE)
  }else{
    for(i in 1:ncol(cluster.map[[1]])){
      imp[,4]<-cluster.map[[j]][,i] 
      
      out<-rbind(head,imp,tail)
      filename<-paste("VWC clusters",i,"_",days[j],"days",sep="")
      write.table(out,paste(filename,".txt",sep=""),quote=FALSE,row.names=FALSE,col.names=FALSE)
    }
    
    
}
    }




