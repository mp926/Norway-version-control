Vogel.scale=function (h,theta,K,Ks,theta.s,theta.r) {
  
  # MAKE SURE THAT theta.s, theta.r and theta are all the same order of magnitude! (i.e. fractions vs. percentages)
  # h and theta are lists of datasets of the same length
  
  
 # Define scaling factors a.theta and a.h
  
  a.theta<-1
  h.c<-1
  a.h<-1
  a.K<-1
  
  
   for (i in 1:length(theta.s)){
     
    a.theta[i]<-(theta.s[i]-theta.r[i])/(mean(theta.s,na.rm=TRUE)-mean(theta.r,na.rm=TRUE))
    
    theta.c<-median(theta[[i]], na.rm=TRUE)
    #theta.c<-quantile(theta[[i]],0.75,type=1, na.rm=TRUE)
    theta.c.idx<-which(theta[[i]]>=theta.c)
    h.c[i]<-mean(h[[i]][theta.c.idx])
  }
  
  for (i in 1:length(h)){
    a.h[i]<-h.c[i]/mean(h.c)
  }
  
  for (i in 1:length(theta.s)){
    a.K[i]<-(Ks[i])/mean(Ks, na.rm=TRUE)
  }
  
  
  # Apply scaling factors to data 
  
  theta.sc<-list()
  h.sc<-list()
  K.sc<-list()
  
for (i in 1:length(theta.s)){
  
  theta.sc[[i]]<-(theta[[i]]-theta.r[i])/a.theta[i]
  h.sc[[i]]<-h[[i]]/a.h[i]
  K.sc[[i]]<-K[[i]]/a.K[i]
}

  
  scaling.factors<-data.frame("a.h"=a.h,"a.theta"=a.theta, "a.K"=a.K)
  
  scaled<-list("scaling.factors"=scaling.factors,"h.sc"=h.sc,"theta.sc"=theta.sc,"K.sc"=K.sc)
  
  return(scaled)
  
}


#theta.v[[123]]/scaled[[1]]$a.h[123]
