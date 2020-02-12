# Checking the scaling factor implementation in HYDRUS vs measured data
# Hypothesis: HYDRUS takes the water retention characteristic curve and multiplies it by the 
# given scaling factor at each node


coords<-read.csv('measurement coords.csv')
coords<-coords[84:121,] # isolate the 2016 lab data
coords$y<-coords$y-200 # subtract the additional 200cm added on for the SGEMs grids

coords$sfth<-c(1.02161000000000, 1.03482000000000,0.973729000000000,0.977854000000000,
              1.05142000000000,	0.943594000000000,	0.983486000000000,	1.05119000000000,
              0.959287000000000,	1.04103000000000,	0.931459000000000,	1.02880000000000,
              1.04003000000000,	1.00387000000000,	1.02464000000000,	1.02860000000000,
              1.01902000000000,	0.989695000000000,	1.05955000000000,	1.03417000000000,
              1.07502000000000,	0.966352000000000,	1.01071000000000,	1.04347000000000,
              0.977435000000000,	1.02034000000000,	0.949503000000000,	1.05106000000000,
              0.995198000000000,	0.975919000000000,	0.970909000000000,	1.05163000000000,
              0.961903000000000,	1.06591000000000,	0.953016000000000,	0.963409000000000,
              0.965926000000000,	1.04047000000000)

coords$sfh<-c(0.864576000000000,	0.971884000000000,	1.15210000000000,	1.12877000000000,
              0.823303000000000,	1.06321000000000,	1.18090000000000,	0.741335000000000,
              1.16657000000000,	0.852764000000000,	1.48712000000000,	0.868481000000000,
              0.927758000000000,	1.93374000000000,	0.990300000000000,	0.959574000000000,
              0.949698000000000,	1.09608000000000,	0.853971000000000,	0.916532000000000,
              0.791040000000000,	1.00380000000000,	0.835697000000000,	0.866337000000000,
              1.35042000000000,	0.887041000000000,	1.51224000000000,	1.30318000000000,
              1.03171000000000,	1.36698000000000,	0.938159000000000,	0.812241000000000,
              1.11283000000000,	0.843895000000000,	1.38808000000000,	1.19791000000000,
              1.54977000000000,	0.841974000000000)




#steps:
#get scaling factors
#Calculate theta(h) for h[1 100000] from reference retention parameters
#Multiply each of those theta(h) values by scaling factors for each to produce "scaled" curve
#Compare to measured data



require(R.matlab)

path<-"C:\\Users\\Matt\\Documents\\Norway\\Water retention\\ku-pF\\Mingming corrected retentions Oct 2019"

mydata<-readMat(paste(path,"\\SKUWRC_kupF_PPE_WP4C_corrected.mat", sep=""))

data<-list()
h.cm<-list()
theta.v<-list()

for (i in 1:(length(mydata$SKUWRC)/length(mydata$SKUWRC[,,1]))){
  
  data[[i]]<-mydata$SKUWRC[,,i][c(2,10,14)]
  h.cm[[i]]<-data[[i]]$h.cm
  theta.v[[i]]<-data[[i]]$theta.v
}
