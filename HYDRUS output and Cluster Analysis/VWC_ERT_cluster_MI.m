cd("C:\Users\Matt\Documents\Norway\Norway-version-control\HYDRUS output and Cluster Analysis")

load("Cluster indices for MI.mat")

% Re-write 4's and 1's for VWC so that HH and LL coincide with electrical
% resisitivity (i.e. HH VWC corresponds to LL ERT, and vice-versa) 

HH_idx=find(VWC0(:,4) == 4);
LL_idx=find(VWC0(:,4) == 1);
VWC0(HH_idx,4)=1;
VWC0(LL_idx,4)=4;

HH_idx=find(VWC3(:,4) == 4);
LL_idx=find(VWC3(:,4) == 1);
VWC3(HH_idx,4)=1;
VWC3(LL_idx,4)=4;

HH_idx=find(VWC144(:,4) == 4);
LL_idx=find(VWC144(:,4) == 1);
VWC144(HH_idx,4)=1;
VWC144(LL_idx,4)=4;

HH_idx=find(VWC147(:,4) == 4);
LL_idx=find(VWC147(:,4) == 1);
VWC147(HH_idx,4)=1;
VWC147(LL_idx,4)=4;

HH_idx=find(VWC150(:,4) == 4);
LL_idx=find(VWC150(:,4) == 1);
VWC150(HH_idx,4)=1;
VWC150(LL_idx,4)=4;

% Calculate NMI
[Ixy0,Norm_Ixy0,w_x,w_y]= MI_GG_Scott(ERT(:,4),VWC0(:,4));
[Ixy3,Norm_Ixy3]=MI_GG_Scott(ERT(:,4),VWC3(:,4));
[Ixy144,Norm_Ixy144]=MI_GG_Scott(ERT(:,4),VWC144(:,4));
[Ixy147,Norm_Ixy147]=MI_GG_Scott(ERT(:,4),VWC147(:,4));
[Ixy150,Norm_Ixy150]=MI_GG_Scott(ERT(:,4),VWC150(:,4));