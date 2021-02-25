cd("C:\Users\Matt\Documents\Norway\Norway-version-control\HYDRUS output and Cluster Analysis")

load("Cluster indices for MI.mat")


% Shifting the numbers around doesn't matter! The spatial pattern of the
% numbers is not changing, just the index telling you what cluster it is
% in. The probabilities that each index will occur/not occur when compared
% to each other will therefore not change, only shift, resulting in an 
% artificial comparison. Comparing HH to LL of the ERT clusters can 
% can therefore only be done directly (i.e. HH to HH, LL to LL, etc.) 

HH_idx=find(VWC0(:,4) == 4);  
LL_idx=find(VWC0(:,4) == 1);
NS_idx=find(VWC0(:,4) == 0);
VWC0(HH_idx,4)=0;
VWC0(LL_idx,4)=1;
VWC0(NS_idx,4)=4;

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

% Compare NMI of VWC clusters to itself over time
[Ixy0_0,Norm_Ixy0_0,Hx0,Hy0,Hxy0_0,Mx0,My0,hpxy0_0,w_x,w_y]= MI_GG_Scott(VWC0(:,4),VWC0(:,4));
[Ixy0_3,Norm_Ixy0_3,Hx0,Hy3,Hxy0_3,Mx0,My3,hpxy0_3,w_x,w_y]= MI_GG_Scott(VWC0(:,4),VWC3(:,4));
[Ixy0_144,Norm_Ixy0_144,Hx0,Hy144,Hxy0_144,Mx0,My144,hpxy0_144,w_x,w_y]=MI_GG_Scott(VWC0(:,4),VWC144(:,4));
[Ixy0_147,Norm_Ixy0_147,Hx0,Hy147,Hxy0_147,Mx0,My147,hpxy0_147,w_x,w_y]=MI_GG_Scott(VWC0(:,4),VWC147(:,4));
[Ixy0_150,Norm_Ixy0_150,Hx0,Hy150,Hxy0_150,Mx0,My150,hpxy0_150,w_x,w_y]=MI_GG_Scott(VWC0(:,4),VWC150(:,4));

[Ixy0,Norm_Ixy0,~,~,Hxy0,~,~,hpxy0,w_x,w_y]= MI_GG_Scott(ERT(:,4),VWC0(:,4));
[Ixy3,Norm_Ixy3,~,~,Hxy3,~,~,hpxy3,~,~]=MI_GG_Scott(ERT(:,4),VWC3(:,4));
[Ixy144,Norm_Ixy144,~,~,Hxy144,~,~,hpxy144,~,~]=MI_GG_Scott(ERT(:,4),VWC144(:,4));
[Ixy147,Norm_Ixy147,~,~,Hxy147,~,~,hpxy147,~,~]=MI_GG_Scott(ERT(:,4),VWC147(:,4));
[Ixy150,Norm_Ixy150,~,~,Hxy150,~,~,hpxy150,~,~]=MI_GG_Scott(ERT(:,4),VWC150(:,4));