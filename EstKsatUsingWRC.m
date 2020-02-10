
% Code to estimate saturated hydraulic conductivity using water retension parameters
%Mingming Qin, Nov, 2018
%input and output units [cm] and [day]
%output, ks: predictions; KsMethod: name of the method used;
%the third output is just for calculation check.
%VGpara=WRCPara(i).vGxVol;
%BCpara=WRCPara(i).BCxVol;
function [Ks,KsMethod,EffPorBC]=EstKsatUsingWRC(VGpara,BCpara)

if VGpara(2)>1;
    VGpara(1)=VGpara(1)/100;
    VGpara(2)=VGpara(2)/100;
end

QrVG=VGpara(1);
QsVG=VGpara(2);
Alpha=VGpara(3);
n=VGpara(4);
if BCpara(2)>1;
    BCpara(1)=BCpara(1)/100;
    BCpara(2)=BCpara(2)/100;
end
QrBC=BCpara(1);
QsBC=BCpara(2);
Pa=BCpara(3); 
lamda=BCpara(4);    
Q33BC=((330/Pa).^(-lamda))*(QsBC-QrBC)+QrBC;
if Q33BC>QsBC
    Q33BC=QsBC;
end
EffPorBC=QsBC-Q33BC;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
u=0.001;% %@20 c(unit:Pa s  or N s/m2 or kg/(m s)		
Dw=999.8;% %density of water @20 c(kg m-3)
g=9.8;%(m/s2)
gamma=0.0728; %N/m  
ConstC=gamma^2/(2*u*Dw*g)*10^6*60*60*24; %C=2.7*10^-4 m3/s unit convert:*10^6*60*60*24 cm3/d

%%%%%%%%%%%%%%%

KsMethod={'Rawls BC','Timlin BC','Han VG',...
    'Matthews VG','Brutsaert BC','Qin BC','Mishra VG','Guarracino VG','Nasta BC'...
    'Katz BC'};%,'Javis2017'


%1 fist column,Rawls 1998
%Ks(1,1)=1930*EffPorBC^(3-lamda);
Ks(1,1)=1930*EffPorBC^(3-lamda);

%2 Timlin1999
%Ks(1,2)=0.0131*(0.148/(Pa*1.86*(2-lamda)^5.34))^0.5*EffPorBC^2.5*(100*60*60*24); %Ks m/s to cm/d
 Ks(1,2)=0.0131*(0.148/(Pa*1.86*(2-lamda)^5.34))^0.5*EffPorBC^2.5*(100*60*60*24); %Ks m/s to cm/d

%3 Han2008 use VG parameters;
Dinf=291*Alpha*10*(1-1/n)^(1/n); %Alpha*10 cm-1 to kPa-1; %Dinf unit:um (Eq 10)
Cinf=20*Dinf;% cm/h (Eq 13)
Qinf=(QsVG-QrVG)*(1+n/(n-1))^(1/n-1)+QrVG; %(Eq 7)
S=n*(QsVG-QrVG)*((2*n-1)/(n-1))^(1/n-2); %(Eq 8)
Pinf=QsVG-Qinf; %effective porosity (Eq 5)
Ks(1,3)=Cinf*Pinf^(3-S/Qinf)*24; %cm/h*24=cm/d  


%4 'Matthews2010'
Ks(1,4)=ConstC*Alpha^2*(QsVG-QrVG)^2; 

%5 Brutsaert1967
Ks(1,5)=ConstC*(QsBC-QrBC)^2/Pa^2*lamda^2/((lamda+1)*(lamda+2)); %cm/d 


%6 'Qin2019'
%%%porosity/tao: simplified with eq: tao=1-0.49*ln(porosity) (Mauret and Renaud 1997)
%Pearson 0.4942 and 3 extreme Ksat;fits Loose core good
%QC=QsBC/(1-0.49*log(QsBC)); 

%%%porosity/tao: simplified with eq: tao=porosity^(-0.4) (Mota 2001)
%Pearson 0.4946 and 3 extreme Ksat; fits Loose core good
QC=QsBC^1.4; 

%porosity/tao: simplified with eq: tao=porosity/(1-(1-porosity)^(2/3)) (Du Plessis and Masliyah 1991)
%Pearson 0.4778 and 5 extreme ksat (27 above 10,000); Fits compacted cores good
%QC=1/(1-(1-QsBC)^(2/3)); 

%(Koponen 1996)
%Pearson 0.4934 and 3 extreme Ksat; fits Loose core good
%QC=QsBC/(1+0.8*(1-QsBC)); 

Ks(1,6)=ConstC*QC*(1/Pa)^2*(QsBC-QrBC)^2*lamda^2./((lamda+1)*(lamda+2));%m/s=(100*60*60*24)cm/d


%7 Mishra1990
%Cmp=108; %cm3/s Eq 2 in Ghanbarian 2017
%Ks(1,3)=Cmp*(QsVG-QrVG)^(5/2)*Alpha^2*60*60*24; %cm/s*60*60*24=cm/d
Cmp=3.89*10^5/2.5; %cm/h
Ks(1,7)=Cmp*(QsVG-QrVG)^(5/2)*Alpha^2*24; %cm/h*24=cm/d

%8 Guarracino2007
Dcal=1.996;

Ks(1,8)=46500*QsVG*Alpha^2*24; %experimental works better then equation below
%Ks(1,8)=1.74*10^5*(2-Dcal)/(4-Dcal)*QsVG*Alpha^2*24;%cm/h*24=cm/d



%9 Nasta2013, use Brooks and Corey
Tao=0.0138;
Ks(1,9)=9.579*10^5*Tao*(lamda/(lamda+2))*QsBC/Pa^2*24; %cm/h*24=cm/d




%10 Kats and Thompson
D=3-lamda;
Rmax=0.149/Pa*0.01;  %cm to m
RcWRC=Rmax*(1-QrBC/QsBC)^(1/(3-D)); %unit m from rmax derictly;Ghanbarian 2017 Eq 10
RcipF=QsBC/3*(1-(RcWRC/(3*Rmax))^(3-D)); %1/F
RcipcC=1/56.5;
k=RcipcC*RcipF*RcWRC^2;
Ks(1,10)=k*Dw*g/u*(100*60*60*24); %ks=dw*g*k/u; m/s to cm/d %from water retention