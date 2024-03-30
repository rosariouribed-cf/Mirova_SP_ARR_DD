################################################################################
####                   Reproducibility code for:                             ###
##        Total carbon accumulation in a tropical forest landscape            ##
# Authors: Carlos A. Sierra, Jorge I. del Valle, Hector I. Restrepo            #
##############################################################     08/2012
# Submitted to Carbon Balance and Management
################################################################################

TC=read.csv("/.../PorcedB2012.csv") #set to location where accompanying data is stored
ks=read.csv("/.../Chambers_k.csv")  #set to location where accompanying data is stored

TCS=0.45*(TC$AGB+TC$Palm+TC$HV+TC$CR+TC$FR+TC$FL+TC$CWD)+TC$SC15+TC$SC30

Palm=ifelse(TC$Palm==0,NA,TC$Palm)
HV=ifelse(TC$HV==0,NA,TC$HV)
CR=ifelse(TC$CR==0,NA,TC$CR)
FR=ifelse(TC$FR==0,NA,TC$FR)
FL=ifelse(TC$FL==0,NA,TC$FL)
CWD=ifelse(TC$CWD==0,NA,TC$CWD)
SOC=TC$SC15+TC$SC30
Dead=0.45*(FL+CWD)
TAGB=0.45*(TC$AGB+TC$Palm+TC$HV)
TBB=0.45*(TC$CR+TC$FR)

par(mfrow=c(3,3), mar=c(5,4,1,1))
plot(TC$Age,TC$AGB, ylab="Tree aboveground biomass", xlab="Age (years)",pch=19)
plot(TC$Age, Palm, ylab="Palm aboveground biomass", xlab="Age (years)",pch=19)
plot(TC$Age,HV, ylab="Herbaceous vegetation", xlab="Age (years)",pch=19,ylim=c(0,1))
plot(TC$Age, TC$CR,  ylab="Coarse root biomass", xlab="Age (years)",pch=19)
plot(TC$Age, FR,  ylab="Fine root biomass", xlab="Age (years)",pch=19)
plot(TC$Age, FL,  ylab="Fine litter", xlab="Age (years)",pch=19)
plot(TC$Age, TC$CWD,  ylab="Coarse woody debris", xlab="Age (years)",pch=19)
plot(TC$Age,TC$SC15,  ylab="Soil carbon  0-15 cm depth", xlab="Age (years)",pch=19,ylim=c(0,100))
plot(TC$Age,TC$SC30, ylab="Soil carbon 15-30 cm depth", xlab="Age (years)",pch=19,ylim=c(0,100))
par(mfrow=c(1,1), mar=c(5,4,4,2))

Age=TC$Age

TAGB.m=nls(TAGB~(247.8*0.45)*(1-exp(-b1*Age))^b2, start=list(b1=0.05,b2=1)) #The fitted model for total aboveground biomass
TBB.m=nls(TBB~(83.7*0.45)*(1-exp(-b1*Age))^b2, start=list(b1=0.05,b2=1)) #The fitted model for total belowground biomass
TDM.m=nls(Dead~(14.7*0.45)/(1+b1*exp(b2*Age)), start=list(b1=4,b2=-0.09)) #The fitted model for total dead mass

# Implement models as function of time
TAGBf=function(t){(247.8*0.45)*(1-exp(-coef(TAGB.m)[1]*t))^coef(TAGB.m)[2]}
TBBf=function(t){(83.7*0.45)*(1-exp(-coef(TBB.m)[1]*t))^coef(TBB.m)[2] }
TDMf=function(t){(14.7*0.45)/(1+coef(TDM.m)[1]*exp(coef(TDM.m)[2]*t))}

x=c(0:150)  #A vector with the x axis for ploting
TAGB.fit=TAGBf(x) #Function for plotting
TBB.fit=TBBf(x)
TDM.fit=TDMf(x)


par(mfrow=c(2,3))
plot(TC$Age,TAGB,xlim=c(0,100),ylim=c(0,200),xlab="Successional age (years)",ylab=expression(paste("TAGB (Mg C ",ha^-1,")")))
lines(x,TAGB.fit)
boxplot(rnorm(1000,(247.8*0.45),(40.5*0.45)),add=TRUE,at=100,outline=FALSE)
abline(h=247.8*0.45,lty=2)
plot(TC$Age,TBB,xlim=c(0,100),ylim=c(0,60),xlab="Successional age (years)",ylab=expression(paste("TBB (Mg C ",ha^-1,")")))
lines(x,TBB.fit)
boxplot(rnorm(1000,(83.7*0.45),(17.2*0.45)),add=TRUE,at=100,outline=FALSE)
abline(h=83.7*0.45,lty=2)
plot(TC$Age,Dead,xlim=c(0,100),ylim=c(0,10),xlab="Successional age (years)",ylab=expression(paste("TDM (Mg C ",ha^-1,")")))
lines(x,TDM.fit)
boxplot(rnorm(1000,(14.7*0.45),(2.2*0.45)),add=TRUE,at=100,outline=FALSE)
abline(h=14.7*0.45,lty=2)
plot(TC$Age,SOC,xlim=c(0,100),ylim=c(0,200),xlab="Successional age (years)",ylab=expression(paste("SOC (Mg C ",ha^-1,")")))
abline(h=mean(SOC,na.rm=T))
abline(h=96.6,lty=2)
boxplot(rnorm(1000,96.6,2.5),add=TRUE,at=100,outline=FALSE)
plot(TC$Age,TCS,xlim=c(0,100),ylim=c(0,400),xlab="Successional age (years)",ylab=expression(paste("TCS (Mg C ",ha^-1,")")))
lines(x,(TAGB.fit+TBB.fit+TDM.fit+mean(SOC,na.rm=T)))
boxplot(rnorm(1000,(111.6+6.6+37.6+96.6),sqrt((18.5^2)+(1^2)+(7.8^2)+(2.5^2))),add=TRUE,at=100,outline=FALSE)
abline(h=111.6+6.6+37.6+96.6,lty=2)
plot(x,TAGB.fit/(TAGB.fit+TBB.fit+TDM.fit+mean(SOC,na.rm=T)),type="l",xlab="Successional age (years)",ylab="Proportion",ylim=c(0,1))
lines(x,TAGB.fit/(TAGB.fit+TBB.fit+TDM.fit),lty=2)
legend("bottomright",c("TAGB/TCS", "TAGB/TM"),lty=c(1,2),bty="n")
par(mfrow=c(1,1))

TOC.hat=TAGB.fit+TBB.fit+TDM.fit+mean(SOC,na.rm=T)
TM.hat=(TAGB.fit+TBB.fit+TDM.fit)
dTOC=c(NA,diff(TOC.hat))

### Independent data
#Calculated from Saldarriaga 1994, and Saldarriaga et al. 1988
Sy=c(rep(10,4),rep(20,4),rep(35,4),rep(60,3),rep(80,4))
VM=c(116,116,33,104, 143,119,120,139, 107,167,161,217, 194,272,234, 194,256,260,221)

#Data from Fonseca et al. 2011, Table 2.
fy=c(0,4.5,6.5,8,10,12,18,20)
fc=c((1.15+99.1),(17.1+95.1),(19.4+96),(36.4+90.1),(87.1+90.4),(49.2+114.8),(31.7+115.1),(47.3+133.1))

par(mfrow=c(2,1),mar=c(4,4,0.5,1))
plot(x,TOC.hat,type="l",xlab=" ",ylab=expression(paste("TCS (Mg C ",ha^-1,")")),ylim=c(0,300),xlim=c(0,100))
points(TC$Age,TCS)
points(Sy,VM,pch=19)
points(fy,fc,pch=15,col=2)
legend("bottomright", c("Tropical Andes","Amazon","Costa Rica"),pch=c(21,19,15),col=c(1,1,2),bty="n")
plot(x,dTOC,type="l",xlab="Age (years)",ylab=expression(paste("NECB (Mg C ",ha^-1," ",yr^-1,")")))
par(mfrow=c(1,1),mar=c(5,4,4,2))

#Differences between predictions and independent data
Sald.pred=TAGBf(Sy)+TBBf(Sy)+TDMf(Sy)+mean(SOC,na.rm=T)
Fons.pred=TAGBf(fy)+TBBf(fy)+TDMf(fy)+mean(SOC,na.rm=T)

RMSE.Sald=sqrt(sum((Sald.pred-VM)^2)/length(VM))
RMSE.Fons=sqrt(sum((Fons.pred-fc)^2)/length(fc))

######### Simulations
Cp=(111.6+6.6+37.6)
k=median(ks$k)
Mass=TAGB.fit+TBB.fit+TDM.fit

Leg1=Cp*exp(-k*x)
Leg2=0.7*Cp*exp(-k*x)
Leg3=0.3*Cp*exp(-k*x)

Legs=c(0,30,70,100)
max.necb=c(max(diff(Mass)),max(diff(Mass+Leg3)),max(diff(Mass+Leg2)),max(diff(Mass+Leg1)))
age.max.necb=c(which.max(diff(Mass)),which.max(diff(Mass+Leg3)),which.max(diff(Mass+Leg2)),which.max(diff(Mass+Leg1)))-1

par(mfrow=c(2,2),mar=c(4,4.5,1.5,1))
plot(x,Leg1,type="l",col=2,xlab=" ",ylab=expression(paste("Carbon stores (Mg C ",ha^-1,")")),xlim=c(0,100),ylim=c(0,160))
lines(x,Mass)
lines(x,Leg2,col=3)
lines(x,Leg3,col=4)
legend(40,60,c("100% Legacy","70% Legacy","30% Legacy","C accumulation"),lty=c(1,1,1,1),col=c(2,3,4,1),bty="n")
title(main="a",cex.main=0.9, font.main=1)

plot(x,Mass+Leg1,col=2,type="l",ylim=c(0,160),xlab="Age (years)",ylab=expression(paste("TCS (Mg C ",ha^-1,")")),xlim=c(0,100))
lines(x,Mass+Leg2,col=3)
lines(x,Mass+Leg3,col=4)
lines(x,Mass)
legend("bottomright",c("100% Legacy","70% Legacy","30% Legacy","0% Legacy"),lty=c(1,1,1,1),col=c(2,3,4,1),bty="n")
title(main="b",cex.main=0.9, font.main=1)

plot(x,c(NA,diff(Mass+Leg1)),col=2,type="l",ylim=c(-30,5),xlab="Age (years)",ylab=expression(paste("NECB (Mg C ",ha^-1," ",yr^-1,")")),xlim=c(0,100))
lines(x,c(NA,diff(Mass+Leg2)),col=3)
lines(x,c(NA,diff(Mass+Leg3)),col=4)
lines(x,c(NA,diff(Mass)))
abline(h=0,lty=2)
legend("bottomright",c("100% Legacy","70% Legacy","30% Legacy","0% Legacy"),lty=c(1,1,1,1),col=c(2,3,4,1),bty="n")
title(main="c",cex.main=0.9, font.main=1)

par(mar=c(4,4,1.5,4))
plot(Legs,max.necb,type="b",ylim=c(2.5,5),bty="n",xlab="% C legacy",ylab=expression(paste("max(NECB) (Mg C ",ha^-1," ",yr^-1,")")))
par(new=TRUE)
plot(Legs,age.max.necb,type="b",axes=FALSE,ylab="",xlab="",col=2)
axis(4,col=2)
mtext("Age max(NECB) (years)", side=4, line=3,cex=0.9)
title(main="d",cex.main=0.9, font.main=1)
par(mfrow=c(1,1),mar=c(5,4,4,2))


Aged1=rexp(10000,1/3)
Aged2=rexp(10000,1/5)
Aged3=rexp(10000,1/10)
Aged4=rexp(10000,1/20)
Aged5=rexp(10000,1/50)


par(mfrow=c(2,2))
hist(Aged1,xlab="Mean age (years)",main="E[t] = 3",freq=F,ylim=c(0,0.25))
hist(Aged2,xlab="Mean age (years)",main="E[t] = 5",freq=F,ylim=c(0,0.25))
hist(Aged3,xlab="Mean age (years)",main="E[t] = 10",freq=F,ylim=c(0,0.25))
hist(Aged4,xlab="Mean age (years)",main="E[t] = 20",freq=F,ylim=c(0,0.25))
par(mfrow=c(1,1))

boxplot(Aged1,Aged2,Aged3,Aged4,Aged5,names=c(3,5,10,20,50))

##
NEPf=function(x,Cp,k){
  A=(247.8*0.45)*(1-exp(-coef(TAGB.m)[1]*x))^coef(TAGB.m)[2] #A vector with the fitted model
  B=(83.7*0.45)*(1-exp(-coef(TBB.m)[1]*x))^coef(TBB.m)[2] #A vector with the fitted model
  M=(14.7*0.45)/(1+coef(TDM.m)[1]*exp(coef(TDM.m)[2]*x)) #A vector with the fitted model
  
  a=(247.8*0.45)*(1-exp(-coef(TAGB.m)[1]*(x-1)))^coef(TAGB.m)[2] #A vector with the fitted model
  b=(83.7*0.45)*(1-exp(-coef(TBB.m)[1]*(x-1)))^coef(TBB.m)[2] #A vector with the fitted model
  m=(14.7*0.45)/(1+coef(TDM.m)[1]*exp(coef(TDM.m)[2]*(x-1))) #A vector with the fitted model
  
  Tot=A+B+M
  tot=a+b+m
  Leg=Cp*exp(-k*x)
  leg=Cp*exp(-k*(x-1))
  NEP=((Tot+Leg)-(tot+leg))
  
  return(data.frame(x,Tot,NEP))
}

#100% legacy

H1=NEPf(x=Aged1,Cp=Cp, k=sample(ks$k,1000,replace=TRUE))
H2=NEPf(x=Aged2,Cp=Cp,k=sample(ks$k,1000,replace=TRUE))
H3=NEPf(x=Aged3,Cp=Cp,k=sample(ks$k,1000,replace=TRUE))
H4=NEPf(x=Aged4,Cp=Cp,k=sample(ks$k,1000,replace=TRUE))
H5=NEPf(x=Aged5,Cp=Cp,k=sample(ks$k,1000,replace=TRUE))

#70% legacy


S1=NEPf(x=Aged1,Cp=0.7*Cp,k=sample(ks$k,1000,replace=TRUE))
S2=NEPf(x=Aged2,Cp=0.7*Cp,k=sample(ks$k,1000,replace=TRUE))
S3=NEPf(x=Aged3,Cp=0.7*Cp,k=sample(ks$k,1000,replace=TRUE))
S4=NEPf(x=Aged4,Cp=0.7*Cp,k=sample(ks$k,1000,replace=TRUE))
S5=NEPf(x=Aged5,Cp=0.7*Cp,k=sample(ks$k,1000,replace=TRUE))

#30% legacy

T1=NEPf(x=Aged1,Cp=0.3*Cp,k=sample(ks$k,1000,replace=TRUE))
T2=NEPf(x=Aged2,Cp=0.3*Cp,k=sample(ks$k,1000,replace=TRUE))
T3=NEPf(x=Aged3,Cp=0.3*Cp,k=sample(ks$k,1000,replace=TRUE))
T4=NEPf(x=Aged4,Cp=0.3*Cp,k=sample(ks$k,1000,replace=TRUE))
T5=NEPf(x=Aged5,Cp=0.3*Cp,k=sample(ks$k,1000,replace=TRUE))

#0% legacy

Z1=NEPf(x=Aged1,Cp=0*Cp,k=sample(ks$k,1000,replace=TRUE))
Z2=NEPf(x=Aged2,Cp=0*Cp,k=sample(ks$k,1000,replace=TRUE))
Z3=NEPf(x=Aged3,Cp=0*Cp,k=sample(ks$k,1000,replace=TRUE))
Z4=NEPf(x=Aged4,Cp=0*Cp,k=sample(ks$k,1000,replace=TRUE))
Z5=NEPf(x=Aged5,Cp=0*Cp,k=sample(ks$k,1000,replace=TRUE))

par(mfrow=c(2,2),mar=c(4,4.5,1.5,1))
boxplot(H1$NEP,H2$NEP,H3$NEP,H4$NEP,H5$NEP,ylim=c(-40,5),names=c(3,5,10,20,50),xlab="Mean age",ylab=expression(paste("NECB (Mg C ",ha^-1," ",yr^-1,")")),main="100% legacy",outline=FALSE)
abline(h=0,lty=2)
abline(h=3.8,col=2)
boxplot(S1$NEP,S2$NEP,S3$NEP,S4$NEP,S5$NEP,ylim=c(-40,5),names=c(3,5,10,20,50),xlab="Mean age",ylab=expression(paste("NECB (Mg C ",ha^-1," ",yr^-1,")")),main="70% legacy",outline=FALSE)
abline(h=0,lty=2)
abline(h=3.8,col=2)
boxplot(T1$NEP,T2$NEP,T3$NEP,T4$NEP,T5$NEP,ylim=c(-40,5),names=c(3,5,10,20,50),xlab="Mean age",ylab=expression(paste("NECB (Mg C ",ha^-1," ",yr^-1,")")),main="30% legacy",outline=FALSE)
abline(h=0,lty=2)
abline(h=3.8,col=2)
boxplot(Z1$NEP,Z2$NEP,Z3$NEP,Z4$NEP,Z5$NEP,ylim=c(-40,5),names=c(3,5,10,20,50),xlab="Mean age",ylab=expression(paste("NECB (Mg C ",ha^-1," ",yr^-1,")")),main="0% legacy",outline=FALSE)
abline(h=0,lty=2)
abline(h=3.8,col=2)
par(mfrow=c(1,1))

################################################################################
# Run on R version 2.14.1