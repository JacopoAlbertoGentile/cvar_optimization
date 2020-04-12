#Cvar_Minimization 
#Monte Carlo Simulation for Cvar
#First: Create a function to minimize the CVar. I ran Monte Carlo simulation assuming return are normaly distributed
F_beta=function(P,beta,Assets)
{
  Time=12
  npaths=10000
  riskfree=0.02
  sigma=0.2
  deltatime=1/Time
  R=rep(0,npaths)
  for(i in 1:npaths)
  {
    R[i]=(max(-P[2]*rnorm(1,mean=mean(Assets[,1]),sd=sd(Assets[,1]))-P[3]*rnorm(1,mean=mean(Assets[,2]),sd=sd(Assets[,2]))-P[4]*rnorm(1,mean=mean(Assets[,3]),sd=sd(Assets[,3]))-P[5]*rnorm(1,mean=mean(Assets[,4]),sd=sd(Assets[,4]))-P[6]*rnorm(1,mean=mean(Assets[,5]),sd=sd(Assets[,5]))-P[1],0))
  }
  out=P[1]+(1/(1-beta))*mean(R) 
  return(out)
}
equf=function(P,beta,Assets)
{
  out=P[2]+P[3]+P[4]+P[5]+P[6]
  return(out)
}
ineq1=function(P,beta,Assets)
{
  out1=P[2]
  out2=P[3]
  out3=P[4]
  out4=P[5]
  out5=P[6]
  z=c(out1,out2,out3,out4,out5)
  return(z)
}
x0=c(1,0,0,0,0,0)
Min=solnp(pars = x0,fun = F_beta,eqfun = equf,eqB = c(1),ineqfun =ineq1,ineqLB =c(0,0,0,0,0),ineqUB =c(Inf,Inf,Inf,Inf,Inf),beta=0.95,control = list("delta"=0.001,"inner.iter"=100,"outer.iter"=50),Assets=Assets)
# resulting weights from the minimization of the Cvar
#Weights=Min$par[2:6]
#Weights=c(0.217272803,0.218831627,0.20792099,0.177297799,0.17867678)
Portfolio_CVar=(as.matrix(Assets)%*%Weights)
#Create Portfolio and ran Control in Sample and Out of Sample
reg1=lm(Portfolio_CVar~Factors$USMuni+Factors$USTbill1.3yr+Factors$USAGG+Factors$GlobalAGG+Factors$USDollar+Factors$Commodities+Factors$LongShortHF+Factors$EventDrivenHF+Factors$MacroHF+Factors$RelativeValueHF+Factors$EMBond+Factors$TIPs+Factors$HighYield+Factors$MSCIWorld+Factors$MSCICanada+Factors$MSCIEAFE+Factors$MSCIEMEquity+Factors$MSCIEurope+Factors$MSCIJapan+Factors$MSCIAsiaex+Factors$USGrowth+Factors$USValue+Factors$USSmallCap+Factors$USMidCap+Factors$Gold+Factors$S.P500)
summary(reg1)
Betas1=c(reg1$coefficients[2],reg1$coeffcients[3],reg1$coefficients[8],reg1$coefficients[12],reg1$coefficients[14],reg1$coefficients[19],reg1$coefficients[26])
BestFactors=data.frame(Factors$USMuni[13:120],Factors$LongShortHF[13:120],Factors$EMBond[13:120],Factors$HighYield[13:120],Factors$MSCIEurope[13:120],Factors$Gold[13:120])
C=cov(as.matrix(BestFactors))
PCA=eigen(C)
NewFactors=(as.matrix(BestFactors)%*%PCA$vectors)
regPCA=lm(Portfolio_CVar[13:120]~NewFactors)
summary(regPCA)
PCA_Factors_BackTesting=NewFactors[1:65,]
reg2=lm(Portfolio_CVar[1:65]~PCA_Factors_BackTesting[,1]+PCA_Factors_BackTesting[,2])
summary(reg2)
Validation_Beta=as.vector(reg2$coefficients)
Const=rep(1,43)
Validation_Factors=cbind(Const,as.matrix(NewFactors[66:108,1:2]))
Validation_Portfolio=Validation_Factors%*%as.array(Validation_Beta)
Assets_Portfolio=as.matrix(Assets[66:108,])%*%Weights
PredictedError=sqrt((mean(Assets_Portfolio-Validation_Portfolio)^2))
###############################################
OutSample_Factors=Data1[13:180,2:27]
CC=cov(OutSample_Factors)
PCA1=eigen(CC)
NewFactors_OutSample=(as.matrix(OutSample_Factors)%*%PCA1$vectors)
Const1=rep(1,13)
Validation_OutofSample=cbind(Const1,NewFactors_OutSample[132:144,1:2])
Validation_OutSamplePortfolio=Validation_OutofSample%*%Validation_Beta