########################################
########################################           Chapter 7 Collinearity
########################################


########################################  Example of Consumer Expenditure and Stock
CoE_Stk<-read.table('./Downloads/P211.txt',header=TRUE)   ## read the data
mod1<-lm(Expenditure~Stock,data=CoE_Stk)
summary(mod1)


###  Standardized residual versus Index plot
pii=hatvalues(mod1)
plot(1:20,mod1$residuals/(summary(mod1)$sigma * sqrt(1-pii)),xlab="Index",ylab="Standardized Residual",type="b")

###  compute the statistics of runs test
n1<-length(which(mod1$residuals >0))    ## number of positive residuals
n2<-length(which(mod1$residuals <0))    ## number of negative residuals
mu<-2*n1*n2/(n1+n2)+1
ssq<-2*n1*n2*(2*n1*n2-n1-n2)/(n1+n2)^2*1/(n1+n2-1)
num_runs<-5
run_stat<-(num_runs-mu)/sqrt(ssq)

### use built-in Runs test
library(snpar)   # or use the library library(randtests)
runs.test(mod1$residuals,threshold=0)    ## by default the threshold is set as the median of the given data





############################################################  Implement Cochrane and Orcutt method
CoE_Stk<-read.table('data/P211.txt',header=TRUE)   ## read the data
mod1<-lm(Expenditure~Stock,data=CoE_Stk)
hatrho<-sum(mod1$residuals[-1] * mod1$residuals[-20])/sum(mod1$residuals^2)    #### calculate hatrho in the first iteration


###### transformation data for the 2nd iteration
CoE_Stk_new<-data.frame(y=CoE_Stk$Expenditure[-1]-CoE_Stk$Expenditure[-20] * hatrho, x<-CoE_Stk$Stock[-1]-CoE_Stk$Stock[-20] * hatrho)
mod2<-lm(y~x, data=CoE_Stk_new)
summary(mod2)

hatrho<-sum(mod2$residuals[-1] * mod2$residuals[-19])/sum(mod2$residuals^2)    #### calculate hatrho in the 2nd iteration
hatrho





############################################################################  Example on Equal Education Opportunity Data
EEO_dat<-read.table('data/P236.txt',header=TRUE)   ## read the data
mod1<-lm(ACHV~.,data=EEO_dat)
summary(mod1)

pii=hatvalues(mod1)
plot(mod1$fitted.values,mod1$residuals/(summary(mod1)$sigma * sqrt(1-pii)),xlab="Fitted Value",ylab="Standardized Residual",type="p")





#############################################################################  Example on French Economy
FREcon_dat<-read.table('data/P241.txt',header=TRUE)   ## read the data
mod1<-lm(IMPORT~.-YEAR, data=FREcon_dat)
summary(mod1)

pii=hatvalues(mod1)
plot(1:dim(FREcon_dat)[1],mod1$residuals/(summary(mod1)$sigma * sqrt(1-pii)),xlab="Index",ylab="Standardized Residual",type="b")

####### use only part of the data
FREcon_dat_new <- FREcon_dat[1:11,]
mod1<-lm(IMPORT~.-YEAR, data=FREcon_dat_new)
summary(mod1)

pii=hatvalues(mod1)
plot(1:dim(FREcon_dat_new)[1],mod1$residuals/(summary(mod1)$sigma * sqrt(1-pii)),xlab="Index",ylab="Standardized Residual",type="b")






##########################################################################  Example on Advertising data
Adv_dat<-read.table('data/P248.txt',header=TRUE)   ## read the data
mod1<-lm(S_t~.,data=Adv_dat)
summary(mod1)


pii=hatvalues(mod1)
plot(1:dim(Adv_dat)[1],mod1$residuals/(summary(mod1)$sigma * sqrt(1-pii)),xlab="Index",ylab="Standardized Residual",type="b")


########### Regress A_t onto P_t, A_{t-1}, P_{t-1}
mod2<-lm(A_t~P_t+A_.t.1.+P_.t.1.,data=Adv_dat)
summary(mod2)



######################################################## Variation Inflation Factor
library(regclass)
EEO_dat<-read.table('data/P236.txt',header=TRUE)   ## read the data
mod1<-lm(ACHV~.,data=EEO_dat)
VIF(mod1)


FREcon_dat<-read.table('data/P241.txt',header=TRUE)   ## read the data
mod1<-lm(IMPORT~.-YEAR, data=FREcon_dat)
VIF(mod1)


Adv_dat<-read.table('data/P248.txt',header=TRUE)   ## read the data
mod1<-lm(S_t~.,data=Adv_dat)
VIF(mod1)




