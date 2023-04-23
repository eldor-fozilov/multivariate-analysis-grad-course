##########################
##### CCA R examples #####
###################################################################################
## references:
## 1. https://blog.naver.com/shoutjoy/222037125132
## 2. https://stats.idre.ucla.edu/r/dae/canonical-correlation-analysis/
#############################################################################################################################
##### 1. Use CCA R package #####
################################
## required libraries
#install.packages("MVT",dependencies=T)
#install.packages("ggplot2",dependencies=T)
#install.packages("GGally",dependencies=T)
#install.packages("CCA",dependencies=T)
#install.packages("CCP",dependencies=T)
#install.packages("knitr",dependencies=T)

library(MVT)
library(ggplot2)
library(GGally)
library(CCA)
library(CCP)
library(knitr)

#### read data
# Dataset from Mardia, Kent and Bibby on 88 students who took examinations in five subjects. 
# The first two subjects were tested with closed book exams and the last three were tested with open book exams.
data(examScor)
examScor
str(examScor)

#### divide raw variables into two groups
X<-examScor[,1:2]
Y<-examScor[,3:5]

head(X)
head(Y)

ggpairs(X)
ggpairs(Y)
ggpairs(examScor)

## correlations
matcor(X,Y)

####### R Canonical Correlation Analysis #######
cc1<-cc(X,Y)
str(cc1)

###### outputs
# 1. canonical correlations #####
# 2. canonical coefficients
# 3. canonical variates (variables/scores) #####
# 4. canonical loadings

#### 1. canonical correlations (sqrt of eigenvlaues)
cc1$cor

# eigenvalues
cc1$cor^2

#### 2. RAW canonical coefficients
cc1$xcoef
cc1$ycoef

#### 3. canonical variates (variables/scores)
cc1$scores$xscores
cc1$scores$yscores

# w.r.t. canonical correlations 
cor(cc1$scores$xscores[,1],cc1$scores$yscores[,1])
cor(cc1$scores$xscores[,2],cc1$scores$yscores[,2])

#### 4. canonical loadings 
##The correlations are between observed variables and canonical variables which are known as the canonical loadings. - for interpretation
cc1$scores$corr.X.xscores
cc1$scores$corr.Y.xscores
cc1$scores$corr.X.yscores
cc1$scores$corr.Y.yscores

#### tests of canonical dimensions ####
rho<-cc1$cor
## Define number of observations, number of variables in first set, and number of variables in the second set.
n<-dim(X)[1]
p<-length(X)
q<-length(Y)

## Calculate p-values using the F-approximations of different test statistics:
p.asym(rho,n,p,q,tstat="Wilks")
p.asym(rho,n,p,q,tstat="Roy")
p.asym(rho,n,p,q,tstat="Hotelling")
p.asym(rho,n,p,q,tstat="Pillai")

##### standardized canonical coefficients ####
## X
s1<-diag(sqrt(diag(cov(X))))
stdccX<-s1%*%cc1$xcoef
rownames(stdccX)<-names(X)
colnames(stdccX)<-c("1st pair","2nd pair")
stdccX

## Y
s2<-diag(sqrt(diag(cov(Y))))
stdccY<-s2%*%cc1$ycoef
rownames(stdccY)<-names(Y)
colnames(stdccY)<-c("1st pair","2nd pair")
stdccY

#### Report ####
cca_result<- rbind(stdccX,stdccY,Canonical_Correlation=rho)
colnames(cca_result)=c("1st pair", "2nd pair")
kable(cca_result, format = "pandoc", digits = 3, caption = "Canonical Correlation Analysis")

#### Plots ####  
Xsc<-cc1$scores$xscores
Ysc<-cc1$scores$yscores

## Plot 1 - Canonical variables (1st pair and 2nd pair)
par(mfrow=c(1,2))
par(pty="s")

plot(Xsc[,1],Ysc[,1],xlab="Closed-Book Canonical variable",ylab="Open-Book Canonical variable",main="1st pair",cex=0.5,pch=16)
plot(Xsc[,2],Ysc[,2],xlab="Closed-Book Canonical variable",ylab="Open-Book Canonical variable",main="2nd pair",cex=0.5,pch=16)

## Plot 2 - canonical coefficients + observations
windows()
par(mfrow=c(1,2))
par(pty="s")

lim1<-range(pretty(Xsc))
lim2<-range(pretty(Ysc))
biplot(Xsc,stdccX,xlab="1st Dimemsion",ylab="2nd Dimension",main="(a) Closed-Book",xlim=lim1,ylim=lim1,cex=0.5,pch=16)
abline(v=0,h=0,lty=2,col=4)
biplot(Ysc,stdccY,xlab="1st Dimemsion",ylab="2nd Dimension",main="(b) Open-Book",xlim=lim1,ylim=lim1,cex=0.5,pch=16)
abline(v=0,h=0,lty=2,col=4)


#####################################################################################################################################
#### 2. Manual ####
###################

### Required R packages
#install.packages("MVT",dependencies=T)
#install.packages("expm")
library(MVT)
library(expm)

### read data
# Dataset from Mardia, Kent and Bibby on 88 students who took examinations in five subjects. 
# The first two subjects were tested with closed book exams and the last three were tested with open book exams.
data(examScor)
examScor
str(examScor)

### (full) Correlation matrix R
R<-round(cor(examScor),3)
R

### Partitioning R
RXX<-R[1:2,1:2]
RYY<-R[3:5,3:5]
RXY<-R[1:2,3:5]
RYX<-t(RXY)

RXX
RYY
RXY
RYX

### Define Q matrix
Q=solve(sqrtm(RYY))%*%RYX%*%solve(sqrtm(RXX))


#####################################################
#### 1. Singular Value Decomposition (SVD)
svd(Q)

## eigenvalues
ev_sv<-svd(Q)$d^2
round(ev_sv,3)

## canonical coefficients
# canonical coefficient a (Y)
a_sv<-solve(sqrtm(RYY))%*%svd(Q)$u
round(a_sv,3)

# canonical coefficient b (X)
b_sv<-solve(sqrtm(RXX))%*%svd(Q)$v
round(b_sv,3)

#######################################################
#### 2. Spectral decomposition
### QQT 
## eigenvalues
eigen(Q%*%t(Q))
ev_s1=eigen(Q%*%t(Q))$values
round(ev_s1,3)

## canonical coefficient a (Y)
a_s<-solve(sqrtm(RYY))%*%eigen(Q%*%t(Q))$vectors
k<-min(dim(RXX)[1],dim(RYY)[1]) ## k: smaller number of variables
a_s<-a_s[,1:k]  ## choose the first k columns!
round(a_s,3)

### QTQ 
## eigenvalues
eigen(t(Q)%*%Q)
ev_s2=eigen(Q%*%t(Q))$values
round(ev_s2,3)

## canonical coefficient b (X)
b_s<-solve(sqrtm(RXX))%*%eigen(t(Q)%*%Q)$vectors
round(b_s,3)

##########################################
#### SVD vs. Spectral Comparisons
## eigenvalues
ev_sv
ev_s1
ev_s2

## canonical coefficient a
a_sv
a_s

## canonical coefficient b
b_sv
b_s

