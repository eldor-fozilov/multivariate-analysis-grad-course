#####################################
## SEM with in R (Lavaan package)  ##
#####################################
#### references
# https://lavaan.ugent.be/tutorial/
# https://global.oup.com/us/companion.websites/9780195367621/examples/

#### required packages
#install.packages("lavaan", dependencies=TRUE)
#install.packages("semPlot", dependencies=TRUE)
#install.packages("lavaanPlot", dependencies=TRUE)

library(lavaan)  ## Main SEM package
library(semPlot) ## for plots
library(lavaanPlot) ## for plots

###########################
#### SYNTAX of Lavaan
# =~	is measured by
# ~	is regressed on
# ~~	is correlated with / variance (e~~e)
# := user defined estimand 


######################################################################
## Example 2: Bollen's industralization/political democracy example ## 
######################################################################
#### read data - Lavaan data format: 1. raw data 2. full covariance matrix
## 1. raw data: read built-in raw data from Lavaan package
pd.data<-PoliticalDemocracy
str(pd.data)
head(pd.data)

## 2. covariance matrix/number of observations
# 2-1. extract cov,n.ob from the raw data
pd.data.cov<-cov(pd.data)
pd.data.cov

n.obs<-dim(pd.data)[1]

# 2-2. input lower triangle covariance matrix/number of observations 
# lower triangle covariance matrix
lower<-'
6.89										
6.25  	15.58									
5.84	  5.84  	10.76								
6.09	  9.51  	6.69  	11.22							
5.06	  5.6    	4.94  	5.7    	6.83						
5.75	  9.39  	4.73  	7.44  	4.98  	11.38					
5.81	  7.54  	7.01  	7.49  	5.82  	6.75	  10.8				
5.67	  7.76  	5.64  	8.01  	5.34  	8.25	  7.59  	10.53			
0.73	  0.62  	0.79  	1.15  	1.08  	0.85	  0.94  	1.1  	0.54		
1.27	  1.49  	1.55  	2.24  	2.06  	1.81	  2	  2.23	  0.99  	2.28	
0.91	  1.17  	1.04  	1.84  	1.58  	1.57	  1.63	  1.69	  0.82  	1.81  	1.98'

pd.data.cov2<-getCov(lower, names=c("y1","y2","y3","y4","y5","y6","y7","y8","x1","x2","x3"))
pd.data.cov2

#number of observations
n.obs<-75
  
### note that there are very small differences (almost identical but not exactly the same) in pd.data.cov and pd.data.cov2 
pd.data.cov-pd.data.cov2


#######################################################################################
###### 1. CFA 1
#### measurement model specification 1
# all exogenous latent variables are correlated by default 
MM1<-'
      # measurement model
      Indust60 =~ x1+x2+x3
      #Indust60 =~ x1
      #Indust60 =~ x2
      #Indust60 =~ x3
      Demo60 =~ y1+y2+y3+y4
      Demo65 =~ y5+y6+y7+y8'

#### fit the model
fit1<-cfa(MM1, data=pd.data) # default option(maximum likelihood) estimator="ML"
#fit1<-cfa(MM1, sample.cov=pd.data.cov, sample.nobs=n.obs)  ## use covariance matrix + number of obs

## report fit measures
fitmeasures(fit1)

## display summary output
summary(fit1,fit.measures=T)

## standardized estimates
# "std.lv", the standardized estimates are on the variances of the (continuous) latent variables only.
# "std.all", the standardized estimates are based on both the variances of both (continuous) observed and latent variables -AMOS. 
summary(fit1,fit.measures=T,standardized=T,rsquare=T)

## report parameter estimates, CI
parameterEstimates(fit1) # default significance-level: 0.05=95% CI
parameterEstimates(fit1,level=0.99) # 99% CI

#### Draw plots
## path plots 1 "semPaths"
par(mfrow=c(1,2))
semPaths(fit1, what="paths", whatLabels="par")
semPaths(fit1, what="paths", whatLabels="stand")

## path plot 2 "lavaanPlot"
# https://www.alexlishinski.com/post/lavaanplot-0-5-1/
#unstandardized plot
lavaanPlot(model = fit1, node_options = list(shape = "box", fontname = "Helvetica"), edge_options = list(color = "grey"), covs = T, coefs = T, sig=0.05, stars=c("regress", "latent", "covs"), stand = F)

#standardized plot
lavaanPlot(model = fit1, node_options = list(shape = "box", fontname = "Helvetica"), edge_options = list(color = "grey"), covs = T, coefs = T, sig=0.05, stars=c("regress", "latent", "covs"), stand = T)


#### modification indices (model fit is not good. Need to find a better model)
# mi value of 3.84 or greater is considered significant at 0.05 alpha
mi1<-modindices(fit1)
mi1

mi1[order(mi1[,4],decreasing=T),]

## possibilities: allow correlations between residuals
# y1 ~~ y5 / y2 ~~ y6 / y3 ~~ y7 / y4 ~~ y8
# y2 ~~ y4 / y6 ~~ y8


##############################################################################
###### 2. CFA 2
#### measurement model specification 2
#### add correlations between residuals
# y1 ~~ y5 / y2 ~~ y6 / y3 ~~ y7 / y4 ~~ y8
# y2 ~~ y4 / y6 ~~ y8

MM2<-'
      # measurement model
      Indust60 =~ x1+x2+x3 
      Demo60 =~ y1+y2+y3+y4
      Demo65 =~ y5+y6+y7+y8
      
      # residual correlations
      y1 ~~ y5
      #y2 ~~ y4
      #y2 ~~ y6
      y2 ~~ y4+y6
      y3 ~~ y7
      #y4 ~~ y8
      #y6 ~~ y8
      y4+y6 ~~ y8'

# y2 ~~ y4+y6 is equivalent to y2 ~~ y4,  y2 ~~ y6
# y4+y6 ~~ y8 is equivalent to y4 ~~ y8,  y6 ~~ y8
   
#### fit the model
fit2<-cfa(MM2, data=pd.data) 
#fit2<-cfa(MM2, sample.cov=pd.data.cov2, sample.nobs=n.obs)  ## use covariance matrix + number of obs

## report fit measures
fitmeasures(fit2)

## display summary output
summary(fit2,fit.measures=T)

## standardized estimates
# "std.lv", the standardized estimates are on the variances of the (continuous) latent variables only.
# "std.all", the standardized estimates are based on both the variances of both (continuous) observed and latent variables. 
summary(fit2,fit.measures=T,standardized=T,rsquare=T)

## report parameter estimates, CI
parameterEstimates(fit2)

#### Draw plots
## path plots 1 "semPaths"
par(mfrow=c(1,2))
semPaths(fit2, what="paths", whatLabels="par")
semPaths(fit2, what="paths", whatLabels="stand")

## path plot 2 "lavaanPlot"
#unstandardized plot
lavaanPlot(model = fit2, node_options = list(shape = "box", fontname = "Helvetica"), edge_options = list(color = "grey"), covs = T, coefs = T, sig=0.05, stars=c("regress", "latent", "covs"), stand = F)

#standardized plot
lavaanPlot(model = fit2, node_options = list(shape = "box", fontname = "Helvetica"), edge_options = list(color = "grey"), covs = T, coefs = T, sig=0.05, stars=c("regress", "latent", "covs"), stand = T)


##############################################################################
#### 3. Structural equation model  
#### model specification
model <- '
  # measurement model
    ind60 =~ x1+x2+x3
    dem60 =~ y1+y2+y3+y4
    dem65 =~ y5+y6+y7+y8
  
  # regressions
    dem60 ~ ind60
    #dem65 ~ dem60
    #dem65 ~ ind60
    dem65 ~ dem60+ind60


  # residual correlations
    y1 ~~ y5
    #y2 ~~ y4
    #y2 ~~ y6
    y2 ~~ y4+y6
    y3 ~~ y7
    #y4 ~~ y8
    #y6 ~~ y8
    y4+y6 ~~ y8'


#### fit the model
fit31<-sem(model, data=pd.data)
#fit31<-sem(model, sample.cov=pd.data.cov.2, sample.nobs=n.obs)  ## use covariance matrix + number of obs

## report fit measures
fitmeasures(fit31)

## display summary output
summary(fit31,fit.measures=T)

## standardized estimates
# "std.lv", the standardized estimates are on the variances of the (continuous) latent variables only.
# "std.all", the standardized estimates are based on both the variances of both (continuous) observed and latent variables. 
summary(fit31,fit.measures=T,standardized=T,rsquare=T)

## report parameter estimates, CI
parameterEstimates(fit31)


#### Draw plots
## path plots 1 "semPaths"
par(mfrow=c(1,2))
semPaths(fit31, what="paths", whatLabels="par")
semPaths(fit31, what="paths", whatLabels="stand")

## path plot 2 "lavaanPlot"
# limitation: correlation between residuals are not shown
#unstandardized plot
lavaanPlot(model = fit31, node_options = list(shape = "box", fontname = "Helvetica"), edge_options = list(color = "grey"), covs = T, coefs = T, sig=0.05, stars=c("regress", "latent", "covs"), stand = F)

#standardized plot
lavaanPlot(model = fit31, node_options = list(shape = "box", fontname = "Helvetica"), edge_options = list(color = "grey"), covs = T, coefs = T, sig=0.05, stars=c("regress", "latent", "covs"), stand = T)

##########################################
#### User-definded estimands
#### model specification
model <- '
  # measurement model
    ind60 =~ x1+x2+x3
    dem60 =~ y1+y2+y3+y4
    dem65 =~ y5+y6+y7+y8
  
  # regressions (label paths)
    dem60 ~ a*ind60
    dem65 ~ b*dem60 + c*ind60
  
  # residual correlations
    y1 ~~ y5
    y2 ~~ y4+y6
    y3 ~~ y7
    y4+y6 ~~ y8

  # user definded estimands (indirect effect & total effect)  
    ie:=a*b   
    te:=c+(a*b)'

#### fit the model
fit32<-sem(model, data=pd.data)  ## default option for standard error calculation: information matrix
#fit32<-sem(model, sample.cov=pd.data.cov.2, sample.nobs=n.obs)  ## use covariance matrix + number of obs

## bootstrapping for calculating standard error
fit4<-sem(model, data=pd.data, se="bootstrap", bootstrap = 2000)

## report fit measures
fitmeasures(fit32)

## display summary output
summary(fit32,fit.measures=T)

## standardized estimates
# "std.lv", the standardized estimates are on the variances of the (continuous) latent variables only.
# "std.all", the standardized estimates are based on both the variances of both (continuous) observed and latent variables. 
summary(fit32,fit.measures=T,standardized=T,rsquare=T)

## report parameter estimates, CI
parameterEstimates(fit32)
parameterEstimates(fit4) ## bootstrap


#### Draw plots
## path plots 1 "semPaths"
par(mfrow=c(1,2))
semPaths(fit32, what="paths", whatLabels="par")
semPaths(fit32, what="paths", whatLabels="stand")

## path plot 2 "lavaanPlot"
#unstandardized plot
lavaanPlot(model = fit32, node_options = list(shape = "box", fontname = "Helvetica"), edge_options = list(color = "grey"), covs = T, coefs = T, sig=0.05, stars=c("regress", "latent", "covs"), stand = F)

#standardized plot
lavaanPlot(model = fit32, node_options = list(shape = "box", fontname = "Helvetica"), edge_options = list(color = "grey"), covs = T, coefs = T, sig=0.05, stars=c("regress", "latent", "covs"), stand = T)





################################################################################################################################################
###############################################################
## Example 3: Bagozzi's performance and satisfaction example ## 
###############################################################
#### read data 
## Note: Lavaan package can only read covariance matrix

# construct a full covariance matrix
# input lower triangle correlation matrix directly 
lower<-'
1							
0.418	 1						
0.394	 0.627  1					
0.129	 0.202	 0.266	 1				
0.189	 0.284	 0.208	 0.365	 1			
0.544	 0.281	 0.324	 0.201	 0.161	 1		
0.507	 0.225	 0.314	 0.172	 0.174	 0.546	 1	
-0.357 	 -0.156   -0.038	 -0.199	 -0.277	 -0.294	 -0.174	 1'

# standard deviation
stdev<-c(2.09,3.43,2.81,1.95,2.06,2.16,2.06,3.65)


ps.data.cor<-getCov(lower, names=c("y1","y2","y3","x1","x2","x3","x4","x5"))
ps.data.cov<-cor2cov(ps.data.cor, stdev)
ps.data.cov

#n
n.obs<-122

######################################################################################
#### 1. Job1: CFA measurement model 
# all exogenous latent variables are correlated by default 
MM<-'
      # measurement model
        AM =~ x2+x1
        TSE =~ x4+x3
        VI =~ x5
        JS =~ y2+y3 
        Per =~ y1
      
        x5 ~~ 0.15*13.322500*x5
        y1 ~~ 0.15*4.3681000*y1'

# single indicator/ assume that reliability =0.85
# (1-reliability) * variance

#### fit the model
fit1<-cfa(MM, sample.cov=ps.data.cov, sample.nobs=n.obs)  ## use covariance matrix + number of obs

## report fit measures
fitmeasures(fit1)

## display summary output
summary(fit1,fit.measures=T)

## standardized estimates
# "std.lv", the standardized estimates are on the variances of the (continuous) latent variables only.
# "std.all", the standardized estimates are based on both the variances of both (continuous) observed and latent variables. 
summary(fit1,fit.measures=T,standardized=T,rsquare=T)

## report parameter estimates, CI
parameterEstimates(fit1) # default significance-level: 0.05

#### Draw plots
## path plots 1 "semPaths"
par(mfrow=c(1,2))
semPaths(fit1, what="paths", whatLabels="par")
semPaths(fit1, what="paths", whatLabels="stand")

## path plot 2 "lavaanPlot"
#unstandardized plot
lavaanPlot(model = fit1, node_options = list(shape = "box", fontname = "Helvetica"), edge_options = list(color = "grey"), covs = T, coefs = T, sig=0.05, stars=c("regress", "latent", "covs"), stand = F)

#standardized plot
lavaanPlot(model = fit1, node_options = list(shape = "box", fontname = "Helvetica"), edge_options = list(color = "grey"), covs = T, coefs = T, sig=0.05, stars=c("regress", "latent", "covs"), stand = T)

######################################################################################
## 2. Job2: Test H1:Spurious correlation of job satisfaction and performance
#### model specification
model5 <- '
  # measurement model
    AM =~ x2+x1
    TSE =~ x4+x3
    VI =~ x5
    JS =~ y2+y3 
    Per =~ y1
    
    x5 ~~ 0.15*13.322500*x5
    y1 ~~ 0.15*4.3681000*y1
    
  # regressions 
    JS ~ AM+TSE+VI
    Per ~ AM+TSE+VI
  
  # correlations
    #AM ~~ TSE+VI
    #TSE ~~ VI
    JS ~~ Per
'

#### fit the model
fit5<-sem(model5, sample.cov=ps.data.cov, sample.nobs=n.obs)  ## use covariance matrix + number of obs

## display summary output
summary(fit5,fit.measures=T)

######################################################################################
## 3. Job5: Test H4:non-recursive relation between job satisfaction and performance
#### model specification
model6 <- '
  # measurement model
    AM =~ x2+x1
    TSE =~ x4+x3
    VI =~ x5
    JS =~ y2+y3 
    Per =~ y1
    
    x5 ~~ 0.15*13.322500*x5
    y1 ~~ 0.15*4.3681000*y1
    
  # regressions 
    JS ~ AM+TSE+VI+Per
    Per ~ AM+TSE+VI+JS
  
  # correlations
    #AM ~~ TSE+VI
    #TSE ~~ VI
'

#### fit the model
fit6<-sem(model6, sample.cov=ps.data.cov, sample.nobs=n.obs)  ## use covariance matrix + number of obs

## display summary output
summary(fit6,fit.measures=T)

# model is not identified! (underidentified)
# delete two non-significant paths from job4 
# 1. AM->Per
# 2. TSE->JS

######################################################################################
## 4. Job6: Test H4 after delete two non-significant structual paths
#### model specification
model7 <- '
  # measurement model
    AM =~ x2+x1
    TSE =~ x4+x3
    VI =~ x5
    JS =~ y2+y3 
    Per =~ y1

    x5 ~~ ((1-0.85)*3.65^2)*x5
    y1 ~~ ((1-0.85)*2.09^2)*y1
    
  # regressions 
    #JS ~ AM+TSE+VI+Per
    JS ~ AM+VI+Per
    #Per ~ AM+TSE+VI+JS
    Per ~ TSE+VI+JS
  
  # correlations
    #AM ~~ TSE+VI
    #TSE ~~ VI
'

#### fit the model
fit7<-sem(model7, sample.cov=ps.data.cov, sample.nobs=n.obs)  ## use covariance matrix + number of obs

## report fit measures
fitmeasures(fit7)

## display summary output
summary(fit7,fit.measures=T)

## standardized estimates
# "std.lv", the standardized estimates are on the variances of the (continuous) latent variables only.
# "std.all", the standardized estimates are based on both the variances of both (continuous) observed and latent variables. 
summary(fit7,fit.measures=T,standardized=T,rsquare=T)

## report parameter estimates, CI
parameterEstimates(fit7)


#### Draw plots
## path plots 1 "semPaths"
par(mfrow=c(1,2))
semPaths(fit7, what="paths", whatLabels="par")
semPaths(fit7, what="paths", whatLabels="stand")

## path plot 2 "lavaanPlot"
#unstandardized plot
lavaanPlot(model = fit7, node_options = list(shape = "box", fontname = "Helvetica"), edge_options = list(color = "grey"), covs = T, coefs = T, sig=0.05, stars=c("regress", "latent", "covs"), stand = F)

#standardized plot
lavaanPlot(model = fit7, node_options = list(shape = "box", fontname = "Helvetica"), edge_options = list(color = "grey"), covs = T, coefs = T, sig=0.05, stars=c("regress", "latent", "covs"), stand = T)

######################################################################################
## 5. Job7: Final model (delete two non-significant paths)
#### model specification
model8 <- '
  # measurement model
    AM =~ x2+x1
    TSE =~ x4+x3
    VI =~ x5
    JS =~ y2+y3 
    Per =~ y1

    x5 ~~ ((1-0.85)*3.65^2)*x5
    y1 ~~ ((1-0.85)*2.09^2)*y1
    
  # regressions 
    JS ~ AM+VI+Per
    
    #Per ~ TSE+VI+JS
    Per ~ TSE
  
  # correlations
    #AM ~~ TSE+VI
    #TSE ~~ VI
'

#### fit the model
fit8<-sem(model8, sample.cov=ps.data.cov, sample.nobs=n.obs)  ## use covariance matrix + number of obs

## report fit measures
fitmeasures(fit8)

## display summary output
summary(fit8,fit.measures=T)

## standardized estimates
# "std.lv", the standardized estimates are on the variances of the (continuous) latent variables only.
# "std.all", the standardized estimates are based on both the variances of both (continuous) observed and latent variables. 
summary(fit8,fit.measures=T,standardized=T,rsquare=T)

## report parameter estimates, CI
parameterEstimates(fit8)


#### Draw plots
## path plots 1 "semPaths"
par(mfrow=c(1,2))
semPaths(fit8, what="paths", whatLabels="par")
semPaths(fit8, what="paths", whatLabels="stand")

## path plot 2 "lavaanPlot"
#unstandardized plot
lavaanPlot(model = fit8, node_options = list(shape = "box", fontname = "Helvetica"), edge_options = list(color = "grey"), covs = T, coefs = T, sig=0.05, stars=c("regress", "latent", "covs"), stand = F)

#standardized plot
lavaanPlot(model = fit8, node_options = list(shape = "box", fontname = "Helvetica"), edge_options = list(color = "grey"), covs = T, coefs = T, sig=0.05, stars=c("regress", "latent", "covs"), stand = T)

