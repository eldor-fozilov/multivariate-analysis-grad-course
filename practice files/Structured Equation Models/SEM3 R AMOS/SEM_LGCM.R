#####################################
##### Latent Growth Curve Model ##### 
#####################################
#### references
# Mike Crowson - https://www.youtube.com/watch?v=4vV47K_L4Ak
# Example - Ch 14 Satisfied example from Schumacker & Lomax (2016)
# lavaan package - http://lavaan.ugent.be/tutorial/growth.html

#### required packages
#install.packages("lavaan", dependencies=TRUE)
#install.packages("semPlot", dependencies=TRUE)
#install.packages("lavaanPlot", dependencies=TRUE)

library(lavaan)
library(semPlot) ## for plots
library(lavaanPlot) ## for plots

#### read data
satisfied<-read.table("satisfied.txt",header=TRUE)
str(satisfied)

#### 1. model without covariates
model1<-'
intercept=~1*y1+1*y2+1*y3+1*y4
slope=~0*y1+1*y2+2*y3+3*y4
#intercept~~slope
'

fit1<-growth(model1,data=satisfied)
summary(fit1,fit.measures=TRUE)

#### Draw plots
## path plots 1 "semPaths"
par(mfrow=c(1,2))
#1.unstandardized plot
semPaths(fit1, what="paths", whatLabels="par")

#2.standardized plot
semPaths(fit1, what="paths", whatLabels="stand")

## path plot 2 "lavaanPlot"
#1.unstandardized plot
lavaanPlot(model = fit1, node_options = list(shape = "box", fontname = "Helvetica"), edge_options = list(color = "grey"), covs = T, coefs = T, sig=0.05, stars=c("regress", "covs"), stand = F)

#2.standardized plot
lavaanPlot(model = fit1, node_options = list(shape = "box", fontname = "Helvetica"), edge_options = list(color = "grey"), covs = T, coefs = T, sig=0.05, stars=c("regress", "covs"), stand = T)

#### 2. model with a covariate (gender)
model2<-'
intercept=~1*y1+1*y2+1*y3+1*y4
slope=~0*y1+1*y2+2*y3+3*y4
intercept~gender
slope~gender
#intercept~~slope
'

fit2<-growth(model2,data=satisfied)
summary(fit2,fit.measures=TRUE)

#### Draw plots
## path plots 1 "semPaths"
windows()
par(mfrow=c(1,2))
#1.unstandardized plot
semPaths(fit2, what="paths", whatLabels="par")

#2.standardized plot
semPaths(fit2, what="paths", whatLabels="stand")

## path plot 2 "lavaanPlot"
#1.unstandardized plot
lavaanPlot(model = fit2, node_options = list(shape = "box", fontname = "Helvetica"), edge_options = list(color = "grey"), covs = T, coefs = T, sig=0.05, stars=c("regress", "covs"), stand = F)

#2.standardized plot
lavaanPlot(model = fit2, node_options = list(shape = "box", fontname = "Helvetica"), edge_options = list(color = "grey"), covs = T, coefs = T, sig=0.05, stars=c("regress", "covs"), stand = T)


