###############################
## Binary logit/probit model ##
###############################
## reference:
# https://michael.hahsler.net/SMU/CS7331/R/logistic_regression.html

## Read the dataset
data(iris)
x<-iris

## create a binary variable of virginica or not
x$virginica<-x$Species=="virginica"
x$Species<-NULL
plot(x, col=x$virginica+1)

x$virginica 

## GLM:binary logit (variance=1 fixed)
model <- glm(virginica ~ .,family = binomial(link="logit"), data=x)
summary(model)

## GLM:binary probit 
model2 <- glm(virginica ~ .,family = binomial(link="probit"), data=x)
summary(model2)

## accuracy (logit)
pr <- predict(model, x, type="response")
round(pr, 2)
pr
hist(pr, breaks=20)
hist(pr[x$virginica==TRUE], col="red", breaks=20, add=TRUE)

table(actual=x$virginica, predicted=pr>.5)

## accuracy (probit)
pr2 <- predict(model2, x, type="response")
round(pr2, 2)
pr2
hist(pr2, breaks=20)
hist(pr2[x$virginica==TRUE], col="red", breaks=20, add=TRUE)

table(actual=x$virginica, predicted=pr2>.5)

## difference in fitted probabilities
pr-pr2
sum(pr-pr2)


##############################################################################################################
##############################################################################################################
##########################################
## Choice-based conjoint model examples ##
##########################################
## Reference: R for Marketing Research and Analytics 2nd ed (Ch 13. Choice Modeling)

##########################################################
### Choice-based conjoint (CBC) survey: minivan choice ###
##########################################################
## 4 different models
#1. Conditional Logit Model
#2. Mixed Logit Model
#3. Multinomial Latent class Logit model
#4. Hierarchical Bayes Multinomial Logit Model

#############################################################################################################
### 1. Conditional Logit Model  ###
###################################
## Required libraries
library(MASS)
#install.packages("mlogit",dependencies = T)
# install mlogit "mlogit_1.0-2.tar"	from https://cran.r-project.org/src/contrib/Archive/mlogit/
library(mlogit)
#install.packages("gmnl",dependencies = T)
library(gmnl)
#install.packages("ChoiceModelR",dependencies = T) 
library(ChoiceModelR)  

#### Generating homogeneous choice data (cbd.df1) ####
## set a random seed
set.seed(666)

## set attributes
attrib<-list(seat = c("6", "7", "8"),
             cargo = c("2ft", "3ft"),
             eng = c("gas", "hyb", "elec"),
             price = c("30", "35", "40"))
attrib

## set coefficient names
coef.names <- NULL
for (a in seq_along(attrib)) {
  coef.names <- c(coef.names ,
                  paste(names(attrib)[a], attrib [[a]][-1], sep=""))
}
coef.names

## coefficient values (TRUE values of part-worth: homogeneous)
coefs<-c(-1, -1, 0.5, -1, -2, -1, -2)
names(coefs) <- coef.names
coefs

## set # of questions, # of alternatives
nques <- 15 # 15 questions
nalt <- 3   # 3 alternatives

## profiles (combination of the attributes)
# [R]expand.grid: Create a data frame from all combinations of the supplied vectors or factors.   
profiles <- expand.grid(attrib)
nrow(profiles) # 3x2x3x3=54
head(profiles)
profiles

## dummy coded profiles
# model.matrix function: Expanding factors to a set of dummy variables
profiles.coded <- model.matrix(~ seat + cargo + eng + price , data=profiles)[ , -1]
head(profiles.coded)
profiles.coded

## respondent ids (200 respondents)
resp.id <- 1:200 

## create cbc data frame
cbc.df1 <- data.frame(NULL)
for (i in seq_along(resp.id)) {
  profiles.i <- sample (1: nrow(profiles), size=nques*nalt)
  utility <- profiles.coded[profiles.i, ] %*% coefs
  wide.util <- matrix(data=utility , ncol=nalt , byrow=TRUE)
  probs <- exp(wide.util) / rowSums(exp(wide.util))
  choice <- apply(probs , 1, function(x) sample (1:nalt , size=1, prob=x))
  choice <- rep(choice , each=nalt)==rep (1:nalt , nques)
  conjoint.i <- data.frame(resp.id=rep(i, nques),
                           ques = rep (1: nques , each=nalt),
                           alt = rep (1:nalt , nques),
                           profiles[profiles.i, ],
                           choice = as.numeric(choice))
  cbc.df1 <- rbind(cbc.df1 , conjoint.i)
}

#### Fitting a Choice Model with mlogit()
## summary stats
head(cbc.df1)
summary(cbc.df1)
xtabs(choice ~ seat , data=cbc.df1)
xtabs(choice ~ cargo , data=cbc.df1)
xtabs(choice ~ eng , data=cbc.df1)
xtabs(choice ~ price , data=cbc.df1)

## data format for mlogit
cbc.mlogit1 <- mlogit.data(data=cbc.df1,choice="choice", shape="long",
                           varying =3:6, alt.levels=paste("pos" ,1:3),
                           id.var="resp.id")

head(cbc.mlogit1)

## no intercept (model 1)
m1 <- mlogit(choice ~ 0 + seat + cargo + eng + price , data = cbc.mlogit1)
summary(m1)

# choice probabilities
head(m1$probabilities)

### Model comparison
## with intercept (Alternative Specific Constant) (model 2) 
m2 <- mlogit(choice ~ seat + cargo + eng + price , data = cbc.mlogit1)
summary(m2)

## compare m1 and m2
lrtest(m1, m2)

## price as number (model 3)
m3 <- mlogit(choice ~ 0 + seat + cargo + eng+as.numeric(as.character(price)), data = cbc.mlogit1)
summary(m3)

## compare m1 and m3
lrtest(m1, m3)



#################################################################################################################################
### 2. Mixed Logit Model ###
############################
## Required libraries
library(MASS)
#install.packages("mlogit",dependencies = T)
# install mlogit "mlogit_1.0-2.tar"	from https://cran.r-project.org/src/contrib/Archive/mlogit/
library(mlogit)
#install.packages("gmnl",dependencies = T)
library(gmnl)
#install.packages("ChoiceModelR",dependencies = T) 
library(ChoiceModelR)  

### Adding Consumer Heterogeneity to Choice Models
## Generating heterogeneous choice Data (no inidividual charicter variable) 
## set a random seed
set.seed(666)

## set attributes
attrib<-list(seat = c("6", "7", "8"),
             cargo = c("2ft", "3ft"),
             eng = c("gas", "hyb", "elec"),
             price = c("30", "35", "40"))
attrib

## set coefficient names
coef.names <- NULL
for (a in seq_along(attrib)) {
  coef.names <- c(coef.names ,
                  paste(names(attrib)[a], attrib [[a]][-1], sep=""))
}
coef.names

## mean and covariance of part-worths
# mean (mu)
mu <- c(-1, -1, 0.5, -1, -2, -1, -2) 
names(mu) <- coef.names
mu

## covariance matrix (Sigma)
# diagonal elements
Sigma <- diag(c(1, 1, 1, 1, 1, 1, 1))
dimnames(Sigma) <- list(coef.names , coef.names)
Sigma
## add correlation
Sigma["enghyb", "engelec"] <- Sigma["engelec", "enghyb"] <- 0.7 
Sigma

## respondent ids (200 respondents)
resp.id <- 1:200 

# draw coefficients from multivariate normal distribution (mu, Sigma)
coefs <- mvrnorm(length(resp.id), mu=mu , Sigma=Sigma)
colnames(coefs) <- coef.names
coefs

## set # of questions, # of alternatives
nques <- 15 # 15 questions
nalt <- 3   # 3 alternatives

# profiles (combination of the attributes)
profiles <- expand.grid(attrib)
nrow(profiles)
head(profiles)

# dummy coded profiles
profiles.coded <- model.matrix(~ seat + cargo + eng + price , data=profiles)[ , -1]
head(profiles.coded)


# create cbc data frame
cbc.df2 <- data.frame(NULL)
for (i in seq_along(resp.id)) {
  profiles.i <- sample (1: nrow(profiles), size=nques*nalt)
  utility <- profiles.coded[profiles.i, ] %*% coefs[i, ] ## coefficient of ith individual
  wide.util <- matrix(data=utility , ncol=nalt , byrow=TRUE)
  probs <- exp(wide.util) / rowSums(exp(wide.util))
  choice <- apply(probs , 1, function(x) sample (1:nalt , size=1, prob=x))
  choice <- rep(choice , each=nalt)==rep (1:nalt , nques)
  conjoint.i <- data.frame(resp.id=rep(i, nques),
                           ques = rep (1: nques , each=nalt),
                           alt = rep (1:nalt , nques),
                           profiles[profiles.i, ],
                           choice = as.numeric(choice))
  cbc.df2 <- rbind(cbc.df2 , conjoint.i)
}


## data format for mlogit
cbc.mlogit2 <- mlogit.data(data=cbc.df2,choice="choice", shape="long",
                           varying =3:6, alt.levels=paste("pos" ,1:3),
                           id.var="resp.id")

head(cbc.mlogit2)

####  Estimating Mixed Logit Models with mlogit()
## summary stats
head(cbc.df2)
summary(cbc.df2)
xtabs(choice ~ seat , data=cbc.df2)
xtabs(choice ~ cargo , data=cbc.df2)
xtabs(choice ~ eng , data=cbc.df2)
xtabs(choice ~ price , data=cbc.df2)

## m4: mixed logit model without correlation
# set random parameters for m4
m4.rpar <- rep("n", length=length(mu)) ## "n" normal distribution
names(m4.rpar) <-  coef.names
m4.rpar

# estimating m4
m4 <- mlogit(choice ~ 0 + seat + eng + cargo + price, data = cbc.mlogit2, panel=TRUE, rpar = m4.rpar, correlation = FALSE)
summary(m4)
mean(m4)
stdev(m4)

# individual-level parameters
ind.par<-fitted(m4,type="parameters")
head(ind.par) 

# choice probabilities
head(m4$probabilities)

## m5: mixed logit model with correlation
m5 <- update(m4, correlation = TRUE)
summary(m5)
mean(m5)
stdev(m5)

# covariance matrix (not vary accurate due to small samples)
cov.mlogit(m5)   

# choice probabilities
head(m5$probabilities)

## compare with conditional logit model (Analyze cdc.df2 data with conditional logit model)
m6 <- mlogit(choice ~ 0 + seat + cargo + eng + price , data = cbc.mlogit2)
summary(m6)

lrtest(m5,m6)


#############################################################################################
### 3. Multinomial latent class logit model ###
###############################################
## Required libraries
#install.packages("gmnl",dependencies = T)
library(gmnl)

#### Generate Latent Class Choice Data
## set a random seed
set.seed(666)

## set attributes
attrib<-list(seat = c("6", "7", "8"),
             cargo = c("2ft", "3ft"),
             eng = c("gas", "hyb", "elec"),
             price = c("30", "35", "40"))

# set coefficient names
coef.names <- NULL

for (a in seq_along(attrib)) {
  coef.names <- c(coef.names, paste(names(attrib)[a], attrib [[a]][-1], sep=""))
}
coef.names

## k number of latent classes
k=3

## coefficient values
coefs<-matrix(c(-1, -1, 0.5, -1, -2, -1, -2,-2,-1.5,0,-2,-1,-0.5,-3,-1.5,-2.5,2,-1.5,0,-2,-0.5),nrow=k,byrow=T)
colnames(coefs) <- coef.names
coefs

## set # of questions, # of alternatives
nques <- 15 # 15 questions
nalt <- 3   # 3 alternatives

## profiles (combination of the attributes)
profiles <- expand.grid(attrib)
nrow(profiles)
head(profiles)

# dummy coded profiles
profiles.coded <- model.matrix(~ seat + cargo + eng + price , data=profiles)[ , -1]
head(profiles.coded)

## number of respondents
n<-1000

## respondent ids
resp.id <- 1:n

## class membership (ture membership)
cm<-sample(seq(k),n,prob=c(0.1,0.3,0.6),replace=T)
cm

## create cbc data frame 3
cbc.df3 <- data.frame(NULL)
for (i in 1:n) {
  profiles.i <- sample (1: nrow(profiles), size=nques*nalt)
  utility <- profiles.coded[profiles.i, ] %*% coefs[cm[i],]  ##coefficient for i's latent class cm[i]
  wide.util <- matrix(data=utility , ncol=nalt , byrow=TRUE)
  probs <- exp(wide.util) / rowSums(exp(wide.util))
  choice <- apply(probs , 1, function(x) sample (1:nalt , size=1, prob=x))
  choice <- rep(choice , each=nalt)==rep (1:nalt , nques)
  conjoint.i <- data.frame(resp.id=rep(i, nques),
                           cm=cm[i],
                           ques = rep (1: nques , each=nalt),
                           alt = rep (1:nalt , nques),
                           profiles.coded[profiles.i, ],
                           choice = as.numeric(choice))
  cbc.df3 <- rbind(cbc.df3 , conjoint.i)
}

## mlogit data format
cbc.mlogit3 <- mlogit.data(data=cbc.df3 , choice="choice", shape="long",alt.levels=paste("pos" ,1:3),
                           varying =3:6, id.var="resp.id")

head(cbc.mlogit3)
tail(cbc.mlogit3)

#### Run LC model with k 
lc<-gmnl(choice ~ seat7+seat8+cargo3ft+enghyb+engelec+price35+price40 |0|0|0|1, 
         data = cbc.mlogit3, model = 'lc', panel = TRUE,Q = k)
# the fifth part is reserved for time-invariant variables that enter in the scale coefficient
#or in the probability assignment in models with latent classes

## outputs
summary(lc)
names(lc)

## coefficients
lc$coef
coefs

## membership coefficients
# class 1 proportion
1/(1+exp(lc$coef[[22]])+exp(lc$coef[[23]]))

# class 2 proportion
exp(lc$coef[[22]])/(1+exp(lc$coef[[22]])+exp(lc$coef[[23]]))

# class 3 proportion
exp(lc$coef[[23]])/(1+exp(lc$coef[[22]])+exp(lc$coef[[23]]))

## membership probabilities
pi_hat <- lc$Qir
colnames(pi_hat) <- c("q = 1", "q = 2", "q = 3")
dim(pi_hat)
round(head(pi_hat), 4)

mp<-as.vector(sapply(1:n, function(i) which(pi_hat[i,] == max(pi_hat[i,]))))
sum(mp==cm)

## class assignment
# class 1
sum(mp==1)
sum(mp==1)/n

# class 2
sum(mp==2)
sum(mp==2)/n

# class 3
sum(mp==3)
sum(mp==3)/n


#####################################################
### 4. Hierarchical Bayes Multinomial Logit Model ###
#####################################################
## Required libraries
library(MASS)
#install.packages("mlogit",dependencies = T)
# install mlogit "mlogit_1.0-2.tar"	from https://cran.r-project.org/src/contrib/Archive/mlogit/
library(mlogit)
#install.packages("gmnl",dependencies = T)
library(gmnl)
#install.packages("ChoiceModelR",dependencies = T) 
library(ChoiceModelR)  

### Adding Consumer Heterogeneity and individual characteristic variable 
## Generate Choice Data (Hierarchical logit)
# set a random seed
set.seed (666)

# set attributes
attrib<-list(seat = c("6", "7", "8"),
             cargo = c("2ft", "3ft"),
             eng = c("gas", "hyb", "elec"),
             price = c("30", "35", "40"))

# set coefficient names
coef.names <- NULL

for (a in seq_along(attrib)) {
  coef.names <- c(coef.names,paste(names(attrib)[a], attrib [[a]][-1], sep=""))
}

coef.names

## mean and covariance of part worths
# mean (mu)
mu <- c(-1, -1, 0.5, -1, -2, -1, -2) 
names(mu) <- coef.names
mu

# covariance matrix (sigma)
Sigma <- diag(c(1, 1, 1, 1, 1, 1, 1))
dimnames(Sigma) <- list(coef.names , coef.names)
Sigma
# correlation
Sigma["enghyb", "engelec"] <- Sigma["engelec", "enghyb"] <- 0.7 
Sigma

## respondent ids
resp.id <- 1:200 

## carpool variable (indiviual characteristic variable)
carpool <- sample(c("yes", "no"), size=length(resp.id), replace=TRUE, prob=c(0.3, 0.7))

## generate coefficients from multivariate normal distribution (mu, Sigma)
coefs <- mvrnorm(length(resp.id), mu=mu , Sigma=Sigma)
colnames(coefs) <- coef.names
coefs

## adjust seat7, seat8 part-worths according to carpool variable
coefs[carpool =="yes", "seat8"] <- coefs[carpool =="yes", "seat8"] + 2
coefs[carpool =="yes", "seat7"] <- coefs[carpool =="yes", "seat7"] + 1.5

## set # of questions, # of alternatives
nques <- 15 # 15 questions
nalt <- 3   # 3 alternatives

## profiles (combination of the attributes)
profiles <- expand.grid(attrib)
nrow(profiles)
head(profiles)

# dummy coded profiles
profiles.coded <- model.matrix(~ seat + cargo + eng + price , data=profiles)[ , -1]  
head(profiles.coded)

## create cdc data frame 4
cbc.df4 <- data.frame(NULL)
for (i in seq_along(resp.id)) {
  profiles.i <- sample (1: nrow(profiles), size=nques*nalt)
  utility <- profiles.coded[profiles.i, ] %*% coefs[i, ] # individual-level coefficient
  wide.util <- matrix(data=utility , ncol=nalt , byrow=TRUE)
  probs <- exp(wide.util) / rowSums(exp(wide.util))
  choice <- apply(probs , 1, function(x) sample (1:nalt , size=1, prob=x))
  choice <- rep(choice , each=nalt)==rep (1:nalt , nques)
  conjoint.i <- data.frame(resp.id=rep(i, nques),
                           ques = rep (1: nques , each=nalt),
                           alt = rep (1:nalt , nques),
                           carpool = rep(carpool[i], nques),
                           profiles[profiles.i, ],
                           choice = as.numeric(choice))
  cbc.df4 <- rbind(cbc.df4 , conjoint.i)
}

#### Fitting a Choice Model
## summary stats
head(cbc.df4)
summary(cbc.df4)
xtabs(choice ~ seat , data=cbc.df4)
xtabs(choice ~ cargo , data=cbc.df4)
xtabs(choice ~ eng , data=cbc.df4)
xtabs(choice ~ price , data=cbc.df4)

#### Fitting Choice Models with mlogit()
## data format
cbc.mlogit4 <- mlogit.data(data=cbc.df4 , choice="choice", shape="long",
                           varying =3:6, alt.levels=paste("pos" ,1:3),
                           id.var="resp.id")
head(cbc.mlogit4)

## new data format for choicemodelr function
choice <- rep(0, nrow(cbc.df4))
choice[cbc.df4[,"alt"]==1] <- cbc.df4[cbc.df4[,"choice"]==1,"alt"]
head(choice)

cbc.coded <- model.matrix(~ seat + cargo + eng + price , data = cbc.df4)  ########## cargo + eng
cbc.coded <- cbc.coded[, -1] # remove the intercept

choicemodelr.data <- cbind(cbc.df4[,1:3], cbc.coded , choice)
head(choicemodelr.data)

carpool <- cbc.df4$carpool[cbc.df4$ques ==1 & cbc.df4$alt ==1]=="yes"
carpool <- as.numeric(carpool)
choicemodelr.demos <- as.matrix(carpool , nrow=length(carpool))
str(choicemodelr.demos)


#### run hb
hb.post <- choicemodelr(data=choicemodelr.data, xcoding=rep(1,7), # 7 1s: 7 continuous variables
                        demos=choicemodelr.demos,
                        mcmc=list(R=10000, use =5000) ,
                        options=list(save=TRUE))

#### output
summary(hb.post)
names(hb.post)
# "betadraw"  "deltadraw" "compdraw"  "loglike"  

# 1.betadraw: individual-level parameters (mean: mu+delta*z, cov: sigma)
# 2.compdraw: [hyper-parameter 1] average population part worths (mean:mu/covariance:rooti)
# 3.deltadraw: [hyper-parameter 2] mean adjustment (carpool coefficient for seat7 & seat 8)

####### 1. individual level parameters (beta) ###########
str(hb.post$betadraw)

# posterior mean of individual-level parameters
beta.post.mean<-apply(hb.post$betadraw, 1:2, mean)

# example: first respondent, seat 7 
hb.post$betadraw[1,1,]
windows()
par(mfrow=c(1,2))
plot(hb.post$betadraw[1,1,],t="l")
plot(density(hb.post$betadraw[1,1,]))
mean(hb.post$betadraw[1,1,])

# example: second respondent, seat 7
windows()
par(mfrow=c(1,2))
plot(hb.post$betadraw[2,1,],t="l")
plot(density(hb.post$betadraw[2,1,]))
mean(hb.post$betadraw[2,1,])

####### 2. [hyper-parameter 1] average population part worths #######
str(hb.post$compdraw)

### I. average population part worths (mean:mu.ap) - ignoring individual characteristics
mu.ap.draw<-matrix(double(7*500),500,7)
for (i in 1:500){
  mu.ap.draw[i,]<-hb.post$compdraw[[i]]$mu
}

head(mu.ap.draw)
dim(mu.ap.draw)
mu.ap.draw

# posterior mean
mu.ap.pm<-apply(mu.ap.draw,2,mean)
mu.ap.pm

### II. average population part worths (cov:Sigma)
Sigma.ap.draw<-matrix(double(49*500),500,49)
for (i in 1:500){
  Sigma.ap.draw[i,]<-as.vector(crossprod(hb.post$compdraw [[i]]$rooti))
}

# posterior mean
Sigma.ap.pm<-colMeans(Sigma.ap.draw, na.rm = FALSE, dims = 1)
matrix(Sigma.ap.pm,7,7)
Sigma
# not well recovered due to small sample size!
# profiles are not optimally drawn (i.e. orthogonal design?)

####### [hyper-parameter 2] mean adjustment (carpool coefficient for seat7 & seat 8)
# posterior mean
delta.pm<-apply(hb.post$deltadraw,2,mean)
delta.pm
# true delta
delta<-c(1.5,2,0,0,0,0,0)
delta

###############################################################################
### parameter recovery of mu <- c(-1, -1, 0.5, -1, -2, -1, -2) 
### seat 7 individual part-worth
beta.post.mean[,1]
mean(beta.post.mean[,1])

# seat 7 adjustment
delta.pm[1]*choicemodelr.demos

beta.post.mean[,1]-delta.pm[1]*choicemodelr.demos

mean(beta.post.mean[,1]-delta.pm[1]*choicemodelr.demos)

mu[1]

### seat 8 individual part-worth
beta.post.mean[,2]
mean(beta.post.mean[,2])

# seat 8 adjustment
delta.pm[2]*choicemodelr.demos

beta.post.mean[,2]-delta.pm[2]*choicemodelr.demos

mean(beta.post.mean[,2]-delta.pm[2]*choicemodelr.demos)

mu[2]


######################################################
### 1st respondent seat7, seat8 coefficient
choicemodelr.demos[1]

# mean (individual char)
delta.pm[1]  # seat 7
delta.pm[2]  # seat 8

# first respondent, seat 7, seat 8 
windows()
par(mfrow=c(2,2))
plot(hb.post$betadraw[1,1,],t="l")
plot(density(hb.post$betadraw[1,1,]))  
plot(hb.post$betadraw[1,2,],t="l")
plot(density(hb.post$betadraw[1,2,]))   

mean(hb.post$betadraw[1,1,])
mean(hb.post$betadraw[1,2,])


##########################################################
### 101, 102, 103 respondent seat7 coefficient
choicemodelr.demos[101:103]  # carpool

# 101,102,103, seat 7 carpool:(N,N,Y)
windows()
par(mfrow=c(3,2))
plot(hb.post$betadraw[101,1,],t="l")
plot(density(hb.post$betadraw[101,1,]))  
plot(hb.post$betadraw[102,1,],t="l")
plot(density(hb.post$betadraw[102,1,]))   
plot(hb.post$betadraw[103,1,],t="l")
plot(density(hb.post$betadraw[103,1,]))  

mean(hb.post$betadraw[1,1,])
mean(hb.post$betadraw[2,1,])
mean(hb.post$betadraw[3,1,])


