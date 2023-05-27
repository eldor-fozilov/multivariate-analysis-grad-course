###########################
## Discriminant Analysis ##
###########################
## reference: https://www.r-bloggers.com/discriminant-analysis-statistics-all-the-way/
## reference: https://acadgild.com/blog/linear-discriminant-analysis-with-r
# http://thatdatatho.com/2018/02/19/assumption-checking-lda-vs-qda-r-tutorial-2/
# http://www.sthda.com/english/articles/36-classification-methods-essentials/146-discriminant-analysis-essentials-in-r/

## load required libraries
#install.packages("MASS",dependencies = T)
#install.packages("klaR",dependencies = T)
#install.packages("car",dependencies = T)
#install.packages("heplots",dependencies = T)
#install.packages("MVN",dependencies = T)
#install.packages("ROCR",dependencies = T)
#install.packages("devtools")
#library(devtools)
#install_github("fawda123/ggord")

library(MASS)
library(klaR)
library(car)
library(heplots)
library(MVN)
library(ggord)
library(ROCR)

## Read the IRIS dataset
df<-iris
str(df)
head(df)
View(df)
summary(df)

## Scatter plot
scatterplotMatrix(~Sepal.Length+Sepal.Width+Petal.Length+Petal.Width|Species,data=df,col=c(2:4))

## Multivariate Normality Assumption for each category
setosa<-df[1:50,1:4]
versicolor<-df[51:100,1:4]
virginica<-df[101:150,1:4]

# 1.setosa
mvn(data=setosa, mvnTest = "mardia")  # Mardia's MVN test
mvn(data=setosa, mvnTest = "hz")  # Henze-Zirkler's MVN test
mvn(data=setosa, mvnTest = "royston") #Royston's MVN test
mvn(data=setosa, mvnTest = "dh") #Doornik-Hansen's MVN test
mvn(data=setosa, mvnTest = "energy") #Energy test

# 2.versicolor
mvn(data=versicolor, mvnTest = "mardia")  # Mardia's MVN test
mvn(data=versicolor, mvnTest = "hz")  # Henze-Zirkler's MVN test
mvn(data=versicolor, mvnTest = "royston") #Royston's MVN test
mvn(data=versicolor, mvnTest = "dh") #Doornik-Hansen's MVN test
mvn(data=versicolor, mvnTest = "energy") #Energy test

# 3.virginica
mvn(data=virginica, mvnTest = "mardia")  # Mardia's MVN test
mvn(data=virginica, mvnTest = "hz")  # Henze-Zirkler's MVN test
mvn(data=virginica, mvnTest = "royston") #Royston's MVN test
mvn(data=virginica, mvnTest = "dh") #Doornik-Hansen's MVN test
mvn(data=virginica, mvnTest = "energy") #Energy test

## Homogeneity of Variances Assumption for three categories
# Box's M test: whether the covariance matrices of independent samples are equal or not
# The null hypothesis for this test is that the observed covariance matrices for the dependent variables are equal across groups. 
# It has been suggested, however, that linear discriminant analysis be used when covariances are equal, and that quadratic discriminant analysis may be used when covariances are not equal
boxM(df[,1:4],df$Species)

## Divide the data into two groups (train data / test data)
set.seed(666)
idx<-sample(c(TRUE, FALSE),nrow(df),replace=T,prob=c(0.60,0.40))
train <- df[idx, ]
test <- df[!idx, ]

## Check how easily we can linearly separate the iris dataset
#pairs(df)

#########################################################################################
## Perform Linear Discriminant Analysis (LDA)
lda_iris<-lda(Species~Sepal.Length+Sepal.Width+Petal.Length+Petal.Width,data=train)
lda_iris

# attributes in LDA result 
attributes(lda_iris)

# prior (equal priors if # of observations are equal)
lda_iris$counts
lda_iris$prior # proportional to counts

# coefficients of linear discriminants
lda_iris$scaling

## histograms of discriminant function values
prd.LDA.train<-predict(lda_iris,train)
str(prd.LDA.train)

# 1. Dimension 1 (Linear discriminant 1)
ldahist(data=prd.LDA.train$x[,1],g=train$Species)
# 2. Dimension 2 (Linear discriminant 2)
ldahist(data=prd.LDA.train$x[,2],g=train$Species)

## Visualizing biplot
ggord(lda_iris, train$Species, ylim=c(-5,5))

## Visualization: partition plots
Sp<-train$Species
Sp<-as.character(Sp)
Sp[Sp=="virginica"]<-"i"
Sp<-as.factor(Sp)
Predictions.lda.train=predict(lda_iris,train)

# 1. Project the decision boundaries to LD1 & LD2 space  
partimat(Sp~Predictions.lda.train$x[,2]+Predictions.lda.train$x[,1],method='lda',main='LDA decision boundaries',name=c('LD2','LD1'),prec=400)
legend("bottomright", inset=0.035, title="Species", c("s:Setosa","v:Versicolor","i:Virginica"))

# 2. Project the decision boundaries to the 2D spaces of four characteristic variables
partimat(Sp~Sepal.Length+Sepal.Width+Petal.Length+Petal.Width,data=train,method='lda')

## Check the accuracy of LDA analysis
# 1. LDA with training data
Predictions.lda.train=predict(lda_iris,train)
table.lda.train.cl<-table(Predictions.lda.train$class, train$Species)
table.lda.train.cl
sum(diag(table.lda.train.cl)/sum(table.lda.train.cl))

# 2. LDA with test data
Predictions.lda.test=predict(lda_iris,test)
table.lda.test.cl<-table(Predictions.lda.test$class, test$Species)
table.lda.test.cl
sum(diag(table.lda.test.cl)/sum(table.lda.test.cl))


#########################################################################################
## Perform Quadratic Discriminant Analysis (QDA)
qda_iris<-qda(Species~Sepal.Length+Sepal.Width+Petal.Length+Petal.Width,data=train)
qda_iris

# priors
qda_iris$counts
qda_iris$prior # proportional to counts

# prediction
prd.qda.train<-predict(qda_iris,train)
str(prd.qda.train)

## Visualization: partition plots
Sp<-train$Species
Sp<-as.character(Sp)
Sp[Sp=="virginica"]<-"i"
Sp<-as.factor(Sp)
Predictions.lda.train=predict(lda_iris,train)

# 1. Project the decision boundaries to LD1 & LD2 space 
partimat(Sp~Predictions.lda.train$x[,2]+Predictions.lda.train$x[,1],method='qda',main='QDA decision boundaries',name=c('LD2','LD1'),prec=400)
legend("bottomright", inset=0.035, title="Species", c("s:Setosa","v:Versicolor","i:Virginica"))

# 2. Project the decision boundaries to the 2D spaces of four characteristic variables
partimat(Sp~Sepal.Length+Sepal.Width+Petal.Length+Petal.Width,data=train,method='qda')

## Check the accuracy of QDA analysis
# 1. QDA with training data
Predictions.qda.train=predict(qda_iris,train)
table.qda.train.cl<-table(Predictions.qda.train$class, train$Species)
table.qda.train.cl
sum(diag(table.qda.train.cl)/sum(table.qda.train.cl))

# 2. QDA with test data
Predictions.qda.test=predict(qda_iris,test)
table.qda.test.cl<-table(Predictions.qda.test$class, test$Species)
table.qda.test.cl
sum(diag(table.qda.test.cl)/sum(table.qda.test.cl))

