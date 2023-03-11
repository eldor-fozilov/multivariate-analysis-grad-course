#################
## PCA example ##
#################
# Source: R For Marketing Research and Analytics p.198-206

##### Install packages
#install.packages("corrplot")
#install.packages("factoextra")

###############
## Example 1 ##
######################################################################################
### Generate three correlated variables
set.seed (98286)
xvar <- sample (1:10 , 100 , replace=TRUE)
yvar <- xvar
yvar[sample (1: length(yvar) , 50) ] <- sample (1:10 , 50 , replace=TRUE)
zvar <- yvar
zvar[sample (1: length(zvar) , 50) ] <- sample (1:10 , 50 , replace=TRUE)
my.vars <- cbind(xvar , yvar , zvar)
my.vars

plot(yvar ~ xvar , data=jitter(my.vars))
cor(my.vars)

my.pca <- prcomp(my.vars,scale=T)
my.pca
# eigenvalues: Standard deviation^2
summary(my.pca)
biplot(my.pca)

###############
## Example 2 ##
######################################################################################
## Consumer Brand Rating Data
brand.ratings <- read.csv("http://goo.gl/IQl8nc")
head(brand.ratings)
tail(brand.ratings)
summary(brand.ratings)
str(brand.ratings)

#Perceptual adjective (column name) |Example survey text
#Perform | Brand has strong performance
#Leader  | Brand is a leader in the field
#Latest  | Brand has the latest products
#Fun     | Brand is fun
#Serious | Brand is serious
#Bargain | Brand products are a bargain
#Value   | Brand products are a good value
#Trendy  | Brand is trendy
#Rebuy   | I would buy from Brand again

## stardardization
brand.sc <- brand.ratings
brand.sc [ , 1:9] <- data.frame(scale(brand.ratings [ , 1:9]))
summary(brand.sc)

## correlation
library(corrplot)
corrplot(cor(brand.sc [ , 1:9]) , order="hclust")

## PCA analysis
brand.pc <- prcomp(brand.sc [ , 1:9],scale=T)  #False is okay
brand.pc
summary(brand.pc)

# Scree plot
plot(brand.pc , type="l")

# 2D plot
biplot(brand.pc)

# eigenvalues/eigenvectors
eigen(cor(brand.sc[ ,1:9])) # sign flipping is okay: https://stats.stackexchange.com/questions/88880/does-the-sign-of-scores-or-of-loadings-in-pca-or-fa-have-a-meaning-may-i-revers

# eigenvalues
(brand.pc$sdev)^2

# eigenvectors
brand.pc$rotation

# principal component scores
brand.pc$x
pcscores<-data.frame(brand.pc$x[,1:3]) #first 3 principal components
pcscores$brand<-brand.sc$brand
head(pcscores)

brand.pc.mean <- aggregate (.~brand , data=pcscores , mean)
brand.pc.mean

## scree plot/cumulative variance
library(factoextra)
fviz_eig(brand.pc)
p<-length(brand.pc$sdev)
cumpro <- cumsum(brand.pc$sdev^2 / sum(brand.pc$sdev^2))
plot(cumpro[0:p], xlab = "PC #", ylab = "Amount of explained variance", main = "Cumulative variance plot")

fviz_pca_biplot(brand.pc,geom = "point", repel = TRUE,
                col.var = "#2E9FDF", # Variables color
                col.ind = "#696969"  # Individuals color
)

## Factor loading
F=brand.pc$rotation%*%diag(brand.pc$sdev)
F

# rowsum
rowSums(F^2)

# column sum = identical to eigenvalues
colSums(F^2) 