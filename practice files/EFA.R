#################
## EFA example ##
#################
### Source: R For Marketing Research and Analytics p.206-214 ###

##########################################################
## Consumer Brand Rating Data: the data used in PCA

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

## stardardization (optional)
brand.sc <- brand.ratings
brand.sc [ , 1:9] <- data.frame(scale(brand.ratings [ , 1:9]))
summary(brand.sc)

#########################################################################################
#install.packages("nFactors",dependencies = T)
library(nFactors)

##########################################
### 1. Find the Number of commom factors
nScree(brand.sc [ , 1:9])
eigen(cor(brand.sc [ , 1:9]))
# scree plot
# PCA analysis
brand.pc <- prcomp(brand.sc [ , 1:9],scale=T)  
plot(brand.pc , type="l")

# factanal/loadings: default cutoff value for printing = 0.1
## Factor anaylsis with two common factors
factanal(brand.sc[ , 1:9] , factors =2)

## Factor anaylsis with three common factors
factanal(brand.sc[ , 1:9] , factors =3)

## Factor anaylsis with three common factors - unstandardized data: the results are exactly the same
factanal(brand.ratings[ , 1:9] , factors =3)

#####################################################
## 2. EFA Rotation: orthogonal vs. oblique???
# 1. Orthogonal rotation - Default option: Varimax method
# 2. Oblique Rotation
#install.packages("GPArotation",dependencies = T)
library(GPArotation) 
# oblimin
brand.fa.ob <- factanal(brand.sc[ , 1:9] , factors =3, rotation="oblimin")
brand.fa.ob

# promax
brand.fa.ob2 <- factanal(brand.sc[ , 1:9] , factors =3, rotation="promax")
brand.fa.ob2


###############################
## 3. Result: Factor Loading
## Factor loading heatmap
#install.packages("gplots",dependencies = T)
#library(gplots)
library(gplots)
library(RColorBrewer)
heatmap.2(brand.fa.ob$loadings,col=brewer.pal(9, "Greens") , trace="none" , key=FALSE , dend="none", Colv=FALSE , cexCol = 1.2, main="\n\n\n\n\nFactor loadings for brand adjectives")

## Path diagram - (|Factor loadings| < 0.3 are excluded) 
#install.packages("semPlot",dependencies = T)
library(semPlot)
semPaths(brand.fa.ob , what="est" , residuals=FALSE, cut =0.3 , posCol=c("white" , "darkgreen") , negCol=c("white" , "red"), edge.label.cex =0.75 , nCharNodes =7)

################################################
## 4. Result: Using Factor Scores for Brands
brand.fa.ob <- factanal(brand.sc[ , 1:9] , factors =3, rotation="oblimin", scores="Bartlett") 
brand.scores <- data.frame(brand.fa.ob$scores) # get the factor scores
brand.scores$brand <- brand.sc$brand # get the matching brands
head(brand.scores)

## Find the overall position for a brand 
brand.fa.mean <- aggregate (.~brand , data=brand.scores , mean)
rownames(brand.fa.mean) <- brand.fa.mean [ , 1] # brand names
brand.fa.mean <- brand.fa.mean[, -1]
names(brand.fa.mean) <- c("Leader" , "Value" , "Latest") # factor names
brand.fa.mean

## Heatmap graphs the scores by brand:
heatmap.2(as.matrix(brand.fa.mean),
           col=brewer.pal(9, "GnBu"), trace="none", key=FALSE, dend="none",
           cexCol =1.2 , main="\n\n\n\n\nMean factor score by brand")
