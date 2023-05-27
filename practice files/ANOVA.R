##############
## 1. ANOVA ##
##############################################################################################
#### 1. One-way ANOVA (One-Factor Model)
## reference: https://rpubs.com/ibecav/308410 
### required libraries
#install.packages("ggplot2",dependencies = T)
library(ggplot2)
#install.packages("lsr",dependencies = T)
library(lsr)
#install.packages("psych",dependencies = T)
library(psych)
#install.packages("car",dependencies = T)
library(car)
#install.packages("tidyverse",dependencies = T)
library(tidyverse)
#install.packages("dunn.test",dependencies = T)
library(dunn.test)
#install.packages("BayesFactor",dependencies = T)
library(BayesFactor)
#install.packages("scales",dependencies = T)
library(scales)
#install.packages("knitr",dependencies = T)
library(knitr)
#install.packages("kableExtra",dependencies = T)
library(kableExtra)
#install.packages("DescTools",dependencies = T)
library(DescTools)

### read data
tyre<-read.csv("https://datascienceplus.com/wp-content/uploads/2017/08/tyre.csv")
str(tyre)
summary(tyre)
head(tyre)
View(tyre)

### description (aggregate)
describe(tyre)
par(mfrow=c(2,1))
hist(tyre$Mileage, main="Mileage distribution across all brands")
boxplot(tyre$Mileage, horizontal = TRUE, main="Mileage distribution across all brands", col = "blue")

### description (by brands)
describeBy(tyre$Mileage,group = tyre$Brand, mat = TRUE, digits = 2)
boxplot(tyre$Mileage~tyre$Brands, main="Boxplot comparing Mileage of Four Brands of Tyre", col= rainbow(4), horizontal = TRUE)

### Run one way ANOVA
tyres.aov<- aov(Mileage~Brands, tyre)
summary(tyres.aov)
names(tyres.aov)

### Post hoc test
# Tukey's Honestly Significant Difference (HSD)
TukeyHSD(tyres.aov, conf.level = 0.95)

par(oma=c(0,5,0,0)) # adjust the margins because the factor names are long
plot(TukeyHSD(tyres.aov, conf.level = 0.95),las=1, col = "red")

# Bonferroni 
pairwise.t.test(tyre$Mileage,tyre$Brands,p.adjust.method = "bonferroni")

# Holm
pairwise.t.test(tyre$Mileage,tyre$Brands,p.adjust.method = "holm")

### Effect size
etaSquared(tyres.aov)
round(etaSquared(tyres.aov,anova = TRUE),2)

### Checking ANOVA assumptions
par(mfrow=c(2,2))
plot(tyres.aov)

# normality
# The normality assumption for an ANOVA stated as: "The distribution of Y within each group is normally distributed." It's the same thing as Y|X and in this context, it's the same as saying the residuals are normally distributed
tyre.anova.residuals <- residuals( object = tyres.aov ) # extract the residuals
# A simple histogram
hist( x = tyre.anova.residuals ) # another way of seeing them
shapiro.test( x = tyre.anova.residuals ) # run Shapiro-Wilk test

# homogeneous variance
leveneTest(tyres.aov) 

##### non-parameteric version
# Kruskal-Wallis rank sum test 
kruskal.test(Mileage~Brands,tyre)

# Dunn test: post hoc pairwise comparisons
dunn.test(tyre$Mileage,tyre$Brands,method = "holm",alpha = 0.01)

##############################################################################################
#### 2. Two-way ANOVA (Two-factor Model)
## reference: https://rpubs.com/ibecav/308410 
## reference: http://www.sthda.com/english/wiki/two-way-anova-test-in-r

# ToothGrowth data: It contains data from a study evaluating the effect of vitamin C on tooth growth in Guinea pigs.
#The experiment has been performed on 60 pigs, where each animal received 
#one of three dose levels of vitamin C (0.5, 1, and 2 mg/day) by one of two delivery methods,
#(orange juice or ascorbic acid (a form of vitamin C and coded as VC). 
#Tooth length was measured and a sample of the data is shown below.

my_data<-ToothGrowth
str(my_data)
# Convert dose as a factor and recode the levels
# as "D0.5", "D1", "D2"

my_data$dose <- factor(my_data$dose, 
                           levels = c(0.5, 1, 2),
                           labels = c("D0.5", "D1", "D2"))

my_data

#### boxplot
# Box plot with two factor variables
boxplot(len ~ supp * dose, data=my_data, frame = FALSE, 
        col = c("#00AFBB", "#E7B800"), ylab="Tooth Length")
# Two-way interaction plot
interaction.plot(x.factor = my_data$dose, trace.factor = my_data$supp, 
                 response = my_data$len, fun = mean, 
                 type = "b", legend = TRUE, 
                 xlab = "Dose", ylab="Tooth Length",
                 pch=c(1,19), col = c("#00AFBB", "#E7B800"))


#### run two-way ANOVA
my_data$supp<-factor(my_data$supp) ## covert to a factor variable
my_data$dose<-factor(my_data$dose) ## covert to a factor variable

len.aov <- aov(len ~ supp + dose + supp:dose, data = my_data)
summary(len.aov)

#### Pairwise-Comparisons
# Tukey
TukeyHSD(len.aov, which = "dose")
TukeyHSD(len.aov, which = "supp")
TukeyHSD(len.aov, which = "supp:dose")

# ScheffeTest
ScheffeTest(len.aov)

##### Assumption checking
par(mfrow=c(2,2))
plot(len.aov)

# homogeneity of variance
leveneTest(len ~ dose, data = my_data)

# normality
len.residuals <- residuals(object = len.aov)
shapiro.test(x = len.residuals)

###############################################################################################
###############################################################################################
###############
## 3. ANCOVA ##
###############
## reference: https://www.statology.org/ancova-in-r/ 
## reference: https://gaopinghuang0.github.io/2017/11/04/ANCOVA-notes#effect-size

##### required packages
#install.packages("pastecs",dependencies = T)
#install.packages("car",dependencies = T)
#install.packages("dplyr",dependencies = T)
#install.packages("multcomp",dependencies = T)
#install.packages("ggplot2",dependencies = T)
#install.packages("lsr",dependencies = T)
#install.packages("effects",dependencies = T)

library(pastecs)
library(car)
library(dplyr)
library(multcomp)
library(ggplot2)
library(lsr)
library(effects)

###################################
##### Step 0: Data generation
# set a random seed
set.seed(10)

#create dataset
data <- data.frame(technique = rep(c("A", "B", "C"), each = 40),
                   current_grade = c(rnorm(40, 87, 10),rnorm(40, 83, 10),rnorm(40, 77, 10)),
                   exam = c(runif(40, 80, 95), runif(40, 75, 90), runif(40, 70, 85)))
data

str(data)

#view first six lines of dataset
head(data)

###################################
###### Step 1: Explore the Data
summary(data)

### descriptive stat of exam by technique
by(data$exam, data$technique, stat.desc, basic=F)

### descriptive stat of current_grade by technique
by(data$current_grade, data$technique, stat.desc, basic=F)

### box plots
par(mfrow=c(2,1))
data %>%
  group_by(technique) %>%
  summarise(mean_grade = mean(current_grade),
            sd_grade = sd(current_grade),
            mean_exam = mean(exam),
            sd_exam = sd(exam))

## exam by technique 
boxplot(exam ~ technique,
        data = data,
        main = "Exam Score by Studying Technique",
        xlab = "Studying Technique",
        ylab = "Exam Score",
        col = "steelblue",
        border = "black"
)

## current_grade by technique 
boxplot(current_grade ~ technique,
        data = data,
        main = "Current Grade by Studying Technique",
        xlab = "Studying Technique",
        ylab = "Current Grade",
        col = "steelblue",
        border = "black"
)

###############################################
##### Step 2: Check the Model Assumptions
## 1. Conduct an ANOVA to test whether the current grade (covariate) is independent of the technique (independent variable).
anova_model <- aov(current_grade ~ technique, data = data)
summary(anova_model)

## 2. Test homogeneity of variances
# Levene's test
leveneTest(exam~technique, data = data)

## 3. normality assumption, 4. homogeneity of regression slopes will be tested later

##################################################
##### Step 3: Fit the ANCOVA Model
data$technique<-factor(data$technique) # convert to a factor variable

# default contrasts: WRONG
contrasts(data$technique) 

# orthogonal contrasts: CORRECT
contrasts(data$technique)<-cbind(c(-2,1,1), c(0,-1,1)) 
contrasts(data$technique)

##fit ANCOVA model
ancova_model <- aov(exam ~ technique + current_grade, data = data)
ancova_model

## summary of model 
# we'd like to use type III sum of squares for the model, since type I sum of squares is dependent upon the order that the predictors are entered into the model
Anova(ancova_model, type="III")

########################################################################################
##### Step 4: Check the Additional Model Assumptions
## 3. normality assumption
par(mfrow=c(2,2))
plot(ancova_model)
residuals <- residuals(object=ancova_model)
shapiro.test(x=residuals) # run Shapiro-Wilk test => Not Good! USE Robust ANCOVA (nonparametric regression)

## 4. homogeneity of regression slopes
# focus on the significance of the interaction term
scatter <- ggplot(data, aes(current_grade, exam, colour = technique))
scatter + geom_point(aes(shape = technique), size = 3) + 
  geom_smooth(method = "lm", aes(fill = technique), alpha = 0.1) +
  labs(x = "current_grade", y = "exam")

hoRS<-update(ancova_model, .~. + current_grade:technique)  ## add the interaction term
Anova(hoRS, type="III")  # from car package

#######################################################################
##### Step 5: Post Hoc Tests 
### Adjusted means
# se=TRUE: show standard errors
adjustedMeans<-effect("technique", ancova_model, se=TRUE)
summary(adjustedMeans)

### Interpret planned contrasts
# contrast 1 2A=B+C  / contrast 2 B=C
contrasts(data$technique) # orthogonal contrasts
summary.lm(ancova_model)

## define the post hoc comparisons to make
# glht: pairwise test between adjusted means - Not the same as pairwise t-test
postHocs<-glht(ancova_model, linfct = mcp(technique = "Tukey"))

#view a summary of the post hoc comparisons
summary(postHocs)

#view the confidence intervals associated with the multiple comparisons
confint(postHocs)

### effect size 
etaSquared(ancova_model)
round(etaSquared(ancova_model,anova = TRUE),2)

###########################
### ANCOVA ASSUMPTIONS
# The same assumptions as for ANOVA (normality, homogeneity of variance and random independent samples)
# + Independence of the covariate and treatment effect
# + Homogeneity of regression slopes

#######################################################################################
#######################################################################################
###############
## 4. MANOVA ##
###############
library(gplots)
library(reshape2)  # for melt() function
library(ggplot2)

# Use iris data set
iris
str(iris)

#view first six rows of iris dataset
head(iris)

# convert data format for boxplot
irisMelt <- melt(iris, id=c('Species'))
irisMelt<-irisMelt[which(irisMelt[,2] == "Sepal.Length"| irisMelt[,2] == "Sepal.Width"),]
names(irisMelt) <- c('Species', 'variable', 'cm')

irisMelt

# boxplot
irisBoxplot <- ggplot(irisMelt, aes(Species, cm, color = variable))
irisBoxplot + geom_boxplot() + labs(x='Species', y='centimeter', color='variable') + scale_y_continuous(breaks=seq(0,20, by=2))

#fit the MANOVA model
model <- manova(cbind(Sepal.Length, Sepal.Width) ~ Species, data = iris)

# view the results - default: "Pillai"'s Trace
summary(model, intercept=T)

# Or use other tests
# summary(model, intercept=TRUE, test="Wilks")
# summary(model, intercept=TRUE, test="Hotelling")
# summary(model, intercept=TRUE, test="Roy")

# univariate ANOVA: the MANOVA takes account of the correlation between dependent variables, and so for these data it has more power to detect group differences.
summary.aov(model)

#visualize mean sepal length and width by species
par(mfrow=c(1,2))
plotmeans(iris$Sepal.Length ~ iris$Species)
plotmeans(iris$Sepal.Width ~ iris$Species)


