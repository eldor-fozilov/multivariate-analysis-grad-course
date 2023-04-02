#####################################################
## cMDS Example: Distances between European Cities ##
#####################################################

#Classical MDS is best applied to metric variables. Torgerson (1958) initially developed this method. 
#It assumes that the data obey distance axioms-they are like a proximity or distance matrix on a map. 

# read city distance data (8 European cities)
city<-as.matrix(read.csv("city.csv",row.names=1,header=T))
city

#### 2D case k=2 ####
euro.mds <- cmdscale(city, eig = TRUE) ## default dimension=2
euro.mds

# dimensions (scree plot)
plot(euro.mds$eig,xlab = "Number of Dim", ylab = "eigenvalue",pch=19,col="blue")


# coordinates
X1 <- euro.mds$points[, 1]
X2 <- euro.mds$points[, 2]

X1
X2

# 2D Plot
plot(X1, -X2, xlab = "X1", ylab = "X2", main = "cmdscale(city)",col="black")  ## reflected
text(X1, -X2, rownames(euro.mds$points), cex = 1, col="blue")

#### 3D case k=3 ####
#install.packages("rgl",dependencies = T)
library(rgl)

euro.mds <- cmdscale(city, k=3, eig = TRUE) 
euro.mds

# coordinates
X1 <- euro.mds$points[, 1]
X2 <- euro.mds$points[, 2]
X3 <- euro.mds$points[, 3]

X1
X2
X3

# 3D plot
plot3d(X1, X2, X3, xlab = "X1", ylab = "X2", zlab = "X3", main = "cmdscale(city)", size=7, col='red')

# Animation  
play3d(spin3d(axis=c(0,1,1), rpm=2), duration=20)


####################################################################################################
######################################################################
## non-metric MDS Example: Perceived dissimilarities for car models ##
######################################################################
library(MASS)

# read car dissimilarity data (10 Brands)
car<-as.matrix(read.csv("car.csv",row.names=1,header=T))
car

##### 2D case
car.mds <- isoMDS(car) #default k=2
car.mds

# stress
st.2<-car.mds$stress*0.01
st.2

# coordinates
X1 <- car.mds$points[,1]
X2 <- car.mds$points[,2]

# 2D Plot
plot(-X1,-X2, xlab = "X1", ylab = "X2", xlim = range(car.mds$points[,1])*1.2, type = "n")
text(-X1,-X2, labels = colnames(car))

# Shepard diagram
car.sh<-Shepard(car[lower.tri(car)], car.mds$points)
plot(car.sh,xlab = "Dissimilarity", ylab = "Distance", xlim = range(car.sh$x), ylim = range(car.sh$x))
points(car.sh$y, pch=19, cex = 1, col = "red")
points(car.sh$yf, pch=3,cex = 1, col = "blue")
#lines(car.sh$x, car.sh$yf, type = "S")

##### 3D case
car.mds <- isoMDS(car,k=3) 
car.mds

# stress
st.3<-car.mds$stress*0.01
st.3

##### Choose dimension (scree plot of stress)
##### Calculate stress for 1D,4D,5D cases
car.mds <- isoMDS(car,k=1) 
st.1<-car.mds$stress*0.01
st.1

car.mds <- isoMDS(car,k=4) 
st.4<-car.mds$stress*0.01
st.4

car.mds <- isoMDS(car,k=5) 
st.5<-car.mds$stress*0.01
st.5

plot(c(st.1,st.2,st.3,st.4,st.5),xlab = "Number of Dim", ylab = "Stress",pch=19,col="blue")




