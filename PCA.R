
# MOTHER DATA
#==================================================================================================
setwd("C:/Users/Gourab/Desktop/R")
cars = read.csv("cars.csv")


#A 2-DIMENSION CASE
#==================================================================================================

#I - THE MAIN DATA
data.2d = cars[,c("Horsepower","Weight")]
plot(data.2d,main="The Main Data",pch=20,cex=0.8)

#variance of the data
apply(data.2d,2,var)
sum(apply(data.2d,2,var))
sum(apply(data.2d,2,sd))


#-------------------------------------------------------------------------------------------------

#II - NORMALIZING THE DATA
for(i in 1:ncol(data.2d))
{
  data.2d[,i] = data.2d[,i] - mean(data.2d[,i])
}

plot(data.2d, main="The Normalized Data")
abline(v=0,h=0,lty=2,col="red")

#variance of the normalized data
apply(data.2d,2,var)
sum(apply(data.2d,2,var))

#-------------------------------------------------------------------------------------------------

#STANDARDIZING THE DATA
data.2d = scale(data.2d)
plot(data.2d, main="The Standardizes Data")
abline(v=0,h=0,lty=2,col="red")

#variance of the normalized data
apply(data.2d,2,var)
sum(apply(data.2d,2,var))


#-------------------------------------------------------------------------------------------------

#PCS FOR THE 2-DIMENSIONAL DATA
#==================================================================================================

?princomp
data.2d = cars[,c("Horsepower","Weight")]
pc.2d = prcomp(data.2d)

#PC LOADINGS
#-----------
pc.2d$rotation


#PC SCORES
#-----------
pc = pc.2d$x
pc[1:5,]
data.2d[1:5,]


#PC VAR
#-----------
pc.2d$sdev^2
cumsum(pc.2d$sdev^2)/sum(pc.2d$sdev^2)*100
pc.2d$sdev^2/sum(pc.2d$sdev^2)*100

sum(pc.2d$sdev^2)
sum(apply(data.2d,2,var))



plot(pc,pch=20,cex=0.8)
abline(v=0,h=0,lty=2,col="red")


#PCA - Orthogonal Transformation
cor(pc)


sum(pc.2d$sdev^2)
sum(apply(data.2d,2,var))


#-------------------------------------------------------------------------------------------------

#PCS FOR THE P-DIMENSIONAL DATA
#==================================================================================================

?princomp
pc = prcomp(cars[,-c(1,8,9)])
names(cars)
ncol(cars)-3

#PC LOADINGS
#-----------
pc$rotation


#PC VAR
#-----------
pc$sdev^2
prop = round(pc$sdev^2/sum(apply(cars[,-c(1,8,9)],2,var))*100,4)
cumprop = cumsum(prop)
plot(1:6,cumprop,type="b",ylim=c(99.5,100))

sum(pc$sdev^2)
sum(apply(cars[,-c(1,8,9)],2,var))


#PC SCORES
#-----------
pc = pc$x
pc[1:5,]
pc = as.data.frame(pc)
round(cor(pc),3)

plot(pc,pch=20,cex=0.8)


#=================================================================================================
#HOUSE PRICE PREDICTION DATA
#========================================================================
house <- read.csv("C:/Users/Gourab/Downloads/kc_house_train_data.csv")
View(house)
summary(house)
View(house[,-c(1,2,3,15,16,17,20,21)])

pc.house1 = prcomp(house[,-c(1,2,3,15,16,17,20,21)])
loadings1 = round(pc.house1$rotation,3)
write.csv(as.data.frame(loadings1),"loading_house1.csv")

pc.house1$sdev
propvar1 = cumsum(pc.house1$sdev)/sum(pc.house1$sdev)*100
propvar1

plot(0:length(propvar1),c(0,propvar1),type="b", ylim = c(0,100))

round(pc.house1$rotation,3)


#========================================================================================
mat = matrix(c(7,3,3,-1),2,2,byrow=T)
mat

egn = eigen(mat)
egn

egn$values
egn$vectors


#========================================================================================
setwd("C:/Users/Gourab/Desktop/R")
cars = read.csv("cars.csv")

cars2 = cars[,c("Horsepower","Weight","Displacement")]
covmat = cov(cars2)
covmat  

#RESULT 1
cov.egn = eigen(covmat)
cov.egn
cov.egn$values

pc = prcomp(cars2)
pc$rotation

#RESULT 2
pc$sdev^2


#RESULT 3
sum(apply(cars2,2,var))
sum(cov.egn$values)

