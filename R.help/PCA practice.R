x1 = c(10,2,8,9,12)
x2 = c(20,5,17,20,22)
x3 = c(10,2,7,10,11)
x4 = c(25,20,18,7,5)
data = data.frame(x1,x2,x3,x4)

pairs(data)

pca1=princomp(data,scores = TRUE,cor = TRUE)
summary(pca1)

##print the score plot
plot(pca1)

##print the biplot
biplot(pca1)

##print the loadings
pca1$loadings

##print the scores
pca$scores


dim(data)
str(data)
head(data)

#check the assumption of PCA
cor(data)
cov(data)
sum(diag(cov(data)))

#PCA computed using Covariance/correlation matrix
pca = princomp(data, cor=F)
names(pca)
summary(pca)
plot(pca, type="lines")
pca$loadings
pca$scores
sum(diag(cov(pca$scores)))

# pca using caret package
library(caret)
preObj = preProcess(data, method=c("pca"), thresh = 1.0)
preObj$rotation
newdata = predict(preObj,data)


#PCA computed using SVD
pca = prcomp(data, scale.=TRUE)
names(pca)
pca
summary(pca)
pca$rotation
pca$x
plot(pca, type="lines")



######## PCA for boston data set.....

library (MASS)
library (ISLR)

fix(Boston)

?Boston

names(Boston)

dim(Boston)

apply(Boston,2,mean)

apply(Boston,2,var)

summary(Boston)

pairs(Boston)

#check the assumption of PCA
cor(Boston)
cov(Boston)
sum(diag(cov(Boston)))
sum(diag(cor(Boston)))

#PCA computed using Covariance/correlation matrix

#pca using correlation as true....

pca = princomp(Boston,scores = TRUE,scale=TRUE, cor=TRUE)

#pca using covariance as true....

pca = princomp(Boston,scores = TRUE,scale=TRUE, cor=FALSE)


summary(pca)

names(pca)

#loadings of pca.....
pca$loadings
loadings(pca)

#pca scores........
pca$scores

#plots of pca......

plot(pca)
plot(pca, type="lines")
screeplot(pca,type = "line",main = "scoreplot")

#biplot of pca.....
biplot(pca)

#rotation........
varimax(pca$sdev)
promax(pca, m=4)

sum(diag(cov(pca$scores)))

####################################################
install.packages("psych")
install.packages("GPArotation")

library(psych)
library(GPArotation)
r=cor(Boston)

cortest.bartlett(r,n=FALSE)

det(r)

shapiro.test(r)

KMO(r)

base=princomp(Boston, rotate="none", nfactors=10)

base$communality

plot(base$values)

#to see eigen values
base$values

#factor analysis
fac1=factanal(Boston,factor=3);fac1

fac2=factanal(Boston,factors = 3,rotation = "varimax");fac2

fac3=factanal(Boston,factors = 3,rotation = "varimax",scores = "regression");fac3

