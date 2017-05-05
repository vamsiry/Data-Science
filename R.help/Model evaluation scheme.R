library(ISLR)

attach(Auto)
head(Auto)
dim(Auto)
names(Auto)

#validation set approch (holdout approch)
set.seed(1)
train=sample(391,196)

lm.fit1=lm(mpg~horsepower, data = Auto, subset = train)
mean((mpg -predict (lm.fit1, Auto))[-train ]^2)

lm.fit$model
lm.fit$model$mpg
summary(lm.fit)

lm.fit2=lm(mpg~poly(horsepower ,2) ,data=Auto ,subset =train )
mean((mpg -predict (lm.fit2 ,Auto))[-train ]^2)

lm.fit3=lm(mpg~poly(horsepower ,3) ,data=Auto ,subset =train )
mean((mpg -predict (lm.fit3 ,Auto))[-train ]^2)

lm.fit4=lm(mpg~poly(horsepower ,7) ,data=Auto ,subset =train )
mean((mpg -predict (lm.fit4 ,Auto))[-train ]^2)


# If we choose a different training set instead, we will obtain somewhat 
#different errors  by setting a diffrent random seed:

set.seed(2)
train=sample(392,196)
lm.fit=lm(mpg~horsepower,subset=train)
mean((Auto$mpg-predict(lm.fit,Auto))[-train]^2)

lm.fit2=lm(mpg~poly(horsepower,2),data=Auto,subset=train)
mean((Auto$mpg-predict(lm.fit2,Auto))[-train]^2)

lm.fit3=lm(mpg~poly(horsepower,3),data=Auto,subset=train)
mean((Auto$mpg-predict(lm.fit3,Auto))[-train]^2)


#Leave-One-Out Cross-Validation----------------------
#if we use glm() to fit a model without passing in the family argument(binomial), 
#then it performs linear regression,just like the lm() function.

glm.fit=glm(mpg~horsepower ,data=Auto)
coef(glm.fit)

lm.fit =lm(mpg~horsepower ,data=Auto)
coef(lm.fit)

#---------------------------------

library (boot)
glm.fit=glm(mpg~horsepower ,data=Auto)
cv.err =cv.glm(Auto ,glm.fit)
cv.err$delta


cv.error=rep (0,5)

for (i in 1:5){
   glm.fit=glm(mpg~poly(horsepower ,i),data=Auto)
   cv.error[i]= cv.glm(Auto, glm.fit)$delta [1]
  }

cv.error

#-----------------------------------------------------------
for (i in 1:5){
  glm.fit=glm(mpg~poly(horsepower,i),data=Auto)
  print(cv.glm(Auto,glm.fit)$delta)
}
#---------------------------------------------------------------------

#=============================================================================

#k-Fold Cross-Validation----------------------------------

set.seed (17)

cv.error.10= rep(0,10)

for(i in 1:10) {
  glm.fit=glm(mpg~poly(horsepower ,i),data=Auto)
  cv.error.10[i]=cv.glm(Auto, glm.fit, K=10)$delta[1]
  }

cv.error.10

#-------------------------------------------------------------------------
#an Application to Default Data==================

library(ISLR)
summary(Default)

for (i in 1:10) {
  set.seed(i)
  train = sample(10000,8000)

glm.fit=glm(default~balance+student,data=Default, family=binomial, subset=train)

glm.probs=predict(glm.fit,Default[-train,],type="response")

#Confusion matrix
glm.pred=rep("No",length(glm.probs))
glm.pred[glm.probs>.5]="Yes"

table(glm.pred,Default$default[-train])

# Error rate
print(mean(glm.pred!=Default$default[-train]))

}

summary(glm.fit)

# K fold CV for default data set==============================


glm.fit=glm(default~balance+student,data=Default, family=binomial)

glm.probs=predict(glm.fit,Default,type="response")

glm.pred=rep("No",length(glm.probs))
glm.pred[glm.probs>.5]="Yes"

table(glm.pred, Default$default)

# Error rate
print(mean(glm.pred != Default$default))

#----------------------------------------------------------------------------

#6 Estimating the Accuracy of a Statistic of Interest

alpha.fn=function(data,index){
  X=data$X[index]
  Y=data$Y[index]
  return((var(Y)-cov(X,Y))/(var(X)+var(Y)-2*cov(X,Y)))
}

alpha.fn(Portfolio,1:100)

set.seed(1)
alpha.fn(Portfolio,sample(100,100,replace=T))

boot(Portfolio,alpha.fn,R=1000)
names(boot)

#7 Estimating the Accuracy of a Linear Regression Model

boot.fn=function(data,index) return(coef(lm(mpg~horsepower ,data=data ,subset =index)))
boot.fn(Auto,1:392)


set.seed(1)
boot.fn(Auto,sample(392,392,replace=T))
boot.fn(Auto,sample(392,392,replace=T))


boot(Auto,boot.fn,1000)

summary(lm(mpg~horsepower ,data=Auto))$coef

boot.fn=function(data,index) coefficients(lm(mpg~horsepower +I( horsepower ^2) ,data=data , subset =index))
set.seed(1)
boot(Auto,boot.fn,1000)

summary(lm(mpg~horsepower +I(horsepower ^2) ,data=Auto))$coef


