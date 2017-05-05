
rm(list=ls())

library (ISLR)

#We then use the subset option in lm() to fit a linear regression using only
#the observations corresponding to the training set.

attach (Auto)

dim(Auto)
summary(Auto)
set.seed (1)
train = sample(392, 196)

lm.fit =lm(mpg~horsepower, data=Auto, subset =train )

mean((mpg - predict(lm.fit, Auto))[-train ]^2)


#Therefore, the estimated test MSE for the linear regression fit is 26.14. We
#can use the poly() function to estimate the test error for the polynomial
#and cubic regressions.


lm.fit2=lm(mpg~poly(horsepower, 2), data=Auto, subset =train )
mean((mpg - predict(lm.fit2, Auto))[-train ]^2)


lm.fit3=lm(mpg~poly(horsepower, 3) ,data = Auto, subset = train )
mean((mpg - predict(lm.fit3, Auto))[-train ]^2)

#These error rates are 19.82 and 19.78, respectively. If we choose a different
#training set instead, then we will obtain somewhat different errors on the
#validation set.

set.seed (2)
train=sample (392,196)

lm.fit =lm(mpg???horsepower ,subset =train)
mean((mpg -predict (lm.fit ,Auto))[-train ]^2)


lm.fit2=lm(mpg???poly(horsepower ,2) ,data=Auto ,subset =train )
mean((mpg -predict (lm.fit2 ,Auto))[-train ]^2)

lm.fit3=lm(mpg???poly(horsepower ,3) ,data=Auto ,subset =train )
mean((mpg -predict (lm.fit3 ,Auto))[-train ]^2)



#5.3.2 Leave-One-Out Cross-Validation
#=====================================

#The LOOCV estimate can be automatically computed for any generalized
#linear model using the glm() and cv.glm() functions.


glm.fit=glm(mpg???horsepower ,data=Auto)
coef(glm.fit)

#In this lab, we will perform linear regression using the glm() function rather
#than the lm() function because the latter can be used together with cv.glm(). 
#The cv.glm() function is part of the boot library.

library (boot)
glm.fit=glm(mpg~horsepower, data=Auto)
cv.err =cv.glm(Auto, glm.fit)
cv.err$delta


#We can repeat this procedure for increasingly complex polynomial fits.
#To automate the process, we use the for() function to initiate a for loop

cv.error=rep (0,5)

for (i in 1:5){
   glm.fit=glm(mpg~poly(horsepower, i), data=Auto)
   cv.error[i]=cv.glm(Auto, glm.fit)$delta [1]
    }
cv.error



#5.3.3 k-Fold Cross-Validation
#==============================

#The cv.glm() function can also be used to implement k-fold CV. Below we
#use k = 10, a common choice for k, on the Auto data set


set.seed (17)

cv.error.10= rep (0, 10)

for (i in 1:10) {
   glm.fit=glm(mpg~poly(horsepower ,i), data=Auto)
   cv.error.10[i]=cv.glm(Auto, glm.fit, K = 10) $delta [1]
   }

cv.error.10


#5.3.4 The Bootstrap
#====================

#we must create a function that computes the statistic of interest.
#Second, we use the boot() function, which is part of the boot library, to
#boot() perform the bootstrap by repeatedly sampling observations from the data
#set with replacement.

alpha.fn=function (data ,index){
   X=data$X [index]
   Y=data$Y [index]
   return ((var(Y)-cov (X,Y))/(var(X)+var(Y) -2* cov(X,Y)))
   }


alpha.fn(Portfolio ,1:100)

#The next command uses the sample() function to randomly select 100 observations
#from the range 1 to 100, with replacement. This is equivalent to constructing 
#a new bootstrap data set and recomputing ^?? based on the new data set

set.seed (1)
alpha.fn(Portfolio ,sample (100 ,100 , replace =T))

boot(Portfolio ,alpha.fn,R=1000)


x=seq(0:19)
sum(x)



































