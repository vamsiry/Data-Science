library(caret)
set.seed(50)
x=sample(1:100,20)
y=sample(1:100,20)
x
y
#1...............
cov(x,y)
cor(x,y)

x=x*30
cov(x,y)
y=y*30
cov(x,y)
cov(x,y)/900

#1.1 scaling(removing bias) wil impact covariance and correlation or not...........

x=as.data.frame(x)
preobj1 = preProcess(x,method=c("scale"))
x1=predict(preobj1,x)
y=as.data.frame(y)
preobj2=preProcess(y,method = "scale")
y1=predict(preobj2,y)
cov(x1,y1)
cor(x1,y1)
x1;y1

#1.2 range(0-1) wil impact covariance and correlation or not...........

x=as.data.frame(x)
preobj1 = preProcess(x,method=c("range"))
x1=predict(preobj1,x)
y=as.data.frame(y)
preobj2=preProcess(y,method = "range")
y1=predict(preobj2,y)
cov(x1,y1)
cor(x1,y1)

x1;y1

#2 centring wil impact covariance and correlation or not...........
x=as.data.frame(x)
preobj1 = preProcess(x,method=c("center"))
x1=predict(preobj1,x)
y=as.data.frame(y)
preobj2=preProcess(y,method = "center")
y1=predict(preobj2,y)
x1
y1
cov(x1,y1)

#2.1 z score(-3:+3) wil impact covariance and correlation or not...........
x=as.data.frame(x)
preobj1 = preProcess(x,method=c("center","scale"))
x1=predict(preobj1,x)
y=as.data.frame(y)
preobj2=preProcess(y,method = c("center","scale"))
y1=predict(preobj2,y)
cov(x1,y1)
cov(x1,y1)/(sd(x1)*sd(y1)) 
cor(x1,y1)
