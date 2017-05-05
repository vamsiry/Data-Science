sessionInfo()

help("options")#

library()

search()

install.packages("jpeg")

ls(pos="package:stats")

apropos('foo')

example(foo) #

data()

getwd()

#view

help("options")
options(digits=3)

options()

help.start()

ls()

x2=2

y1=15

ls(pattern="x")

ls()

x2=10
class(x2)

string(x2)

rm(x2)

>data()

v=c(rep(463,"111010"))

v = 1 : 20
v=c(1:20)

v=1:20

y <- c(TRUE, TRUE, FALSE, FALSE) 

1 + 3 + 5 + 7

1:20

""1:20" , "19:1""

19:1

a=1:20

rep(463,10)
rep((4,6,3)(11,10,10))

a=c(1:10)

v1=1:20

v2=19:1

v3=c(v1,v2)

v1

v2

v3
a=4

b=6

c=3

h=c(a,b,c)

d=rep(h,10)
d1=c(d,a)

d1

v=rep((a=4,by=11),(b=6,by=10),(c=3,by=10))

rep(a=4,by=11)

seq(4,10,by=.5)

v=c(4,6,3)

v1=rep(v,10)

v2=c(v1,4)
v2

x=seq(3,6,by=.1)

cos(x)

a=rep(4,10)

b=rep(6,20)

c=rep(3,30)

d=c(a,b,c)

d
paste("label",1:30,sep = " ")

paste("fn",1:30,sep="")

xvec=sample(0:999,250,replace=T)

xvec[xvec>600]

xvec[xvec[xvec>600]]

set.seed(50)
xvec=sample(0:999,250,replace=T)

yvec=sample(0:999,250,replace=T)

yvec1=yvec[-1]

yve1-xvec=("yvec1"-"xvec")

xvec
yvec

yvec1

yvec1-xvec

f=yvec[yvec>600]

h=match(f,yvec1)

h

h1=h[-1]

h2=xvec[h1]
h2

h3=xvec%%2

h3

count(h3=0)

h4=(h3<1)
h4

count[h3<1]

h3[h3==0]

count[h3==0]

count(0,"h3")

h3[names(h3)==0]

[count(h3=0)]
[h3==3]

frequency(0,h3)

round(5.369, digits = 3)

signif(5.369, digits = 2)

trunc(5.369)
sqrt(10)

E^0

log(1)

log10(5)

x="abcdef"
substr(x,2,4)="234567"

x

is.numeric(x)

is.character(x)
is.array(x)

is.complex(x)

is.list(x)

is.logical(x)

typeof(x)

class(x)

is.numeric(x)
str(x)

y1=as.numeric(x)

y1

typeof(y1)
y=as.data.frame.noquote(x)

y

x="abceef"

typeof(x)

type.convert(x)
y=as.numeric(x)

y=12345

z=as.character(y)

class(z)

class(y)

y="abcd";class(y);

z=as.data.frame(y);class(z)

c(a,c,s,w,grg,rg,rg,rgr,gr,geg,e)

c("s")
n=c(">")

class(n)

n=as.logical(n)

class(n)
date()

timestamp()

mat=matrix(1:8,2,4,1)
class(matrix(1:8,2,4,1))

mat=as.data.frame(mat)

class(mat)

mat=as.list(mat);class(mat)
class(mat)

x2 <- runif(10, 5.0, 7.5)

x2

length(x2)

install.packages("caret")




x = sample(-100:100,50)

#Normalized Data
normalized = (x-min(x))/(max(x)-min(x))

#Histogram of example data and normalized data
par(mfrow=c(1,2))
hist(x,xlab="Data",col="lightblue",main="")
hist(normalized,xlab="Normalized Data",col="lightblue",main="")


newvalue= ('max'-'min')/(max-min)*(value-max)+max'
or
newvalue= ('max'-'min')/(max-min)*(value-min)+min'.

library(caret)
x=c(sample(1:100,15))
x=as.data.frame(x)
preobj=preProcess(x,method = c("scale", "center"))
z=predict(preobj,x)
z
a=as.data.frame((apply(x,2,min)))
a                 

#--------------------------------------

set.seed(111)

d=rpois(25,8)
d

GetCI=function(x,level=.95){
  m = mean(x)
  n = length(x)
  SE = sd(x)/sqrt(n)
  ci = m+c(-1,1)*qt(.975,n-1)*SE
  return(list(mean = m, se = SE, ci = ci))
  }

GetCI(d)

#----------------------------------------
# 07/24/2016


#assign values to the objects
x=c(2,3,4,5)
x

assign("x",c(2,5,6,7,9))
x

y=c(4,5,7)


## see which objects are alresdy occupied

ls()

objects()

# to remove objects
rm("x")

#some vector calculus examples

x=c(y,5,y)
objectrandom = x<5
objectrandom 

sum(x)
sqrt(x)

# to see which value this position has
x[1]
x[[1]]

# () round brackets as the standatd brackets
# [] if we are dealing with index ositions of vector
# {} for functions and loops

#--------------------------------------------------------------------
# Data sequences

seq(3,5)

seq(from=3, to =5)

seq(from=3, length =5)

seq(from=3, length =5, by=.5)

seq(from=3, by=.5, length =5) #argument order doesnot matter if u specify

# paste functions - characters
paste("XYZ", 1:10)

paste("XYZ", c(2,5,7,"test",4.5))

paste("XYZ", 1:10, sep = "")
paste("XYZ", 1:10, sep = " ")

#what do u think this can be useful for to create labels etc

# to repeat sequeances

rep(c(3,4,5), 5)

rep(1:10, times = 5)

x=c(1,2,3,4)

rep(x, each = 3)

rep(x, each = 3, times = 5)

# knowing the position

x=c(4:20)

which(x==10)

#reverse of

x[3]

# Function in R
#------------------------




























