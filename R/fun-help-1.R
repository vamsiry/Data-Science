
#package plyr for count function

#####################################################################
################## 11 .  Apply Function on Rows     ############## 
#####################################################################

# Apply Function
#================
#apply function : to the rows or columns of a matrix or data frame.
#It cannot be applied on lists or vectors

aa = as.data.frame(iris)
str(aa)
aa = aa[,sapply(aa,is.numeric)] #extracting only numeric col.

x1 = apply(aa, 2, FUN = mean)
x2 = apply(aa, 2, FUN = median)
x3 = apply(aa, 2, FUN = sd)
x4 = apply(aa, 2, FUN = var)

data.frame(mean = x1,x2,x3,x4)


apply(iris[,-5], 1, max) #Maximum value of each row
apply(iris[,-5], 1, max, na.rm = TRUE)  #To ignore NAs,(default na.rm is TRUE)


apply(iris[,-5], 1, mean) #Calculate mean value across row
apply(iris[,-5], 1, mean, na.rm = TRUE)

lapply(iris[,-5], median)

lapply(iris[,-5], function(x) median(x, na.rm = TRUE))

lapply(dat, function(x) x + 1)  #Apply a custom function add 1 to each value

apply(iris[,-5] == 0, 1, sum, na.rm= TRUE) #Calculate number of 0s in each row

apply(iris[,-5] > 5, 1,  sum, na.rm= TRUE) #Calculate number of values greater than 5 in each row

df = iris[apply(iris[,-5], 1, sd, na.rm = TRUE)>=3,] #Select all rows having "sd" value greater than or equal to 4


helper = apply(data, 1, function(x){any(is.na(x))}) #Remove rows having NAs
df2 = data[!helper,]

df3 = apply(data,1, function(x) length(unique(na.omit(x)))) #Count unique values across row



################################################################################
#==================================================================   
# 1. Function 'apply' on a simple matrix:

mymatrix = matrix(data = c(6,34,923,5,0, 112:116, 5,9,34,76,2, 545:549), nrow = 5)
mymatrix

apply(mymatrix, MARGIN = 1, FUN = mean)
apply(mymatrix, MARGIN = 2, FUN = mean)
apply(mymatrix, MARGIN = 2, FUN = sort)

#2. Using 'lapply' on a data.frame 'mtcars'
#dataset (hint: 'lapply', 'sapply', 'mapply').

lapply(mtcars, FUN = min) -> l # 'lapply' gives a list,
sapply(mtcars, FUN = min) -> s #  'sapply' and 'mapply' give vectors per default 
mapply(mtcars, FUN = min) -> m #  'sapply' and 'mapply' give vectors per default 

l; s; m

listobjects = list(l, s, m)

sapply(FUN = class, X = listobjects)


#3. 'mapply'
#=============
#a. Use 'mapply' to get a list of 10 elements. The list is an alteration of 'A' and 'F'. 
#The lengths of those 10 alternating elements decreases step by step from 10 to 1.

mapply(rep, c("A", "F"), 10:1)
mapply(rep, c("A", "F"), 10:1, USE.NAMES = F)


#4. Titanic Casualties - Use the standard 'Titanic' dataset which is part of R Base
#==================================================================================
data("Titanic")

#a. Use an appropriate apply function to get the sum of males vs females aboard.

apply(Titanic, 2, sum)

#b. Get a table with the sum of survivors vs sex.
apply(Titanic, c(2,4), sum)

# c. Get a table with the sum of passengers by sex vs age.
apply(Titanic, c(3,2), sum)


# 5. Extracting elements from a list of matrices with 'lapply'
# a. Create 'listobj' which is a list of four matrices - see data:
first = matrix(38:66, 3)
second = matrix(56:91, 3)
third = matrix(82:145, 3)
fourth = matrix(46:93, 5)

listobj = list(first, second, third, fourth)

# b. Extract the second column from the list of matrices (from each single matrix).
lapply(listobj, "[",  , 2)

# c. Extract the third row from the list of matrices.
lapply(listobj, "[",  3, )


# 6. Plotting with the 'apply' family. Use the dataset 'iris' from R Base.
#a. Get a boxplot for each numerical column of the 'iris' dataset (four boxplots).
head(iris)
apply(iris[,1:4], 2, boxplot)


# Get one violin box plot for each numeric column,
install.packages("vioplot")
library(vioplot)
apply(iris[,1:4], 2, vioplot, col = "salmon", names = "")

#----------------------------------
tapply(mtcars$hp, mtcars$cyl, mean)
#you can have the mean power by cylinder capacity. This function is
#very usefull on descriptive analysis. BUT sometimes you have lists, 
#not vectors. In this case just use lappy or sapply (simplify the output).

lista <- list(a=c('one', 'tow', 'three'), b=c(1,2,3), c=c(12, 'a')) 
lapply(lista, length) ## return a list
sapply(lista, length) ## coerce to a vector


data <- split(mtcars, mtcars$gear) ## split(like group_by function)

fits <- lapply(data, function(x) return(lm(x$mpg~x$disp)$coef)) ## apply
do.call(rbind, fits) ## recombine

###########################################################################
##################################################################
##########################################
######### Writing functions in r 
##########################################
x1 =1:10
x2 = 11:20

f1 <- function(x, y) {
  z1 <- x + y
  z2 <- x - y
  z3 = x/y
  z4 = mean(x)
  z5 = mean(y)
  data.frame(z1, z2,z3,z4,z5) 
}

f1(x1,x2)


#---------------------------------------------
mysummary <- function(x,npar=TRUE,print=TRUE) {
  if (!npar) {
    center <- mean(x); spread <- sd(x)
  } else {
    center <- median(x); spread <- mad(x)
  }
  if (print & !npar) {
    cat("Mean=", center, "\n", "SD=", spread, "\n")
  } else if (print & npar) {
    cat("Median=", center, "\n", "MAD=", spread, "\n")
  }
  result <- list(center=center,spread=spread)
  return(result)
}

# invoking the function
set.seed(1234)
x <- rpois(500, 4)
mysummary(x)

a <- rnorm(100)
mysummary(a)


#-------------------------------------------------
square.it <- function(x) {
  square <- x * x
  square
}

square.it(3)
square.it(c(1, 4, 2))

matrix1 <- cbind(c(3, 10), c(4, 5))
square.it(matrix1)

#------------------------------------------------
#Local vs global environment

fun1 <- function(x) {
  3 * x - 1
}

fun1(5)

#---------------------
#will not display result
fun2 <- function(x) {
  y <- 3 * x - 1
  
}

fun2(5)

#-------------------
fun2 <- function(x) {
  y <- 3 * x - 1
  y
}

fun2(5)

#What would be the final value of x after running the following program
#------------------------------------------------
x = 3

mult <- function(j)
{
  x = j * 2
  return(x)
}

mult(5)

# x value is 2 bcz x is defined outside the function

#--------------------------------------------------
my.fun <- function(X.matrix, y.vec, z.scalar) {
  
  # use my previous function square.it() to square the scalar and save result
  sq.scalar <- square.it(z.scalar)
  
  # multiply the matrix by the vector using %*% operator
  mult <- X.matrix %*% y.vec
  
  # multiply the two resulting objects together to get a final object
  final <- mult * sq.scalar
  
  # return the result
  return(final)
}


my.mat <- cbind(c(1, 3, 4), c(5, 4, 3))
my.vec <- c(4, 3)
my.fun(X.matrix = my.mat, y.vec = my.vec, z.scalar = 9)

my.fun(my.mat, my.vec, 9)

#---------------------------------------------
another.fun <- function(sq.matrix, vector) {
  
  # transpose matrix and square the vector
  step1 <- t(sq.matrix)
  step2 <- vector * vector
  
  # save both results in a list and return
  final <- list(step1, step2)
  return(final)
}


outcome <- another.fun(sq.matrix = cbind(c(1, 2), c(3, 4)), vector = c(2, 3))

outcome[[1]]

#-----------------------------------------------------
#Tricks for troubleshooting and debugging

my.fun(X.matrix = my.mat, y.vec = c(2, 3, 6, 4, 1), z.scalar = 9)

debug(my.fun)
my.fun(X.matrix = my.mat, y.vec = c(2, 3, 6, 4, 1), z.scalar = 9)

#-----------------------------------------------------------
#Printing out what's happening (sanity checks)

my.fun <- function(X.matrix, y.vec, z.scalar) {
  print("xmatrix")
  print(X.matrix)
  
  print("yvec")
  print(y.vec)
  
  print("Dimensions")
  print(dim(X.matrix))
  print(length(y.vec))
  
  # use my previous function square.it() to square the scalar and save result
  sq.scalar <- square.it(z.scalar)
  print(paste("sq.scalar=", sq.scalar))
  
  # multiply the matrix by the vector using %*% operator
  mult <- X.matrix %*% y.vec
  
  # multiply the two resulting objects together to get a final object
  final <- mult * sq.scalar
  
  # return the result
  return(final)
}

my.fun(X.matrix = my.mat, y.vec = c(2, 3, 6, 4, 1), z.scalar = 9)


#------------------------------------------------
#Using the stop() and stopifnot() functions to write your own error messages

my.second.fun <- function(matrix, vector) {
  
  if (dim(matrix)[2] != length(vector)) {
    stop("Can't multiply matrix%*%vector because the dimensions are wrong")
  }
  
  product <- matrix %*% vector
  
  return(product)
  
}

my.second.fun(my.mat, c(6, 5))
my.second.fun(my.mat, c(6, 5, 7))


#IF ELSE Statement
#===============================================

samples = data.frame(x =c(rep(1:10)), y=letters[1:10])

samples$t1 = ifelse(samples$x>6,2,1)

samples$t2 = ifelse(samples$y=="a",2,1)

samples$t3 = ifelse(samples$x>1 & samples$y=="b" ,2,1)

samples$t4 = ifelse(samples$x>=1 & samples$x<=4,1,ifelse(samples$x>=5 & samples$x<=7,2,3))



#############################################################################
#===============
# 2. For Loop
#===============
dat <- data.frame(x = c(1:5,NA),z = c(1, 1, 0, 0, NA,0),y = 5*c(1:6))

#Example 1 : Maximum value of each column
#-----------------------------------------------
#Prior to starting a loop, we need to make sure we create an empty vector. 
#The empty vector is defined by x=NULL. Next step is to define the number 
#of columns for which loop over would be executed. It is done with ncol 
#function. The length function could also be used to know the number of column.

x = NULL

for (i in 1:ncol(dat)){
  x[i]= max(dat[i], na.rm = TRUE)}

x

# above FOR LOOP program can be written like the code below -

x = vector("double", ncol(dat))
for (i in seq_along(dat)){
  x[i]= max(dat[i], na.rm = TRUE)}

x
#The vector function can be used to create an empty vector. The seq_along finds out what to loop over.


#Example 2 : Split IRIS data based on unique values in "species" variable
--------------------------------------------------------------------------
 for (i in 1:length(unique(iris$Species))) {
    require(dplyr)
    assign(paste("iris",i, sep = "."), 
           filter(iris, Species == as.character(unique(iris$Species)[i])))
  }


filter(iris, Species == as.character(unique(iris$Species)[]))


#Combine Data within LOOP
#----------------------------
#In the example below, we are combining rows in iterative process.

#Method 1 : Use do.call with rbind
#-----------------------------------
temp =list()
for (i in 1:length(unique(iris$Species))) {
  series= data.frame(Species =as.character(unique(iris$Species))[i])
  temp[[i]] =series
}

output = do.call(rbind, temp)
output


#Method 2 :  Use Standard Looping Technique
#-----------------------------------------------
dummydt=data.frame(matrix(ncol=0,nrow=0))

for (i in 1:length(unique(iris$Species))) {
  series= data.frame(Species =as.character(unique(iris$Species))[i])
  if (i==1) {output = rbind(dummydt,series)}  
  else {output = rbind(output,series)}
}

output


#changing above as function
dummydt=data.frame(matrix(ncol=0,nrow=0))

temp = function(data, var) {
  for (i in 1:length(unique(data[[var]]))) {
    series= data.frame(Species = as.character(unique(data[[var]]))[i])
    if (i==1) {output = rbind(dummydt,series)}  
    else {output = rbind(output,series)}
  }
  return(output)}
temp(iris, "Species")


#For Loop and Sapply Together
#-----------------------------
for (i in which(sapply(dat, is.numeric))) {
  dat[is.na(dat[,i]), i] <- median(dat[, i],  na.rm = TRUE)
}

#======================
# 3. While Loop in R
#======================
#A while loop is more broader than a "forloop" because you can rescript any
# "forloop" as a while loop but not vice-versa.

#we are checking whether a number is an odd or even,

i=1
while(i<15)
{
  if(i%%2==0)
    print(paste(i, "is an Even number"))
  else if(i%%2>0)
    print(paste(i, "is an Odd number"))
  i=i+1
}

#--------
i= 1:10

Mod(i/2)

#Loop Concepts : Break and Next
#-------------------------------
#Break Keyword : When a loop encounters 'break' it stops the iteration and
#breaks out of loop

for (i in 1:3) {
  for (j in 3:1) {
    if ((i+j) > 4) {
      break    } else {
        print(paste("i=", i, "j=", j))
      }
  }
}


#Next Keyword:When a loop encounters 'next', it terminates the current
#iteration and moves to next iteration.

for (i in 1:3) {
  for (j in 3:1) {
    if ((i+j) > 4) {
      next   }  else {
      print(paste("i=", i, "j=", j))
    }
  }
}

############################################################################
#######  pracice on %>%(pipe operator) ##########

install.packages("babynames")
library(babynames) # data package
library(dplyr)     # provides data manipulating functions.
library(magrittr)  # ceci n'est pas un pipe
library(ggplot2)   # for graphics

babynames %>%
  filter(name %>% substr(1, 3) %>% equals("Ste")) %>%
  group_by(year, sex) %>%
  summarize(total = sum(n)) %>%
  qplot(year, total, color = sex, data = ., geom = "line") %>%
  add(ggtitle('Names starting with "Ste"')) %>%
  print


set.seed(1) # reproducability

# Utility function for sampling.
sample_with_replace <-
  function(v, n = 100) sample(v, size = n, replace = TRUE)

# Generate some auction data for the example.
auction.data <-
  data.frame(
    Price    = 1:100 %>% sample_with_replace,
    Quantity = 1:10  %>% sample_with_replace,
    Type     = 0:1 %>% 
      sample_with_replace %>% factor(labels = c("Buy", "Sell"))
    
  ) %T>%
  (lambda(x ~ x %>% head %>% print))



###########################################################################

# Extras
############
##########################################################################
#What is the difference between the following two programs ?
#===========================================================

temp = data.frame(v1<-c(1:10),v2<-c(5:14))
temp = data.frame(v1=c(1:10),v2=c(5:14))

#In the first case, a data frame temp which has 2 variables with improper
#variable names. The second code creates a data frame temp with proper
#variable names


########################################################################
#Does the following code work?
#==================================
ifelse(df$var1==NA, 0,1)

#It does not work. The logic operation on NA returns NA. It does not TRUE or FALSE.

#This code works 

ifelse(is.na(df$var1), 0,1)



##########################################################################
############## visualization ###################

#How to produce histogram
#===========================
df = sample(1:100, 25)
hist(df, right=FALSE)

colors = c("red", "yellow", "green", "violet", "orange", "blue", "pink", "cyan")
hist(df,  right=FALSE,  col=colors, main="Main Title ", xlab="X-Axis Title")


#How to produce bar graph
#============================
mydata = sample(LETTERS[1:5],16,replace = TRUE)
mydata.count= table(mydata)
barplot(mydata.count)


colors = c("red", "yellow", "green", "violet", "orange", "blue", "pink", "cyan")
barplot(mydata.count, col=colors, main="Main Title ", xlab="X-Axis Title")



#How to produce Pie Chart
#=============================
mydata = sample(LETTERS[1:5],16,replace = TRUE)
mydata.count= table(mydata)
pie(mydata.count, col=rainbow(12))

library(plot3D)
fun = function(x,y) {
  return(x+y)
}
x = seq(-2, 4, 0.5)
y = seq(-2, 4, 0.5) 
f = outer(x,y,fun)

windows(width=50, height=60)
persp3D(x, y, f, xlab="x", ylab="y", zlab="f", theta = 30, phi = 10)
X11()
persp3D(x, y, f, xlab="x", ylab="y", zlab="f",color.palette = heat.colors, theta = 30, phi = 10, colkey = FALSE)
X11()
persp3D(x, y, f, xlab="x", ylab="y", zlab="f",color.palette = heat.colors, border = "#808080", theta = 30, phi = 10, colkey = FALSE, ticktype="detailed")

x = c(-1,1)
y = c(2,0) 
z = c(3,3)
points3D(x,y,z, pch = 20, col = 'red', add = TRUE)

hist3D(z=x, border="black")

x = as.matrix(x)
scatter3D (x,y,z)


library(plot3D)
##  Simulate data:
set.seed(2002)
x <- rnorm(1000)
y <- rnorm(1000)

##  Create cuts:
x_c <- cut(x, 20)
y_c <- cut(y, 20)

##  Calculate joint counts at cut levels:
z <- table(x_c, y_c)

##  Plot as a 3D histogram:
hist3D(z=z, border="black")

##  Plot as a 2D heatmap:
image2D(z=z, border="black")

plot(iris$Sepal.Length,iris$Petal.Length,  col=iris$Species )
plot3(iris$Sepal.Width,iris$Petal.Width,col = iris$Species)

plot(iris$Sepal.Length,iris$Sepal.Width,col = iris$Species)

plot(iris$Petal.Length,iris$Petal.Width,col = iris$Species)

plot(iris$Sepal.Length,iris$Petal.Width,col = iris$Species)

plot(iris$Sepal.Width,iris$Petal.Length,col = iris$Species)
library(ggplot2)

# qplot()
qplot(df$X1, df$X2, colour = df$Colour)
qplot(iris$Sepal.Width,iris$Petal.Length, colour= iris$Species)

p <- ggplot(iris, aes(iris$Sepal.Width,iris$Petal.Length, colour = iris$Species))
p <- p + geom_point() + xlab("POS") + ylab("CS")
p


#########################################################################
#How to generate random numbers between 1 and 100
#===============================================

rand = runif(100, min = 1, max = 100)

norm <- rnorm(100) #sample of 100 with mean 0 and standard deviation 1   
mean(norm)
sd(norm)
norm <- rnorm(100, 2, 5)
mean(norm)
sd(norm)

set.seed(124)
pois <- rpois(100, lambda = 3)#Poisson distribution with lambda=3
mean(pois)
var(pois)


set.seed(124)
binom <- rbinom(100, 20, 0.2) #Binomial dist with size=20 and prob=.2
mean(binom)
sd(binom)

# point probability for a specific value of a  normal dist
dnorm(-1.96)


# plotting the density function of a normal distribution: N(2, .25)
x <- seq(0, 4, 0.1)
plot(x, dnorm(x, 2, 0.5), type = "l")

# plotting the density function of a binomial distribution: Binom(30, .25)
y <- 0:30
plot(y, dbinom(y, 30, 0.25), type = "h")

qnorm(rnorm(100))

#--------------------------------------
x1 = rpois(100,20)# random sample of size 100 with a mean of 20

x2 = dpois(1:100,200,F)# density

x3 = ppois(10,100)# probability

x4 = qpois(100,100)# quantile



##################################################################################
#One Sample t-test
x = c(0.593, 0.142, 0.329, 0.691, 0.231, 0.793, 0.519, 0.392, 0.418)
t.test(x, alternative="greater", mu=0.3)


#From the output we see that the p-value = 0.029. Hence, there is moderately strong 
#evidence that the mean Salmonella level in the ice cream is above 0.3 MPN/g


#Two-sample t-tests

#Below is the relevant R-code when assuming equal stand ard deviation
Control = c(91, 87, 99, 77, 88, 91)
Treat = c(101, 110, 103, 93, 99, 104)

t.test(Control,Treat,alternative="less", var.equal=TRUE)


#Below is the relevant R-code when not  assuming equal standard deviation

t.test(Control,Treat,alternative="less")

#Paired t- tests

reg = c(16, 20, 21, 22, 23, 22, 27, 25, 27, 28)
prem = c(19, 22, 24, 24, 25, 25, 26, 26, 28, 32)
t.test(prem,reg,alternative="greater", paired=TRUE)


###########################################################################
# both logitModel and probitModel
#---------------------------------
set.seed(1)
probLower = vector(length=1000)

for(i in 1:1000){      
  x = rnorm(1000)
  y = rbinom(n=1000, size=1, prob=pnorm(x))
  
  logitModel  = glm(y~x, family=binomial(link="logit"))
  probitModel = glm(y~x, family=binomial(link="probit"))
  
  probLower[i] = deviance(probitModel)<deviance(logitModel)
}

sum(probLower)/1000

deviance(probitModel)

deviance(logitModel)

deviance(logitModel)-deviance(probitModel)

summary(logitModel)
summary(probitModel)


#################################################################################
#In R, the which() function gives you the position of elements of a logical vector that are TRUE.

which(letters=="z") 

ls = data.frame( x1 = ceiling(runif(10)*10),
                 x2 = ceiling(runif(10)*10),
                 x3 = runif(10),
                 x4= rep(letters[1:5],2))

i=which(names(ls)== "x4")
which(ls$x1 == max(ls$x1))
which(ls$x1 == 7 & ls$x2 == 4)
length(which(ls$x1 == ls$x2))
ls[which(ls$x1 == ls$x2),"x1"]
check = which(sapply(ls, is.numeric))
colnames(ls)[check]

#--------------------------------------------
#which code is faster
#---------------------
ls = data.frame( x1 = ceiling(runif(100)*10),
                 x2 = ceiling(runif(100)*10))

start.time <- Sys.time()
length(which(ls$x1 == ls$x2))
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

start.time <- Sys.time()
sum(ls$x1 == ls$x2)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

##########################################################################





