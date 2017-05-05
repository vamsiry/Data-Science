
########################### DATA CLEANING ##############################


#####################################################################
###################### Missing Data #####################
######################################################################

is.na(x) # returns TRUE of x is missing

mydata$v1[mydata$v1==99] <- NA #Recoding Values to Missing

mean(x, na.rm=TRUE) # Excluding Missing Values while calculation

mydata[!complete.cases(mydata),] # list rows of data that have missing values


newdata <- na.omit(mydata)  # create new dataset without missing data


#Detecting NAs
#==============

is.na(z)         # Is it NA?
which (is.na(z)) # Which one is NA?

#To find all the rows in a data frame with at least one NA, try this: 
unique (unlist (lapply (z, function (x) which (is.na (z)))))


#Ways to Exclude Missing Values
#===============================

#mean(), median(), colSums(), var(), sd(), min() and max() all take the na.rm argument

#Note that cor() and its relatives don't work that way: with those you need
#to supply the use= argument. This is to permit more complicated handling 
#of missing values than simply omitting them. 


na.action=na.fail #which just stops when it encounters any missing values
na.fail #Stop if any missing values are encountered
na.omit #Drop out any rows with missing values
na.exclude #Drop out rows with missing values, but keeps track of where 
           # they were (so that when you make predictions, for example, 
           #you end up with a vector whose length is that of the original response.) 

na.pass   #Take no action.

na.tree.replace (library (tree): 
#For discrete variables, adds a new category called "NA" to replace the missing values.

na.gam.replace (library gam): 
#Operates on discrete variables like na.tree.replace(); for numerics, 
#NAs are replaced by the mean of the non-missing entries


#Example
  
a <- data.frame (c1 = 1:8, c2 = factor (c("a", "b", "a", "c", "b", "c", "a", "b")))
a[4,1] <- a[6,2] <- NA    # This repeated assignment is legal and does what you expect.
levels(a$c2) 
na.fail (a)
na.exclude (a)
a = na.gam.replace (a) #library(gam)


#Special Case 1a: Missing Values in Factor Vectors
#=====================================================
#We noted above that a missing value in a factor variable is displayed as <NA> rather than just NA. 
#Again, missing values do not have a level, but you can change a missing value to one of the existing levels


a <- factor (c("a", "b", "c", "b", "c", "b", "a", "c", "c")) # create the factor

levels(a) 

a[3] <- "d" #warning message tells you that some NAs have geen generated.

levels(a)[1] <- "AA"

as.character(a)
levels(a)

#Internal Storage and Extra Levels
#-------------------------------

a <- factor (c(1, 2, 3, 2, 3, 2, 1), levels=1:4, labels=c("Small", "Medium", "Large", "Huge"))
levels(a)
table(a)

table (a[,drop=T])

#Special Case 2: Missing Values in Character Vectors
#========================================================
#Character vectors can have missing values. They display as NA in the usual way. 
#This really isn't a special case at all. 

a <- factor (c(1, 2, 3, 2, 3, 2, 1), levels=1:4, labels=c("Small", "Medium", "Large", "Huge"))

a[3] <- NA

table (a)

table (a, exclude=NULL)

sum (is.na (a))   

#Special Case 3: NaNs
#=====================
#In addition to NA, R has a special value NaN for "not a number." 0/0 is an 
#example of a calculation that will produce a NaN. NaNs print as NaN, but 
#generally act like NAs

#For example, a computation done on an NaN produces an NaN; if you try to extract
#the NaNth element of a vector, you get NA

#One more special value is Inf. If you need them, there are is.nan() and
#functions for finding things that are NaN or infinite and not NA. 


#Why is my numeric variable a factor?
#====================================
# "numeric" variable actually contains some non-numeric entries (like "NA" or "Missing" or an empty space)

Steve$G <- as.numeric(levels(Steve$G)[Steve$G])  
as.numeric(as.character(Steve$G))


#How do I convert factors to character vectors in a data frame?
#===============================================================
for (i in 1:ncol (a)) if (class (a[,i]) == "factor") a[,i] <- as.character(a[,i])


#When are factor variables a big pain?
#======================================
#Factor variables are a pain when you're cleaning your data because they're hard
#to update. My approach has always been to convert the variable to character with 
#as.character(), then handle the variable as a character vector, and then convert
#it to factor (using factor() or as.factor()) at the end. 



#Operations on Missing Values
#============================

x <- c(1, 2, NA, 4)                 
y<-c(2,5,8,NA)
z=cbind(x,y)

x + 1       # NA + 1 = NA
sum(x)      # This produces NA because we can't add NAs
length(x)   # This is okay


############
as.numeric (c("1", "2", "4"))
is.numeric(c("1", "2", "three", "4"))

c(1, 2, 3)[4]
NA - 1
a <- data.frame (a = 1:3, b = 2:4)
a[,4]
a[4,] 
a[1,2] <- NA  
a[a$b < 4,]
a[,is.numeric(a)]
class(a)
str(a)

###############

g <- as.data.frame(matrix(c(1:5, NA), ncol = 2))

g

na.omit(g)

na.exclude(g)

na.fail(g)

na.pass(g)

#Missing values in analysis

anscombe <- within(anscombe, {
  y1[1:3] <- NA
})

model.omit <- lm(y2 ~ y1, data = anscombe, na.action = na.omit)
model.exclude <- lm(y2 ~ y1, data = anscombe,na.action = na.exclude)


resid(model.omit)

resid(model.exclude)

fitted(model.omit)

fitted(model.exclude)

x1 <- c(1, 4, 3, NA, 7)
x2 <- c("a", "B", NA, "NA")

mean(x1)
mean(x1, na.rm = TRUE)
summary(x1)
table(x1)
table(x1, useNA = "ifany")
table(1:3, useNA = "always")

x1s <- sort(x1)
length(x1s)
sort(x1, na.last = TRUE)

#========================================================================
############## mice package for missing values imputation ################

#https://gist.github.com/mick001/df77b69b30ef6ff9fc0b

data <- airquality
data[4:10,3] <- rep(NA,7)
data[1:5,4] <- NA

data <- data[-c(5,6)]
summary(data)


#check for features (columns) and samples (rows) where more than 5% of the 
#data is missing using a simple function
pMiss <- function(x){sum(is.na(x))/length(x)*100}
apply(data,2,pMiss)
apply(data,1,pMiss)


#Using mice for looking at missing data pattern
install.packages("mice")
library(mice)
md.pattern(data)


#A perhaps more helpful visual representation can be obtained using the VIM package as follows
install.packages("VIM")
library(VIM)
aggr_plot <- aggr(data, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(data), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))


#Another (hopefully) helpful visual approach is a special box plot
marginplot(data[c(1,2)])


#Imputing the missing data : The mice() function takes care of the imputing process
tempData <- mice(data,m=5,maxit=50,meth='pmm',seed=500)
summary(tempData)


#A couple of notes on the parameters:

#m=5 refers to the number of imputed datasets. Five is the default value.

#meth='pmm' refers to the imputation method. 
#In this case we are using  predictive mean matching as imputation method.
#Other imputation methods can be used, type methods(mice) for imputation methods.

#If you would like to check the imputed data, for instance for the variable Ozone

tempData$imp$Ozone

tempData$meth

completedData <- complete(tempData,1)


#Inspecting the distribution of original and imputed data 
#use a scatterplot and plot Ozone against all the other variables
library(lattice)
xyplot(tempData,Ozone ~ Wind+Temp+Solar.R,pch=18,cex=1)

#What we would like to see is that the shape of the magenta points (imputed)
#matches the shape of the blue ones (observed). The matching shape tells us 
#that the imputed values are indeed "plausible values"
#Another helpful plot is the density plot:

densityplot(tempData)
stripplot(tempData, pch = 20, cex = 1.2)


#Pooling
#============
#Suppose that the next step in our analysis is to fit a linear model to the data.
#You may ask what imputed dataset to choose. The mice package makes it again very
#easy to fit a a model to each of the imputed dataset and then pool the results 
#together

modelFit1 <- with(tempData,lm(Temp~ Ozone+Solar.R+Wind))
summary(pool(modelFit1))


tempData2 <- mice(data,m=50,seed=245435)
modelFit2 <- with(tempData2,lm(Temp~ Ozone+Solar.R+Wind))
summary(pool(modelFit2))






#############################################################################

################  R Function : Creating Dummy variables  ##########################
#========================================


DF <- data.frame(strcol = c("A", "A", "B", "F", "C", "G", "C", "D", "E", "F"))

for(level in unique(DF$strcol)){
  DF[paste("strcol", level, sep = "_")] <- ifelse(DF$strcol == level, 1, 0)}


#--------------------------------
DF <- factor(c("A", "A", "B", "F", "C", "G", "C", "D", "E", "F"))

b = contrasts(DF)
b

contrasts(DF) <- contr.treatment(7)

contrasts(DF) <- contr.treatment(7,2)

contrasts(DF) <- contr.helmert(7)


#-----------------------------------------------------------
x=factor(c("A", "A", "B", "F", "C", "G", "C", "D", "E", "F"))

year.f = as.factor(x)
dummies = model.matrix(~year.f-1)


set.seed(1)
dat <- data.frame(sex = sample(c("male","female"), 10, replace = TRUE))

model.matrix( ~ sex - 1, data = dat)

head(model.matrix(~Carseats$ShelveLoc-1))
View(Carseats)


#----------------------------------------------------------------


set.seed(001) # generating some data
sex <- factor(sample(1:2, 10, replace=TRUE)) # this is what you have

sex<-factor(ifelse(as.numeric(sex)==2, 1,0)) # this is what you want
sex  

#If you want labels to be 0 = Male and 1 = Female, then...
sex<-factor(ifelse(as.numeric(sex)==2, 1,0), labels=c('M', 'F')) 
sex 

#-------------------------------------------------------------------
library(ISLR)

attach(Carseats)

View(Carseats)


hsb2 <- within(Carseats, {
  race.ct <- C(Carseats$ShelveLoc, treatment)
  print(attributes(race.ct))
})


hsb3 <- within(Carseats, {
  race.ct <- C(Carseats$ShelveLoc, helmert)
  print(attributes(race.ct))
})


hsb4 <- within(Carseats, {
  race.ch1 <- C(Carseats$ShelveLoc, helmert, 3)
  print(attributes(race.ch1))
})


a = contrasts(Carseats$ShelveLoc)

contrasts(Carseats$ShelveLoc) <- contr.treatment(3)



#---------------------------------------------------


a#Actually you don't need to create a dummy variable in order to estimate a model 
#using lm, let's see this example:

set.seed(001) # Generating some data
N <- 100
x <- rnorm(N, 50, 20)

y <- 20 + 3.5*x + rnorm(N)
sex <- factor(sample(1:2, N, replace=TRUE))

# Estimating the linear model 
lm(y ~ x + sex) # using the first category as the baseline (this means sex==1)

# renaming the categories and labelling them
sex<-factor(ifelse(as.numeric(sex)==2, 1,0), labels=c('M', 'F'))
lm(y ~ x + sex)  # the same results, baseline is 'Male'



#############################################################################

################  Zero- and Near Zero-Variance Predictors  ##########################

library(caret)

db_train <- read.csv("C:\\Users\\Vamsi\\Desktop\\R.Alg\\practice\\kaggle\\Santander Customer Satisfaction\\train.csv", na.strings = "")

dim(db_train)

nzv1 <- db_train[,-nearZeroVar(db_train)]

dim(nzv1)

nzv2 = nearZeroVar(db_train, saveMetrics = TRUE)

dim(nzv2)

str(nzv2, vec.len=1) 

nzv2[nzv2[,"zeroVar"] > 0, ] 

nzv2[nzv2$nzv2,]



#############################################################################

################  Identifying Correlated Predictors  ##########################

#While there are some models that thrive on correlated predictors (such as pls),
#other models may benefit from reducing the level of correlation between the 
#predictors. 

dim(db_train)

x1 <- cor(db_train)

chart.Correlation(x1)

corrplot(x1, type = "upper")

MatrCorLar <- melt(x1)

ggplot(x1, aes(x=Var1, y=Var2, fill=value))+geom_tile()
ggplot(x1, aes(x = Var1, y = Var2, fill = value))+geom_tile()


highCorr <- sum(abs(x1[upper.tri(x1)]) > .999)



library(usdm)
df = # Data Frame
vif(db_train)


qr(db_train)
qr(db_train)$pivot

