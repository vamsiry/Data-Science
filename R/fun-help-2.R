
# All the previously allowed assignment operators (<-, :=, _, and <<-) remain fully in effect.

x := 3
help(":=")
?`<-` 

#####################################################################
##################7 . Data Exploration with R   ############## 
#####################################################################

#Import data into R

#mydata <- read.csv("C:/Users/Deepanshu/Documents/Book1.csv", header=TRUE)
mydata=fix(Boston)

summary(mydata)
summary( mydata[3]) #To calculate summary of a particular column, say third column

summary( mydata$lstat) #To calculate summary of a particular column by its name

names(mydata) #Lists variables in a dataset 

nrow(mydata) #Calculate number of rows in a dataset

ncol(mydata) #Calculate number of columns in a dataset

str(mydata) #List structure of a dataset

head(mydata) #First 6 rows of dataset

head(mydata, n=5) #First n rows of dataset

head(mydata, n= -1) #All rows but the last row

tail(mydata) #Last 6 rows of dataset

tail(mydata, n=5) #Last n rows of dataset

tail(mydata, n= -1) #All rows but the first row


#############################################################################
# Playing with data.frames along with which() function to extract col etc..
#-------------------------------------------------------------------------
library(ggplot2)
data("diamonds")

#checking class of variable 
#==========================
str(diamonds)

data.frame(sapply(diamonds, class)) #class of each variable

which(sapply(diamonds, class) !="numeric") # for non numeric col numbers

names(which(sapply(diamonds, class) !="numeric")) # for non numeric col names

which(lapply(diamonds, class) !="numeric")

which(mapply(diamonds, class) !="numeric")


# Extracting Numeric and Factor Variables 
#============================================

cols <- sapply(diamonds, is.numeric)
abc = diamonds [,cols]

num_col = diamonds[ ,sapply(diamonds, is.numeric)] # Extracting Numeric columns

fat_col = diamonds[,sapply(diamonds, is.factor)] # Extracting factor columns

#Note : mydata is a dataframe


#=============================================================================
# function for extracting diff tyes of variables from the data set
#------------------------------------------------------------
varlist <- function (df=NULL,type=c("numeric","factor","character"), pattern="", exclude=NULL) {
  vars <- character(0)
  if (any(type %in% "numeric")) {
    vars <- c(vars,names(df)[sapply(df,is.numeric)])
  }
  if (any(type %in% "factor")) {
    vars <- c(vars,names(df)[sapply(df,is.factor)])
  }  
  if (any(type %in% "character")) {
    vars <- c(vars,names(df)[sapply(df,is.character)])
  }  
  vars[(!vars %in% exclude) & grepl(vars,pattern=pattern)]
}


## All variable names starting with cred
varlist(german_data,pattern="^cred")

## All numeric variable
varlist(iris,type="numeric")

## All factor variable except variable gb and variables starting with c
varlist(german_data,type="factor",exclude=c("gb",varlist(german_data,pattern="^c")))

## Same as previous, only using pattern instead of c()
varlist(german_data,type="factor",exclude=varlist(german_data,pattern="^c|gb"))

#Once we have list of column names, it is easy to use sapply and do real job:
sapply(german_data[,varlist(german_data,type="numeric",pattern="credit")], summary)

#Of course, we can have our own function in sapply:
sapply(german_data[,varlist(german_data,type="numeric",pattern="credit")], function (x) length(unique(x)))
#=============================================================================


# Converting a factor to integer or viceversa
#=============================================

a <- factor(c(2, 4, 3, 3, 4))
a1 = as.numeric(a) #Incorrect Way
a2 = as.numeric(as.character(a)) #Correct Way
str(a2)

#Converting Factor Variables to Numeric                 
dat <- data.frame(x = c(1:5,NA),z = c(1, 1, 0, 0, NA,0),y = factor(5*c(1:6)))
index <- sapply(dat, is.factor)
dat[index] <- lapply(dat[index], function(x) as.numeric(as.character(x)))


#14. Converting Multiple Numeric Variables to Factor 
#===============================================
mydata  = train
#1. Using Column Index Numbers
names <- c(1:3,5)
mydata[,names] <- lapply(mydata[,names] , factor)
str(mydata)


#2. Using Column Names
names <- c('Credit' ,'Balance')
mydata[,names] <- lapply(mydata[,names] , factor)
str(mydata)


#3.  Converting all variables
col_names <- names(mydata)
mydata[,col_names] <- lapply(mydata[,col_names] , factor)


#4. Converting all numeric variables to factors
mydata[,sapply(mydata,is.numeric)] <- lapply(mydata[sapply(mydata, is.numeric)], as.factor)

data.frame(apply(mydata,2,class))

#5. Checking unique values in a variable and convert to factor only those
#variables having unique count less than 4

col_names <- sapply(mydata, function(col) length(unique(col)) < 4)
mydata[ , col_names] <- lapply(mydata[ , col_names] , factor)


#===========================================================================
#Dataset below has the characteristics of my large dataset. I am managing it in
#data.table, some columns are loaded as chr despite they are numbers and I want to 
#convert them into numerics and these column names are known 
example("data.table")

library(data.table)
dt = data.table(A=LETTERS[1:10],B=letters[1:10],C=as.character(runif(10)),D = as.character(runif(10))) # simplified version
str(dt)

strTmp = c('C','D') # Name of columns to be converted to numeric

dt[, (strTmp) := lapply(.SD, as.numeric), .SDcols = strTmp]
str(dt)

class(sd(dt$C))


#Unique rows common to both the datasets
#=========================================

df1=data.frame(ID=c(1:5), Score=c(50:54))
df2=data.frame(ID=c(3,5,7:9), Score=c(52,60:63))

library(dplyr)
comb = intersect(df1,df2)
comb2 = union(df1,df2)



###############################
##How to combine data frames?
###############################

x = c(1:5)
y = c("m","f","f","m","f")
z=cbind(x,y)
z
z = rbind(x,y)

#While using cbind() function, make sure the number of rows must be equal in both the datasets
#While using rbind() function, make sure both the number and names of columns must be same
#If names of columns would not be same, wrong data would be appended to columns or records might go missing.

#How to combine data by rows when different number of columns?
#=================================================================

df = data.frame(x = c(1:4), y = c("m","f","f","m"))
df2 = data.frame(x = c(5:11))

#The bind_rows() function from dplyr package can be used to combine data frames when number of columns do not match
library(dplyr)
combdf = bind_rows(df,df2)

# Merging (Matching)
#----------------------

mydata <- merge(mydata1, mydata2, by=c("ID")) #merges only common cases to both datasets.

df1 <- data.frame(ID = c(1, 2, 3, 4, 5),
                  w = c('a', 'b', 'c', 'd', 'e'),
                  x = c(1, 1, 0, 0, 1),
                  y=rnorm(5),
                  z=letters[1:5])

df2 <- data.frame(ID = c(1, 7, 3, 6, 8),
                  a = c('z', 'b', 'k', 'd', 'l'),
                  b = c(1, 2, 3, 0, 4),
                  c =rnorm(5),
                  d =letters[2:6])


df3 = merge(df1, df2, by ="ID")  #Inner Join

#If the primary key (matching variable) do not have same name in both the tables (data frames),
df33 = merge(df1, df2, by.x ="ID", by.y="ID")


df4 = merge(df1, df2, by ="ID", all.x = TRUE) #Left Join

df5 = merge(df1, df2, by ="ID", all.y = TRUE) #Right Join

df6 = merge(df1, df2, by ="ID", all = TRUE) #Full (Outer) Join

df5 = merge(df1, df2, by ="ID",all.x = TRUE, all.y = TRUE) #Full (Outer) Join

df7 = merge(df1, df2, by = NULL) #Cross Join


###########################################################################
#What is the use of which() function in R?
#============================================

#The which() function returns the position of elements of a logical vector that are TRUE

mydata=data.frame(x = c(1,3,10,5,7))
which(mydata$x==max(mydata$x))


#What is the use of with() and by() functions? What are its alternatives?
#========================================================================

df=data.frame(x=c(1:6), y=c(1,2,4,6,8,12))

#You are asked to perform this calculation : (x+y) + (x-y) . 
#Most of the R programmers write like code below -

(df$x + df$y) + (df$x - df$y)

#Using with() function, you can refer your data frame and make the 
#above code compact and simpler-
with(df, (x+y) + (x-y))

#The with() function is equivalent to pipe operator in dplyr package.
library(dplyr)
df %>% mutate((x+y) + (x-y))


#The by() function is equivalent to group by function in SQL.  
#It is used to perform calculation by a factor or a categorical variable.
#The group_by() function in dply package can perform the same task.
df = data.frame(var1=factor(c(1,2,1,2,1,2)), var2=c(10:15))
with(df, by(df, var1, function(x) mean(x$var2)))

library(dplyr)
df %>% group_by(var1)%>% summarise(mean(var2))



#################################################################################
#####################################################################
################## 8 . Data Manipulation with R   ############## 
#####################################################################

#1. Replacing / Recoding values
#================================

#Create Dummy Data

mydata = data.frame(State = ifelse(sign(rnorm(25))==-1,'Delhi','Goa'), Q1= sample(1:25))

mydata$Q1[mydata$Q1==1] <- 6 #we are replacing 1 with 6 in Q1 variable

#we are replacing "Delhi" with "Mumbai" in State variable. We need to convert the state 
#variable from factor to character.
mydata$State = as.character(mydata$State)

mydata$State[mydata$State=='Delhi'] <- 'Mumbai'

mydata[mydata == 2 | mydata == 3] <- NA # we are replacing 2 and 3 with NA values in whole dataset


#Another method : You have to first install the car package.
#==========================================================

install.packages("car")
library("car")

mydata = data.frame(State = ifelse(sign(rnorm(25))==-1,'Delhi','Goa'), Q1= sample(1:25))

# Recode 1 to 6
mydata$Q1 <- recode(mydata$Q1, "1=6")

mydata$Q1 <- recode(mydata$Q1, "1:4=0; 5:6=1") ## Recoding 1 through 4 to 0 and 5 and 6 to 1

mydata$Q1 <- recode(mydata$Q1, "lo:4=0; 5:hi=1") # Recoding lowest value through 4 to 0 and 5 to highest value to 1

mydata$Q1 <- recode(mydata$Q1, "lo:4=0; 5:6=1;else = 3")

mydata$Ques1<- recode(mydata$Q1, "1:4=0; 5:6=1") # Create a new column called Ques1



#IF ELSE Statement
#===============================================

samples = data.frame(x =c(rep(1:10)), y=letters[1:10])

samples$t1 = ifelse(samples$x>6,2,1)

samples$t2 = ifelse(samples$y=="a",2,1)

samples$t3 = ifelse(samples$x>1 & samples$y=="b" ,2,1)

samples$t4 = ifelse(samples$x>=1 & samples$x<=4,1,ifelse(samples$x>=5 & samples$x<=7,2,3))


#3. Renaming variables
#===============================

df = data.frame(var1=c(1:5))
colnames(df)[colnames(df) == 'var1'] <- 'variable1'


library(plyr)
mydata <- rename(mydata, c(Q1 = "var1") ) # Rename Q1 variable to var1
samples = rename(samples,c(t1="v1",t2="v2",t3="v3",t4="v4"))


library(dplyr)
df= rename(df, variable1=var1) # Rename "var1" variable to "variable1"


# 4. Keeping and Dropping Variables
#======================================

mydata1 <- mydata[1:2] #keep only first two variables 

mydata1 <- mydata[c(1,3:6)] #keep first and third through sixth variables .

newdata <- samples[c("v1", "v2", "v3")]

mydata [-3]  # Deleting a particular column (Fifth column)

mydata$Ques1 <- NULL #Dropping Q3 variable

mydata [-(3:4) ]  #Deleting multiple columns

df = subset(mydata, select = -c(x,z) ) #Dropping multiple variables by their names


#5. Subset data (Selecting Observations)
#===============================================

mydata = data.frame(Name = ifelse(sign(rnorm(25))==-1,'ABC','DEF'), age = sample(1:25))

newdata <- mydata[1:10,]  #Selecting first 10 observerations

mydata11<-subset(mydata, age==10) #Subset data in R

newdata<-subset(mydata, age==3) #Copy data into a new data frame in R


newdata<-subset(mydata, Name=="ABC" & age==3) #Conditional Statement (AND) while selecting observations

newdata<-subset(mydata, Name=="ABC" | age==3) #Conditional Statement (OR) while selecting observations

newdata<-subset(mydata, age>=3)  #Greater than or less than expression

newdata<-subset(mydata, is.na(age)) #Keeping only missing records

newdata<-subset(mydata, !is.na(age)) #Keeping only non-missing records

count(newdata$Name)


# 6. Sorting
#====================

x = sample(1:50)
x = sort(x, decreasing = TRUE) #Sorting a vector

mydata = data.frame(Gender = ifelse(sign(rnorm(25))==-1,'F','M'), SAT= sample(1:25)) #Sorting a data frame

mydata.sorted <- mydata[order(mydata$Gender),] #Sort gender variable in ascending order

mydata.sorted1 <- mydata[order(mydata$Gender, -mydata$SAT),]

#Note : "-" sign before mydata$SAT tells R to sort SAT variable in descending order


mydata = data.frame(score = ifelse(sign(rnorm(25))==-1,1,2),experience= sample(1:25))

#Task : You need to sort score variable on ascending order and then sort experience variable on descending order.
#R Base Method
mydata1 <- mydata[order(mydata$score, -mydata$experience),]

#With dplyr package
mydata1 = arrange(mydata, score, desc(experience))


#Difference between sort(), rank() and order() functions?
#---------------------------------------------------------
#The sort() function is used to sort a 1 dimension vector or a single variable of data.
#The rank() function returns the ranking of each value.
#The order() function returns the indices that can be used to sort the data.

set.seed(1234)
x = sample(1:50, 10)  
x
sort(x)
rank(x)
order(x)
x[order(x)]


#7. Value labeling (donot understand)
#===================================

#Use factor() for nominal data
mydata$Gender <- factor(mydata$Gender, levels = c(1,2), labels = c("M", "F"))


#Use ordered() for ordinal data

mydata$var2 <- ordered(mydata$var2, levels = c(1,2,3,4),
                       labels = c("Strongly agree", "Somewhat agree", 
                                  "Somewhat disagree", "Strongly disagree"),replace = T)


#8. Aggregate by groups(Summarize Data )
#==========================================

samples = data.frame(x =c(rep(1:10)), y=round((rnorm(10))))

x <- aggregate(x~y, samples, mean, na.rm = TRUE) #calculates mean for variable "x" by grouped variable "y".


set.seed(1)
data <- data.frame(X = paste("s", sample(1:3, 15, replace = TRUE), sep = ""),
                   Y = ceiling(rnorm(15)), Z = rnorm(15), A = rnorm(15),
                   B = rnorm(15))

dat1 = aggregate(Z ~ X, data=data, FUN=mean) #Calculate Mean of Z by grouping variable X
dat2 = aggregate(Z~ X + Y, data=data, FUN=mean) #Calculate Mean of Z by 2 grouping variables
dat3 = aggregate(cbind(Y,Z)~X, data=data, FUN=mean)
dat4 = aggregate(.~X, data=data, FUN=mean)

#9. Function 'aggregate'
#==============================
head(mtcars)
#a. Use 'aggregate' on 'mtcars'. Calculate the median for each column sorted by
# the number of carburetors. Use the standard 'x', 'by' and 'FUN' arguments.

aggregate(x = mtcars, by = list(mtcars$carb), FUN = median)

levels(mtcars$carb)
class(mtcars$carb)
unique(mtcars$carb)
dim(mtcars)

#b. Calculate again the median based on 'carb', but this time use the 
#'formula-dot' notation.

aggregate(. ~ carb, data = mtcars, median)

aggregate(carb~ . , data = mtcars, median)


#Concatenate Text Based on Criteria
testDF <- data.frame(v1 = c(1,3,5,7,8,3,5,NA,4,5,7,9),
                     v2 = c(11,33,55,77,88,33,55,25,44,55,77,99) )
aggregate(v2 ~ v1, data = testDF, FUN=paste, sep=",")


#10. Modulo division in a matrix
#================================

#a. Get the object 'mymatrix' as below

mymatrix = matrix(data = c(6,34,923,5,0, 112:116, 5,9,34,76,2, 545:549), nrow = 5)
mymatrix

#b. Use 'apply' to perform a modulo division by 10 on each value of the matrix.
#The new matrix contains the rest of the modulo division.

apply(mymatrix, c(1,2), function(x) x%%10)



#How to calculate cartesian product of two datasets
#=======================================================

#The cartesian product implies cross product of two tables (data frames). 
#For example, df1 has 5 rows and df2 has 5 rows. 
#The combined table would contain 25 rows (5*5)

comb = merge(df1,df2,by=NULL) #same as Cross Join



#With SQL Joins
install.packages("sqldf")
library(sqldf)
df9 = sqldf('select df1.*, df2.* from df1 left join df2 on df1.ID = df2.ID')





#####################################################################
######## 12 . #How to use Indexing Operators in List in R ############# 
#####################################################################

#R has main 3 indexing operators. They are as follows :

# 1. [ ] = always returns a list with a single element.

# 2. [[ ]] = returns a list element

# 3. $ = returns elements from list that have names associated with it, not necessarily same class

dat <- list(str='R', vec=c(1,2,3), bool=TRUE)

a = dat["str"]
a
class(a)

b = dat[["str"]]
b
class(b)

c = dat$str
c
class(b)


# Note : Both $ and [[ ]] works same. But it is advisable to use [[ ]] in functions and loops. 


dat[[c("Bal02","ivtable")]] #extract a list of list
dat1$Bal02$ivtable



#####################################################################
######## 13 . Keep / Drop Columns from Data Frame  ############# 
#####################################################################

#Create a data frame

mydata <- data.frame(a=letters[1:10], x=rnorm(10), y=rnorm(10), z=rnorm(10), k= rnorm(10))
mydata

drops <- c("x","z")
df = mydata[,!(names(mydata) %in% drops)] #Method I 

df = subset(mydata, select = -c(x,z) ) #Method II 


df <- mydata[ -c(1,3:4) ] #Drop columns by column index numbers


#Keep columns by their names

keeps <- c("x","z")
df = mydata[keeps] #Method I


df = subset(mydata, select = c(x,z) ) #Method II 

#------------------------
#R Base Method

df = subset(mydata, select = -c(x,y,z))

#With dplyr package 

df = select(mydata, -c(x,y,z))


#Subset columns by their name pattern 
#=======================================
mydata = read.table(text="
                    INC_A SAC_A INC_B ASD_A
                    2 1 5 12
                    3 4 2 13
                    ", header=TRUE)

mydata1 = mydata[,grepl("^INC",names(mydata))] #Keeping columns whose name starts with "INC"
mydata2 = mydata[,!grepl("^INC",names(mydata))]#Dropping columns whose name starts with "INC"
mydata12 = mydata[,grepl("_A$",names(mydata))]#Keeping columns whose name contain "_A" at the end
mydata22 = mydata[,!grepl("_A$",names(mydata))]#Dropping columns whose name contain "_A" at the end
mydata32 = mydata[,grepl("*S",names(mydata))]#Keeping columns whose name contain the letter "S"
mydata33 = mydata[,!grepl("*S",names(mydata))]#Dropping columns whose name contain the letter "S"



#R Function for Keep / Drop Column Function
#-----------------------------------------
KeepDrop = function(data,cols,newdata,drop=1) {
  # Double Quote Output Dataset Name
  t = deparse(substitute(newdata))
  
  # Drop Columns
  if(drop == 1){
    newdata = data [ , !(names(data) %in% scan(textConnection(cols), what="", sep=" "))]}
  # Keep Columns
  else {
    newdata = data [ , names(data) %in% scan(textConnection(cols), what="", sep=" ")]}
  assign(t, newdata, .GlobalEnv)
}

KeepDrop(data=mydata,cols="a x", newdata=dt, drop=0)
KeepDrop(data=mydata,cols="a x", newdata=dt, drop=1)
mydata
dt
#--------------------------

df <- data.frame(x=1:5, y=2:6, z=3:7, u=4:8)

df[ , -which(names(df) %in% c("z","u"))]

df[ , !names(df) %in% c("z","u")] 

subset(df, select=-c(z,u))
df[ , c("x","y")]
subset(df, select=c(x,y))


DF = read.table(text = "
                fruit state grade y1980 y1990 y2000
                apples Ohio   aa    500   100   55
                apples Ohio   bb      0     0   44
                apples Ohio   cc    700     0   33
                apples Ohio   dd    300    50   66
                ", sep = "", header = TRUE, stringsAsFactors = FALSE)

DF[ , !names(DF) %in% c("grade")]

library('data.table')
DT = as.data.table(DF)
DT[ , !names(DT) %in% c("grade")]
DT[ , !names(DT) %in% c("grade"), with=FALSE]

#How to remove duplicate values by a column
#===========================================

data = data.frame(y=sample(1:25, replace = TRUE), x=rnorm(25))

#R Base Method

test = subset(data, !duplicated(data[,"y"]))


#dplyr Method

test1 = distinct(data, y, .keep_all= TRUE)


##################################################################
########### packages for transposing data with R ################
##################################################################

#reshape2 and tidyr packages are most popular packages for reshaping data in R.

data <- read.table(text="X Y    Z
                   ID12   2012-06    566
                   ID1    2012-06  10239
                   ID6    2012-06    524
                   ID12   2012-07   2360
                   ID1    2012-07   13853
                   ID6    2012-07    2352
                   ID12   2012-08   3950
                   ID1    2012-08   14738
                   ID6    2012-08   4104",header=TRUE)


#In reshape2 package, there are two function for transforming long-format data to wide format.
#---------------------------------------------------------------------------------------------
#dcast function returns a data frame as output.
#acast function returns a vector, matrix or array as output.


if (!require(reshape2)){install.packages('reshape2')
  library(reshape2)
}

mydt = dcast(data,X~Y,value.var = "Z")


#More than 1 ID Variable
xx=dcast(data, Year + SemiYear ~ Product,  value.var = "Income")
#In the above code, "Year + SemiYear" are the 2 ID variables. We want "Product" variable to be moved to columns.


dcast(data, Year ~ Product, value.var = "Income")
#Warning : Aggregation function missing: defaulting to length

dcast(data, Year ~ Product, fun.aggregate = sum, value.var = "Income")



#Convert Wide Format Data to Long Format
#========================================
mydata = read.table(text= "ID setosa versicolor virginica
                    1 5.1 NA NA
                    2 4.9 NA NA
                    3 NA 7 NA
                    4 NA 6.4 NA
                    5 NA NA 6.3
                    6 NA NA 5.8
                    ", header=TRUE)


library(reshape2)
x = colnames(mydata[,-1])
t = melt(mydata,id.vars = "ID",measure.vars = x , variable.name="Species",
         value.name="Sepal.Length",na.rm = TRUE)


# How melt function works :
#--------------------------
# 1. id.vars - ID variables to keep in the final output.
# 2. measure.vars - variables to be transformed
# 3. variable.name - name of variable used to store measured variable names
# 4. value.name - name of variable used to store values


#####################################################################
########### 15. Error Handling in R   ############# 
#####################################################################
require(randomForest)

#In R, we can handle errors with try() and inherits(object-name,'try-error').

mtry <- try(tuneRF(dt[, -3], dat3[,3], ntreeTry=100, stepFactor=1.5,improve=0.01))

if (!inherits(mtry, "try-error")) {
  best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
  rf <- randomForest(ID~.,data=dt, mtry=best.m, importance=TRUE, ntree=1000)
} else {
  rf <- randomForest(ID~.,data=dt, importance=TRUE, ntree=1000)
}



#####################################################################
#########################################################################
# Calculate number of hours, days, weeks, months and years between 2 dates
#=========================================================================

dates <- as.Date(c("2015-09-02", "2016-09-05"))

difftime(dates[2], dates[1], units = "hours")
difftime(dates[2], dates[1], units = "days")
floor(difftime(dates[2], dates[1], units = "weeks"))
floor(difftime(dates[2], dates[1], units = "days")/365)

# With lubridate package
install.packages("lubridate")
library(lubridate)
interval(dates[1], dates[2]) %/% hours(1)
interval(dates[1], dates[2]) %/% days(1)
interval(dates[1], dates[2]) %/% weeks(1)
interval(dates[1], dates[2]) %/% months(1)
interval(dates[1], dates[2]) %/% years(1)

#The number of months unit is not included in the base difftime() function 
#so we can use interval() function of lubridate() package.


#How to add 3 months to a date
#-----------------------------
mydate <- as.Date("2015-09-02")
mydate + months(3)


#Extract date and time from timestamp
#-----------------------------------------

mydate <- as.POSIXlt("2015-09-27 12:02:14")
library(lubridate)
date(mydate) # Extracting date part
format(mydate, format="%H:%M:%S") # Extracting time part

#Extracting various time periods
day(mydate)
month(mydate)
year(mydate)
hour(mydate)
minute(mydate)
second(mydate)

########################################################################################
#########################################################################################












































