
sessionInfo()#description of the version of R and its attached packages used in the current session

q() # to quit R

getwd()# print the current working directory 

setwd()# change to mydirectory

help("options")## learn about available options 

options()# view current option settings

options(digits=3) # number of digits to print on output

install.packages("dplyr") #typically downloads the package  from  CRAN and installs it for use

install.packages("dplyr", repos="http://lib.stat.cmu.edu/R/CRAN") #Installing Packagesfrom any Repository

#How to remove all the objects
#================================

rm(list=ls())

#Install R Package Directly From GitHub
#============================================

#Step I : Install and load devtools package

install.packages("devtools")
library(devtools)

#Step II : Install Package from GitHub

install_github("tomasgreif/woe")

#----------------------------------------------

remove.packages(pkgs, lib, version) # to remove package

.libPaths() # to get the library path

search() #to see a list of packages that are currently attached to the system

library() #to list all the available libraries on your system with a short description. Run the function without any arguments

library(dplyr) # or require(dplyr) #Loading Packages

ls(pos="package:stats") #Exploring a Package

help.start()	# general help

help(foo)	# help about function foo

help("train") #help about function train

?foo	# same thing

apropos("foo") # list all function containing string foo

example(foo)	# show an example of function foo

getwd() ## just checking what the current working directory is

save.image() ## save to the current working directory 

save.image("C:\\Program Files\\R\\R-2.5.0\\bin\\.RData") ## save to a specific file and location

ls() #Listing workspace objects in your current R session
ls(pattern="x") #To list all objects starting with the letter   x:

#.If you assign a value to an object that already exists then the contents of the object will be overwritten with  the new value (without a warning!).

rm(x, x2) #to remove one or more objects   from your session.

history()	#display last 25 commands 

history(max.show=Inf) #display  all previous commands

# save your command history
savehistory(file="myfile")	# default is ".Rhistory"

# recall your command history
loadhistory(file="myfile")  # default is ".Rhistory"

data( ) #to see the available datasets in R. The results will depend on which packages you have loaded.

help(datasetname) #for details on a sample dataset

#-------------------------------------------------

#To update R to latest version
install.packages("installr")
library(installr)
updateR()

#---------------------------------------------------------------------------

#resinstalling packages in R after update

#--run in the old version of R 
setwd("C:/R packages/")
setwd("C:/Temp/")
packages <- installed.packages()[,"Package"] 
save(packages, file="Rpackages") 

#INSTALL NEW R VERSION

#--run in the new version 
setwd("C:/R packages/")
setwd("C:/Temp/") 
load("Rpackages") 
for (p in setdiff(packages, installed.packages()[,"Package"])) 
  install.packages(p) 

#----------------------------------------------------------------------------
#updatng all packages

all.packages <- installed.packages()
r.version <- paste(version[['major']], '.', version[['minor']], sep = '')

for (i in 1:nrow(all.packages))
{
  package.name <- all.packages[i, 1]
  package.version <- all.packages[i, 3]
  if (package.version != r.version)
  {
    print(paste('Installing', package.name))
    install.packages(package.name)
  }
}

#---------------------------------------------------

# get the latest installr package:
if (!require('devtools')) install.packages('devtools'); require('devtools')
install_github('installr', 'talgalili')
require(installr)

# read the data (this will take a LOOOONG time)
RStudio_CRAN_data_folder (0)
mode(package_ip_id) <- "numeric"
dend_package_ip_id

#------------------------------------------------------------

#As an interesting fact, you can also create a matrix from a vector.

age <- c(23, 44, 15, 12, 31, 16)
age

dim(age) <- c(2,3)
age
class(age)

#You can also join two vectors using cbind() and rbind() functions. But, make sure that both vectors have same number of elements. If not, it will return NA values.

x <- c(1, 2, 3, 4, 5, 6)
y <- c(20, 30, 40, 50, 60)
cbind(x, y)
rbind(x, y)

#How to save everything in R session
#=====================================

save.image(file="dt.RData")

######################################################################
# R Tuto-1.R
##############
#The c function is widely used to combine values to form a vector.
x=c(1,2,3)
x

#R uses NA to represent Not Available, or missing values
x=c(1,2,3,NA)
sum(x)

#To calculate sum excluding NA, use na.rm =  TRUE (By default, it is FALSE).
sum(x,na.rm = TRUE)

#R is case-sensitive, so you have to use the exact case that the program requires.

# Object names in R can be any length consisting of letters, numbers, underscores ''_'' or the period ''.''

#Object names in R should begin with a letter.

#Unlike SAS and SPSS, R has several different data structures including vectors, factors, data frames, matrices, arrays, and lists. The data frame is most like a dataset in SAS.


################################################
#Editing functions in R
############################################


#You can use fix() function and give the name of an existing function, 
#R shows you the code for that function in a NotePad window and you can 
#type whatever you like.
fix(x)

#Retrieve your previous command : You can retrieve it with the UP arrow key and edit it to run again.

#install.packages("sas7bdat")
#library("sas7bdat") 
save.image("mywork.RData") 
load("mywork.RData")


#To tell R which data set to use : attach(mydata)


############################################
#R : Create Dummy Data
############################################


#Method 1 : Enter Data Manually

df1 <- data.frame(ID = c(1, 2, 3, 4, 5),
                  w = c('a', 'b', 'c', 'd', 'e'),
                  x = c(1, 1, 0, 0, 1))

#Method 2 : Sequence of numbers, letters, months and random numbers
seq(1, 16, by=2)
month.abb[1:8] 
sample(10:20, 8, replace = TRUE)
LETTERS[1:8]
letters[1:8]
df2 <- data.frame(a = seq(1,16,by=2), b = LETTERS[1:8], x= month.abb[1:8], y = sample(10:20,8, replace = TRUE), z=letters[1:8])


#Method 3 : Create numeric grouping variable
df3 = data.frame(X = sample(1:3, 15, replace = TRUE))


#Method 4 : Random Numbers with mean 0 and std. dev 1
set.seed(1)
df4 <- data.frame(Y = rnorm(15), Z = ceiling(rnorm(15)))

x=rnorm(10)
z=ceiling(rnorm(x))
mean(z)
sd(z)

#Method 5 : Create binary variable (0/1)
set.seed(1)
ifelse(sign(rnorm(15))==-1,0,1)

#In the code above, if sign of a random number is negative, it returns 0. Otherwise, 1.


#Method 6 : Copy Data from Excel to R
#It creates 3 columns - X, Y and Z. The header = TRUE tells R to consider first row as header.
data = read.table(text = "x	y	z
                  1	s	t
                  2	d	t
                  3	r	t
                  1	h	r
                  2	v	r
                  5	j	w
                  8	t	w
                  ", header = TRUE)
data
names(data)
rownames(data)


#Method 7: Create character grouping variable
mydata = sample(LETTERS[1:5],16,replace = TRUE)
mydata

xx = model.matrix(~mydata)[,-1]
xx

new = model.matrix(~ Species, data=data.frame(iris))
head(new)

diet <- factor(c(1,1,1,1,2,2,2,2))
sex <- factor(c("f","f","m","m","f","f","m","m"))
model.matrix(~ diet + sex)

model.matrix(~ diet + sex + diet:sex)
model.matrix(~ diet*sex)


#Create Dummy Columns From Categorical Variable 
#==============================================

DF <- data.frame(strcol = c("A", "A", "B", "F", "C", "G", "C", "D", "E", "F"))

for(level in unique(DF$strcol)){
  DF[paste("strcol", level, sep = "_")] <- ifelse(DF$strcol == level, 1, 0)}


################################################################
################# 5 . Importing Data into R ###################
#===============================================================

#Reading large CSV File with R 
#================================

#Method I : Using data.table library 
library(data.table)
yyy = fread("C:\\Users\\Deepanshu\\Documents\\Testing.csv", header = TRUE)


#Method II : Using bigmemory library
library(bigmemory)
y <- read.big.matrix("C:\\Users\\Deepanshu\\Documents\\Testing.csv", type = "integer", header=TRUE)
dim(y)
#coerce a big.matrix to a matrix
yy= as.matrix(y)


#1. Reading a comma-delimited text file (CSV)
#=============================================

#mydata <- read.csv("c:/mydata.csv")

#Important Note : BIG CSV Files should be imported with fread function of data.table.

#library(data.table)
#mydata = fread("c:/mydata.csv")


#If you have the names (headers) in the first row
#mydata <- read.csv("c:/mydata.csv", header=TRUE)


#If you want to set any value to a missing value
#mydata <- read.csv("c:/mydata.csv", header=TRUE, na.strings="."))


#If you want to set multiple values to missing values.
#mydata <- read.csv("c:/mydata.csv", header=TRUE, na.strings=  c("A" , "B" ))



#2. Reading a tab-delimited text file
#====================================

#If you don't have the names (headers) in the first row
#mydata <- read.table("c:/mydata.txt")


#If you have the names (headers) in the first row
#mydata <- read.table("c:/mydata.txt", header=TRUE)


#If you want to set any value to a missing value
#mydata <- read.table("c:/mydata.txt", header=TRUE, na.strings="."))



#3. Reading Excel File
#========================

#The best way to read an Excel file is to save it to a CSV format and import it using the CSV method 
#mydata <- read.csv("c:/mydata.csv", header=TRUE)


#Step 1 : install.packages("readxl")

#Step 2 : Define path and sheet name in the code below

#library(readxl)
#read_excel("my-old-spreadsheet.xls")
#read_excel("my-new-spreadsheet.xlsx")



# Specify sheet with a number or name
#read_excel("my-spreadsheet.xls", sheet = "data")
#read_excel("my-spreadsheet.xls", sheet = 2)



# If NAs are represented by something other than blank cells,
# set the na argument
#read_excel("my-spreadsheet.xls", na = "NA") 



# 4. Reading SAS File
#========================

#Step 1 : Install the package once
#install.packages("haven")


#Step 2 : Define path in the code below
#library("haven")
#read_sas("c:/mydata.sas7bdat")


# 5. Reading SPSS File
#========================

#Step 1 : Install the package once
#install.packages("haven")

#Step 2 : Define path in the code below
#library("haven")
#read_spss("c:/mydata.sav")


#6. Load Data from R
#========================

#load("mydata.RData")



#####################################################################
################# 6 . Exporting Data in R ###########################
#####################################################################


#1. Writing comma-delimited text file (CSV)
#=========================================

#write.csv(mydata,"C:/Users/Deepanshu/Desktop/test.csv")



#2. Writing tab-delimited text file
#==================================

#write.table(mydata, "C:/Users/Deepanshu/Desktop/test.txt", sep="\t")



#3. Writing Excel File
#========================

#Step 1 : install.packages("xlsReadWrite")


# Step 2 : Define path and sheet name in the code below
#library(xlsReadWrite)
#write.xls(mydata, "c:/mydata.xls")



# 4. Writing SAS File
#========================

#Step 1 : install.packages("foreign")


#Step 2 : Define path in the code below
#library(foreign)
#write.foreign(mydata, "c:/mydata.txt", "c:/mydata.sas",   package="SAS")



# 5. Writing SPSS File
#========================

#Step 1 : install.packages("foreign")


#Step 2 : Define path in the code below

#library(foreign)
#write.foreign(mydata, "c:/mydata.txt", "c:/mydata.sps",   package="SPSS")




#####################################################################
#### 7 . Reading and Saving data file in R session  ############## all doubt
#####################################################################
mydata = c(1:20)

#Suppose you want to save an individual object in R and read it later.
#Saving data file in R session
#saveRDS(mydata, "logistic.rds")

#Reading stored data from R session.
#mydata = readRDS("logistic.rds")

#Another way : Saving data file in R session
# save(mydata,file="E:\\logistic.rdata")

#Loading stored data from R session
#load("E:\\logistic.rdata", ex <- new.env())
ls(ex)

#Saving multiple objects in R session
#save(mydata, data2, file="1.RData")


#Saving everything in R session
#save.image(file="1.RData")

load(file = "1.RData")

########################################################################
#A/B testing
#--------------
site1 = c(.40, 500) # pink
site2 = c(.30, 550) # black

abtestfunc <- function(ad1, ad2){
  sterror1 = sqrt( ad1[1] * (1-ad1[1]) / ad1[2] )
  sterror2 = sqrt( ad2[1] * (1-ad2[1]) / ad2[2] )
  minmax1 = c((ad1[1] - 1.28*sterror1) * 100, (ad1[1] + 1.28*sterror1) * 100)
  minmax2 = c((ad2[1] - 1.28*sterror2) * 100, (ad2[1] + 1.28*sterror2) * 100)
  print( round(minmax1,2) )
  print( round(minmax2,2) )
}

abtestfunc(site1, site2)

x <- y <- rep(0,1000)
runif(1)
rnorm(1, mean=1)
