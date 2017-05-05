

#######################################################
################# dplyr package  #############
###############################################

#select()   	Selecting columns (variables) 
#filter() 	  Filter (subset) rows. 
#arrange() 	  Sort the data 
#summarise() 	Summarise (or aggregate) data
#mutate() 	  Creating New Variables
#join() 	    Joining data frames (tables) 	
#group_by() 	Group the data 

#merge
#bind_rows(), bind_col()
#-----------------------------------------
#Dplyr aims to provide a function for each basic verb of data manipulation:

#select() (and slice(),rename(),distict(),sample_n(),sample_frac())
#filter() (and slice())
#arrange()
#distinct()
#mutate() (and transmute())
#summarise()
#sample_n() (and sample_frac())
#--------------------------------------------
require(dplyr)
mydata = read.csv("C:\\Users\\Vamsi\\Desktop\\R.Alg\\practice\\0.R help\\sampledata.csv")

sample_n(mydata,3) #selects random rows from a data frame (or table) with 3 rows

sample_frac(mydata,0.1) #selects random 10% of rows from a data frame (or table) 

#normal sampling method
mydata1 = mydata[sample(nrow(mydata),nrow(mydata)*.5),]

dim(mydata)
x1 = distinct(mydata) #used to eliminate duplicates.
dim(x1)

#Remove Duplicate Rows based on a variable
#The .keep_all function is used to retain all other variables in the output data frame.
x2 = distinct(mydata, Index, .keep_all= TRUE)
dim(x2)

#Remove Duplicate Rows based on a multiple variable
x3 = distinct(mydata, Index, Y2010, .keep_all= TRUE)
dim(x3)

require(dplyr)
require(nycflights13)
distinct(flights, origin, dest)
dim(flights)
names(flights)

##########################
# select( ) Function
#########################
#for Selecting Variables (or Columns)
head(mydata)
mydata2 = select(mydata, Index, State:Y2008)
head(mydata2)

#The minus sign before a variable tells R to drop the variable.
mydata3 = select(mydata, -Index, -State)
head(mydata3)

mydata3 = select(mydata, -c(Index,State))
head(mydata3)

#Selecting or Dropping Variables starts with 'Y'
#-----------------------------------------------
mydata4 = select(mydata, starts_with("Y"))
head(mydata4)

mydata5 = select(mydata, -starts_with("Y"))
head(mydata5)

#starts_with(x, ignore.case = TRUE) 	Starts with a prefix
#ends_with() 	Ends with a prefix
#contains() 	Contains a literal string
#matches() 	Matches a regular expression
#num_range() 	Numerical range like x01, x02, x03.
#one_of() 	Variables in character vector.
#everything() 	All variables.


#Selecting Variables contain 'I' in their names
mydata6 = select(mydata, contains("I"))
head(mydata6)

#Reorder Variables
mydata7 = select(mydata, State, everything())
head(mydata7)

########################################
# rename( ) Function : To Rename Variables
########################################
mydata8 = rename(mydata, Index1=Index) #renaming 'Index' variable to 'Index1'.

#####################
# filter( ) Function 
#####################
dim(mydata)
# filter rows and retain only those values in which Index is equal to A.
mydata9 = filter(mydata, Index == "A")
head(mydata9)

mydata11 = filter(mydata, Index %in% c("A", "C")) #Multiple Selection Criteria
head(mydata11);dim(mydata11)

mydata12 = filter(mydata, Index %in% c("A", "C") & Y2002 >= 1300000 ) #'AND' Condition in Selection Criteria
head(mydata12);dim(mydata12)

mydata13 = filter(mydata, Index %in% c("A", "C") | Y2002 >= 1300000) #'OR' Condition in Selection Criteria
head(mydata13);dim(mydata13)

mydata14 = filter(mydata, !Index %in% c("A", "C")) #NOT Condition
head(mydata14);dim(mydata14)

#grepl function is used to search for pattern matching.
#here we are looking for records wherein column state contains 'Ar' in their name.
mydata15 = filter(mydata, grepl("Ar", State))
head(mydata15);dim(mydata15)

#---------------------------------------
require(dplyr)
library(nycflights13)
filter(flights, month == 1, day == 1)

#This is equivalent to the more verbose code in base R:
flights[flights$month == 1 & flights$day == 1, ]

filter(flights, month == 1 | month == 2)

slice(flights, 1:10) #To select rows by position, use slice()

########################
# summarise( ) Function 
#########################
#used to summarize data.
summarise(mydata, Y2015_mean = mean(Y2015), Y2015_med=median(Y2015))

summarise(flights, delay = mean(dep_delay, na.rm = TRUE))

#Summarize Multiple Variables
summarise_at(mydata, vars(Y2005, Y2006), funs(n(), mean, median))


#Summarize with Custom Functions : The dot (.) denotes each variables 
#specified in the second argument of the function

summarise_at(mydata, vars(Y2011, Y2012),
             funs(n(), missing = sum(is.na(.)), mean(., na.rm = TRUE), median(.,na.rm = TRUE)))

#Summarize all Numeric Variables==============================
numdata = mydata[sapply(mydata,is.numeric)] 

summarise_all(numdata, funs(n(),mean,median))

#Summarize Factor Variable==========================
summarise_all(mydata["Index"], funs(nlevels(.), sum(is.na(.))))


######################################
# arrange() function : to Sort the data
#######################################

#arrange(data_frame, variable(s),_to_sort)
#or
#data_frame %>% arrange(variable(s),_to_sort)

ss = arrange(mydata, Index, Y2011) #Sort Data by Multiple Variables
head(ss)

ss1 = arrange(mydata, desc(Index), Y2011)
head(ss1)

arrange(flights, year, month, day)

arrange(flights, desc(arr_delay)) #Use desc() to order a column in descending order:


##########################
#  Pipe Operator %>%
#########################
#dplyr utilizes pipe operator from another package (magrittr).

filter(data_frame, variable == value)
or
data_frame %>% filter(variable == value)


dt = sample_n(select(mydata, Index, State),10)
or 
dt = mydata %>% select(Index, State) %>% sample_n(10)


#group_by() function : Group data by categorical variable
#=======================================================
group_by(data, variables)
or
data %>% group_by(variables)

#Example 24 : Summarise Data by Categorical Variable
t = summarise_at(group_by(mydata, Index), vars(Y2011, Y2012), funs(n(), mean(., na.rm = TRUE)))
t

t = mydata %>% group_by(Index) %>%
  summarise_at(vars(Y2011:Y2015), funs(n(), mean(., na.rm = TRUE)))
t

#---------------------------------
vignette("window-functions")

#split the complete dataset into individual planes and then summarise 
#each plane by counting the number of flights (count = n()) and computing
#the average distance (dist = mean(Distance, na.rm = TRUE)) and arrival 
#delay (delay = mean(ArrDelay, na.rm = TRUE)). 

dim(flights)
head(flights)
str(flights)
length(unique(flights$tailnum))

by_tailnum <- group_by(flights, tailnum)

delay <- summarise(by_tailnum,
                   count = n(),
                   dist = mean(distance, na.rm = TRUE),
                   delay = mean(arr_delay, na.rm = TRUE))

delay1 <- filter(delay, count > 20, dist < 2000)


require(ggplot2)
ggplot(delay1, aes(dist, delay)) +
  geom_point(aes(size = count), alpha = 1/2) +
  geom_smooth() +
  scale_size_area()

#You use summarise() with aggregate functions, which take a vector of 
#values and return a single number. There are many useful examples of 
#such functions in base R like min(), max(), mean(), sum(), sd(), 
#median(), and IQR()

#dplyr provides a handful of others:
#n(): the number of observations in the current group
#n_distinct(x):the number of unique values in x.
#first(x), last(x) and nth(x, n) - these work similarly to x[1], 
#x[length(x)], and x[n] but give you more control over the result 
#if the value is missing.

#find the number of planes and the number of flights that go to each possible destination:
destinations <- group_by(flights, dest)

summarise(destinations,
          planes = n_distinct(tailnum),
          flights = n()
          )

# flights counts year month and daywise
#When you group by multiple variables, each summary peels off one level 
#of the grouping. That makes it easy to progressively roll-up a dataset:
names(flights)

daily <- group_by(flights, year, month, day)
(per_day <- summarise(daily, flights = n()))
(per_month <- summarise(per_day, flights = sum(flights)))
(per_year  <- summarise(per_month, flights = sum(flights)))


#However you need to be careful when progressively rolling up summaries 
#like this: it's ok for sums and counts, but you need to think about 
#weighting for means and variances (it's not possible to do this exactly 
#for medians).

#Chaining

a1 <- group_by(flights, year, month, day)
a2 <- select(a1, arr_delay, dep_delay)
a3 <- summarise(a2,
                arr = mean(arr_delay, na.rm = TRUE),
                dep = mean(dep_delay, na.rm = TRUE))
a4 <- filter(a3, arr > 30 | dep > 30)

#Or if you don't want to save the intermediate results, you need 
#to wrap the function calls inside each other:

filter(
  summarise(
    select(
      group_by(flights, year, month, day),
      arr_delay, dep_delay
    ),
    arr = mean(arr_delay, na.rm = TRUE),
    dep = mean(dep_delay, na.rm = TRUE)
  ),
  arr > 30 | dep > 30
)

#This is difficult to read because the order of the operations is from 
#inside to out. Thus, the arguments are a long way away from the 
#function. To get around this problem, dplyr provides the %>% operator. 
#x %>% f(y) turns into f(x, y) so you can use it to rewrite multiple 
#operations that you can read left-to-right, top-to-bottom:

flights %>%
  group_by(year, month, day) %>%
  select(arr_delay, dep_delay) %>%
  summarise(
    arr = mean(arr_delay, na.rm = TRUE),
    dep = mean(dep_delay, na.rm = TRUE)
  ) %>%
  filter(arr > 30 | dep > 30)


#########################################
#do() function : Compute within groups
#########################################

do(data_frame, expressions_to_apply_to_each_group)
#Note : The dot (.) is required to refer to a data frame.

#Example 25 : Filter Data within a Categorical Variable
#---------------------------------------------------------
#Suppose you need to pull top 2 rows from 'A', 'C' and 'I' categories of variable Index.
t = mydata %>% filter(Index %in% c("A", "C","I")) %>% group_by(Index) %>%
  do(head( . , 2))
  
t

#Example 26 : Selecting 3rd Maximum Value by Categorical Variable
#-------------------------------------------------------------------
t = mydata %>% select(Index, Y2015) %>%
  filter(Index %in% c("A", "C","I")) %>%
  group_by(Index) %>%
  do(arrange(.,desc(Y2015))) %>%  slice(3)

t

#Using Window Functions
#----------------------
#Like SQL, dplyr uses window functions that are used to subset data within a group.
# We could use min_rank() function that calculates rank in the preceding example,

t = mydata %>% select(Index, Y2015) %>%
  filter(Index %in% c("A", "C","I")) %>%
  group_by(Index) %>%
  filter(min_rank(desc(Y2015)) == 3)

t

#Example 27 : Summarize, Group and Sort Together 
#-----------------------------------------------

t = mydata %>%
  group_by(Index)%>%
  summarise(Mean_2014 = mean(Y2014, na.rm=TRUE),
            Mean_2015 = mean(Y2015, na.rm=TRUE)) %>%
  arrange(desc(Mean_2015))

t

##########################################
#mutate() function : Creates new variables
##########################################
mutate(data_frame, expression(s) )
or
data_frame %>% mutate(expression(s))

#Example 28 : Create a new variable
#-------------------------------------
mydata1 = mutate(mydata, change=Y2015/Y2014)

#Example 29 : Multiply all the variables by 1000
#------------------------------------------------
dim(mydata)
mydata11 = mutate_all(mydata, funs("new" = mydata[,3:ncol(mydata)]* 1000))

head(mydata11)
head(mydata)

#Example 30 : Calculate Rank for Variables
#------------------------------------------
mydata12 = mutate_at(mydata, vars(Y2008:Y2010), funs(Rank=min_rank(.)))

#By default, min_rank() assigns 1 to the smallest value and high number to the largest value. 
#In case, you need to assign rank 1 to the largest value of a variable, use min_rank(desc(.))

mydata13 = mutate_at(mydata, vars(Y2008:Y2010), funs(Rank=min_rank(desc(.))))


#Example 31 : Select State that generated highest income among the variable 'Index'
#------------------------------------------------------------------------------------

out = mydata %>% group_by(Index) %>% filter(min_rank(desc(Y2015)) == 1) %>%
      select(Index, Y2015)
out

#Example 32 : Cumulative Income of 'Index' variable
#-----------------------------------------------------

out2 = mydata %>% group_by(Index) %>% mutate(Total=cumsum(Y2015)) %>%
  select(Index, Y2015, Total)

out2

#####################################
#  join() function : Join two datasets
#####################################
left_join(x, y, by = )
right_join(x, y, by = )
inner_join(x, y, by = )
full_join(x, y, by = )
semi_join(x, y, by = )
anti_join(x, y, by = )

#x, y - datasets (or tables) to merge / join
#by - common variable (primary key) to join by.

#Example 33 : Common rows in both the tables
#-----------------------------------------------
df1 <- data.frame(ID = c(1, 2, 3, 4, 5),w = c('a', 'b', 'c', 'd', 'e'),
                  x = c(1, 1, 0, 0, 1),y=rnorm(5),z=letters[1:5])
                  
df2 <- data.frame(ID = c(1, 7, 3, 6, 8),a = c('z', 'b', 'k', 'd', 'l'),
                  b = c(1, 2, 3, 0, 4),c =rnorm(5),d =letters[2:6])  
                  
                                  

df3 = inner_join(df1, df2, by = "ID")
#If the primary key does not have same name in both the tables, try the following way
inner_join(df1, df2, by = c("ID"="ID1"))


df4 = left_join(df1, df2, by = "ID")
df5 = right_join(df1, df2, by = "ID")

df6 = full_join(df1, df2, by = "ID")
df7 = semi_join(df1, df2, by = "ID")

df8 = inner_join(df1, df2, by = "ID")
df9 = anti_join(df1, df2, by = "ID")

##############################
#  Combine Data Vertically
#############################
intersect(x, y) #Rows that appear in both x and y.
union(x, y) #Rows that appear in either or both x and y.
setdiff(x, y) #Rows that appear in x but not y.

#Example 35 : Applying INTERSECT
#--------------------------------
mtcars$model <- rownames(mtcars)
first <- mtcars[1:20, ]
second <- mtcars[10:32, ]

intersect(first, second) #INTERSECT selects unique rows that are common to both the data frames.

#Example 36 : Applying UNION
#----------------------------
#UNION displays all rows from both the tables and removes duplicate records
#By using union_all function, it allows duplicate rows in the combined dataset.

union(first, second)
union_all(first, second)

#Example 37 : Rows appear in one table but not in other table
#-------------------------------------------------------------
setdiff(first, second)

#Example 38 : IF ELSE Statement
#------------------------------
if_else(condition, true, false, missing = NULL)

df <- c(-10,2, NA)
if_else(df < 0, "negative", "positive", missing = "missing value")


#Create a new variable with IF_ELSE
#--------------------------------------
df =data.frame(x = c(1,5,6,NA))
df$newvar = if_else(df$x<5, df$x+1, df$x+2,0)


#Example 39 :  Apply ROW WISE Operation 
#------------------------------------------
#Suppose you want to find maximum value in each row of variables 2012, 2013, 2014, 2015. 
#The rowwise() function allows you to apply functions to rows
df = mydata %>%
  rowwise() %>% mutate(Max= max(Y2012:Y2015)) %>%
  select(Y2012:Y2015,Max)


#Example 40 : Combine Data Frames
#----------------------------------
df1=data.frame(ID = 1:6,  x=letters[1:6])
df2=data.frame(ID = 7:12, x=letters[7:12])

xy = bind_rows(df1,df2)
#or
xy = rbind(df1,df2)

xy = bind_cols(x,y)
#or
xy = cbind(x,y)


#Example 41 : Calculate Percentile Values
#----------------------------------------
#The quantile() function is used to determine Nth percentile value

mydata %>% group_by(Index) %>%
  summarise(Pecentile_25=quantile(Y2015, probs=0.25),
            Pecentile_50=quantile(Y2015, probs=0.5),
            Pecentile_75=quantile(Y2015, probs=0.75),
            Pecentile_99=quantile(Y2015, probs=0.99))

#The ntile() function is used to divide the data into N bins.

x= data.frame(N= 1:10)
x = mutate(x, pos = ntile(x$N,5))


#Example 42 : Automate Model Building
#--------------------------------------

length(unique(mtcars$cyl))

by_cyl <- group_by(mtcars, cyl)

models <- by_cyl %>% do(mod = lm(mpg ~ disp, data = .))

summarise(models, rsq = summary(mod)$r.squared)
models %>% do(data.frame(
  var = names(coef(.$mod)),
  coef(summary(.$mod)))
)


###########################################################################
######################################################
########## (Tidyr package) for data tidying: #########
######################################################
#gather() takes multiple columns, and gathers them into key-value pairs: it makes "wide" data longer
#spread() takes two columns (key & value) and spreads in to multiple columns, it makes "long" data wider

#unite() combines multiple columns into a single column
#separate() splits a single column into multiple columns

#gather() function: converts wide data to longer format
#------------------------------------------------------
# It is analogous to the "melt" function from reshape2

gather(data, key, value, ..., na.rm = FALSE, convert = FALSE)

data %>% gather(key, value, ..., na.rm = FALSE, convert = FALSE)

#if you do not supply arguments for na.rm or convert values then the defaults are used
library(tidyr)
library(dplyr)
head(mtcars)

mtcars$car <- rownames(mtcars)
mtcars <- mtcars[, c(12, 1:11)]

mtcarsNew <- mtcars %>% gather(attribute, value, -car)
head(mtcarsNew)
tail(mtcarsNew)
dim(mtcarsNew)
dim(mtcars)
names(mtcars)

#combine wide to long from "mpg:gear" variables only
mtcarsNew <- mtcars %>% gather(attribute, value, mpg:gear)
head(mtcarsNew)

#spread - converts long data to wider format. 
#----------------------------------------------
#It is analogous to the "cast" function from reshape2.
spread(data, key, value, fill = NA, convert = FALSE, drop = TRUE)

mtcarsSpread <- mtcarsNew %>% spread(attribute, value)
head(mtcarsSpread)
dim(mtcarsSpread)


#unite - combines two or more columns into a single column.
#----------------------------------------------------------

unite(data, col, ..., sep = "_", remove = TRUE)

#where ... represents the columns to unite and col represents the column to add.

#Let us create some fake data:
  
set.seed(1)
date <- as.Date('2016-01-01') + 0:14
hour <- sample(1:24, 15)
min <- sample(1:60, 15)
second <- sample(1:60, 15)
event <- sample(letters, 15)
data <- data.frame(date, hour, min, second, event)
data

#Now, let us combine the date, hour, min, and second columns into
#a new column called datetime. Usually, datetime in R is of 
#the form Year-Month-Day Hour:Min:Second.

dataNew <- data %>% 
            unite(datehour, date, hour, sep = ' ') %>%
            unite(datetime, datehour, min, second, sep = ':')
dataNew


#separate - splits one column into two or more columns.
#-----------------------------------------------------
separate(data, col, into, sep = "[^[:alnum:]]+", remove = TRUE,
         convert = FALSE, extra = "warn", fill = "warn", ...)


separate(data, col, into, sep = " ", remove = TRUE, convert = FALSE)

data %>% separate(col, into, sep = " ", remove = TRUE, convert = FALSE)


data1 <- dataNew %>% 
  separate(datetime, c('date', 'time'), sep = ' ') %>% 
  separate(time, c('hour', 'min', 'second'), sep = ':')
data1

####################
# unnest from tidyr
####################
mygenes = read.table(text= "Entrez  symbols

7841    MOGS,CDG2B,CWH41,DER7,GCS1
4248    MGAT3,GNT-III,GNT3
5728    PTEN,BZS,CWS1,DEC,GLM2,MHAM,MMAC11,TEP1
", header=TRUE)

library(tidyr)
library(dplyr)
mygenes %>% 
  mutate(symbols=strsplit(as.character(symbols), ",")) %>% 
  unnest(symbols)


mygenes %>% 
  mutate(symbols=strsplit(as.character(symbols), ",")) %>% 
  unnest(symbols) %>% 
  subset(symbols %in% c("DER7", "DEC"))


library(tidyr)
df <- data.frame(x = c(NA, "a.b", "a.d", "b.c"))
df %>% separate(x, c("A", "B"))

########################################################################
####################################################
########### reshape2 for transposing data ##########
####################################################

if (!require(reshape2)){install.packages('reshape2')
  library(reshape2)
}
require(reshape2)

#(reshape2) Convert Wide Format Data to Long Format
#---------------------------------------------------
mydata = read.table(text= "ID setosa versicolor virginica
                    1 5.1 NA NA
                    2 4.9 NA NA
                    3 NA 7 NA
                    4 NA 6.4 NA
                    5 NA NA 6.3
                    6 NA NA 5.8
                    ", header=TRUE)

x = colnames(mydata[,-1])

t = melt(mydata,id.vars = "ID",measure.vars = x , variable.name="Species",
         value.name="Sepal.Length",na.rm = TRUE)

tt1= melt(mtcars,id.var = "car",measure.var = colnames(mtcars[,-1]),
         variable.name = "vamsi",value.name = "values",na.rm = TRUE)

tt1

dcast(tt1, car~vamsi, value.var = "values")


# How melt function works :
#--------------------------
# 1. id.vars - ID variables to keep in the final output.
# 2. measure.vars - variables to be transformed
# 3. variable.name - name of variable used to store measured variable names
# 4. value.name - name of variable used to store values


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


#(reshape2) Convert long-format data to wide format.
#--------------------------------------------------
#dcast function returns a data frame as output.
#acast function returns a vector, matrix or array as output.

mydt = dcast(data,X~Y,value.var = "Z")

#More than 1 ID Variable
xx=dcast(data, Year + SemiYear ~ Product,  value.var = "Income")
#In the above code, "Year + SemiYear" are the 2 ID variables. We want "Product" variable to be moved to columns.


dcast(data, Year ~ Product, value.var = "Income")
#Warning : Aggregation function missing: defaulting to length

dcast(data, Year ~ Product, fun.aggregate = sum, value.var = "Income")


########################################################################
####################################################
########### Psych Package ##########
####################################################
require(psych)

sapply(iris, class)
 
Transformed <- transform(mydata, height = as.integer(height), weight = as.integer(weight))

describe(iris)

describe.by(iris,iris$Species)

describeBy(iris,iris$Species)


########################################################################
####################################################
########### Lubridate Package ##########
####################################################
#this packages is frequently used with data comprising of timely data.

library(lubridate)

#current date and time
now()
n_time <- now()

#using update function
n_update <- update(n_time, year = 2013, month = 10)
n_update  

#add days, months, year, seconds
d_time <- now()
d_time + ddays(1)
d_time + dweeks(2)
d_time + dyears(3)
d_time + dhours(2)
d_time + dminutes(50)
d_time + dseconds(60)

#extract date,time
n_time$hour <- hour(now())
n_time$minute <- minute(now())
n_time$second <- second(now())
n_time$day <- day(now())
n_time$month <- month(now())
n_time$year <- year(now())

#check the extracted dates in separate columns
#----------------------------------------------
new_data <- data.frame(n_time$hour, n_time$minute, n_time$second,n_time$day,n_time$month, n_time$year)
new_data

ymd("20110604")
mdy("06-04-2011")
dmy("04/06/2011")

#Time Zones
#----------
arrive <- ymd_hms("2011-06-04 12:00:00", tz = "Pacific/Auckland")
arrive

leave <- ymd_hms("2011-08-10 14:00:00", tz = "Pacific/Auckland")
leave

#Setting and Extracting information
#----------------------------------
second(arrive)
second(arrive) <- 25
arrive
second(arrive) <- 0
wday(arrive)
wday(arrive, label = TRUE)

#Time Intervals
auckland <- interval(arrive, leave)
auckland <- arrive %--% leave



########################################################################
####################################################
########### data.table package ##########
####################################################

data("airquality")
mydata <- airquality
head(airquality,6)

data(iris)
myiris <- iris

library(data.table)
mydata <- data.table(mydata)
mydata

myiris <- data.table(myiris)
myiris

#subset rows - select 2nd to 4th row
mydata[2:4,]

#select columns with particular values
myiris[Species == 'setosa']

myiris[Species %in% c('setosa', 'virginica')]

#select columns. Returns a vector
mydata[,Temp]
mydata[,.(Temp,Month)]
mydata[,sum(Ozone, na.rm = TRUE)] #returns sum of selected column
mydata[,.(sum(Ozone, na.rm = TRUE), sd(Ozone, na.rm = TRUE))]

myiris[,{print(Sepal.Length)
plot(Sepal.Width)
  NULL}]


#grouping by a variable
myiris[,.(sepalsum = sum(Sepal.Length)), by=Species]

#select a column for computation, hence need to set the key on column
setkey(myiris, Species)

#selects all the rows associated with this data point
myiris['setosa']
myiris[c('setosa', 'virginica')]


########################################################################
####################################################
##### Stringer,TM,wordcloud,snowball package ######
####################################################
#str_pad and str_trim

require(stringr)
myzipcodevector = seq(500084:500091)

str_pad(myzipcodevector, 5, "left", "0")









###############################################################
# base Character Functions
#-------------------------

#In R, strings are stored in a character vector. You can create strings with a single quote / double quote.

x = "I love R Programming"
x
class(x)

Y = as.character(25) #25 is stored as a character
class(Y) 

x = "I love R Programming"
is.character(x) #check whether a vector is a character or not


#1. Concatenate Strings : str_c(x,y)
#============================

paste (objects, sep = " ", collapse = NULL)

#The sep= keyword denotes a separator or delimiter. The default separator is a single space. 
#The collapse= keyword is used to separate the results

x = "Deepanshu"
y ="Bhalla"

paste(x,y) #base pkg
str_c(x,y) #Stringr pkg

paste(x, y, sep= ",")
str_c(x,y,sep = " ")

#To create column names from x1 through x10
paste("x", seq(1,10), sep = "")

str_c("x", 1:10, sep = "")

#Use of 'Collapse' keyword
paste("x", seq(1,10), sep="", collapse=", ")

str_c("x", 1:10, sep = "", collapse=", ")


#2. length of a character value : str_length(x)
#================================
x = "I love R Programming"

nchar(x) #Base pkg 

str_length(x) #Stringr pkg


#3. Extract or replace substrings : str_sub(x,1,4)
#================================================

#The substr() function is used to extract strings in a character vector
#substr(character_vector, starting_position, end_position)

x = "abcdef"

substr(x,1,4) #Base pkg

str_sub(x,1,4)#Stringr pkg


#Replace Substring - substr(x, starting position, end position) = Value
substr(x, 1, 2) = "66"  #Replace Substring


substr("abcdef", 2, 4)

substring("abcdef", 1:6, 1:6)
## strsplit is more efficient ...

substr(rep("abcdef", 4), 1:4, 4:5)

x <- c("asfef", "qwerty", "yuiop[", "b", "stuff.blah.yech")
substr(x, 2, 5)

substring(x, 5)

substring(x, 2, 4:6)

substring(x, 2) <- c("..", "+++")
x

#--------------
unlist(strsplit("a.b.c", "."))
unlist(strsplit("a.b.c", "[.]"))

noquote(strsplit("A text I want to display with spaces", NULL)[[1]])

x <- c(as = "asfef", qu = "qwerty", "yuiop[", "b", "stuff.blah.yech")
# split x on the letter e
strsplit(x, "e")
#---------------


#4. Repeat the character N times
#=======================================

strrep("vamsi",10) #base pkg

str_dup("vamsi",10) #Stringr pkg


#5. Remove Leading and Trailing Spaces
#=========================================
x= " deepanshu    bhalla "

trimws(x) #base pkg

str_trim(x) #Stringr pkg
 
#Converting Multiple Spaces to a Single Space
#------------------------------
x1 = 'Here is  some   text   I      wrote   '

Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre1.8.0_101')

library(qdap)

Trim(clean(x1)) #base pkg & qdap pkg

str_trim(clean(x1)) #Stringr pkg & qdap pkg

library(qdapRegex)
rm_white(mystring) 


#pad a string with extra whitespace on the left, right, or both sides.
#--------------------------------
str_pad(x1,1)


#6. Extract Word from a String
#=====================================

#to pull a first or last word from a character string.
#word(string, position of word to extract, separator) 

x = "I love R Programming"

word(x,4) #Stringr pkg

word(x,-3,sep = " ")

#sep=" " denotes a single space as a delimiter (It's the default delimiter in the word function)


#How to extract last name from full name
#----------------

dt2 = read.table(text="var
                 Sandy,Jones
                 Dave,Jon,Jhonson
                 ", header=TRUE)

#word() function of stringr package is used to extract or scan word 
#from a string. -1 in the second parameter  denotes the last word.

dt2$var2 = word(dt2$var, -1, sep = ",") #Stringr pkg
word("vamsi krishna . reddy", -1, sep = "[.]")

#7. Convert to Uppercase / Lowercase /Propercase
#============================================================

x = "I love R Programming"

tolower(x)
toupper(x)

str_to_lower(x)
str_to_upper(x)
str_to_title(x) #Stringr pkg


################ Pattern matching ################
#===================================================

#Pattern matching functions detect, locate, exract, match, replace, split strings


library(stringr)
strings <- c(" 219 733 8965", "329-293-8753 ", "banana", "595 794 7569",
             "387 287 6718", "apple", "233.398.9187  ", "482 952 3315", "239 923 8115",
             "842 566 4692", "Work: 579-499-7527", "$1000", "Home: 543.355.3679")
phone <- "([2-9][0-9]{2})[- .]([0-9]{3})[- .]([0-9]{4})"

# Which strings contain phone numbers?
str_detect(strings, phone)
strings[str_detect(strings, phone)]

# Where in the string is the phone number located?
loc <- str_locate(strings, phone)
loc

# Extract just the phone numbers
str_sub(strings, loc[, "start"], loc[, "end"])

# Or more conveniently:
str_extract(strings, phone)

# Pull out the three components of the match
str_match(strings, phone)

# Anonymise the data
str_replace(strings, phone, "XXX-XXX-XXXX")


#------------------------------------------------------
#Figure  1:   Simple  string  matching  functions  for  processing  a 
#character  vector  containing  phone  numbers (among other things).

library(stringr)
col2hex <- function(col) {
  rgb <- col2rgb(col)
  rgb(rgb["red", ], rgb["green", ], rgb["blue", ], max = 255)
}

# Goal replace colour names in a string with their hex equivalent
strings <- c("Roses are red, violets are blue", "My favourite colour is green")
colours <- str_c("\\b", colors(), "\\b", collapse="|")

# This gets us the colours, but we have no way of replacing them
str_extract_all(strings, colours)

# Instead, let's work with locations
locs <- str_locate_all(strings, colours)

sapply(seq_along(strings), function(i) {
string <- strings[i]
loc <- locs[[i]]
# Convert colours to hex and replace
hex <- col2hex(str_sub(string, loc[, "start"], loc[, "end"]))
str_sub(string, loc[, "start"], loc[, "end"]) <- hex
string
})
#------------------------------------------------------------

#Pattern matching in "base pkg"
#---------------------------
grep(pattern, text, ignore.case=FALSE,perl = FALSE,value = FALSE, fixed=TRUE, useBytes = FALSE, invert = FALSE)
grep(pattern, x , ignore.case=FALSE, fixed=FALSE) 
#Search for pattern in x. 
#If fixed =FALSE then pattern is a regular expression. 
#If fixed=TRUE then pattern is a text string. Returns matching indices.


#grep(), grepl(), regexpr(), gregexpr(), regexec() search for matches to 
#argument within each element of character vector

#regexpr function is used to identify where a pattern is within a character vector, where each element is searched separately and returns vector
#gregexpr function is used to identify where a pattern is within a character vector, where each element is searched separately and its returned object is a list 

grep("A", c("b","A","c"),ignore.case=FALSE, fixed=TRUE) 


x = c("Deepanshu", "Dave", "Sandy", "drahim", "Jades")

x[grepl("^D",x)] #Keeping characters starts with the letter 'D'

#Note : It does not return 'drahim' as pattern mentioned above is case-sensitive

x[grepl("(?i)^d",x)] #To make it case-insensitive, we can add (?i) before ^D.

x[!grepl("(?i)^d",x)] #Keeping characters do not start with the letter 'D'

x[grepl("s$",x)] #Keeping characters end with 'S'

x[grepl("(?i)*s",x)] #Keeping characters contain "S"


#--------------------
URL    <- "http://rfunction.com/code/1202/BarackObamaTweets.txt"
tweets <- read.delim(URL)
tweets <- as.character(tweets[,1])

these <- grep("[Rr]omney", tweets)
#---------------------


#sub, gsub performs the replacement of first and all matches resp.
#----------------------------------------------------------------
sub(pattern, replacement, x, ignore.case =FALSE, fixed=FALSE) 
#Find pattern in x and replace with replacement text. 
#If fixed=FALSE then pattern is a regular expression.
#If fixed = T then pattern is a text string.

sub("okay", "fine", "She is okay.")
sub("\\s", ".", "Hello There")  
sub("[a:e]", ".", "Hello There")  
gsub("[^a:e]", ".", "Hello There")  

cols = c("x1", "x2", "x3")
sub("x", "Year", cols)







#12. Splitting a Character Vector(Split a data frame )
#======================================================
#In case of text mining. it is required to split a string to calculate the most frequently used keywords in the list

x = c("I love R Programming")
strsplit(x, " ")

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


mydt2 = split(data, data$Y) #Split a data frame

mydt2[[1]] #Get first list element

sapply(mydt2 , function(x) mean(x$Z))#Calculate mean on each list element


#Split a list into multiple data frames
for(i in 1:length(mydt2)) {
  assign(paste0("t.", i), mydt2[[i]])
}


#4. String Formatting
#============================

#Suppose the value is stored in fraction and you need to convert it to percent.
#The sprintf is used to perform C-style string formatting.

sprintf(fmt, ...)
#he keyword fmt denotes string format. The format starts with the symbol % followed by numbers and letters.

x = 0.25
sprintf("%.0f%%",x*100) #convert "X" to percent. 
sprintf("%.2f%%",x*100) #2desimals


a = seq(1, 5)
sprintf("x%03d", a) #'d' in the format is used for numeric value.

sprintf("%s has %d rupees", "Ram", 500) #'s' in the format is used for character string.


#13. Selecting Multiple Values
#================================

x = sample(LETTERS,100, replace = TRUE)
x[x %in% c("A","B","C")]






















