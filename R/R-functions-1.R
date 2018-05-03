
install.packages("swirl")
packageVersion("swirl")
library(swirl)
install_course("Advanced R Programming")

swirl()

Sys.Date()

mean(c(2,4,5))


boring_function <- function(x) {
  x
}

#mean of given values
my_mean <- function(my_vector) {
  x = sum(my_vector)/length(my_vector)
  
}


#incerement a value by default value
increment <- function(number, by = 1){
  number + by
}



#remainder of the value
remainder <- function(num, divisor = 2) {
  num %% divisor
}

#if there is a way you can see a function's arguments (besides looking at the
#documentation). Thankfully, you can use the args() function! Type:args(remainder)
args(remainder)


add_two_numbers <- function(num1, num2){
      num1 + num2
   }
  

multiply_two_numbers <- function(num1, num2){
  	num1 * num2
   }
  
some_function <- function(func){
      func(2, 4)
   }

evaluate <- function(fun, dat){
  fun(dat)
}

# Functions that are not named are appropriately known as anonymous functions

evaluate(sd,c(1.4, 3.6, 7.9, 8.8))

evaluate(mean,c(1.4, 3.6, 7.9, 8.8))

evaluate(function(x){x+1},6) #first argument is a tiny anonymous function

#an anonymous function to return the first element of the vector c(8, 4, 0).
#Your anonymous function should only take one argument which should be a variable `x`.
evaluate(function(x){x[1]},c(8,4,0))


#using evaluate() along with an anonymous function to return the 
#last element of the vector c(8, 4, 0).
evaluate(function(x){x[length(x)]},c(8,4,0))


#For the rest of the course we're going to use the paste() function
paste("Programming", "is", "fun!")


simon_says <- function(...){
     paste("Simon says:", ...)
  }


telegram <- function(...){
  paste("START",...,"STOP")
  
}

telegram(vamsi)

#===================================================================
#Let's explore how to "unpack" arguments from an ellipses when you use the
# ellipses as an argument in a function. Below I have an example function that
# is supposed to add two explicitly named arguments called place,adjective,noun

mad_libs <- function(...){
  # Do your argument unpacking here!
  
  args = list(...) #called ellipsis inside of a list
  
  place = args[["place"]]
  adjective = args[["adjective"]]
  noun = args[["noun"]]
  
  # Don't modify any code below this comment.
  # Notice the variables you'll need to create in order for the code below to
  # be functional!
  paste("News from", place, "today where", adjective,
        "students took to the streets in protest of the new",
        noun, "being installed on campus.")
}


mad_libs("place"= c("v1","v2","v3"),"adjective"=c(1,2,3),"noun" = c(5a,5b,5c))

#=======================================================================

#Control structures in R allow you to control the flow of execution of a 
#series of R expressions. Basically, control structures allow you to put 
#some "logic" into your R code, rather than just always executing the
#same R code every time. Control structures allow you to respond to
#inputs or to features of the data and execute different R expressions
#accordingly.

#Commonly used control structures are
#if and else: testing a condition and acting on it
#for: execute a loop a fixed number of times
#break: break the execution of a loop
#next: skip an iteration of a loop
#Most control structures are not used in interactive sessions, but
#rather when writing functions or longer expressions

## Generate a uniform random number
x <- runif(1, 0, 10)
x

if(x > 3) {
  y <- 10
} else {
  y <- 0
}

print(y)

int_to_string(4)
gt(2, 5)
is_even(34)
square(4)
add_talk(5, 3)
paste_talk("red", "head")


numbers <- rnorm(10)
numbers

for(i in 1:10) {
  print(numbers[i])
}


x <- c("a", "b", "c", "d")

for(i in 1:4) {
  ## Print out each element of 'x'
  print(x[i]) 
}

## Generate a sequence based on length of 'x'
for(i in seq_along(x)) {   
  print(x[i])
}


for(letter in x) {
  print(letter)
}


for(i in 1:4) print(x[i])

#nested for loop

x <- matrix(1:6, 2, 3)
x
class(x)

for(i in seq_len(nrow(x))) {
  for(j in seq_len(ncol(x))) {
    print(x[i, j])
  }   
}

#next is used to skip an iteration of a loop.
for(i in 1:100) {
  if(i <= 20) {
    ## Skip the first 20 iterations
    next                 
  }
  #x = i*2
  print(i*2)
  ## Do something here
}


#break is used to exit a loop immediately, regardless of what 
#iteration the loop may be on.

for(i in 1:100) {
  
  print(i)
  
  if(i > 20) {
    ## Stop loop after 20 iterations
    break  
  }     
}


#Control structures like if-else and for allow you to control the flow of 
#an R program

#Control structures mentioned here are primarily useful for writing programs; 
#for command-line interactive work, the "apply" functions are typically 
#more useful.

#FUNCTIONS
#=========
#Functions are used to encapsulate a sequence of expressions that are 
#executed together to achieve a specific goal. A single function typically
#does "one thing well"---often taking some input and the generating output
#that can potentially be handed off to another function for further 
#processing. Drawing the lines where functions begin and end is a key
#skill for writing functions. When writing a function, it's important 
#to ask yourself what do I want to encapsulate?

#There is going to be a user who will desire the ability to modify
#certain aspects of your code to match their specific needs or 
#application. Aspects of your code that can be modified often become 
#function arguments that can be specified by the user

# When writing any function it's important to ask what will the user
#want to modify in this function? Ultimately, the answer to this
#question will lead to the function's interface

library(readr)
library(dplyr)

getwd()
setwd("C:\\Users\\vamsi\\Desktop\\projects\\practice\\data")

#--------------------------------------------------------------------
#CODE
#===========
## Download data from RStudio (if we haven't already)

if(!file.exists("2016-07-20.csv.gz")) {
  download.file("http://cran-logs.rstudio.com/2016/2016-07-20.csv.gz", 
                "2016-07-20.csv.gz")
}

cran <- read_csv("2016-07-20.csv.gz", col_types = "ccicccccci")

head(cran)

cran %>% filter(package == "filehash") %>% nrow


#--------------------------------------------------------------------
#FUCTION INTERFACE.
#====================
## pkgname: package name (character)
## date: YYYY-MM-DD format (character)
num_download <- function(pkgname, date) {
  ## Construct web URL
  year <- substr(date, 1, 4)
  src <- sprintf("http://cran-logs.rstudio.com/%s/%s.csv.gz",
                 year, date)

  ## Construct path for storing local file
  dest <- file.path("data", basename(src))
  
  ## Don't download if the file is already there!
  if(!file.exists(dest))
    download.file(src, dest, quiet = TRUE)
  
  cran <- read_csv(dest, col_types = "ccicccccci", progress = FALSE)
  cran %>% filter(package == pkgname) %>% nrow

  }


num_download("filehash", "2016-07-20")
num_download("Rcpp", "2016-07-19")

#-------------------------------------------------------------------------
#DEFAULT VALUES
#====================
num_download <- function(pkgname, date = "2016-07-20") {
  year <- substr(date, 1, 4)
  src <- sprintf("http://cran-logs.rstudio.com/%s/%s.csv.gz",
                 year, date)
  dest <- file.path("data", basename(src))
  if(!file.exists(dest))
    download.file(src, dest, quiet = TRUE)
  cran <- read_csv(dest, col_types = "ccicccccci", progress = FALSE)
  cran %>% filter(package == pkgname) %>% nrow
  
}

num_download("Rcpp")

#--------------------------------------------------------------------------
#RE-AFCTORING CODE
#==================
#Construct the path to the remote and local log file
#Download the log file (if it doesn't already exist locally)
#Read the log file into R
#Find the package and return the number of downloads

check_for_logfile <- function(date) {
  year <- substr(date, 1, 4)
  src <- sprintf("http://cran-logs.rstudio.com/%s/%s.csv.gz",
                 year, date)
  dest <- file.path("data", basename(src))
  if(!file.exists(dest)) {
    val <- download.file(src, dest, quiet = TRUE)
    if(!val)
      stop("unable to download file ", src)
  }
  dest

}

#This file takes the original download code from num_download() and adds
#a bit of error checking to see if download.file()was successful (if 
#not, an error is thrown with stop()).

#----

num_download <- function(pkgname, date = "2016-07-20") {
  dest <- check_for_logfile(date)
  cran <- read_csv(dest, col_types = "ccicccccci", progress = FALSE)
  cran %>% filter(package == pkgname) %>% nrow
}   


#-----------------------------------------------------------------
#DEPENDENCY CHECKING  
#===================
#The num_downloads() function depends on the readr and dplyr packages. Without them installed, the function won't run. 
check_pkg_deps <- function() {
  if(!require(readr)) {
    message("installing the 'readr' package")
    install.packages("readr")
  }
  if(!require(dplyr))
    stop("the 'dplyr' package needs to be installed first")
}


#Now, our updated function can check for package dependencies.
num_download <- function(pkgname, date = "2016-07-20") {
  check_pkg_deps()
  dest <- check_for_logfile(date)
  cran <- read_csv(dest, col_types = "ccicccccci", progress = FALSE)
  cran %>% filter(package == pkgname) %>% nrow
}

num_download("Rcpp")

#------------------------------------------------------------------
#VECTORIZATION
#==========================
#One final aspect of this function that is worth noting is that as currently
#written it is not vectorized. This means that each argument must be a single
#value---a single package name and a single date. However, in R, it is a
#common paradigm for functions to take vector arguments and for those 
#functions to return vector or list results. Often, users are bitten by
#unexpected behavior because a function is assumed to be vectorized when
#it is not.


#One way to vectorize this function is to allow the pkgname argument to be 
#a character vector of package names. This way we can get download statistics
#for multiple packages with a single function call. Luckily, this is fairly 
#straightforward to do. The two things we need to do are


#Adjust our call to filter() to grab rows of the data frame that fall within a vector of package names
#Use a group_by() %>% summarize() combination to count the downloads for each package.

## 'pkgname' can now be a character vector of names
num_download <- function(pkgname, date = "2016-07-20") {
  check_pkg_deps()
  dest <- check_for_logfile(date)
  cran <- read_csv(dest, col_types = "ccicccccci", progress = FALSE)
  cran %>% filter(package %in% pkgname) %>% 
    group_by(package) %>%
    summarize(n = n())
}  

num_download("Rcpp")

num_download(c("filehash", "weathermetrics"))


#------------------------------------------------------------------------------
#ARUUMENT CHECKING
#===============
#Checking that the arguments supplied by the reader are proper is a good way to
#prevent confusing results or error messages from occurring later on in the 
#function. It is also a useful way to enforce documented requirements for a function.

num_download <- function(pkgname, date = "2016-07-20") {
  
  check_pkg_deps()
  
  ## Check arguments
  if(!is.character(pkgname))
    stop("'pkgname' should be character")
  if(!is.character(date))
    stop("'date' should be character")
  if(length(date) != 1)
    stop("'date' should be length 1")
  
  dest <- check_for_logfile(date)
  
  cran <- read_csv(dest, col_types = "ccicccccci", progress = FALSE)
  
  cran %>% filter(package %in% pkgname) %>% 
    group_by(package) %>%
    summarize(n = n())
}    


num_download("filehash", c("2016-07-20", "2016-0-21"))
num_download("filehash", c("2016-07-20"))
num_download(c("filehash", "weathermetrics"))


#Developing functions is a key aspect of programming in R and typically involves a bottom-up process.
#1.Code is written to accomplish a specific task or a specific instance of a task.
#2.The code is examined to identify key aspects that may be modified by other users;
#these aspects are abstracted out of the code and made arguments of a function.
#3.Functions are written to accomplish more general versions of a task; specific
#instances of the task are indicated by setting values of function arguments.
#4.Function code can be re-factored to provide better modularity and to divide
#functions into specific sub-tasks.
#5.Functions can be assembled and organized into R packages.


#===============================================================================
#traceback() prints the deparsed call stack deepest call first, and returns
#it invisibly. The calls may print on more than one line, and the first line
#for each call is labelled by the frame number. The number of lines printed 
#per call can be limited via max.lines. 

foo <- function(x) { print(1); bar(2) }
bar <- function(x) { x + a.variable.which.does.not.exist }
## Not run: 
foo(2) # gives a strange error
traceback()
options(error=recover)

#traceback()
#===============
#The traceback() by default prints the call stack of the last error. Apart 
#from tracing the errors, this function is also helpful in times if we require
#to print the current stack.


x <- as.list(-2:2)
x[[2]] <- "what?!?"
x

sapply(x, function(x) 1/x)

y <- rep(NA, length(x))
y

for (i in 1:length(x)) {
      y[i] <-  1/x[[i]]
 }

#options(error = browser) 
#options(error = NULL)
#options(error = recover)
#debuggingState(on=FALSE)

#---------------------------------------------
lapply(1:10, function(x){ browser(); mean})

#---------------------------------------------
sapply(x, function(x) try(1/x))

lapply(1:10, function(x){ try(mean(rnorm(x)))})

#---------------------------------------------
x <- as.list(-2:2)
x[[2]] <- "what?!?"
x

library(plyr)
library(dplyr)
laply(x, function(x) 1/x, .inform = TRUE)

#---------------------------------------------
sapply(x, function(x) {
  res <- tryCatch(1 / x,
                  error=function(e) {
                    cat("Failed on x = ", x, "\n", sep="") ## browser()
                    stop(e)
                  })
})

#-----------------------------------------------

myfn<-function(x){x+1}
blah<-sapply(x,function(x){myfn(x)})

blah<-sapply(x, myfn)

#-------------------------------------------------
x <- 1:5
y <- x + rnorm(length(x),0,1)
x
y
f <- function(x,y) {
  y <- c(y,1)
  lm(y~x)
}

options(error=recover)
f(x,y)


traceback()
f(x,y)


debug(f)
f(x,y)

#-------------------------------------------------

#If I am running a long R script from the command line (R --slave script.R),
#how can I get it to give line numbers at errors?

#I don't want to add debug commands to the script if at all possible --
#I just want R to behave like most other scripting languages 


traceback() #This won't give you the line number, but it will tell you 
#where the failure happens in the call stack which is very helpful:


#the usual debugging suspects:
#debug()
#browser()
#options(error=recover) [followed by options(error = NULL) to revert it]












