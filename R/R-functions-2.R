
#Source : https://www.datamentor.io/r-programming/vector

#Recursion :A function that calls itself is called a recursive function and this technique is known as recursion.
#This special programming technique can be used to solve problems by breaking them into smaller and simpler sub-problems.

#Recursive function to find factorial
recursive.factorial <- function(x) {
    if (x == 0)   
      return (1)
    else           
      return (x * recursive.factorial(x-1))
  }

recursive.factorial(5)

######### OR #################
#Example: Find Factorial of a number using recursion
recur_factorial <- function(n) {
  if(n <= 1) {
    return(1)
  } else { 
    return(n * recur_factorial(n-1))
  }
}

recur_factorial(10)

############# OR ######################
#factorial of a number without using a recursive function.
#The factorial of a number is the product of all the integers from 1 to that number.
#Factorial is not defined for negative numbers and the factorial of zero is one, 0! = 1.

# take input from the user
num = as.integer(readline(prompt="Enter a number: "))

factorial = 1

# check is the number is negative, positive or zero

if(num < 0) {
  print("Sorry, factorial does not exist for negative numbers")
} else if(num == 0) {
  print("The factorial of 0 is 1")
} else {
  for(i in 1:num) {
    factorial = factorial * i
  }
  print(paste("The factorial of", num ,"is",factorial))
}


#----------------------------------------------
#Convert Decimal into Binary using Recursion in R

#Decimal number is converted into binary by dividing the number
#successively by 2 and printing the remainder in reverse order.

convert_to_binary <- function(n) {
  if(n > 1) {
    convert_to_binary(as.integer(n/2))
  }
  cat(n %% 2)
}
  
convert_to_binary(100)
convert_to_binary(52)

#-------------------------------------------------------------------------
#Fibonacci Sequence Using Recursion in R
#Example: Fibonacci Sequence in R

recurse_fibonacci <- function(n) {
  if(n <= 1) {
    return(n)
  } else {
    return(recurse_fibonacci(n-1) + recurse_fibonacci(n-2))
  }
}

# take input from the user
nterms = as.integer(readline(prompt="How many terms? "))

# check if the number of terms is valid
if(nterms <= 0) {
  print("Plese enter a positive integer")
} else {
  print("Fibonacci sequence:")
  for(i in 0:(nterms-1)) {
    print(recurse_fibonacci(i))
  }
}

#################### OR ######################
#Example: Print Fibonacci Sequence without recurssion 

#The first two terms are 0 and 1. All other terms are obtained by 
#adding the preceding two terms.

#This means to say the nth term is the sum of (n-1)th and (n-2)th term.

# take input from the user
nterms = as.integer(readline(prompt="How many terms? "))

# first two terms
n1 = 0
n2 = 1
count = 2

# check if the number of terms is valid
if(nterms <= 0) {
  print("Plese enter a positive integer")
} else {
  if(nterms == 1) {
    print("Fibonacci sequence:")
    print(n1)
  } else {
    print("Fibonacci sequence:")
    print(n1)
    print(n2)
    while(count < nterms) {
      nth = n1 + n2
      print(nth)
      # update values
      n1 = n2
      n2 = nth
      count = count + 1
    }
  }
}

#-----------------------------------------------------------------
#R Program to Check Prime Number

#A positive integer greater than 1 which has no other factors except 1 
#and the number itself is called a prime number.

#Example to check whether an integer (entered by the user) is a 
#prime number or not using control statements.


num = as.integer(readline(prompt="Enter a number: "))
flag = 0

# prime numbers are greater than 1
if(num > 1) {
  # check for factors
  flag = 1
  for(i in 2:(num-1)) {
    if ((num %% i) == 0) {
      flag = 0
      break
    }
  }
} 

if(num == 2)    flag = 1
if(flag == 1) {
  print(paste(num,"is a prime number"))
} else {
  print(paste(num,"is not a prime number"))
}

#----------------------------------------------------------------------
#R Program to check Armstrong Number

#An Armstrong number, also known as narcissistic number, is a number 
#that is equal to the sum of the cubes of its own digits.

#For example, 370 is an Armstrong number since 370 = 3*3*3 + 7*7*7 + 0*0*0.

# take input from the user
num = as.integer(readline(prompt="Enter a number: "))

# initialize sum
sum = 0

# find the sum of the cube of each digit
temp = num

while(temp > 0) {
  digit = temp %% 10
  sum = sum + (digit ^ 3)
  temp = floor(temp / 10)
}

# display the result
if(num == sum) {
  print(paste(num, "is an Armstrong number"))
} else {
  print(paste(num, "is not an Armstrong number"))
}

#---------------------------------------------------------------------------
#R Program to Check for Leap Year
#A leap year is exactly divisible by 4 except for century years (years 
#ending with 00). The century year is a leap year only if it is 
#perfectly divisible by 400.

# Program to check if the input year is a leap year or not
year = as.integer(readline(prompt="Enter a year: "))

if((year %% 4) == 0) {
  if((year %% 100) == 0) {
    if((year %% 400) == 0) {
      print(paste(year,"is a leap year"))
    } else {
      print(paste(year,"is not a leap year"))
    }
  } else {
    print(paste(year,"is a leap year"))
  }
} else {
  print(paste(year,"is not a leap year"))
}


#If a year is divisible by 4, 100 and 400, it's a leap year.

#If a year is divisible by 4 and 100 but not divisible by 400, it's not a leap year.

#If a year is divisible by 4 but not divisible by 100, it's a leap year.

#If a year is not divisible by 1, it's not a leap year.

#This logic is implemented in the above program using nested if...else statement.

#------------------------------------------------------------------------

#Use Recursive Functions in R to Find Sum of Series 1²+2²+3²+...+n²

# Recursive Functions in R Example
Sum.Series <- function(number)
{
  if(number == 0) {
    return (0)
  } else {
    return ((number * number ) + Sum.Series(number - 1))
  }
}

Sum.Series(5)

#-----------------------------------------------------------------------------
#######################################################################
#R-Functions
#----------------

#Functions are used to logically break our code into simpler 
#parts which become easy to maintain and understand.

#Here, we can see that the reserved word function is used to declare a 
#function in R.

#The statements within the curly braces form the body of the function. 
#These braces are optional if the body contains only a single expression.

pow <- function(x, y) {
  # function to print x raised to the power y
  result <- x^y
  print(paste(x,"raised to the power", y, "is", result))
}

pow(8,2)
pow(x = 8,y = 2)
pow(y = 2, x = 8)


#R Return() Value from Function
#----------------------------------
#Many a times, we will require our functions to do some processing 
#and return back the result. This is accomplished with the return()
#function in R.

#Let us look at an example which will return whether a given number 
#is positive, negative or zero.

check <- function(x) {
  if (x > 0) {
    result <- "Positive"
  }
  else if (x < 0) {
    result <- "Negative"
  }
  else {
    result <- "Zero"
  }
  return(result)
}

check(-5)

#Functions without return()
#-----------------------------------
#If there are no explicit returns from a function, the value of the 
#last evaluated expression is returned automatically in R.

check <- function(x) {
  if (x > 0) {
    result <- "Positive"
  }
  else if (x < 0) {
    result <- "Negative"
  }
  else {
    result <- "Zero"
  }
  result
}

check(-5)

#We generally use explicit return() functions to return a value 
#immediately from a function.

#If it is not the last statement of the function, it will prematurely
#end the function bringing the control to the place from which it was 
#called.

check <- function(x) {
  if (x>0) {
    return("Positive")
  }
  else if (x<0) {
    return("Negative")
  }
  else {
    return("Zero")
  }
}

check(0)
  
#In the above example, if x > 0, the function immediately returns 
#"Positive" without evaluating rest of the body.

#-----------------------------------------------------------------------------
#Multiple Returns()
#--------------------
#The return() function can return only a single object. If we want 
#to return multiple values in R, we can use a list (or other objects)
#and return it.

multi_return <- function() {
  my_list <- list("color" = "red", "size" = 20, "shape" = "round")
  return(my_list) 
}

#Here, we create a list my_list with multiple elements and return this single list.

a <- multi_return()

a$color
a$size
a$shape


#Example: Simple Calculator in R
#----------------------------------
# Program make a simple calculator that can add, subtract, multiply and divide using functions
add <- function(x, y) {
  return(x + y)
}
subtract <- function(x, y) {
  return(x - y)
}
multiply <- function(x, y) {
  return(x * y)
}
divide <- function(x, y) {
  return(x / y)
}
# take input from the user
print("Select operation.")
print("1.Add")
print("2.Subtract")
print("3.Multiply")
print("4.Divide")
choice = as.integer(readline(prompt="Enter choice[1/2/3/4]: "))
num1 = as.integer(readline(prompt="Enter first number: "))
num2 = as.integer(readline(prompt="Enter second number: "))
operator <- switch(choice,"+","-","*","/")

result <- switch(choice, add(num1, num2), subtract(num1, num2), multiply(num1, num2), divide(num1, num2))
print(paste(num1, operator, num2, "=", result))

#Two numbers are taken from the user and a switch branching is used to execute a particular function.

##########################################################################
#----------------------------------------------------
#R Environment and Scope : #R Programming Environment
#-------------------------
#In order to write functions in a proper way and avoid unusual errors,
#we need to know the concept of environment and scope in R.

#Environment can be thought of as a collection of objects (functions, variables etc.).

#An environment is created when we first fire up the R interpreter. Any variable we define, is now in this environment.

#The top level environment available to us at the R command prompt is the global environment called R_GlobalEnv.

#Global environment can be referred to as .GlobalEnv in R codes as well.

#We can use the ls() function to show what variables and functions are defined in the current environment.

#Moreover, we can use the environment() function to get the current environment

a <- 2
b <- 5
f <- function(x) x<-0
ls()
environment()
.GlobalEnv

#In the above example, we can see that a, b and f are in the R_GlobalEnv environment. 
#Notice that x (in the argument of the function) is not in this global environment. When we define a function, a new environment is created.
#In the above example, the function f creates a new environment inside the global environment.
#Actually an environment has a frame, which has all the objects defined, and a pointer to the enclosing (parent) environment.
#Hence, x is in the frame of the new environment created by the function f. This environment will also have a pointer to R_GlobalEnv.

#Example: Cascading of environments
f <- function(f_x){
  g <- function(g_x){
    print("Inside g")
    print(environment())
    print(ls())
  }
  g(5)
  print("Inside f")
  print(environment())
  print(ls())
}

#Now when we run it from the command prompt, we get.
f(6)

#Here, we defined function g inside f and it is clear that they both have different environments with different objects within their respective frames.


#R Programming Scope
#---------------------
outer_func <- function(){
  b <- 20
  inner_func <- function(){
    c <- 30
  }
}
a <- 10

#Global variables
#-----------------
#Global variables are those variables which exists throughout the execution of a program. It can be changed and accessed from any part of the program.
#However, global variables also depend upon the perspective of a function.
#from the perspective of inner_func(), both a and b are global variables.
#However, from the perspective of outer_func(), "b" is a local variable
#and only "a" is global variable. The variable c is completely invisible to outer_func().


#Local variables
#----------------
#On the other hand, Local variables are those variables which exist only 
#within a certain part of a program like a function, and is released 
#when the function call ends.
#In the above program the variable "c" is called a local variable.

#If we assign a value to a variable with the function inner_func(),
#the change will only be local and cannot be accessed outside the function.

#This is also the same even if names of both global variable and local variables matches.

#For example, if we have a function as below.

outer_func <- function(){
  a <- 20
  inner_func <- function(){
    a <- 30
    print(a)
  }
  inner_func()
  print(a)
}

#When we call it,
a <- 10
outer_func()
print(a)

#We see that the variable "a" is created locally within the environment frame
#of both the functions and is different to that of the global environment frame.

#Accessing global variables
#----------------------------
#Global variables can be read but when we try to assign to it, a new 
#local variable is created instead.

#To make assignments to global variables, superassignment operator, <<-, is used.

#When using this operator within a function, it searches for the variable 
#in the parent environment frame, if not found it keeps on searching the 
#next level until it reaches the global environment.

#If the variable is still not found, it is created and assigned at the global level.
outer_func <- function(){
  inner_func <- function(){
    a <<- 30
    print(a)
  }
  inner_func()
  print(a)
}

outer_func()
print(a)

#When the statement a <<- 30 is encountered within inner_func(), it looks
#for the variable "a" in outer_func() environment.

#When the search fails, it searches in R_GlobalEnv.

#Since, "a" is not defined in this global environment as well, it is 
#created and assigned there which is now referenced and printed from 
#within inner_func() as well as outer_func().

#############################################################################
#----------------------------------------------------------------
#R Infix Operator
#--------------------

#Most of the operators that we use in R are binary operators (having two operands).
#Hence, they are infix operators, used between the operands.
#Actually, these operators do a function call in the background.

#For example, the expression a+b is actually calling the function `+`() 
#with the arguments a and b, as `+`(a, b).

#Note: the back tick (`), this is important as the function name 
#contains special symbols

5+3

`+`(5,3)

5*3-1

`-`(`*`(5,3),1)

#It is possible to create user-defined infix operators in R. This
#is done by naming a function that starts and ends with %.

#an example of user-defined infix operator to see if a number is 
#exactly divisible by another.
#Example: User defined infix operator

`%divisible%` <- function(x,y){
  if (x%%y ==0) return (TRUE)
  else          return (FALSE)
}

10 %divisible% 3

`%divisible%`(10,5)

#Things to remember while defining your own infix operators are that 
#they must start and end with %. Surround it with back tick (`) in the 
#function definition and escape any special symbols.

#Predefined infix operators in R
# %% 	Remainder operator
# %/% 	Integer division
# %*% 	Matrix multiplication
# %o% 	Outer product
# %x% 	Kronecker product
# %in% 	Matching operator


#----------------------------------------------------------------------
#R switch() Function
#----------------------

#The switch() function in R tests an expression against elements of a list.
#If the value evaluated from the expression matches item from the list, the 
#corresponding value is returned.

#switch (expression, list)

switch(2,"red","green","blue")
switch(1,"red","green","blue")

#If the numeric value is out of range (greater than the number of items in the list or smaller than 1), then, NULL is returned.

x = switch(4,"red","green","blue")
x

#Example: switch() Function with as String Expression
#The expression used in the switch () function can be a string as well. 
#In this case, the matching named item's value is returned.
switch("color", "color" = "red", "shape" = "square", "length" = 5)
switch("length", "color" = "red", "shape" = "square", "length" = 5)


#---------------------------------------------------------------------------
#Source : https://www.r-bloggers.com/control-structures-loops-in-r/

words = c("R", "datascience", "machinelearning","algorithms","AI") 

words.names = function(x) {
  for(name in x){ 
    print(name) 
  }
} 

words.names(words)


#Hands on exercise of what we have learnt so far
#-----------------------------------------------------
#We create a data frame DF, run for loop, ifelse in a function and call the function
#create 3 vectors name,age,salary
name = c("David","John","Mathew")
Age = c(30,40,50)
salary = c(30000,120000,55000)

#create a data frame DF by combining the 3 vectors using cbind() function
DF = data.frame(cbind(name,Age,salary))

DF
dim(DF)

#write a function which displays the salaried person name
findHighSalary = function(df){
  Maxsal = 0
  empname = ""
  for(i in 1:nrow(DF)){
    tmpsal = as.numeric(DF[i,3] )
    if(tmpsal > Maxsal){
      Maxsal = tmpsal
      empname = DF[i,1]
      #empname = DF$name[DF$salary == tmpsal]
    }
  }
  return(as.character(empname))
}

#calling the function
findHighSalary(DF)

#--------------------------------------------------------------------------


























