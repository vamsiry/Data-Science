
#Source : https://www.datamentor.io/r-programming#tutorial

#Source : http://ramnathv.github.io/pycon2014-r/learn/controls.html
#Source : https://www.datamentor.io/r-programming/if-else-statement


#R-if statement
#-------------
#If the test_expression is TRUE, the statement gets executed. 
#But if it's FALSE, nothing happens.

x <- 5
if(x > 0){
  print("Positive number")
}

#R-if.else statement
#--------------------
#The else part is optional and is only evaluated if test_expression is FALSE.
#note that else must be in the same line as the closing braces of the if statement.

x <- -5
if(x > 0){
  print("Non-negative number")
} else {
  print("Negative number")
}

### OR ####
if(x > 0) print("Non-negative number") else print("Negative number")

### OR ###
x <- -5
y <- if(x > 0) 5 else 6
y


#R-if.else Ladder
#--------------------
#The if.else ladder (if.else.if) statement allows you execute a block 
# of code among more than 2 alternatives

x <- 0

if (x < 0) {
  print("Negative number")
} else if (x > 0) {
  print("Positive number")
} else {
  print("Zero")
}


#-----------------------------------
#There is an easier way to use if.else statement specifically 
#for vectors in R programming.

#You can use ifelse() function instead; the vector equivalent form 
#of the if.else statement.

#Example: ifelse() function
#------------------------------
a = c(5,7,2,9)

ifelse(a %% 2 == 0,"even","odd")


#R-for Loop
#-----------
#Loops are used in programming to repeat a specific block of code. 

#Here, sequence is a vector and val takes on each of its value during 
#the loop. In each iteration, statement is evaluated.

#Below is an example to count the number of even numbers in a vector.
x <- c(2,5,3,9,8,11,6)
count <- 0

for (val in x) {
  if(val %% 2 == 0)  count = count+1
}

print(count)


#R-while Loop
#----------------
#while loops are used to loop until a specific condition is met.

i <- 1

while (i < 6) {
  print(i)
  i = i+1
}


#R break and next Statement
#--------------------------
#a normal looping sequence can be altered using the break or the next statement.

#break statement
#----------------

#A break statement is used inside a loop (repeat, for, while) to stop 
#the iterations and flow the control outside of the loop.

#In a nested looping situation, where there is a loop inside another 
#loop, this statement exits from the innermost loop that is being evaluated.

#Note: the break statement can also be used inside the  else 
#branch of if...else statement.

x <- 1:5

for (val in x) {
  if (val == 3){
    break
  }
  print(val)
}

#next statement
#----------------
#A next statement is useful when we want to skip the current iteration of
#a loop without terminating it. 

#On encountering next, the R parser skips further evaluation and 
#starts next iteration of the loop.

#Note: the next statement can also be used inside the  else branch 
#of if...else statement.

x <- 1:5

for (val in x) {
  if (val == 3){
    next
  }
  print(val)
}


#R-repeat loop
#---------------------
#A repeat loop is used to iterate over a block of code multiple number of times.

#There is no condition check in repeat loop to exit the loop.

#We must ourselves put a condition explicitly inside the body of the 
#loop and use the break statement to exit the loop. Failing to do so 
#will result into an infinite loop.

#repeat { statement }
#In the statement block, we must use the break statement to exit the loop.
x <- 1

repeat {
  print(x)
  x = x+1
  if (x == 6){
    break
  }
}


########################################################################
############################################################################

#Source : https://ramnathv.github.io/pycon2014-r/learn/controls.html
#---------------------------------------------------------------
#If
#------
x <- 1:15

if (sample(x, 1) <= 10) {
  print("x is less than 10")
} else {
  print("x is greater than 10")
}

###############
#See the code below for nested if else statement
x=10

if(x>1 & x<7){
  print("x is between 1 and 7")}else if(x>8 & x< 15){
    print("x is between 8 and 15")
  }

#---------------------------------------------------------
#Vectorization with ifelse
#--------------------------
ifelse(x <= 10, "x less than 10", "x greater than 10")

#Other valid ways of writing if/else

if (sample(x, 1) < 10) {
  print(y <- 5)
} else {
  print(y <- 0)
}


if (sample(x, 1) < 10) {
  5
} else {
  0
}

#--------------------------------------------------------------
#for loop
#--------
#A for loop works on an iterable variable and assigns successive 
#values till the end of a sequence.

for (i in 1:10) {
  print(i)
}

x = c(1,2,3,4,5)
for(i in 1:5){
  print(x[i])
}

#################
x <- c("apples", "oranges", "bananas", "strawberries")

for (i in x) {
  print(x[i])
}

for (i in 1:4) {
  print(x[i])
}

for (i in seq(x)) {
  print(x[i])
}

for (i in 1:4) print(x[i])

#----------------------------------------------------------------
#Nested loops
#-----------------
m <- matrix(1:10, 2)

for (i in seq(nrow(m))) {
  for (j in seq(ncol(m))) {
    print(m[i, j])
  }
}

#---------------------------------------------------------------
# While loop
#----------------
#Be sure there is a way to exit out of a while loop.
i <- 1

while (i < 10) {
  print(i)
  i <- i + 1
}


x = 2.987
while(x <= 4.987) { 
  x = x + 0.987
  print(c(x,x-2,x-1)) 
}
#-------------------------------------------------------------
#Next
#------------
#Next statement enables to skip the current iteration of 
#a loop without terminating it. 

#printing even numbers
for (i in 1:20) {
  if (i%%2 == 1) {
    next
  } else {
    print(i)
  }
}

################
x = 1: 4 

for (i in x) { 
  if (i == 2){ 
    next}
  print(i)
}

#-------------------------------------------------------------
#Repeat and break
#------------------ 
#The repeat loop is an infinite loop and used in association 
#with a break statement.

a = 1

repeat { print(a) 
  a = a+1 
if(a > 4) {
  break
  }
}

#------------------------------------------------------------
# Break 
#------------
#A break statement is used in a loop to stop the iterations and 
#flow the control outside of the loop. 

x = 1:10 

for (i in x){ 
  if (i == 5){ 
    break 
  }
  print(i)
}

#-----------------------------------------------------------------









































