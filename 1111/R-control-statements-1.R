
#Source https://www.datacamp.com/community/tutorials/tutorial-on-loops-in-r
#Advanced functions
#https://www.datacamp.com/courses/writing-functions-in-r/

#Best loops : https://swcarpentry.github.io/r-novice-inflammation/15-supp-loops-in-depth/

#An Introduction To Loops in R

#for,while,repeat,break,next,

#among the control flow commands, the loop constructs are for,
#while and repeat, with the additional clauses "break" and "next".



# Create a vector filled with random normal values
u1 <- rnorm(30)
print("This loop calculates the square of the first 10 elements of vector u1")

# Initialize `usq`
usq <- c()

for(i in 1:10) {
  # i-th element of `u1` squared into `i`-th position of `usq`
  usq[i] <- u1[i]*u1[i]
}
  

print(usq)


a = rnorm(1:10)

ff= c()

for (i in 1:10) {
  ff[i] = a[i]^2
}

print(ff)

#=====================================================
# Create a 30 x 30 matrix (of 30 rows and 30 columns)
mymat <- matrix(nrow=30, ncol=30)

# For each row and for each column, assign values based on position: product of two indexes
for(i in 1:dim(mymat)[1]) {
  for(j in 1:dim(mymat)[2]) {
    mymat[i,j] = i*j
  }
}

# Just show the upper left 10x10 chunk
mymat[1:10, 1:10]

#============================================================
# Create your three-dimensional array
my_array <- array(0, c(5, 5, 10))

for (i in 1:dim(my_array)[1]) {
  for (j in 1:dim(my_array)[2]) {
    for (k in 1:dim(my_array)[3]) {
      my_array[i,j,k] = i*j*k
    }
  }
}

# Show a 10x10x15 chunk of your array
print(my_array)
#===============================================================
# Insert your own integer here
my_int <- 5

nr <- as.integer(my_int)

# Create a `n` x `n` matrix with zeroes
mymat <- matrix(0, nr, nr)

# For each row and for each column, assign values based on position
# These values are the product of two indexes
for(i in 1:dim(mymat)[1]) {
  for(j in 1:dim(mymat)[2]) {
    mymat[i,j] = i*j
  }
}

print(mymat)

# Show the first 10x10 chunk or the first `nr` x `nr` chunk
if (nr > 10) {
  mymat[1:10, 1:10]
} else mymat[1:nr, 1:nr]

#===================================================
for (year in c(2010:2015)){
  print(paste("The year is", year))
}

#====================================================
#odd numbers in R
for (i in 1:10) {
  if (!i %% 2){
    next
  }
  print(i)
}



#==================================================================
##########################################################################
#For Loops: Popular But Not Always Perfect for Your Use

#The for loop is by far the most popular and its construct implies 
#that the number of iterations is fixed and known in advance, as 
#in cases like "generate the first 200 prime numbers" or "enlist 
#the 10 most important customers


#But what if you do not know or control the number of iterations and 
#one or several conditions may occur which are not predictable beforehand?

#For example, you may want to count the number of clients living in 
#an area identified by a specified postal code, or the number of 
#clicks on a web page banner within the last two days, or similar 
#unforeseen events.

#In cases like these, the while loop and its cousin repeat may come to the rescue



# Your User Defined Function
readinteger <- function(){
  n <- readline(prompt="Please, enter your ANSWER: ")
}

response <- as.integer(readinteger())

while (response!=42) {   
  print("Sorry, the answer to whatever the question MUST be 42");
  response <- as.integer(readinteger())
}


#===============================================================
#########################################################################
#Repeat Loops

#this loop "repeat until" to emphasize the fact that the instructions
#i1 and i2 are executed until the condition remains False (F) or, 
#equivalently, becomes True (T), thus exiting; but in any case, at 
#least once.

readinteger <- function(){
  n <- readline(prompt="Please, enter your ANSWER: ") 
}

repeat {   
  response <- as.integer(readinteger());
  if (response == 42) {
    print("Well done!");
    break
  } else print("Sorry, the answer to whatever the question MUST be 42");
}

#===============================================================

# Interruption and Exit Loops in R (So how do you exit from a loop?)

#Break Your Loops With "break"
#===============================
#When the R interpreter encounters a break, it will pass control to 
#the instruction immediately after the end of the loop (if any). In 
#the case of nested loops, the break will permit to exit only from 
#the innermost loop.

# Make a lower triangular matrix (zeroes in upper right corner)
#--------------------------------------------------------------
m=10 
n=10

# A counter to count the assignment
ctr=0

# Create a 10 x 10 matrix with zeroes 
mymat = matrix(0,m,n)

for(i in 1:m) {
  for(j in 1:n) {   
    if(i==j) { 
      break;
    } else {
      # you assign the values only when i<>j
      mymat[i,j] = i*j
      ctr=ctr+1
    }
  }
  print(i*j) 
}

# Print how many matrix cells were assigned
print(ctr)
print(mymat)

#=================================================================

#The Use of next in Loops

#"next" discontinues a particular iteration and jumps to the next cycle. 

# Make a diagnoal matrix zero
#--------------------------------------------------------------
m=10 
n=10

# A counter to count the assignment
ctr=0

# Create a 10 x 10 matrix with zeroes 
mymat = matrix(0,m,n)

for(i in 1:m) {
  for(j in 1:n) {   
    if(i==j) { 
      next;
    } else {
      # you assign the values only when i<>j
      mymat[i,j] = i*j
      ctr=ctr+1
    }
  }
  print(i*j) 
}

# Print how many matrix cells were assigned
print(ctr)
print(mymat)

#------------------------------
m=20

for (k in 1:m){
  if (!k %% 2)
    next
  print(k)
}

#=====================================================================

v1 = c(1:20)
v2 = c(2:21)
v3 = 0

for (i in 1:length(v1)) { 
  v3[i] <-v1[i] + v2[i] 
}
v3
#---------------------------------
# This is a bad loop with 'growing' data
set.seed(42)
m=10
n=10

# Create matrix of normal random numbers
mymat <- replicate(m, rnorm(n))

class(mymat)

# Transform into data frame
mydframe <- data.frame(mymat)

for (i in 1:m) {
  for (j in 1:n) {
    mydframe[i,j]<-mydframe[i,j] + 10*sin(0.75*pi)
  }
}

print(mydframe)


set.seed(42)
m=10
n=10
mymat <- replicate(m, rnorm(n)) 
mydframe <- data.frame(mymat)
mydframe <- mydframe + 10*sin(0.75*pi)
mydframe


#-------------------------------------------------------

# Insert `system.time()` to measure loop execution
system.time(

for (i in 1:m) {
  for (j in 1:n) {
    mydframe[i,j] <- mydframe[i,j] + 10*sin(0.75*pi)
  }
}
)
# Add `system.time()` to measure vectorized execution
system.time(
mydframe<-mydframe + 10*sin(0.75*pi))

#-------------------------------------------------------------
#sweep(), by() and aggregate() and are occasionally used in conjunction with the elements of the apply() family


# define matrix `mymat` by replicating the sequence `1:5` for `4` times and transforming into a matrix
mymat<-matrix(rep(seq(5), 4), ncol = 5)

# `mymat` sum on rows
apply(mymat, 1, sum)

# `mymat` sum on columns
apply(mymat, 2, sum)

# With user defined function within the apply that adds any number `y` to the sum of the row 
# `y` is set at `4.5` 
apply(mymat, 1, function(x, y) sum(x) + y, y=4.5)

# Or produce a summary column wise for each column
apply(mymat, 2, function(x) summary(mymat))



msg <- c("Hello")
i <- 1


repeat {
  
  for(i in 1:10){
    if(!i %% 2 )
      next
      print(i)
    
}}


for (i in 1:10) {
  if (!i %% 2){
    next
  }
  print(i)
}


a <- 1:10
b <- 1:10

res <- numeric(length = length(a))
for (i in seq_along(a)) {
  res[i] <- a[i] + b[i]
}
res


res2 <- a + b
all.equal(res, res2)


seq_along("kk","hg","ff","uu","dd","uu")









  

















































