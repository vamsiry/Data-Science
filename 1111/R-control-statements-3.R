


#Print the numbers from 1 to 100
#When the number is a multiple of 3 print "Fizz" instead of the number
#When the number is a multiple of 5 print "Buzz instead of the number
#When the number is a multiple of both print "FizzBuzz"


# Print the numbers from 1 to 100

for (i in 1:100) {
  print(i)
}


#----------------------------------------
# Print the numbers from 1 to 100
# For multiples of 3 print "Fizz"
#i <- 1
for (i in 1:100) {
  if(i %% 3 == 0) {print("Fizz")}
  else print(i)
}


#----------------------------------------
# Print the numbers from 1 to 100
# For multiples of 3 print "Fizz"
# For multiples of 5 print "Buzz"

for (i in 1:100) {
  if (i %% 3 == 0) {print("Fizz")}
  else if (i %% 5 == 0) {print("Buzz")}
  else print(i)
}

#----------------------------------------
# Print the numbers from 1 to 100
# For multiples of 3 print "Fizz"
# For multiples of 5 print "Buzz"
# For multiples of both print "FizzBuzz"

for (i in 1:100) {
  if (i %% 3 == 0 & i %% 5 == 0) {print("FizzBuzz")}
  else if (i %% 3 == 0) {print("Fizz")}
  else if (i %% 5 == 0) {print("Buzz")}
  else print(i)
}

#----------------------------------------
#http://rprogramming.net/phone-pad-interview-test-solution-r/
#Phone Pad Interview Test Solutions in R

#Connect to Database : http://rprogramming.net/connect-to-database-in-r/

#Recode data in R : http://rprogramming.net/recode-data-in-r/









