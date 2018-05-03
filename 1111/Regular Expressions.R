

#Regular Expressions Every R programmer Should Know
#---------------------------------------------------



#Regex: The backslash, \
#Regex: The hat ,^, and dollar, $
#Regex: Round parentheses,(), and the pipe, |
#Regex: Square parentheses,[], and the asterisk, *
#Regular expressions. How they can be cruel!
#Well we're here to make them a tad easier. 
#To do so we're going to make use of the stringr package

library("stringr")

#We're going to use the str_detect() and str_subset() functions. 
#In particular the latter. These have the syntax

#function_name(STRING, REGEX_PATTERN)

#str_detect() is used to detect whether a string contains a certain pattern. 
#At the most basic use of these functions, we can match strings of text. For instance

jr = c("Theo is first", "Esther is second", "Colin - third")
str_detect(jr, "Theo")
jr[str_detect(jr, "Theo")]

str_detect(jr, "is")

# If we want to return the actual strings that contain
#these patterns, we use str_subset()

str_subset(jr, "Theo")
str_subset(jr, "is")




























