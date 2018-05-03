
#https://www.r-bloggers.com/author/chuck-powell/

#https://ibecav.github.io/Functionalize/
#=============================================

setwd("C:\\Users\\vamsi\\Desktop\\projects\\practice\\better R functions\\")

OfInterest <- read.csv("https://raw.githubusercontent.com/ibecav/ibecav.github.io/master/Rmdfiles/ofinterest.csv")

write.csv(OfInterest,"C:\\Users\\vamsi\\Desktop\\projects\\practice\\better R functions\\OfInterest.csv")

str(OfInterest)



#=========================================================================
### Manual and painful way
## Create a new workbook

install.packages("openxlsx")
library(openxlsx)
wb <- createWorkbook()

# education by each of the other 4 variables
NameofSheet <- "CoverageByEducation"
TheData <- table(OfInterest$EDUCATION,OfInterest$NOTCOV)
addWorksheet(wb = wb, sheetName = NameofSheet)
writeData(wb = wb, sheet = NameofSheet, x = TheData, borders = "n")

NameofSheet <- "MedbillByEducation"
TheData <- table(OfInterest$EDUCATION,OfInterest$MEDBILL)
addWorksheet(wb = wb, sheetName = NameofSheet)
writeData(wb = wb, sheet = NameofSheet, x = TheData, borders = "n")

### many repeating lines removed ###
NameofSheet <- "PNMED12MByAge"
TheData <- table(OfInterest$AGE,OfInterest$PNMED12M)
addWorksheet(wb = wb, sheetName = NameofSheet)
writeData(wb = wb, sheet = NameofSheet, x = TheData, borders = "n")

#https://stackoverflow.com/questions/27952451/error-zipping-up-workbook-failed-when-trying-to-write-xlsx
Sys.getenv("R_ZIPCMD", "zip")
Sys.setenv(R_ZIPCMD= "C://RBuildTools//3.4//bin//zip") 

saveWorkbook(wb, "BetterExcelExample.xlsx", overwrite = TRUE) ## save to working directory


#==============================================================================
knitr::opts_chunk$set(echo = TRUE, warning = FALSE)
library(dplyr)

library(ggplot2)
theme_set(theme_bw()) # set theme to my personal preference
# install.packages("openxlsx")
require(openxlsx)

#Breaking the task down
##----------------------
depvars <- list(Coverage = OfInterest$NOTCOV, ProbPay = OfInterest$MEDBILL, CareDelay = OfInterest$PDMED12M, NeedNotGet = OfInterest$PNMED12M)
indvars <- list(Education = OfInterest$EDUCATION, Earnings = OfInterest$EARNINGS, Age = OfInterest$AGE)

# these two are identical use head because the list is more than 100,000 entries long
head(depvars$Coverage)
head(depvars[[1]])

lapply(depvars, function (x) table(OfInterest$EDUCATION,x))

lapply(indvars, function (y) table(y,OfInterest$NOTCOV))

lapply(depvars, function (x) lapply(indvars, function (y) table(y,x)))


TablesList <- lapply(depvars, function (x) lapply(indvars, function (y) table(y,x)))
TablesList$Coverage$Earnings


#For a little more fun
#--------------------
for (i in seq_along(TablesList$Coverage)) {print(TablesList$Coverage[[i]])}

for (j in seq_along(TablesList)) {print(TablesList[[j]][[1]])}


#Driving Excel through automation
#--------------------------------
## Create a new empty workbook
wb <- createWorkbook()
## nested for loop
for (j in seq_along(TablesList)) { #top list with depvars
  for (i in seq_along(TablesList[[j]])) { #for each depvar walk the indvars
    TheData <- TablesList[[j]][[i]]
    NameofSheet <- paste0(names(TablesList[j]), "By", names(TablesList[[j]][i]))
    addWorksheet(wb = wb, sheetName = NameofSheet)
    writeData(wb = wb, sheet = NameofSheet, x = TheData, borders = "n")
  }
}
## Save our new workbook
saveWorkbook(wb, "newversion.xlsx", overwrite = TRUE) ## save to working directory



############################################################################
##############################################################################
#Using R to 'drive' MS Excel -- 3/27/2018
#-----------------------------------------------
#https://ibecav.github.io/RtoExcel/
#====================================


knitr::opts_chunk$set(echo = TRUE, warning = FALSE)
library(dplyr)

FullFile <- read.csv(file = "C:\\Users\\vamsi\\Desktop\\projects\\practice\\better R functions\\personsx.csv")
dim(FullFile)

#A little dplyr to make our life easier
#---------------------------------------
FullFile <- select(FullFile, -starts_with("L"))
dim(FullFile)

FullFile <- select(FullFile, -starts_with("INT"))
dim(FullFile)


FullFile$REGION <- recode_factor(FullFile$REGION,
                                 `1` = "Northeast",
                                 `2` = "Midwest",
                                 `3` = "South",
                                 `4` = "West")

FullFile$SEX <- recode_factor(FullFile$SEX, `1` = "Male", `2` = "Female")

FullFile$RACERPI2 <- recode_factor(FullFile$RACERPI2,
                                   `1` = "White only",
                                   `2` = "Black/African American only",
                                   `3` = "AIAN only", 
                                   `4` = "Asian only",
                                   `5` = "Race group not releasable",
                                   `6` = "Multiple race")

FullFile$PDMED12M <- recode_factor(FullFile$PDMED12M, `1` = "Yes", `2` = "No")

summary(FullFile$PDMED12M)

FullFile$PNMED12 <- recode_factor(FullFile$PNMED12, `1` = "Yes", `2` = "No")

FullFile$PNMED12M <- recode_factor(FullFile$PNMED12M, `1` = "Yes", `2` = "No")

FullFile$NOTCOV <- recode_factor(FullFile$NOTCOV, `1` = "Not covered", `2` = "Covered")

FullFile$COVER <- recode_factor(FullFile$COVER,
                                `1` = "Private",
                                `2` = "Medicaid and other public",
                                `3` = "Other coverage",
                                `4` = "Uninsured",
                                `5` = "Do not know")
FullFile$PLNWRKS1 <- recode_factor(FullFile$PLNWRKS1,
                                   `1` = "Through employer",
                                   `2` = "Through union",
                                   `3` = "Through workplace, but don't know if employer or union",
                                   `4` = "Through workplace, self-employed or professional association",
                                   `5` = "Purchased directly",
                                   `6` = "Through Healthcare.gov or the Affordable Care Act",
                                   `7` = "Through a state/local government or community program",
                                   `8` = "Other",
                                   `9` = "Through school",
                                   `10` = "Through parents",
                                   `11` = "Through relative other than parents")
FullFile$HCSPFYR <- recode_factor(FullFile$HCSPFYR,
                                  `0` = "Zero",
                                  `1` = "Less than $500",
                                  `2` = "$500 - $1,999",
                                  `3` = "$2,000 - $2,999",
                                  `4` = "$3,000 - $4,999",
                                  `5` = "$5,000 or more")

FullFile$MEDBILL <- recode_factor(FullFile$MEDBILL, `1` = "Yes", `2` = "No")

FullFile$MEDBPAY <- recode_factor(FullFile$MEDBPAY, `1` = "Yes", `2` = "No")

# I am thinking that earnings can be collapsed into three attributes:  low; medium; high
FullFile$EARNINGS <- recode_factor(FullFile$ERNYR,
                                   `1` = "$01-$34,999",
                                   `2` = "$01-$34,999",
                                   `3` = "$01-$34,999",
                                   `4` = "$01-$34,999",
                                   `5` = "$01-$34,999",
                                   `6` = "$01-$34,999",
                                   `7` = "$35,000-$74,999",
                                   `8` = "$35,000-$74,999",
                                   `9` = "$35,000-$74,999",
                                   `10` = "$35,000-$74,999",
                                   `11` = "$75,000 and over")

# Education the same:  low; medium; high
FullFile$EDUCATION <- recode_factor(FullFile$EDUC1,
                                    `0` = "HSchool Grad or less",
                                    `1` = "HSchool Grad or less",
                                    `2` = "HSchool Grad or less",
                                    `3` = "HSchool Grad or less",
                                    `4` = "HSchool Grad or less",
                                    `5` = "HSchool Grad or less",
                                    `6` = "HSchool Grad or less",
                                    `7` = "HSchool Grad or less",
                                    `8` = "HSchool Grad or less",
                                    `9` = "HSchool Grad or less",
                                    `10` = "HSchool Grad or less",
                                    `11` = "HSchool Grad or less",
                                    `12` = "HSchool Grad or less",
                                    `13` = "HSchool Grad or less",
                                    `14` = "HSchool Grad or less",
                                    `15` = "Some college or AA degree",
                                    `16` = "Some college or AA degree",
                                    `17` = "Some college or AA degree",
                                    `18` = "Bachelor's or higher",
                                    `19` = "Bachelor's or higher",
                                    `20` = "Bachelor's or higher",
                                    `21` = "Bachelor's or higher")

# Age also collapsed: low; medium; high
FullFile$AGE <- cut(FullFile$AGE_P,
                    breaks = c(-Inf, 18, 61, Inf),
                    labels = c("Less than 18", "18 to 60", "More than 60"),
                    right = FALSE)

table(FullFile$EDUC1,FullFile$EDUCATION)


OfInterest <- select(FullFile, AGE, REGION, SEX, EDUCATION, EARNINGS, PDMED12M, PNMED12M, NOTCOV, MEDBILL)

str(OfInterest)

summary(OfInterest)


#Driving Excel
#--------------
table(OfInterest$AGE,OfInterest$NOTCOV)

write.xlsx(table(OfInterest$AGE,OfInterest$NOTCOV), file = "SimpleExcelExample.xlsx")


#Finishing up the manual way
#-------------------------------
### Manual and painful way
## Create a new workbook
wb <- createWorkbook()
# education by each of the other 4 variables
NameofSheet <- "CoverageByEducation"
TheData <- table(OfInterest$EDUCATION,OfInterest$NOTCOV)
addWorksheet(wb = wb, sheetName = NameofSheet)
writeData(wb = wb, sheet = NameofSheet, x = TheData, borders = "n")
NameofSheet <- "MedbillByEducation"
TheData <- table(OfInterest$EDUCATION,OfInterest$MEDBILL)
addWorksheet(wb = wb, sheetName = NameofSheet)
writeData(wb = wb, sheet = NameofSheet, x = TheData, borders = "n")
NameofSheet <- "PDMED12MByEducation"
TheData <- table(OfInterest$EDUCATION,OfInterest$PDMED12M)
addWorksheet(wb = wb, sheetName = NameofSheet)
writeData(wb = wb, sheet = NameofSheet, x = TheData, borders = "n")
NameofSheet <- "PNMED12MByEducation"
TheData <- table(OfInterest$EDUCATION,OfInterest$PNMED12M)
addWorksheet(wb = wb, sheetName = NameofSheet)
writeData(wb = wb, sheet = NameofSheet, x = TheData, borders = "n")
# earnings  by each of the other 4 variables
NameofSheet <- "CoverageByEarnings"
TheData <- table(OfInterest$EARNINGS,OfInterest$NOTCOV)
addWorksheet(wb = wb, sheetName = NameofSheet)
writeData(wb = wb, sheet = NameofSheet, x = TheData, borders = "n")
NameofSheet <- "MedbillByEarnings"
TheData <- table(OfInterest$EARNINGS,OfInterest$MEDBILL)
addWorksheet(wb = wb, sheetName = NameofSheet)
writeData(wb = wb, sheet = NameofSheet, x = TheData, borders = "n")
NameofSheet <- "PDMED12MByEarnings"
TheData <- table(OfInterest$EARNINGS,OfInterest$PDMED12M)
addWorksheet(wb = wb, sheetName = NameofSheet)
writeData(wb = wb, sheet = NameofSheet, x = TheData, borders = "n")
NameofSheet <- "PNMED12MByEarnings"
TheData <- table(OfInterest$EARNINGS,OfInterest$PNMED12M)
addWorksheet(wb = wb, sheetName = NameofSheet)
writeData(wb = wb, sheet = NameofSheet, x = TheData, borders = "n")
# age by each of the other 4 variables
NameofSheet <- "CoverageByAge"
TheData <- table(OfInterest$AGE,OfInterest$NOTCOV)
addWorksheet(wb = wb, sheetName = NameofSheet)
writeData(wb = wb, sheet = NameofSheet, x = TheData, borders = "n")
NameofSheet <- "MedbillByAge"
TheData <- table(OfInterest$AGE,OfInterest$MEDBILL)
addWorksheet(wb = wb, sheetName = NameofSheet)
writeData(wb = wb, sheet = NameofSheet, x = TheData, borders = "n")
NameofSheet <- "PDMED12MByAge"
TheData <- table(OfInterest$AGE,OfInterest$PDMED12M)
addWorksheet(wb = wb, sheetName = NameofSheet)
writeData(wb = wb, sheet = NameofSheet, x = TheData, borders = "n")
NameofSheet <- "PNMED12MByAge"
TheData <- table(OfInterest$AGE,OfInterest$PNMED12M)
addWorksheet(wb = wb, sheetName = NameofSheet)
writeData(wb = wb, sheet = NameofSheet, x = TheData, borders = "n")
#
saveWorkbook(wb, "BetterExcelExample.xlsx", overwrite = TRUE) ## save to working directory


########################################################################
##########################################################################
#https://ibecav.github.io/customize/

#Writing functions for dplyr and ggplot2 - April 2, 2018

#-------------------------------------------

OfInterest <- read.csv("ofinterest.csv")

str(OfInterest)

#Better plotting through R
#-------------------------------
### with dplyr and ggplot
OfInterest %>%
  filter(!is.na(EDUCATION), !is.na(NOTCOV))  %>%
  group_by(EDUCATION,NOTCOV) %>%
  count() %>%
  ggplot(aes(fill=EDUCATION, y=n, x=NOTCOV)) +
  geom_bar(position="dodge", stat="identity")



OfInterest %>%
  filter(!is.na(EDUCATION), !is.na(NOTCOV))  %>%
  group_by(EDUCATION,NOTCOV) %>%
  count() %>%
  ggplot(aes(fill=EDUCATION, y=n, x=NOTCOV)) +
  geom_bar(stat="identity")


OfInterest %>%
  filter(!is.na(EDUCATION), !is.na(NOTCOV))  %>%
  group_by(EDUCATION,NOTCOV) %>%
  count() %>%
  ggplot(aes(fill=EDUCATION, y=n, x=NOTCOV)) +
  geom_bar(stat="identity", position="fill")


OfInterest %>%
  #  filter(!is.na(EDUCATION), !is.na(NOTCOV))  %>%
  group_by(EDUCATION,NOTCOV) %>%
  count() %>%
  ggplot(aes(fill=EDUCATION, y=n, x=NOTCOV)) +
  geom_bar(stat="identity", position="fill")


#------------------------------
#### THIS WON'T WORK ####
PlotMe <- function(dataframe,x,y){
  dataframe %>%
    filter(!is.na(x), !is.na(y))  %>%
    group_by(x,y) %>%
    count() %>%
    ggplot(aes(fill=x, y=n, x=y)) +
    geom_bar(position="dodge", stat="identity") ->p
  plot(p)
}

PlotMe(OfInterest,EDUCATION,NOTCOV)
#### THIS WON'T WORK ####

#makes heavy use of non standard evaluation "NSE" 
#which makes it tricky to program functions with. 

#--------------------------------
PlotMe <- function(dataframe,x,y){
  aaa <- enquo(x)
  bbb <- enquo(y)
  dataframe %>%
    filter(!is.na(!! aaa), !is.na(!! bbb))  %>%
    group_by(!! aaa,!! bbb) %>%
    count() %>%
    ggplot(aes_(fill=aaa, y=~n, x=bbb)) +
    geom_bar(position="dodge", stat="identity") ->p
  plot(p) # not strictly necessary
}

PlotMe(OfInterest,EDUCATION,NOTCOV)



#"Banging" out an NSE solution (pun intended)\
#----------------------------------------------

JustDplyr <- function(dataframe,x,y){
  aaa <- enquo(x)
  bbb <- enquo(y)
  dataframe %>%
    filter(!is.na(!! aaa), !is.na(!! bbb))  %>%
    group_by(!! aaa,!! bbb) %>%
    count() -> JustAnExample
  return(as.data.frame(JustAnExample))
}

JustDplyr(OfInterest,EDUCATION,NOTCOV)

#===================================================================
#===================================================================
PlotMe <- function(dataframe,x,y){
  aaa <- enquo(x)
  bbb <- enquo(y)
  dataframe %>%
    filter(!is.na(!! aaa), !is.na(!! bbb))  %>%
    group_by(!! aaa,!! bbb) %>%
    count() %>%
    ggplot(aes_(fill=aaa, y=~n, x=bbb)) +
    geom_bar(position="dodge", stat="identity") ->p
  #   plot(p) # not strictly necessary
}

depvars <- list(as.name("NOTCOV"), as.name("MEDBILL"))
indvars <- list(as.name("EARNINGS"), as.name("AGE"))
lapply (indvars, PlotMe, dataframe=OfInterest, y =NOTCOV)


lapply (depvars, PlotMe, dataframe=OfInterest, x =EDUCATION)



lapply(depvars, function (x) lapply(indvars, function (y) PlotMe(dataframe=OfInterest,y=y,x=x)))
#Learning from failure




xwhich <- c(6:9)
ywhich <- c(1,4,5)

colnames(OfInterest[xwhich[[1]]])

# The name of the column in OfInterest which corresponds to 6 bare PDMED12M
as.name(colnames(OfInterest[xwhich[[1]]]))


indvars<-list() # create empty list to add to
depvars<-list() # create empty list to add to
totalcombos <- 1 # keep track of where we are

for (j in seq_along(xwhich)) {
  for (k in seq_along(ywhich)) {
    depvars[[totalcombos]] <- as.name(colnames(OfInterest[xwhich[[j]]]))
    indvars[[totalcombos]] <- as.name(colnames(OfInterest[ywhich[[k]]]))
    cat("iteration #", totalcombos, 
        " xwhich=", xwhich[[j]], " depvars = ", as.name(colnames(OfInterest[xwhich[[j]]])),
        " ywhich=", ywhich[[k]], " indvars = ", as.name(colnames(OfInterest[ywhich[[k]]])),
        "\n", sep = "")
    totalcombos <- totalcombos +1
  }
}


PlotMe <- function(dataframe,x,y){
  aaa <- enquo(x)
  bbb <- enquo(y)
  dataframe %>%
    filter(!is.na(!! aaa), !is.na(!! bbb))  %>%
    group_by(!! aaa,!! bbb) %>%
    count() %>%
    ggplot(aes_(fill=aaa, y=~n, x=bbb)) +
    geom_bar(position="dodge", stat="identity") ->p
  plot(p) # now necessary
  # invisible(return(p))
  # NULL
  # return(print("Printing plot to default device"))
}

invisible(mapply(PlotMe,x=indvars,y=depvars,MoreArgs = list(dataframe=OfInterest)))

whatdidIgetback <- invisible(mapply(PlotMe,x=indvars,y=depvars,MoreArgs = list(dataframe=OfInterest)))
whatdidIgetback


#########################################################################
#########################################################################
#https://ibecav.github.io/betterfunctions2/
#https://ibecav.github.io/betterfunctions/

#https://www.r-bloggers.com/writing-better-r-functions-part-one-april-6-2018/

#Writing better R functions part one - April 6, 2018

library(dplyr)
library(openxlsx)
library(ggplot2)

### with dplyr and ggplot manually
mtcars %>%
  filter(!is.na(am), !is.na(cyl))  %>%
  group_by(am,cyl) %>%
  count() %>%
  ggplot(aes(fill=am, y=n, x=cyl)) +
  geom_bar(position="dodge", stat="identity")


#then turned it into a function after we learned about NSE:
PlotMe <- function(dataframe,x,y){
  aaa <- enquo(x)
  bbb <- enquo(y)
  dataframe %>%
    filter(!is.na(!! aaa), !is.na(!! bbb))  %>%
    group_by(!! aaa,!! bbb) %>%
    count() %>%
    ggplot(aes_(fill=aaa, y=~n, x=bbb)) +
    geom_bar(position="dodge", stat="identity") ->p
  plot(p)
}

PlotMe(mtcars,am,cyl)


#wrote some code that allows us to be more efficient if we want
#to print multiple pairings in the same data set.

# Build two vectors
xwhich <- c(2,10:11) # let's put cyl, gear, and carb in here
ywhich <- c(8:9) # let's put vs and am in here
indvars<-list() # create empty list to add to
depvars<-list() # create empty list to add to
totalcombos <- 1 # keep track of where we are

# loop through the vectors and build our lists
for (j in seq_along(xwhich)) {
  for (k in seq_along(ywhich)) {
    depvars[[totalcombos]] <- as.name(colnames(mtcars[xwhich[[j]]]))
    indvars[[totalcombos]] <- as.name(colnames(mtcars[ywhich[[k]]]))
    cat("iteration #", totalcombos, 
        " xwhich=", xwhich[[j]], " depvars = ", as.name(colnames(mtcars[xwhich[[j]]])),
        " ywhich=", ywhich[[k]], " indvars = ", as.name(colnames(mtcars[ywhich[[k]]])),
        "\n", sep = "")
    totalcombos <- totalcombos +1
  }
}

mapply(PlotMe, x=indvars, y=depvars, MoreArgs= list(dataframe=mtcars))



#Making our function better
#---------------------------

PlotMe <- function(dataframe,x,y){
  aaa <- enquo(x)
  bbb <- enquo(y)
  dataframe %>%
    filter(!is.na(!! aaa), !is.na(!! bbb))  %>%
    group_by(!! aaa,!! bbb) %>%
    count() %>%
    ggplot(aes_(fill=aaa, y=~n, x=bbb)) +
    geom_bar(position="dodge", stat="identity") +
    ggtitle("Crosstabulation of mtcars variables") +
    ylab("Count") ->p
  plot(p)
}

PlotMe(mtcars,am,cyl)



PlotMe <- function(dataframe,x,y){
  aaa <- enquo(x)
  bbb <- enquo(y)
  dfname <- enquo(dataframe)
  dataframe %>%
    filter(!is.na(!! aaa), !is.na(!! bbb))  %>%
    group_by(!! aaa,!! bbb) %>%
    count() %>%
    ggplot(aes_(fill=aaa, y=~n, x=bbb)) +
    geom_bar(position="dodge", stat="identity") +
    ggtitle(bquote("Crosstabs"~.(dfname)*.(aaa)~"by"*.(bbb))) +
    ylab("Count") ->p
  plot(p)
}

PlotMe(mtcars,am,cyl)



mtcars %>%
  filter(!is.na(am), !is.na(cyl))  %>%
  mutate(am = factor(am), cyl = factor(cyl)) %>%
  group_by(am,cyl) %>%
  count()




PlotMe <- function(dataframe,x,y){
  aaa <- enquo(x)
  bbb <- enquo(y)
  dfname <- enquo(dataframe)
  dataframe %>%
    filter(!is.na(!! aaa), !is.na(!! bbb))  %>%
    mutate(!!quo_name(aaa) := factor(!!aaa), !!quo_name(bbb) := factor(!!bbb)) %>%
    group_by(!! aaa,!! bbb) %>%
    count() %>%
    ggplot(aes_(fill=aaa, y=~n, x=bbb)) +
    geom_bar(position="dodge", stat="identity") +
    ggtitle(bquote("Crosstabs"~.(dfname)*.(aaa)~"by"*.(bbb))) +
    ylab("Count") ->p
  plot(p)
}

PlotMe(mtcars,am,cyl)



#Everyone makes mistakes
##########################
#The first few functions I wrote I didn't worry about error-checking.
#After all I was the only user and I'd be fine. Little did I know that I
#would forget the next time I used a function months later.


#If your function relies on certain libraries, test for them with a require statement.
#Good chance to set some defaults you like, such as theme_set(theme_bw()).
#Did the user pass you the right number of arguments?
#Is the first argument a valid dataframe?
#If, like me, you're asking for a dataframe and some columns in it,
    #are the variables present in the dataframe?
#What if anything will you do about missing values?
  

PlotMe <- function(dataframe,x,y){
  # error checking
  if (!require(ggplot2)) {
    stop("Can't continue can't load ggplot2")
  }
  theme_set(theme_bw())
  if (!require(dplyr)) {
    stop("Can't continue can't load dplyr")
  }
  dfname <- enquo(dataframe)
  if (length(match.call()) <= 3) {
    stop("Not enough arguments passed... requires a dataframe, plus two variables")
  }
  if (!exists(deparse(substitute(dataframe)))) {
    stop("The first item in your list does not exist")
  }
  if (!is(dataframe, "data.frame")) {
    stop("The first name you passed does not appear to be a data frame")
  }
  if (!deparse(substitute(x)) %in% names(dataframe)) {
    stop(paste0("'", deparse(substitute(x)), "' is not the name of a variable in '",deparse(substitute(dataframe)),"'"))
  }
  if (!deparse(substitute(y)) %in% names(dataframe)) {
    stop(paste0("'", deparse(substitute(y)), "' is not the name of a variable in '",deparse(substitute(dataframe)),"'"), call. = FALSE)
  }
  missing <- apply(is.na(dataframe[,c(deparse(substitute(x)),deparse(substitute(y)))]), 1, any)
  if (any(missing)) {
    warning(paste(sum(missing)), " row(s) not plotted because of missing data")
  }
  aaa <- enquo(x)
  bbb <- enquo(y)
  dataframe %>%
    filter(!is.na(!! aaa), !is.na(!! bbb))  %>%
    mutate(!!quo_name(aaa) := factor(!!aaa), !!quo_name(bbb) := factor(!!bbb)) %>%
    group_by(!! aaa,!! bbb) %>%
    count() %>%
    ggplot(aes_(fill=aaa, y=~n, x=bbb)) +
    geom_bar(position="dodge", stat="identity") +
    ggtitle(bquote("Crosstabs"~.(dfname)*.(aaa)~"by"*.(bbb))) +
    ylab("Count") ->p
  plot(p)
}

PlotMe(mtcars,am,cyl)

PlotMe(mtcars)

PlotMe(MtCaRs,am,cyl)

PlotMe(PlotMe,am,Cyl)

PlotMe(mtcars,AM,cyl) # one variable doesn't exist

PlotMe(mtcars,am,Cyl) # the other doesn't exist

MtCaR <- mtcars

MtCaR[1,2] <- NA

PlotMe(MtCaR,am,cyl) # warn about missings



#111











































