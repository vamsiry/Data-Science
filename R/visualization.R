

data<-data.frame(Stat11=rnorm(100,mean=3,sd=2),
                 Stat21=rnorm(100,mean=4,sd=1),
                 Stat31=rnorm(100,mean=6,sd=0.5),
                 Stat41=rnorm(100,mean=10,sd=0.5),
                 Stat12=rnorm(100,mean=4,sd=2),
                 Stat22=rnorm(100,mean=4.5,sd=2),
                 Stat32=rnorm(100,mean=7,sd=0.5),
                 Stat42=rnorm(100,mean=8,sd=3),
                 Stat13=rnorm(100,mean=6,sd=0.5),
                 Stat23=rnorm(100,mean=5,sd=3),
                 Stat33=rnorm(100,mean=8,sd=0.2),
                 Stat43=rnorm(100,mean=4,sd=4))


boxplot(data)

boxplot(data,las = 2)

boxplot(data, las = 2, par(mar = c(12, 5, 4, 2)+ 0.1))

###############################################################################
#What Is A Histogram?

#A histogram is a visual representation of the distribution of a dataset. 
#As such, the shape of a histogram is its most obvious and informative
#characteristic: 

#it allows you to easily see where a relatively large 
#amount of the data is situated and where there is very little data to be 
#found (Verzani 2004). 


#histogram consists of an x-axis, an y-axis and various bars #of different
#heights. The y-axis shows how frequently the values on the x-axis occur 
#in the data, while the bars group ranges of values or continuous categories
#on the x-axis. The latter explains why histograms don't have gaps between the bars.


hist(AirPassengers)


hist(AirPassengers, 
     main="Histogram for Air Passengers", 
     xlab="Passengers", 
     border="blue", 
     col="green",
     xlim=c(100,700),
     las=1, 
     breaks=5)


hist(AirPassengers, main="Histogram for Air Passengers") 


#outlierss of the column
#===========================
boxplot(rnorm(100,mean=0,sd=10),plot = FALSE)$out

#Grab the outliers
outliers = boxplot(dd$x, plot=FALSE)$out

#Extract the outliers from the original data frame
dd[dd$x %in% outliers,]


library(ggplot2)
#How to label all the outliers in a boxplot
set.seed(492)
y <- rnorm(2000)
x1 <- sample(letters[1:2], 2000,T)
x2 <- sample(letters[1:2], 2000,T)
lab_y <- sample(letters[1:4], 2000,T)
# plot a boxplot with interactions:
boxplot.with.outlier.label(y~x2*x1, lab_y)



ggplot(data=iris,aes(x=Sepal.Width,y=Sepal.Length,colour=Species))+geom_point()
ggplot(data=iris,aes(x=Sepal.Width,y=Sepal.Length))+geom_point(aes(colour=Species))
ggplot()+geom_point(data=iris,aes(x=Sepal.Width,y=Sepal.Length,colour=Species))


#===================================================================
library(tidyverse)

url <- "http://varianceexplained.org/files/Brauer2008_DataSet1.tds"

cleaned_data1 <- read_delim(url, delim = "\t")

names(cleaned_data1)
head(cleaned_data1$NAME)

# Clean and tidy the data
cleaned_data <- read_delim(url, delim = "\t") %>%
  separate(NAME, c("name", "BP", "MF", "systematic_name", "number"), sep = "\\|\\|") %>%
  mutate_at(vars(name:systematic_name), funs(trimws)) %>%
  select(-number, -GID, -YORF, -GWEIGHT) %>%
  gather(sample, expression, G0.05:U0.3) %>%
  separate(sample, c("nutrient", "rate"), sep = 1, convert = TRUE) %>%
  filter(!is.na(expression), systematic_name != "")



# Visualize a set of four genes
cleaned_data %>%
  filter(BP == "leucine biosynthesis") %>%
  ggplot(aes(rate, expression, color = nutrient)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~name + systematic_name)


library(knitr)
knit('001-minimal.Rmd')








