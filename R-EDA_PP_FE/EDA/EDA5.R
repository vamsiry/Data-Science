#ggplot2 allows you to create graphs that represent both univariate and 
#multivariate numerical and categorical data in a straightforward manner. 
#Grouping can be represented by color, symbol, size, and transparency. 
#The creation of trellis plots (i.e., conditioning) is relatively simple. 

#There is a helper function called qplot() (for quick plot) that can hide much of this complexity when creating standard graphs.
qplot(x, y, data=, color=, shape=, size=, alpha=, geom=, method=, formula=, facets=, xlim=, ylim= xlab=, ylab=, main=, sub=) 



#Here are some examples using automotive data (car mileage, weight, number of gears, number of cylinders, etc.) contained in the mtcars data frame. 
# ggplot2 examples
library(ggplot2)

# create factors with value labels
mtcars$gear <- factor(mtcars$gear,levels=c(3,4,5),
                      labels=c("3gears","4gears","5gears"))
mtcars$am <- factor(mtcars$am,levels=c(0,1),
                    labels=c("Automatic","Manual"))
mtcars$cyl <- factor(mtcars$cyl,levels=c(4,6,8),
                     labels=c("4cyl","6cyl","8cyl"))

# Kernel density plots for mpg
# grouped by number of gears (indicated by color)
qplot(mpg, data=mtcars, geom="density", fill=gear, alpha=I(.5),
      main="Distribution of Gas Milage", xlab="Miles Per Gallon",
      ylab="Density")

# Scatterplot of mpg vs. hp for each combination of gears and cylinders
# in each facet, transmittion type is represented by shape and color
qplot(hp, mpg, data=mtcars, shape=am, color=am,
      facets=gear~cyl, size=I(3),
      xlab="Horsepower", ylab="Miles per Gallon")

# Separate regressions of mpg on weight for each number of cylinders
qplot(wt, mpg, data=mtcars, geom=c("point", "smooth"),
      method="lm", formula=y~x, color=cyl,
      main="Regression of MPG on Weight",
      xlab="Weight", ylab="Miles per Gallon")

# Boxplots of mpg by number of gears
# observations (points) are overlayed and jittered
qplot(gear, mpg, data=mtcars, geom=c("boxplot", "jitter"),
      fill=gear, main="Mileage by Gear Number",
      xlab="", ylab="Miles per Gallon") 



#ggplot2 functions can be chained with "+" signs to generate the final plot.
library(ggplot2)

p <- qplot(hp, mpg, data=mtcars, shape=am, color=am,
           facets=gear~cyl, main="Scatterplots of MPG vs. Horsepower",
           xlab="Horsepower", ylab="Miles per Gallon")

# White background and black grid lines
p + theme_bw()

# Large brown bold italics labels
# and legend placed at top of plot
p + theme(axis.title=element_text(face="bold.italic",
                                  size="12", color="brown"), legend.position="top") 