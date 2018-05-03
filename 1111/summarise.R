


x = data.frame(a = sample(1:20,100,T),b = sample(20:50,100,T))
class(x)

x %>% select(a,b) %>% summarise(tt = length(x$a[x$a>10]),ttt= length(x$a[x$a>20])) %>%
  
  x %>% mutate(ff = findInterval(x$a, seq(0.5, 1, by=0.1))) %>% group_by(ff)%>% summarise(tt = length(a))

x %>% mutate(ff = cut(x$a,c(0,5,10,15,21),labels = 1:4)) %>% group_by(ff)%>%
  summarise(tt = length(a))


x %>% group_by(a) %>% summarise(n())

x %>% group_by(a) %>% summarise(ff = n(),tt = mean(b),gg = sd(b))

x %>% group_by(a) %>% summarise(ff = n(),tt = mean(b),gg = sd(b)) %>% filter(gg>15)


x %>% filter(complete.cases(x)) %>% group_by(a) %>% summarise(ff = n(),tt = mean(b),gg = sd(b)) 


x %>%  summarise(ff = length(x$a[x$a==15])) 



findInterval(x$a, seq(0.5, 1, by=0.1))





findInterval(seq(0, 1, l=20), seq(0.5, 1, by=0.1))


##################################################################################
#https://www.analyticsvidhya.com/blog/2015/12/7-important-ways-summarise-data/
#Methods to Summarise Data in R

#1. apply
m <- matrix(c(1:10, 11:20), nrow = 10, ncol = 2)
apply(m, 1, mean)


#2. lapply
l <- list(a = 1:10, b = 11:20)
lapply(l, mean)


#3.  sapply
l <- list(a = 1:10, b = 11:20) 
l.mean <- sapply(l, mean)
class(l.mean)

#4. tapply
attach(iris)
# mean petal length by species
tapply(iris$Petal.Length, Species, mean)


#5. by
attach(iris)
by(iris[, 1:4], Species, colMeans)

#6. sqldf
install.packages("sqldf")
library(sqldf)
attach(iris)

                       
##########################################################################################                       


### Generate a random data set                                                                                                                                                  
data <- data.frame(names=c("Type1","Type2")[as.numeric((runif(n=100)>=0.5))+1],data=rnorm(100,100,sd=25))

### Use the aggregate function to split and get the mean of the data                                                                                                            
aggregate(data$data,list(data$names),mean)

### Use the sapply and split functions to do the same thing                                                                                                                     
s <- split(data$data,list(data$names))
sapply(s,mean)
# Or the same thing in one line                                                                                                                                                 
sapply(split(data$data,list(data$names)),mean)

### Below is a function which is just a group of commands used to stop                                                                                                          
### you having to type the same code in again for another dataset                                                                                                               

plotData <- function(data,cols){
  ### Draw a box plot of the data                                                                                                                                                 
  plot(data$data ~ data$names,col=cols,pch=20)
  ### Run a t-test on the split data                                                                                                                                              
  pval <- t.test(data$data ~ data$names)$p.value
  ### Are they significantly different ?                                                                                                                                          
  areSig <- c("Not Significant","Significant")[as.numeric(pval<=0.05)+1]
  ### Calculate the density of the data, after splitting                                                                                                                          
  dens <- lapply(split(data$data,data$names),density)
  ### Draw an empty figure with the correct x and y limits of the data                                                                                                            
  plot(1,xlim=c(0,max(sapply(dens,function(x) max(x$x)))),ylim=c(0,max(sapply(dens,function(x) max(x$y)))))
  ### Draw the density plots for each data type                                                                                                                                   
  lapply(1:length(dens),function(x) lines(dens[[x]],col=cols[x],lwd=3))
  ### Add a legens                                                                                                                                                                
  legend("topleft",legend=names(dens),col=cols,lwd=4)
  ### Add a title with the p-value and wether it is significant or not                                                                                                            
  title(paste("P-value=",format.pval(pval),areSig))
}

### Draw figures in a 2 x 2 grid                                                                                                                                                
par(mfrow=c(2,2))
### Run the plotData function on the data object                                                                                                                                
plotData(data,cols=brewer.pal(8,"Dark2"))

### Make a new version of the data object, which should be significantly different, as they have different means                                                                
data <- data.frame(names=rep(c("Type3","Type4"),each=50),data=c(rnorm(50,100,sd=20),rnorm(50,50,sd=10)))
### Plot the new version of the data                                                                                                                                            
plotData(data,cols=brewer.pal(8,"Set1"))