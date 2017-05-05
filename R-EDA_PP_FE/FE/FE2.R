
#feature engg-basics.R
#======================
library(caret)
setwd("C:\\Users\\Vamsi\\Desktop\\R.Alg\\practice\\data sets\\datasets\\")

winedata = read.csv("wine.txt", header = TRUE)
dim(winedata)
str(winedata)

#Remove the variables which have 95% NAs
#=========================================
threshold_val = 0.95 * dim(winedata)[1]
include_cols = !apply(winedata, 2, function(y) sum(is.na(y)) > threshold_val)
winedata = winedata[, include_cols]


#Find the variables which have very less variance
#=================================================
nearZvar = nearZeroVar(winedata, saveMetrics = TRUE)
winedata = winedata[nearZvar$nzv==FALSE]
cor(winedata)

#variance-based-filteration--------------
setwd("D:\\digit_recognizer")
digit_train = read.csv("train.csv", header = TRUE, na.strings=c("NA",""))

nzv_obj = nearZeroVar(digit_train, saveMetrics = T)
digit_train1 = digit_train[,nzv_obj$zeroVar==F]
dim(digit_train1)
digit_train2 = digit_train[,nzv_obj$nzv==F]
dim(digit_train2)

#Find the variables which are highly correlated
#==============================================
corr_matrix = abs(cor(winedata))
diag(corr_matrix) = 0
correlated_col = findCorrelation(corr_matrix, verbose = FALSE , cutoff = .60)
winedata = winedata[-correlated_col]
cor(winedata)
dim(winedata)

###############################################################
#correlation-based-filteration.R
#==================================
library(caret)
library(corrplot)

setwd("C:\\Users\\Vamsi\\Desktop\\R.Alg\\practice\\data sets\\datasets\\restaurent-rp")

restaurant_train = read.csv("train.csv", header = TRUE, na.strings=c("NA",""))
dim(restaurant_train)
str(restaurant_train)
restaurant_train1 = restaurant_train[,-1]
str(restaurant_train1)

# picking only numerical attributes for correlation matrix
numeric_attr = sapply(restaurant_train1, is.numeric)
correlations = cor(restaurant_train1[,numeric_attr])
#plotting correlation matrix
X11()
corrplot(correlations)
corrplot(correlations, order = "hclust")
corrplot(correlations, order = "hclust", addrect=3)

# finding highly correlated featues using correlation matrix
filtered_features_correlation = findCorrelation(abs(correlations), cutoff = 0.95)
restaurant_train1 = restaurant_train[,-filtered_features_correlation]


#covariance-correlation.R
#=========================
library(ggplot2)
stock_plot = function(s1,s2) {
  df = data.frame(a=s1,b=s2)
  X11()
  print(ggplot(df) + geom_point(aes(x = a, y = b)))
  print(cov(df$a, df$b))
  print(cor(df$a, df$b))
}

s1 = c(100, 200, 300, 400)
s2 = c(10, 20, 30, 50)
stock_plot(s1,s2)


s3 = c(100, 200, 300,  400)
s4 = c(50, 40, 35, 32)
stock_plot(s3, s4)

s5 = c(100, 200, 300, 400)
s6 = c(1, 2, 3, 5)
stock_plot(s5,s6)

s7 = c(100, 200, 300, 400)
s8 = c(500, 600, 700, 800)
stock_plot(s7,s8)

#why mean of z-scores is 0?
x = c(10,20,30,40, 50, 60, 70)
x_z = (x - mean(x) ) / sd(x)
df = data.frame(x, x_z)
mean(x)
mean(x_z)

######################################################################
#eigenvectors.R
#===============
migration = matrix(c(.9,.05,.1,.95),2,2,byrow = TRUE)
#initial_population = c(300,100)
#initial_population = c(200,100)
initial_population = c(100,100)
initial_population_mat = as.matrix(initial_population)

after_population_frame = data.frame(v=c(),h=c())
for(i in 1:100) {
  after_population = migration %*% initial_population_mat
  after_population_frame[i,1] = round(after_population[1,1])
  after_population_frame[i,2] = round(after_population[2,1])
  initial_population_mat = after_population
}

e = eigen(migration)
