

new3 = new

y <- new3$Responders
cbind(freq=table(y), percentage=prop.table(table(y))*100)

#------------------------------------------------------------------------
#to check the cumulative frequency distribution of a categorical variable
t = data.frame(table(y))

t$cumfreq = cumsum(t$Freq)

t$cumpercent= round(t$cumfreq / sum(t$Freq)*100,2)
t


#----------------------------------------------------
#Onehot encoding for categorical variable
# with(dataset,
#      data.frame(model.matrix(~RACE-1,dataset),
#                 AGE.BELOW.21,CLASS))

# Identifying numeric variables
names(new3[sapply(new3, is.factor)])
dput(as.character(paste(names(new3[sapply(new3, is.factor)]),sep = "")))

dim(new3)
# use fullRank to avoid the 'dummy trap' automaticall convert factor var to dummy
require(caret)
dmy <- dummyVars(" ~ .", data = new3, fullRank=T)
new3 <- data.frame(predict(dmy, newdata = new3))
dim(new3)

names(new3[sapply(new3, is.numeric)])


#--------------------------------------
##### Removing identical features 
#(all should be numeric if not  need to do seperately for nemeric and categorical)

features_pair <- combn(names(new3), 2, simplify = F)

toRemove <- c()

for(pair in features_pair) {
  f1 <- pair[1]
  f2 <- pair[2]
  
  if (!(f1 %in% toRemove) & !(f2 %in% toRemove)) {
    if (all(new3[[f1]] == new3[[f2]])) {
      cat(f1, "and", f2, "are equals.\n")
      toRemove <- c(toRemove, f2)
    }
  }
}


dim(new3)
new3[,toRemove] = NULL
dim(new3)



#-------------------------------------------------
##### Removing constant features(some how NearZerovar in diff way)

dim(new3)

cat("\n## Removing the constants features.\n")
for (f in names(new3)) {
  if (length(unique(new3[[f]])) == 1) {
    cat(f, "is constant in train. We delete it.\n")
    new3[[f]] <- NULL
    
  }
}


dim(new3)

#----------------------------------------
########## Removing NearZerovar col not always the solution

#By default, a predictor is classified as near-zero variance if the percentage
#of unique values in the samples is less than {10\%} and when the frequency 
#ratio mentioned above is greater than 19 (95/5). These default values can be 
#changed by setting the arguments uniqueCut and freqCut.

# x = nearZeroVar(new3, saveMetrics = TRUE)
# 
# x[x[,"zeroVar"] > 0, ]
# x[x[,"zeroVar"] + x[,"nzv"] > 0, ]


new4 = new3

require(caret)

dim(new3)

new3 = new3[,-nearZeroVar(new3)]

dim(new3)

############################################################################
#--------------------------------------------------
#Finding high correlation features

# Identifying numeric variables
numericData <- new3[sapply(new3, is.numeric)]

# Calculate correlation matrix
descrCor <- cor(numericData)

# find attributes that are highly corrected
highlyCorrelated <- findCorrelation(descrCor, cutoff=0.7)

#================================================================
# find and remove vectors that are highly corrolated to other vectors
#searches through corrlation matrix and returns a vector of integers 
#correxponding to columns to remove to reduce pairwise correlation

HIGHCOR <- findCorrelation(cor(new3[,1:ncol(new3)]), cutoff = .7, verbose = FALSE)
dim(HIGHCOR)

grep("Responders", colnames(new3))

dim(new3)
new3 <- new3[,-HIGHCOR]

newy = new3$Responders

new3$Responders  = newy
#=====================================
# most correlate variable with  data 
cor(data,na.omit = T)
symnum(cor(data), use = "complete.obs")


#=======================================================================
# find and remove vectors that are linear combinations of other vectors
#QR decomposition is used to determine if the matrix is full rank and then identify
#the sets of columns that are involved in the dependencies. 

#To "resolve" them, columns are iteratively removed and the matrix rank is rechecked.
#The trim.matrix function in the subselect package can also be used to accomplish the same goal.

LINCOMB <- findLinearCombos(new3)
head(LINCOMB)

dim(new3)
new3 <- new3[, -LINCOMB$remove]
dim(new3)


write.csv(new3, file = "new3.csv")

#========================================
#use vif to find correation amoung more than one variable

library(usdm)
train11 = vif(new3)
train11$Variables1 <- row.names(train11$Variables)
train11[order(train11$VIF,decreasing = T), ]


xx = data.frame(train11$Variables,round(train11$VIF,3))
write.csv(xx, file = "MyData.csv")

dim(new3)
#columns removing vif > 50
tempcol = c("OCCUP_ALL_NEW.1","OCCUP_ALL_NEW.2","OCCUP_ALL_NEW.5","OCCUP_ALL_NEW.6","OCCUP_ALL_NEW.7","OCCUP_ALL_NEW.8","ATM_D_prev1","count_C_prev1","count_D_prev1","COUNT_ATM_D_prev1","COUNT_BRANCH_C_prev1","COUNT_IB_D_prev1","custinit_DR_cnt_prev1","ATM_amt_prev1","ATM_CW_Amt_prev1","ATM_CW_Cnt_prev1","CNR_prev1","ATM_D_prev2","count_C_prev2","count_D_prev2","COUNT_ATM_D_prev2","COUNT_BRANCH_C_prev2","COUNT_IB_D_prev2","custinit_CR_cnt_prev2","custinit_DR_cnt_prev2","ATM_amt_prev2","ATM_CW_Amt_prev2","ATM_CW_Cnt_prev2","CNR_prev2","ATM_D_prev3","count_C_prev3","count_D_prev3","COUNT_ATM_D_prev3","COUNT_BRANCH_C_prev3","COUNT_IB_D_prev3","custinit_DR_cnt_prev3","ATM_amt_prev3","ATM_CW_Amt_prev3","ATM_CW_Cnt_prev3","CNR_prev3","CR_AMB_Prev3","ATM_D_prev4","count_C_prev4","count_D_prev4","COUNT_ATM_D_prev4","COUNT_BRANCH_C_prev4","COUNT_IB_D_prev4","ATM_amt_prev4","ATM_CW_Amt_prev4","ATM_CW_Cnt_prev4","BRN_CW_Amt_prev4","BRN_CW_Cnt_prev4","CNR_prev4","BAL_prev4","CR_AMB_Prev4","ATM_D_prev5","count_C_prev5","count_D_prev5","COUNT_ATM_D_prev5","COUNT_BRANCH_C_prev5","COUNT_IB_D_prev5","custinit_DR_cnt_prev5","ATM_amt_prev5","ATM_CW_Amt_prev5","ATM_CW_Cnt_prev5","CNR_prev5","BAL_prev5","CR_AMB_Prev5","ATM_D_prev6","count_C_prev6","count_D_prev6","COUNT_ATM_D_prev6","COUNT_BRANCH_C_prev6","COUNT_IB_D_prev6","custinit_DR_cnt_prev6","ATM_amt_prev6","ATM_CW_Amt_prev6","ATM_CW_Cnt_prev6","CNR_prev6","BAL_prev6","CR_AMB_Prev6","FD_AMOUNT_BOOK_PrevQ1","RD_AMOUNT_BOOK_PrevQ1","Total_Invest_in_MF_PrevQ1","count_No_of_MF_PrevQ1","count_No_of_MF_PrevQ2","AL_TAG_LIVE","RD_TAG_LIVE","AL_DATE","CASH_WD_CNT_Last6","I_AQB_PrevQ1","I_AQB_PrevQ2","I_CR_AQB_PrevQ1","I_CR_AQB_PrevQ2","I_CNR_PrevQ1","I_CNR_PrevQ2","vintagee.1","vintagee.2","vintagee.3","vintagee.4","vintagee.5","vintagee.6","vintagee.7","vintagee.8","vintagee.9","vintagee.11","vintagee.12","vintagee.13","vintagee.14")
new3[,tempcol] = NULL
dim(new3)

#columns removing vif > 10
tempcol2 = c("vintage.1","vintage.2","vintage.3","vintage.4","vintage.5","vintage.6","vintage.7","vintage.8","vintage.9","vintage.10","vintage.11","vintage.12","vintage.13","vintage.14","OCCUP_ALL_NEW.1","OCCUP_ALL_NEW.2","OCCUP_ALL_NEW.3","OCCUP_ALL_NEW.4","OCCUP_ALL_NEW.5","OCCUP_ALL_NEW.6","OCCUP_ALL_NEW.7","OCCUP_ALL_NEW.8","FINAL_WORTH_prev1.1","FINAL_WORTH_prev1.2","FINAL_WORTH_prev1.3","ENGAGEMENT_TAG_prev1.1","ENGAGEMENT_TAG_prev1.2","ENGAGEMENT_TAG_prev1.3","C_prev1","ATM_D_prev1","BRANCH_C_prev1","count_C_prev1","count_D_prev1","COUNT_ATM_D_prev1","COUNT_BRANCH_C_prev1","COUNT_BRANCH_D_prev1","COUNT_IB_C_prev1","COUNT_IB_D_prev1","COUNT_POS_D_prev1","ATM_amt_prev1","ATM_CW_Amt_prev1","ATM_CW_Cnt_prev1","CNR_prev1","BAL_prev1","EOP_prev1","CR_AMB_Prev1","C_prev2","ATM_D_prev2","BRANCH_C_prev2","count_C_prev2","count_D_prev2","COUNT_ATM_D_prev2","COUNT_BRANCH_C_prev2","COUNT_BRANCH_D_prev2","COUNT_IB_D_prev2","COUNT_POS_D_prev2","ATM_amt_prev2","ATM_CW_Amt_prev2","ATM_CW_Cnt_prev2","CNR_prev2","BAL_prev2","EOP_prev2","CR_AMB_Prev2","C_prev3","ATM_D_prev3","BRANCH_C_prev3","count_C_prev3","count_D_prev3","COUNT_ATM_D_prev3","COUNT_BRANCH_C_prev3","COUNT_BRANCH_D_prev3","COUNT_IB_D_prev3","COUNT_POS_D_prev3","ATM_amt_prev3","ATM_CW_Amt_prev3","ATM_CW_Cnt_prev3","BRN_CASH_Dep_Amt_prev3","BRN_CASH_Dep_Cnt_prev3","CNR_prev3","BAL_prev3","EOP_prev3","CR_AMB_Prev3","C_prev4","ATM_D_prev4","BRANCH_C_prev4","count_C_prev4","count_D_prev4","COUNT_ATM_D_prev4","COUNT_BRANCH_C_prev4","COUNT_BRANCH_D_prev4","COUNT_IB_C_prev4","COUNT_IB_D_prev4","COUNT_POS_D_prev4","ATM_amt_prev4","ATM_CW_Amt_prev4","ATM_CW_Cnt_prev4","CNR_prev4","BAL_prev4","EOP_prev4","CR_AMB_Prev4","C_prev5","ATM_D_prev5","BRANCH_C_prev5","count_C_prev5","count_D_prev5","COUNT_ATM_D_prev5","COUNT_BRANCH_C_prev5","COUNT_BRANCH_D_prev5","COUNT_IB_C_prev5","COUNT_IB_D_prev5","COUNT_POS_D_prev5","ATM_amt_prev5","ATM_CW_Amt_prev5","ATM_CW_Cnt_prev5","BRN_CASH_Dep_Amt_prev5","BRN_CASH_Dep_Cnt_prev5","CNR_prev5","BAL_prev5","EOP_prev5","CR_AMB_Prev5","C_prev6","ATM_D_prev6","BRANCH_C_prev6","count_C_prev6","count_D_prev6","COUNT_ATM_D_prev6","COUNT_BRANCH_C_prev6","COUNT_BRANCH_D_prev6","COUNT_IB_C_prev6","COUNT_IB_D_prev6","COUNT_POS_D_prev6","ATM_amt_prev6","ATM_CW_Amt_prev6","ATM_CW_Cnt_prev6","CNR_prev6","BAL_prev6","EOP_prev6","CR_AMB_Prev6","FD_AMOUNT_BOOK_PrevQ1","NO_OF_FD_BOOK_PrevQ1","NO_OF_FD_BOOK_PrevQ2","NO_OF_RD_BOOK_PrevQ1","NO_OF_RD_BOOK_PrevQ2","RD_AMOUNT_BOOK_PrevQ1","Total_Invest_in_MF_PrevQ1","Total_Invest_in_MF_PrevQ2","count_No_of_MF_PrevQ1","count_No_of_MF_PrevQ2","Dmat_Investing_PrevQ1","Dmat_Investing_PrevQ2","OTHER_LOANS_PREM_CLOSED_PREVQ1","AL_TAG_LIVE","BL_TAG_LIVE","CV_TAG_LIVE","GL_TAG_LIVE","MF_TAG_LIVE","OTHER_LOANS_TAG_LIVE","PL_TAG_LIVE","RD_TAG_LIVE","FD_TAG_LIVE","TWL_TAG_LIVE","lap_tag_live","AL_DATE","BL_DATE","CV_DATE","GL_DATE","LAP_DATE","OTHER_LOANS_DATE","PL_DATE","TWL_DATE","CASH_WD_CNT_Last6","I_AQB_PrevQ1","I_AQB_PrevQ2","I_CR_AQB_PrevQ1","I_CR_AQB_PrevQ2","I_CNR_PrevQ1","I_CNR_PrevQ2","CR_AMB_Drop_Build_3","CR_AMB_Drop_Build_4","CR_AMB_Drop_Build_5","Complaint_Logged_PrevQ1","Complaint_Resolved_PrevQ1")
new3[,tempcol2] = NULL
dim(new3)


#columns removing vif > 25
tempcol3 = c("vintage.1","vintage.2","vintage.3","vintage.4","vintage.5","vintage.6","vintage.7","vintage.8","vintage.9","vintage.10","vintage.11","vintage.12","vintage.13","vintage.14","OCCUP_ALL_NEW.1","OCCUP_ALL_NEW.2","OCCUP_ALL_NEW.5","OCCUP_ALL_NEW.6","OCCUP_ALL_NEW.7","OCCUP_ALL_NEW.8","FINAL_WORTH_prev1.1","FINAL_WORTH_prev1.2","FINAL_WORTH_prev1.3","ENGAGEMENT_TAG_prev1.1","ENGAGEMENT_TAG_prev1.2","ENGAGEMENT_TAG_prev1.3","ATM_D_prev1","count_C_prev1","count_D_prev1","COUNT_ATM_D_prev1","COUNT_BRANCH_C_prev1","COUNT_IB_D_prev1","ATM_amt_prev1","ATM_CW_Amt_prev1","ATM_CW_Cnt_prev1","CNR_prev1","BAL_prev1","ATM_D_prev2","count_C_prev2","count_D_prev2","COUNT_ATM_D_prev2","COUNT_BRANCH_C_prev2","COUNT_IB_D_prev2","ATM_amt_prev2","ATM_CW_Amt_prev2","ATM_CW_Cnt_prev2","CNR_prev2","BAL_prev2","ATM_D_prev3","count_C_prev3","count_D_prev3","COUNT_ATM_D_prev3","COUNT_BRANCH_C_prev3","COUNT_IB_D_prev3","ATM_amt_prev3","ATM_CW_Amt_prev3","ATM_CW_Cnt_prev3","BAL_prev3","EOP_prev3","CR_AMB_Prev3","ATM_D_prev4","count_C_prev4","count_D_prev4","COUNT_ATM_D_prev4","COUNT_BRANCH_C_prev4","COUNT_IB_D_prev4","ATM_amt_prev4","ATM_CW_Amt_prev4","ATM_CW_Cnt_prev4","CNR_prev4","BAL_prev4","CR_AMB_Prev4","ATM_D_prev5","count_C_prev5","count_D_prev5","COUNT_ATM_D_prev5","COUNT_BRANCH_C_prev5","COUNT_IB_D_prev5","ATM_amt_prev5","ATM_CW_Amt_prev5","ATM_CW_Cnt_prev5","CNR_prev5","BAL_prev5","CR_AMB_Prev5","ATM_D_prev6","count_C_prev6","count_D_prev6","COUNT_ATM_D_prev6","COUNT_BRANCH_C_prev6","COUNT_IB_D_prev6","ATM_amt_prev6","ATM_CW_Amt_prev6","ATM_CW_Cnt_prev6","CNR_prev6","BAL_prev6","CR_AMB_Prev6","FD_AMOUNT_BOOK_PrevQ1","RD_AMOUNT_BOOK_PrevQ1","Total_Invest_in_MF_PrevQ1","Total_Invest_in_MF_PrevQ2","count_No_of_MF_PrevQ1","count_No_of_MF_PrevQ2","AL_TAG_LIVE","GL_TAG_LIVE","MF_TAG_LIVE","OTHER_LOANS_TAG_LIVE","PL_TAG_LIVE","RD_TAG_LIVE","FD_TAG_LIVE","AL_DATE","GL_DATE","OTHER_LOANS_DATE","PL_DATE","CASH_WD_CNT_Last6","I_AQB_PrevQ1","I_AQB_PrevQ2","I_CR_AQB_PrevQ1","I_CR_AQB_PrevQ2","I_CNR_PrevQ1","I_CNR_PrevQ2","CR_AMB_Drop_Build_4","CR_AMB_Drop_Build_5")

#=========================================\
# To identify the factor mismatch in Train & Test
#for (n in names(train.imp.com))
#  if (is.factor(train.imp.com[[n]])) {
#      if (length(levels(train.imp.com[[n]])) != length(levels(test.imp.com[[n]]))) {    
#            print(n)     
#    }     
#  }


# ####################################################
# #FEATURE ENGENEERING
# #================================================
# #PCA computed using Covariance/correlation matrix
# pca = princomp(data, cor=F)
# names(pca)
# summary(pca)
# plot(pca, type="lines")
# pca$loadings
# pca$scores
# sum(diag(cov(pca$scores)))
# 
# #=========================
# # pca using caret package
# library(caret)
# preObj = preProcess(data, method=c("pca"), thresh = 1.0)
# preObj$rotation
# newdata = predict(preObj,data)
# 
# #=======================
# #PCA computed using SVD
# pca = prcomp(data, scale.=TRUE)
# names(pca)
# summary(pca)
# pca$rotation
# pca$x
# plot(pca, type="lines")

####################################################################################
# MODEL BULDING

new4 = new3

#logistic regression #without PCA

table(new3$mark)

train = new3[new3$mark == 5,]
dim(train)

test = new3[new3$mark == 6,]
dim(test)

train$mark = NULL
test$mark = NULL
test$Responders = NULL  


train$Responders = as.factor(train$Responders)

dt = sort(sample(nrow(train), nrow(train)*.7))
trn <- train[dt, ]
val <- train[-dt, ]

table(train$Responders)
table(trn$Responders)
table(val$Responders)

dim(trn)
dim(val)

trn_y = trn$Responders
val_y = val$Responders


# varNames = names(trn_y)
# # Exclude ID or Response variable
# varNames <- varNames[!varNames %in% c("Responders")]
# 
# # add + sign between exploratory variables
# varNames1 <- paste(varNames, collapse = "+")
# 
# # Add response variable and convert to a formula object
# glm.form <- as.formula(paste("Responders", varNames1, sep = " ~ "))


grep("Responders", colnames(trn))


model = glm(trn$Responders ~., data=trn[,-194],family = binomial(link="logit"))

summary(model) # display results
confint(model) # 95% CI for the coefficients
exp(coef(model)) # exponentiated coefficients
exp(confint(model)) # 95% CI for exponentiated coefficients
predict(model, type="response") # predicted values
residuals(model, type="deviance") # residuals 
vif(model)


pr = predict.glm(model, val[,-194], type="response")

data.frame(round(pr,3))
summary(round(pr,3))

library(InformationValue)
optCutOff <- optimalCutoff(val_y, pr)[1]

#Misclassification Error
#-------------------------
InformationValue::misClassError(val_y, pr, threshold = optCutOff)

InformationValue::confusionMatrix(val_y, pr, threshold = optCutOff)


#ROC
#-----
plotROC(val_y, pr)

Concordance(val_y, pr)

InformationValue::sensitivity(val_y, pr, threshold = optCutOff)

InformationValue::specificity(val_y, pr, threshold = optCutOff)

table(val_y)


require(caTools)
pr=ifelse(is.na(pr),mean(!is.na(pr)),pr)
cat('AUC:',colAUC(pr,val_y),'\n')



### Plot the probability of bad credit
histogram(~pr|val_y,
          
          layout = c(2, 1),
          nint = 20,
          xlab = "Probability of Bad Credit",
          type = "count")


#Gain and Lift Chart 
lift <- function(depvar, predcol, groups=10) {
  if(!require(dplyr)){
    install.packages("dplyr")
    library(dplyr)}
  if(is.factor(depvar)) depvar <- as.integer(as.character(depvar))
  if(is.factor(predcol)) predcol <- as.integer(as.character(predcol))
  helper = data.frame(cbind(depvar, predcol))
  helper[,"bucket"] = ntile(-helper[,"predcol"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(depvar), funs(total = n(),
                                    totalresp=sum(., na.rm = TRUE))) %>%
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups)))
  return(gaintable)
}


dt = lift(val_y , pr, groups = 10)


#plot Cumulative Lift
graphics::plot(dt$bucket, dt$Cumlift, type="l", ylab="Cumulative lift", xlab="Bucket")


#----------------------------------------------------------
# Using ROCR package 

install.packages("ROCR")
library(ROCR)

#pred<-prediction(actual,predicted)
rocr_obj<-prediction(pr,val_y)


auc<-performance(rocr_obj,"auc")
unlist(auc@y.values)

Roc.obj <- performance(rocr_obj, measure="tpr", x.measure="fpr")
plot(roc.obj,main="Cross-Sell - ROC Curves",
     xlab="1 ??? Specificity: False Positive Rate",
     ylab="Sensitivity: True Positive Rate",
     col="blue")
abline(0,1,col="grey")


gain.obj<-performance(rocr_obj,"tpr","fpr")
plot(gain.obj,col="red", lwd=2,main="ROC curve")
abline(0,1, lty = 8, col = "grey")


#Getting Lift Charts in R
#--------------------------
lift.obj <- performance(rocr_obj, measure="lift", x.measure="rpp")
plot(lift.obj,main="Cross-Sell - Lift Chart",
     xlab="% Populations",
     ylab="Lift",col="blue",colorize=TRUE)
abline(1,0,col="grey")



#Cumulative Lift Chart using R
#------------------------------
#gains package in R could be used for creating gains table and also plotting cumulative lift chart. 
install.packages("gains")
library(gains) 

gains.cross <- gains(as.numeric(pr1), as.numeric(val_y), groups=10)
print(gains.cross)

# gains table
actual <- ifelse(val_y == 1,1,0)
gains.cross <- gains(actual=actual , 
                     predicted=pr,
                     groups=10)
print(gains.cross)
                     
#######################################################################################
#===============================================================================

#######################################################################################
#===============================================================================

# varNames = names(trn_y)
# # Exclude ID or Response variable
# varNames <- varNames[!varNames %in% c("Responders")]
# 
# # add + sign between exploratory variables
# varNames1 <- paste(varNames, collapse = "+")
# 
# # Add response variable and convert to a formula object
# glm.form <- as.formula(paste("Responders", varNames1, sep = " ~ "))

grep("Responders", colnames(trn))

# create model using recursive partitioning on the training data set
require(rpart)
x.rp <- rpart(trn$Responders ~. , data=trn[,-333], method="class",
        control = rpart.control(cp=0, minsplit = 2, minbucket = 7, 
                                maxdepth = 10, usesurrogate = 2, xval =10 ))
printcp(x.rp) 
plotcp(x.rp) 
summary(x.rp) 

pruned <- prune(x.rp, cp = bestcp)
pfit<- prune(x.rp, cp= fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])  

pfit<- prune(x.rp, cp=0.01160389)    # prune the tree  from cptable 

CV.fit = xpred.rpart(pfit, xval = 10, cp, return.all = FALSE)   #Return Cross-Validated Predictions

# predict classes for the evaluation data set
x.rp.pred <- predict(x.rp, type="class", newdata=val[,-333])
# score the evaluation data set (extract the probabilities)
x.rp.prob <- predict(x.rp, type="prob", newdata=val[,-333])


# To view the decision tree, uncomment this line.
# plot(x.rp, main="Decision tree created using rpart")

#---------------------------------------------
# create model using conditional inference trees
require(party)
x.ct <- ctree(trn$Responders ~., data=trn[,-333])
x.ct.pred <- predict(x.ct, newdata=val[,-333])
x.ct.prob <-  1- unlist(treeresponse(x.ct, val[,-333]), use.names=F)[seq(1,nrow(val[,-333])*2,2)]

# To view the decision tree, uncomment this line.
# plot(x.ct, main="Decision tree created using condition inference trees")



#-----------------------------------------------------------------------------------------
# create model using random forest and bagging ensemble using conditional inference trees
x.cf <- cforest(trn$Responders ~., data=trn[,-194], control = cforest_unbiased(mtry = ncol(trn)-2))
x.cf.pred <- predict(x.cf, val[,-194])
x.cf.prob <-  1- unlist(treeresponse(x.cf, val[,-194]), use.names=F)[seq(1,nrow(val[,-194])*2,2)]


# create model using random forest and bagging ensemble using conditional inference trees


#######################################################################################
#===============================================================================

#----------------------------------------------------------------
data.frame(sort(colSums(is.na(alldata))))

data.frame(sort((colSums(is.na(alldata))/nrow(alldata))*100))


train = alldata[alldata$mark==5,]
dim(train)
train$yy = target
train$mark = NULL

train$yy= as.factor(target)

train1 = train[,colSums(is.na(train)) == 0]

library(e1071)

model <- naiveBayes(yy ~ ., data = train1)

test1 = train1
test1$yy = NULL

preds <- predict(model, newdata = test1[1:1000,], type = c("class"))

conf_matrix <- table(preds,target[1:1000])


nb_laplace1 <- naiveBayes(yy~., data=train1, laplace=5)
laplace1_pred <- predict(nb_laplace1, test1[1:1000,], type="class")

table(laplace1_pred,target[1:1000])


############################################################