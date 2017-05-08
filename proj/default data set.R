
#Loan Default Prediction - Imperial College London


require(data.table)
require(bit64)
system.time(train <- fread("./data/train_v2.csv", header=TRUE, sep=","))

#------------------------------

library(caTools)

tt=read.csv('train_v2.csv')

tt=tt[,c('f527','f528','loss')]

set.seed(2014)

tt=tt[sample(nrow(tt)),]

train=tt[1:(0.7*nrow(tt)),]

test=tt[-(1:(0.7*nrow(tt))),]

train[,ncol(train)]=ifelse(train[,ncol(train)]>0,1,0)

test[,ncol(test)]=ifelse(test[,ncol(test)]>0,1,0)

model= glm(as.factor(train[,ncol(train)])~.,data=train[,-ncol(train)],family=binomial)

pr = predict(model, test[,-ncol(test)],type="response")

pr=ifelse(is.na(pr),mean(!is.na(pr)),pr)
cat('AUC:',colAUC(pr,test[,ncol(test)]),'\n')


####################################################################################
#GBM vs. GLM AUC


