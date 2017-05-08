
library(ggplot2) # Data visualization
library(readr) # CSV file I/O, e.g. the read_csv function
library(jsonlite)

library(dplyr)
library(tidytext)
library(reshape2)
library(stringr)
library(lubridate)
library(purrr)

library(caret)
library(xgboost)
library(MLmetrics)

seed = 1985
set.seed(seed)

# unlist every variable except `photos` and `features` and convert to tibble
#Train
train <- fromJSON("C:\\Users\\Vamsi\\Desktop\\R.Alg\\practice\\data sets\\2sigma\\train.json")
vars <- setdiff(names(train), c("photos", "features"))
train <- map_at(train, vars, unlist) %>% tibble::as_tibble(.)
train_id <-train$listing_id

#Test
test <- fromJSON("C:\\Users\\Vamsi\\Desktop\\R.Alg\\practice\\data sets\\2sigma\\test.json")
vars <- setdiff(names(test), c("photos", "features"))
test <- map_at(test, vars, unlist) %>% tibble::as_tibble(.)
test_id <-test$listing_id


#add dummy interest level for test
test$interest_level <- 'none'


#combine train and test data
train_test <- rbind(train,test)

####################################################
####################################################
#Feature engineering from feature column(VERSION-1)
#--------------------------------------------------

#train_test$feature_length<-nchar(train_test$features) # Feature for feature length

train_test$feature_count <- lengths(train_test$features)

train_test[unlist(map(train_test$features,is_empty)),]$features = 'Nofeat' #Add fill for listings lacking any features


##Process Word features
word_remove = c('allowed', 'building','center', 'space','2','2br','bldg','24',
                '3br','1','ft','3','7','1br','hour','bedrooms','bedroom','true',
                'stop','size','blk','4br','4','sq','0862','1.5','373','16','3rd','block',
                'st','01','bathrooms')

#create sparse matrix for word features
word_sparse<-train_test[,names(train_test) %in% c("features","listing_id")]
train_test$features = NULL


#Create word features
word_sparse <- word_sparse %>%
  filter(map(features, is_empty) != TRUE) %>%
  tidyr::unnest(features) %>%
  unnest_tokens(word, features)

data("stop_words")

#remove stop words and other words
word_sparse = word_sparse[!(word_sparse$word %in% stop_words$word),]
word_sparse = word_sparse[!(word_sparse$word %in% word_remove),]

#get most common features and use (in this case top 150)
top_word <- as.character(as.data.frame(sort(table(word_sparse$word),decreasing = TRUE)[1:25])$Var1)
word_sparse = word_sparse[word_sparse$word %in% top_word,]
word_sparse$word = as.factor(word_sparse$word)
word_sparse<-dcast(word_sparse, listing_id ~ word,length, value.var = "word")

#merge word features back into main data frame
train_test<-merge(train_test,word_sparse, by = "listing_id", sort = FALSE,all.x=TRUE)

####################################################
####################################################
#Feature engineering from feature column(VERSION-2)
#--------------------------------------------------
# Creating tidy dataset for the Feature column

train_test%>%
  filter(map(features, is_empty) != TRUE) %>%
  tidyr::unnest(features) %>%
  unnest_tokens(word, features)%>%
  select(listing_id,word)->train_feature

# Creating document term matrix for feature column
train_dtm <- train_feature %>% 
  count(listing_id, word) %>%
  cast_dtm(listing_id, word, n)

library(tm)
# Removing sparse terms from the DTM
train_dtm<-removeSparseTerms(train_dtm, .90)


# converting DTM to dataframe
train_dtm_mat<-as.matrix(train_dtm)
train_dtm_mat<-as.data.frame(train_dtm_mat)

# Creating column for listing_id variable
train_dtm_mat$listing_id<-as.integer(row.names(train_dtm_mat))

# Joining text and other features
train_data_final<-left_join(train_data,train_dtm_mat,by='listing_id')

########################################################################################

#preprocessiong
#=======================
names(train_test)
summary(train_test)
summary(train_test$price)
summary(as.Date(train_test$created))

summary(train_test[train_test$bathrooms==0,])
summary(train_test[train_test$bedrooms==0,])
summary(train_test[train_test$bedrooms==0 | train_test$bathrooms==0,])
summary(train_test[train_test$bedrooms==0 & train_test$bathrooms==0,])
summary(train_test[train_test$price==2600,])


#%tile caping for price variable to remove outliers
require(scales)
train_test$price <- squish(train_test$price, round(quantile(train_test$price, c(.0005, .9998))))

train_test[train_test$bedrooms==0 | train_test$bathrooms==0,] = 1

train_test[train_test$bathrooms>train_test$bedrooms+1,] = train[train_test$bedrooms,]

summary(train_test$bedrooms)
summary(train_test$bathrooms)
summary(train_test$price)

#Create training time features
#---------------------------
train_test$created<-ymd_hms(train_test$created) # Convert time stamp
train_test$month<-month(train_test$created)     # Creating month column
train_test$day<-day(train_test$created)
train_test$week<-week(train_test$created)       # Creating week column
train_test$hour<-hour(train_test$created)
train_test$created = NULL


#add feature for length of features
#-------------------------------------
train_test$photo_count <- lengths(train_test$photos)
train_test$photos = NULL


# Feature for description length
#---------------------------------
#train_test$description_length<-nchar(train_test$description) 

##Length of description in words
train_test$description_len<-sapply(strsplit(train_test$description, "\\s+"), length)
train_test$description = NULL


#price to bedroom ratio
#------------------------
train_test$bed_price <- train_test$price/train_test$bedrooms
train_test[which(is.infinite(train_test$bed_price)),]$bed_price = train_test[which(is.infinite(train_test$bed_price)),]$price

summary(train_test$bed_price)


#add sum of rooms and price per room
#---------------------------------------
train_test$room_sum <- train_test$bedrooms + train_test$bathrooms
train_test$room_diff <- train_test$bedrooms - train_test$bathrooms
train_test$room_price <- train_test$price/train_test$room_sum
train_test$bed_ratio <- train_test$bedrooms/train_test$room_sum
train_test[which(is.infinite(train_test$room_price)),]$room_price = train_test[which(is.infinite(train_test$room_price)),]$price


# Creating feature for building with high number of unique manager
#==================================================================
train_test%>%
  group_by(building_id)%>%
  summarise(manager_count=n_distinct(manager_id))%>%
  mutate(IsManaged=ifelse(manager_count>10,'Highly_managed','Less_managed'))%>%
  right_join(train_test,by='building_id')->train_test

table(train_test$IsManaged)
#---------------------------
train_test%>%
  group_by(building_id)%>%
  summarise(manager_count=n_distinct(manager_id))%>%
  mutate(IsManaged=ifelse(manager_count>10,'1','0'))%>%
  right_join(train_test,by='building_id')->train_test

train_test$IsManaged = as.factor(train_test$IsManaged)

table(train_test$IsManaged)
class(train_test$IsManaged)

# Creating feature for building with high number of flats
#===========================================================
train_test%>%
  group_by(building_id)%>%
  summarise(building_count=n())%>%
  mutate(IsBigBuilding=ifelse(building_count>20,'Big_Building','Small_Building'))%>%
  right_join(train_test,by='building_id')->train_test

table(train_test$IsBigBuilding)
#------------------------------
train_test%>%
  group_by(building_id)%>%
  summarise(building_count=n())%>%
  mutate(IsBigBuilding=ifelse(building_count>20,'1','0'))%>%
  right_join(train_test,by='building_id')->train_test

train_test$IsBigBuilding = as.factor(train_test$IsBigBuilding)

table(train_test$IsBigBuilding)
class(train_test$IsBigBuilding)


# Creating feature for building is solo or building
#====================================================
train_test$Has_Building<-as.factor(ifelse(train_test$building_id==0,'Solo','Building'))
#------------------
train_test$Has_Building<-as.factor(ifelse(train_test$building_id==0,'0','1'))
class(train_test$Has_Building)




#########################################################################
train_test$building_id = NULL
train_test$manager_id = NULL
train_test$display_address = NULL
train_test$street_address = NULL
train_test$features = NULL

#convert building and manager id to integer
train_test$building_id<-as.integer(factor(train_test$building_id))
train_test$manager_id<-as.integer(factor(train_test$manager_id))

#convert street and display address to integer
train_test$display_address<-as.integer(factor(train_test$display_address))
train_test$street_address<-as.integer(factor(train_test$street_address))

train_test$display_address[2]
train_test$street_address[2]


#log transform features, these features aren't normally distributed
#-------------------------------------------------------------------
train_test$photo_count <- log(train_test$photo_count + 1)
train_test$feature_count <- log(train_test$feature_count + 1)
train_test$price <- log(train_test$price + 1)
train_test$room_price <- log(train_test$room_price + 1)
train_test$bed_price <- log(train_test$bed_price + 1)

#######################################################################
#split train test
train <- train_test[train_test$listing_id %in%train_id,]
test <- train_test[train_test$listing_id %in%test_id,]


#Convert labels to integers
#--------------------------
train$interest_level<-as.integer(factor(train$interest_level))
y <- train$interest_level
y = y - 1

head(train$interest_level)
head(y)

train$interest_level = NULL
test$interest_level = NULL


train[is.na(train)] = -999
test[is.na(test)] = -999

#convert xgbmatrix
dtest <- xgb.DMatrix(data.matrix(test),missing=-999)

#create folds
kfolds<- 10
folds<-createFolds(y, k = kfolds, list = TRUE, returnTrain = FALSE)
fold <- as.numeric(unlist(folds[1]))

x_train<-train[-fold,] #Train set
x_val<-train[fold,] #Out of fold validation set

y_train<-y[-fold]
y_val<-y[fold]

#------------------------------------------
#convert to xgbmatrix
dtrain = xgb.DMatrix(data.matrix(x_train), label=y_train, missing=-999)
dval = xgb.DMatrix(data.matrix(x_val), label=y_val, missing=-999)

#Parameters for XGB
xgb_params = list(
  objective= 'multi:softprob',
  booster = "gbtree",
  eval_metric= "mlogloss",
  eval_metric = "merror",
  eta = 0.05,
  max_depth= 1,
  subsample = 0.7,
  colsample_bytree= 0.7,
  min_child_weight= 1,
  num_class = 3,
  print.every.n = 1
)


#perform training
set.seed = 999
gbdt = xgb.train(params = xgb_params,
                 data = dtrain,
                 nrounds =2500,
                 watchlist = list(train = dtrain, val=dval),
                 print_every_n = 25,
                 early_stopping_rounds=50)

allpredictions =  (as.data.frame(matrix(predict(gbdt,dtest), nrow=dim(test), byrow=TRUE)))

#-----------------------
dt = xgb.DMatrix(data.matrix(train), missing=-999)
pred1 = (as.data.frame(matrix(predict(gbdt,dt), nrow=dim(train), byrow=TRUE)))

######################
##Generate Submission
allpredictions = cbind (allpredictions, test$listing_id)
names(allpredictions)<-c("high","low","medium","listing_id")
allpredictions=allpredictions[,c(1,3,2,4)]
write.csv(allpredictions,paste0(Sys.Date(),"-BaseModel-20Fold-Seed",seed,".csv"),row.names = FALSE)


####################################
###Generate Feature Importance Plot
imp <- xgb.importance(names(train),model = gbdt)
xgb.ggplot.importance(imp)







