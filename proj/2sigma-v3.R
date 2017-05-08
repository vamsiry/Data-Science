
#https://www.kaggle.com/satyaprakash1986/two-sigma-connect-rental-listing-inquiries/rf-submission/notebook

library(ggplot2) # Data visualization
library(readr) # CSV file I/O, e.g. the read_csv function
library(jsonlite)
library(dplyr)
library(purrr)
library(lubridate)
library(randomForest)
library(stringr)
library(tidytext)

# Train data
train_data <- fromJSON("C:\\Users\\Vamsi\\Desktop\\R.Alg\\practice\\data sets\\2sigma\\train.json")
vars <- setdiff(names(train_data), c("photos", "features"))
train_data <- map_at(train_data, vars, unlist) %>% tibble::as_tibble(.)

# Test data
test_data <- fromJSON("C:\\Users\\Vamsi\\Desktop\\R.Alg\\practice\\data sets\\2sigma\\test.json")
vars <- setdiff(names(test_data), c("photos", "features"))
test_data <- map_at(test_data, vars, unlist) %>% tibble::as_tibble(.)


# Creating backup data 
train_data_bak <- train_data
test_data_bak <- test_data


#Feature engineering from feature column
# Creating tidy dataset for the Feature column
train_data_bak%>%
  filter(map(features, is_empty) != TRUE) %>%
  tidyr::unnest(features) %>%
  unnest_tokens(word, features)%>%
  select(listing_id,word)->train_feature

test_data_bak%>%
  filter(map(features, is_empty) != TRUE) %>%
  tidyr::unnest(features) %>%
  unnest_tokens(word, features)%>%
  select(listing_id,word)->test_feature



# Creating document term matrix for feature column
train_dtm <- train_feature %>% 
  count(listing_id, word) %>%
  cast_dtm(listing_id, word, n)

test_dtm <- test_feature %>% 
  count(listing_id, word) %>%
  cast_dtm(listing_id, word, n)


library(tm)

# Removing sparse terms from the DTM
train_dtm<-removeSparseTerms(train_dtm, .90)
test_dtm<-removeSparseTerms(test_dtm, .90)


# converting DTM to dataframe
train_dtm_mat<-as.matrix(train_dtm)
train_dtm_mat<-as.data.frame(train_dtm_mat)
test_dtm_mat<-as.matrix(test_dtm)
test_dtm_mat<-as.data.frame(test_dtm_mat)

#---------------------------------------------------
head(train_dtm_mat)

data.frame(sort(colSums(is.na(train_dtm_mat))))
lapply(train_dtm_mat,function(x)table(x))
#----------------------------------------------------------

# Creating column for listing_id variable
train_dtm_mat$listing_id<-as.integer(row.names(train_dtm_mat))
test_dtm_mat$listing_id<-as.integer(row.names(test_dtm_mat))


#Create training features
train_data$created<-ymd_hms(train_data$created) # Convert time stamp
train_data$month<-month(train_data$created)     # Creating month column
train_data$week<-week(train_data$created)       # Creating week column
train_data$mday<-mday(train_data$created)       # Creating month of day feature
train_data$description_length<-nchar(train_data$description) # Feature for description length
train_data$feature_length<-nchar(train_data$features) # Feature for feature length
train_data$feature_count<-str_count(train_data$features,",") # Feature for tag count
train_data$photo_count<-str_count(train_data$photos,",") # Feature for photo count
train_data$Has_Building<-as.factor(ifelse(train_data$building_id==0,'Solo','Building'))

#---------------------------------------
head(train_data$feature_length)
head(train_data$feature_count)
head(train_data$photo_count)

dim(train_data)
head(train_data$building_id)

table(train_data$building_id)
table(train_data$Has_Building)

head(train_data[train_data$building_id==0])
train_data$building_id[993]
train_data$building_id[994]

#----------------------------------------


# Creating feature for building with high number of unique manager
train_data%>%
  group_by(building_id)%>%
  summarise(manager_count=n_distinct(manager_id))%>%
  mutate(IsManaged=ifelse(manager_count>10,'Highly_managed','Less_managed'))%>%
  right_join(train_data,by='building_id')->train_data

table(train_data$IsManaged)


# Creating feature for building with high number of flats
train_data%>%
  group_by(building_id)%>%
  summarise(building_count=n())%>%
  mutate(IsBigBuilding=ifelse(building_count>20,'Big_Building','Small_Building'))%>%
  right_join(train_data,by='building_id')->train_data


table(train_data$IsBigBuilding)


#Create test features
test_data$created<-ymd_hms(test_data$created)
test_data$month<-month(test_data$created)
test_data$week<-week(test_data$created)
test_data$mday<-mday(test_data$created)
test_data$description_length<-nchar(test_data$description)
test_data$feature_length<-nchar(test_data$features)
test_data$feature_count<-str_count(test_data$features,",")
test_data$photo_count<-str_count(test_data$photos,",")
test_data$Has_Building<-as.factor(ifelse(test_data$building_id==0,'Solo','Building'))



# Creating feature for building with high number of unique manager
test_data%>%
  group_by(building_id)%>%
  summarise(manager_count=n_distinct(manager_id))%>%
  mutate(IsManaged=ifelse(manager_count>10,'Highly_managed','Less_managed'))%>%
  right_join(test_data,by='building_id')->test_data



# Creating feature for building with high number of flats
test_data%>%
  group_by(building_id)%>%
  summarise(building_count=n())%>%
  mutate(IsBigBuilding=ifelse(building_count>20,'Big_Building','Small_Building'))%>%
  right_join(test_data,by='building_id')->test_data



# creating factor Y variable
train_data$interest_level<-as.factor(train_data$interest_level)


# Joining text and other features
train_data_final<-left_join(train_data,train_dtm_mat,by='listing_id')
test_data_final<-left_join(test_data,test_dtm_mat,by='listing_id')


# Removing NAs
dim(train_data_final)
data.frame(sort(colSums(is.na(train_data_final))))
table(train_data_final$elevator)
table(train_data_final$interest_level)

train_data_final[is.na(train_data_final)]<-0
test_data_final[is.na(test_data_final)]<-0


#Running RandomForest with 50 trees

output.forest <- randomForest(interest_level ~ month + week + mday + description_length +
                                bathrooms + bedrooms + latitude + longitude + price + feature_length +
                                feature_count + photo_count + Has_Building + building_count 
                              + manager_count + building + doorman + elevator+laundry+allowed+cats+
                                dogs+fee+no+center+fitness+outdoor+space+dishwasher+floors+hardwood+
                                pre+war+room+deck+roof+dining+high+unit, 
                              data = train_data_final,ntree=500)


predicted <- predict(output.forest,test_data_final,type="prob")


allpredictions = as.data.frame(cbind (predicted, test_data$listing_id))
names(allpredictions)<-c("high","low","medium","listing_id")
allpredictions=allpredictions[,c(1,3,2,4)]



head(allpredictions)

write.csv(allpredictions,'sample_submission4.csv',row.names=FALSE)

##############################################################################

# Train data
train_data <- fromJSON("C:\\Users\\Vamsi\\Desktop\\R.Alg\\practice\\data sets\\2sigma\\train.json")
vars <- setdiff(names(train_data), c("photos", "features"))
train_data <- map_at(train_data, vars, unlist) %>% tibble::as_tibble(.)

# Test data
test_data <- fromJSON("C:\\Users\\Vamsi\\Desktop\\R.Alg\\practice\\data sets\\2sigma\\test.json")
vars <- setdiff(names(test_data), c("photos", "features"))
test_data <- map_at(test_data, vars, unlist) %>% tibble::as_tibble(.)

train = train_data
test = test_data

str(train)
names(train)
class(train$interest_level)
table(train$interest_level)

train$photos = NULL
train$features = NULL
train$description = NULL
train$created = NULL

train[is.na(train)]<-0

train$Has_Building = as.factor(train$Has_Building)
train$IsManaged = as.factor(train$IsManaged)
train$IsBigBuilding = as.factor(train$IsBigBuilding)

train$building_id<-as.integer(factor(train$building_id))
train$manager_id<-as.integer(factor(train$manager_id))
train$display_address<-as.integer(factor(train$display_address))
train$street_address<-as.integer(factor(train$street_address))

train$interest_level<-as.integer(factor(train$interest_level))
train$interest_level = as.factor(train$interest_level)

y <- train$interest_level
y = y - 1
train$interest_level = NULL

require(randomForest)
varNames = names(train)

# Exclude ID or Response variable
varNames <- varNames[!varNames %in% c("interest_level")]

# add + sign between exploratory variables
varNames1 <- paste(varNames, collapse = "+")

# Add response variable and convert to a formula object
rf.form <- as.formula(paste("interest_level", varNames1, sep = " ~ "))

fit.rf = randomForest(rf.form, data = train, mtry = 7, ntree=500, importance=T)

fit.rf
fit.rf$confusion
fit.rf$mtry
fit.rf$importance                           


###########################################################################








