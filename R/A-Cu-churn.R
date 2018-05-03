
#customer churn

require(data.table)
require(dplyr)
require(rpart)

cctrn = fread("C:\Users\vamsi\Desktop\Vamsi\Data\A-vidya\C-churn\train.csv",
           data.table = F,na.strings = c("","NA",">","?","na"," "),head = T,stringsAsFactors = FALSE)

cctst = fread("C:\Users\vamsi\Desktop\Vamsi\Data\A-vidya\C-churn\test.csv",
              data.table = F,na.strings = c("","NA",">","?","na"," "),head = T,stringsAsFactors = FALSE)



dim(cctrn)
dim(cctst)

trnID = cctrn$UCIC_ID
tstID = cctst$UCIC_ID
target = cctrn$Responders

cctrn$UCIC_ID = NULL
cctst$UCIC_ID = NULL

cctst$Responders = 3

cctrn$mark = 5
cctst$mark = 6


alldata = rbind(cctrn,cctst)
dim(alldata)


rm(cctrn)
rm(cctst)

#converting all character/string var to  numeric var 
alldata[sapply(alldata, is.character)] =
      lapply(alldata[sapply(alldata, is.character)], function(x) as.integer(factor(x),levels = sort(unique(x)) ))


#checking missing values column wise
#====================================
data.frame(sort(apply(alldata, 2, function(x) { sum(is.na(x)) })))

#check ns's in each column corresponding to each value in target 
#==============================================================
train.na.per.response <- 
  sapply(sort(unique(alldata$Responders)), function(x) {apply(alldata[alldata$Responders == x, ], 2, function(y) { sum(is.na(y)) }) })

train.na.per.response


#Replacing all missing values with "0"
#========================================
cnames = names(alldata)
alldata[, cnames] [is.na(alldata[, cnames])] <- 0

#alldata[,matches] = apply(alldata[,matches], 2, function(x) {ifelse(is.na(x), 0, x) }) another way


data.frame(sort(apply(alldata, 2, function(x) { sum(is.na(x)) })))


#summarization based on target column for understanding 
#=========================================================

#summarise(group_by(alldata, Responders), mean(C_prev1, na.rm = TRUE))

dput(as.character(paste(names(alldata),sep = "")))

cnames1 = c("NO_OF_Accs", "HNW_CATEGORY", "vintage", "EMAIL_UNSUBSCRIBE", 
            "OCCUP_ALL_NEW", "city", "dependents", "zip", "FINAL_WORTH_prev1", 
            "ENGAGEMENT_TAG_prev1", "C_prev1", "D_prev1", "ATM_C_prev1", 
            "ATM_D_prev1", "BRANCH_C_prev1", "BRANCH_D_prev1", "IB_C_prev1", 
            "IB_D_prev1", "MB_C_prev1", "MB_D_prev1", "POS_C_prev1", "POS_D_prev1", 
            "count_C_prev1", "count_D_prev1", "COUNT_ATM_C_prev1", "COUNT_ATM_D_prev1", 
            "COUNT_BRANCH_C_prev1", "COUNT_BRANCH_D_prev1", "COUNT_IB_C_prev1", 
            "COUNT_IB_D_prev1", "COUNT_MB_C_prev1", "COUNT_MB_D_prev1", "COUNT_POS_C_prev1", 
            "COUNT_POS_D_prev1", "custinit_CR_amt_prev1", "custinit_DR_amt_prev1", 
            "custinit_CR_cnt_prev1", "custinit_DR_cnt_prev1", "ATM_amt_prev1", 
            "ATM_CW_Amt_prev1", "ATM_CW_Cnt_prev1", "BRN_CW_Amt_prev1", "BRN_CW_Cnt_prev1", 
            "BRN_CASH_Dep_Amt_prev1", "BRN_CASH_Dep_Cnt_prev1", "CNR_prev1", 
            "BAL_prev1", "EOP_prev1", "CR_AMB_Prev1", "C_prev2", "D_prev2", 
            "ATM_C_prev2", "ATM_D_prev2", "BRANCH_C_prev2", "BRANCH_D_prev2", 
            "IB_C_prev2", "IB_D_prev2", "MB_C_prev2", "MB_D_prev2", "POS_C_prev2", 
            "POS_D_prev2", "count_C_prev2", "count_D_prev2", "COUNT_ATM_C_prev2", 
            "COUNT_ATM_D_prev2", "COUNT_BRANCH_C_prev2", "COUNT_BRANCH_D_prev2", 
            "COUNT_IB_C_prev2", "COUNT_IB_D_prev2", "COUNT_MB_C_prev2", "COUNT_MB_D_prev2", 
            "COUNT_POS_C_prev2", "COUNT_POS_D_prev2", "custinit_CR_amt_prev2", 
            "custinit_DR_amt_prev2", "custinit_CR_cnt_prev2", "custinit_DR_cnt_prev2", 
            "ATM_amt_prev2", "ATM_CW_Amt_prev2", "ATM_CW_Cnt_prev2", "BRN_CW_Amt_prev2", 
            "BRN_CW_Cnt_prev2", "BRN_CASH_Dep_Amt_prev2", "BRN_CASH_Dep_Cnt_prev2", 
            "CNR_prev2", "BAL_prev2", "EOP_prev2", "CR_AMB_Prev2", "C_prev3", 
            "D_prev3", "ATM_C_prev3", "ATM_D_prev3", "BRANCH_C_prev3", "BRANCH_D_prev3", 
            "IB_C_prev3", "IB_D_prev3", "MB_C_prev3", "MB_D_prev3", "POS_C_prev3", 
            "POS_D_prev3", "count_C_prev3", "count_D_prev3", "COUNT_ATM_C_prev3", 
            "COUNT_ATM_D_prev3", "COUNT_BRANCH_C_prev3", "COUNT_BRANCH_D_prev3", 
            "COUNT_IB_C_prev3", "COUNT_IB_D_prev3", "COUNT_MB_C_prev3", "COUNT_MB_D_prev3", 
            "COUNT_POS_C_prev3", "COUNT_POS_D_prev3", "custinit_CR_amt_prev3", 
            "custinit_DR_amt_prev3", "custinit_CR_cnt_prev3", "custinit_DR_cnt_prev3", 
            "ATM_amt_prev3", "ATM_CW_Amt_prev3", "ATM_CW_Cnt_prev3", "BRN_CW_Amt_prev3", 
            "BRN_CW_Cnt_prev3", "BRN_CASH_Dep_Amt_prev3", "BRN_CASH_Dep_Cnt_prev3", 
            "CNR_prev3", "BAL_prev3", "EOP_prev3", "CR_AMB_Prev3", "C_prev4", 
            "D_prev4", "ATM_C_prev4", "ATM_D_prev4", "BRANCH_C_prev4", "BRANCH_D_prev4", 
            "IB_C_prev4", "IB_D_prev4", "MB_C_prev4", "MB_D_prev4", "POS_C_prev4", 
            "POS_D_prev4", "count_C_prev4", "count_D_prev4", "COUNT_ATM_C_prev4", 
            "COUNT_ATM_D_prev4", "COUNT_BRANCH_C_prev4", "COUNT_BRANCH_D_prev4", 
            "COUNT_IB_C_prev4", "COUNT_IB_D_prev4", "COUNT_MB_C_prev4", "COUNT_MB_D_prev4", 
            "COUNT_POS_C_prev4", "COUNT_POS_D_prev4", "custinit_CR_amt_prev4", 
            "custinit_DR_amt_prev4", "custinit_CR_cnt_prev4", "custinit_DR_cnt_prev4", 
            "ATM_amt_prev4", "ATM_CW_Amt_prev4", "ATM_CW_Cnt_prev4", "BRN_CW_Amt_prev4", 
            "BRN_CW_Cnt_prev4", "BRN_CASH_Dep_Amt_prev4", "BRN_CASH_Dep_Cnt_prev4", 
            "CNR_prev4", "BAL_prev4", "EOP_prev4", "CR_AMB_Prev4", "C_prev5", 
            "D_prev5", "ATM_C_prev5", "ATM_D_prev5", "BRANCH_C_prev5", "BRANCH_D_prev5", 
            "IB_C_prev5", "IB_D_prev5", "MB_C_prev5", "MB_D_prev5", "POS_C_prev5", 
            "POS_D_prev5", "count_C_prev5", "count_D_prev5", "COUNT_ATM_C_prev5", 
            "COUNT_ATM_D_prev5", "COUNT_BRANCH_C_prev5", "COUNT_BRANCH_D_prev5", 
            "COUNT_IB_C_prev5", "COUNT_IB_D_prev5", "COUNT_MB_C_prev5", "COUNT_MB_D_prev5", 
            "COUNT_POS_C_prev5", "COUNT_POS_D_prev5", "custinit_CR_amt_prev5", 
            "custinit_DR_amt_prev5", "custinit_CR_cnt_prev5", "custinit_DR_cnt_prev5", 
            "ATM_amt_prev5", "ATM_CW_Amt_prev5", "ATM_CW_Cnt_prev5", "BRN_CW_Amt_prev5", 
            "BRN_CW_Cnt_prev5", "BRN_CASH_Dep_Amt_prev5", "BRN_CASH_Dep_Cnt_prev5", 
            "CNR_prev5", "BAL_prev5", "EOP_prev5", "CR_AMB_Prev5", "C_prev6", 
            "D_prev6", "ATM_C_prev6", "ATM_D_prev6", "BRANCH_C_prev6", "BRANCH_D_prev6", 
            "IB_C_prev6", "IB_D_prev6", "MB_C_prev6", "MB_D_prev6", "POS_C_prev6", 
            "POS_D_prev6", "count_C_prev6", "count_D_prev6", "COUNT_ATM_C_prev6", 
            "COUNT_ATM_D_prev6", "COUNT_BRANCH_C_prev6", "COUNT_BRANCH_D_prev6", 
            "COUNT_IB_C_prev6", "COUNT_IB_D_prev6", "COUNT_MB_C_prev6", "COUNT_MB_D_prev6", 
            "COUNT_POS_C_prev6", "COUNT_POS_D_prev6", "custinit_CR_amt_prev6", 
            "custinit_DR_amt_prev6", "custinit_CR_cnt_prev6", "custinit_DR_cnt_prev6", 
            "ATM_amt_prev6", "ATM_CW_Amt_prev6", "ATM_CW_Cnt_prev6", "BRN_CW_Amt_prev6", 
            "BRN_CW_Cnt_prev6", "BRN_CASH_Dep_Amt_prev6", "BRN_CASH_Dep_Cnt_prev6", 
            "CNR_prev6", "BAL_prev6", "EOP_prev6", "CR_AMB_Prev6", "FRX_PrevQ1", 
            "EFT_SELF_TRANSFER_PrevQ1", "Billpay_Active_PrevQ1", "Billpay_Reg_ason_Prev1", 
            "FD_AMOUNT_BOOK_PrevQ1", "FD_AMOUNT_BOOK_PrevQ2", "NO_OF_FD_BOOK_PrevQ1", 
            "NO_OF_FD_BOOK_PrevQ2", "NO_OF_RD_BOOK_PrevQ1", "NO_OF_RD_BOOK_PrevQ2", 
            "RD_AMOUNT_BOOK_PrevQ1", "RD_AMOUNT_BOOK_PrevQ2", "Total_Invest_in_MF_PrevQ1", 
            "Total_Invest_in_MF_PrevQ2", "count_No_of_MF_PrevQ1", "count_No_of_MF_PrevQ2", 
            "Dmat_Investing_PrevQ1", "Dmat_Investing_PrevQ2", "AGRI_PREM_CLOSED_PREVQ1", 
            "AL_CNC_PREM_CLOSED_PREVQ1", "AL_PREM_CLOSED_PREVQ1", "BL_PREM_CLOSED_PREVQ1", 
            "CC_PREM_CLOSED_PREVQ1", "CE_PREM_CLOSED_PREVQ1", "CV_PREM_CLOSED_PREVQ1", 
            "EDU_PREM_CLOSED_PREVQ1", "OTHER_LOANS_PREM_CLOSED_PREVQ1", "PL_PREM_CLOSED_PREVQ1", 
            "RD_PREM_CLOSED_PREVQ1", "FD_PREM_CLOSED_PREVQ1", "TL_PREM_CLOSED_PREVQ1", 
            "TWL_PREM_CLOSED_PREVQ1", "AGRI_Closed_PrevQ1", "AL_CNC_Closed_PrevQ1", 
            "AL_Closed_PrevQ1", "BL_Closed_PrevQ1", "CC_CLOSED_PREVQ1", "CE_Closed_PrevQ1", 
            "CV_Closed_PrevQ1", "EDU_Closed_PrevQ1", "GL_Closed_PrevQ1", 
            "OTHER_LOANS_Closed_PrevQ1", "PL_Closed_PrevQ1", "RD_CLOSED_PREVQ1", 
            "FD_CLOSED_PREVQ1", "TL_Closed_PrevQ1", "TWL_Closed_PrevQ1", 
            "DEMAT_CLOSED_PREV1YR", "SEC_ACC_CLOSED_PREV1YR", "AGRI_TAG_LIVE", 
            "AL_CNC_TAG_LIVE", "AL_TAG_LIVE", "BL_TAG_LIVE", "CC_TAG_LIVE", 
            "CE_TAG_LIVE", "CV_TAG_LIVE", "DEMAT_TAG_LIVE", "EDU_TAG_LIVE", 
            "GL_TAG_LIVE", "HL_TAG_LIVE", "SEC_ACC_TAG_LIVE", "INS_TAG_LIVE", 
            "LAS_TAG_LIVE", "MF_TAG_LIVE", "OTHER_LOANS_TAG_LIVE", "PL_TAG_LIVE", 
            "RD_TAG_LIVE", "FD_TAG_LIVE", "TL_TAG_LIVE", "TWL_TAG_LIVE", 
            "lap_tag_live", "AGRI_DATE", "AL_CNC_DATE", "AL_DATE", "BL_DATE", 
            "CE_DATE", "CV_DATE", "EDU_DATE", "GL_DATE", "LAP_DATE", "LAS_DATE", 
            "OTHER_LOANS_DATE", "PL_DATE", "TL_DATE", "TWL_DATE", "Charges_PrevQ1", 
            "Charges_cnt_PrevQ1", "NO_OF_COMPLAINTS", "CASH_WD_AMT_Last6", 
            "CASH_WD_CNT_Last6", "Billpay_Active_PrevQ1_N", "Billpay_Reg_ason_Prev1_N", 
            "Charges_cnt_PrevQ1_N", "FRX_PrevQ1_N", "brn_code", "RBI_Class_Audit", 
            "age", "gender_bin", "Recency_of_CR_TXN", "Recency_of_DR_TXN", 
            "Recency_of_IB_TXN", "Recency_of_ATM_TXN", "Recency_of_BRANCH_TXN", 
            "Recency_of_POS_TXN", "Recency_of_MB_TXN", "Recency_of_Activity", 
            "I_AQB_PrevQ1", "I_AQB_PrevQ2", "I_CR_AQB_PrevQ1", "I_CR_AQB_PrevQ2", 
            "I_CNR_PrevQ1", "I_CNR_PrevQ2", "I_NRV_PrevQ1", "I_NRV_PrevQ2", 
            "CR_AMB_Drop_Build_1", "CR_AMB_Drop_Build_2", "CR_AMB_Drop_Build_3", 
            "CR_AMB_Drop_Build_4", "CR_AMB_Drop_Build_5", "Req_Logged_PrevQ1", 
            "Req_Resolved_PrevQ1", "Query_Logged_PrevQ1", "Query_Resolved_PrevQ1", 
            "Complaint_Logged_PrevQ1", "Complaint_Resolved_PrevQ1", "NO_OF_CHEQUE_BOUNCE_V1", 
            "Percent_Change_in_Credits", "Percent_Change_in_FT_Bank", "Percent_Change_in_FT_outside", 
            "Percent_Change_in_Self_Txn", "Percent_Change_in_Big_Expenses", 
             "mark")


#t(summarise_at(group_by(alldata, Responders), vars(cnames1), funs( n(), mean(., na.rm = TRUE))))
t(summarise_at(group_by(alldata, Responders), vars(cnames1), funs(mean(., na.rm = TRUE))))

#checking all columns based on quantile wise data dispersion
t(sapply(alldata[,cnames1], function(x) {quantile(x, probs = c(0.1, 0.2, 0.3, 0.4, 0.5,0.6,0.7,0.8,0.9,.93,.96,.97,.98,.99,.999,1.0),na.rm = T) }))


#checking factor variables
summarise_if(new, is.factor, funs(nlevels(.)))

#checking factor variables
summarise_if(new, is.numeric(), funs(mean(., na.rm = TRUE)))



#checking outliers
#=====================

outlierKD <- function(dt, var) {
  var_name <- eval(substitute(var),eval(dt))
  na1 <- sum(is.na(var_name))
  m1 <- mean(var_name, na.rm = T)
  par(mfrow=c(2, 2), oma=c(0,0,3,0))
  boxplot(var_name, main="With outliers")
  hist(var_name, main="With outliers", xlab=NA, ylab=NA)
  outlier <- boxplot.stats(var_name)$out
  mo <- mean(outlier)
  var_name <- ifelse(var_name %in% outlier, NA, var_name)
  boxplot(var_name, main="Without outliers")
  hist(var_name, main="Without outliers", xlab=NA, ylab=NA)
  title("Outlier Check", outer=TRUE)
  na2 <- sum(is.na(var_name))
  cat("Outliers identified:", na2 - na1, "n")
  cat("Propotion (%) of outliers:", round((na2 - na1) / sum(!is.na(var_name))*100, 1), "n")
  cat("Mean of the outliers:", round(mo, 2), "n")
  m2 <- mean(var_name, na.rm = T)
  cat("Mean without removing outliers:", round(m1, 2), "n")
  cat("Mean if we remove outliers:", round(m2, 2), "n")
  response <- readline(prompt="Do you want to remove outliers and to replace with NA? [yes/no]: ")
  if(response == "y" | response == "yes"){
    dt[as.character(substitute(var))] <- invisible(var_name)
    assign(as.character(as.list(match.call())$dt), dt, envir = .GlobalEnv)
    cat("Outliers successfully removed", "n")
    return(invisible(dt))
  } else{
    cat("Nothing changed", "n")
    return(invisible(var_name))
  }
}


outlierKD(alldata, C_prev1)


describe(new$C_prev1,na.rm = T)
str(boxplot.stats(new$C_prev1)$out)

#=============================================================================================
#never ever remove "unique(length) = 1" or "any column" at very begining makes deficult to 
# fetch columns based on names later 
#=============================================================================================




#preprocessing from col 2 to Col 11
#======================================
#removing outliers in no.of accounts column
table(alldata$NO_OF_Accs) 
alldata$NO_OF_Accs = pmin(alldata$NO_OF_Accs, 10)

class(alldata$NO_OF_Accs)
alldata$NO_OF_Accs = as.factor(alldata$NO_OF_Accs)


#checking HNW_CATEGORY column 
table(alldata$HNW_CATEGORY)
class(alldata$HNW_CATEGORY)
alldata$HNW_CATEGORY = as.factor(alldata$HNW_CATEGORY)


#removing outliers in vintage column and converting in no.of years from days
summary(alldata$vintage)
describe(alldata$vintage)
dim(alldata[alldata$vintage > 5000,])
quantile(alldata$vintage, probs = c(0, 0.2, 0.3, 0.4, 0.5,0.6,0.7,0.8,0.9,.95,.97,1.0))

alldata$vintage = pmin(alldata$vintage, 5000)
alldata$vintage = as.factor(round(alldata$vintage / 365))
table(alldata$vintage)
alldata$vintage = as.factor(alldata$vintage)

#adding 0 inplace of email unsubscriptions 
summary(alldata$EMAIL_UNSUBSCRIBE)
table(alldata$EMAIL_UNSUBSCRIBE)
class(alldata$EMAIL_UNSUBSCRIBE)
alldata$EMAIL_UNSUBSCRIBE = as.factor(alldata$EMAIL_UNSUBSCRIBE)
class(alldata$EMAIL_UNSUBSCRIBE)

#occupancy col converting to factor and need to convert to onehot encoding
summary(alldata$OCCUP_ALL_NEW)
table(alldata$OCCUP_ALL_NEW)
# alldata$OCCUP_ALL_NEW[is.na(alldata$OCCUP_ALL_NEW)] = 6
alldata$OCCUP_ALL_NEW = as.factor(alldata$OCCUP_ALL_NEW) 
class(alldata$OCCUP_ALL_NEW)


#customer city col need to check later
summary(alldata$city)
table(alldata$city)
alldata$city = NULL


#removing outliers in no.of dependends
table(alldata$dependents)
quantile(alldata$dependents, probs = c(0, 0.2, 0.3, 0.4, 0.5,0.6,0.7,0.8,0.9,.95,.97,.99,.9999,1))
alldata$dependents = pmin(alldata$dependents,10)
summary(alldata$dependents)


#removing zip code column
alldata$zip = NULL


#imputing missing values in FINAL_WORTH_prev1 col 
summary(alldata$FINAL_WORTH_prev1)
table(alldata$FINAL_WORTH_prev1)
#alldata$FINAL_WORTH_prev1[is.na(alldata$FINAL_WORTH_prev1)] = 3
alldata$FINAL_WORTH_prev1 = as.factor(alldata$FINAL_WORTH_prev1)


#Engagement of Customer with Bank as on Month1 column
summary(alldata$ENGAGEMENT_TAG_prev1)
table(alldata$ENGAGEMENT_TAG_prev1)
#alldata$ENGAGEMENT_TAG_prev1[is.na(alldata$ENGAGEMENT_TAG_prev1)] = 2
alldata$ENGAGEMENT_TAG_prev1 = as.factor(alldata$ENGAGEMENT_TAG_prev1)


#===============================================================================
#preprocessing from col 12 to Col 245 
#================================================================================

new = alldata

#all columns which all having amounts in rupees and converting to "binary"*****
#converting multiple columns into binary from numarc by ifelse(x>0, 1, 0)
dput(as.character(paste("ATM_D_prev",c(1:6),sep = "")))

tt = c("ATM_C_prev1", "ATM_C_prev2", "ATM_C_prev3", "ATM_C_prev4", 
       "ATM_C_prev5", "ATM_C_prev6","ATM_D_prev1", "ATM_D_prev2", "ATM_D_prev3", "ATM_D_prev4", 
       "ATM_D_prev5", "ATM_D_prev6","BRANCH_C_prev1", "BRANCH_C_prev2", "BRANCH_C_prev3", "BRANCH_C_prev4", 
       "BRANCH_C_prev5", "BRANCH_C_prev6","BRANCH_D_prev1", "BRANCH_D_prev2", "BRANCH_D_prev3", "BRANCH_D_prev4", 
       "BRANCH_D_prev5", "BRANCH_D_prev6","IB_C_prev1", "IB_C_prev2", "IB_C_prev3", "IB_C_prev4", "IB_C_prev5", 
       "IB_C_prev6","IB_D_prev1", "IB_D_prev2", "IB_D_prev3", "IB_D_prev4", "IB_D_prev5", 
       "IB_D_prev6","MB_C_prev1", "MB_C_prev2", "MB_C_prev3", "MB_C_prev4", "MB_C_prev5", 
       "MB_C_prev6","MB_D_prev1", "MB_D_prev2", "MB_D_prev3", "MB_D_prev4", "MB_D_prev5", 
       "MB_D_prev6","POS_C_prev1", "POS_C_prev2", "POS_C_prev3", "POS_C_prev4", 
       "POS_C_prev5", "POS_C_prev6","POS_D_prev1", "POS_D_prev2", "POS_D_prev3", "POS_D_prev4", 
       "POS_D_prev5", "POS_D_prev6")


str(new[,tt])
summary(new[,tt])
describe(new[,tt])
t(summarise_at(group_by(new, Responders), vars(c(tt)), funs(mean(., na.rm = TRUE))))

# treatement replacing values in each colum by ifelse(x>0, 1, 0)
new[,tt] = apply(new[,tt], 2, function(x) {ifelse(x > 0, 1, 0)} )

describe(new[,tt])
table(new[,tt])

#new[,tt[tt %in% names(new)]] = sapply(new[,tt[tt %in% names(new)]],function(x){as.integer(as.character(x))})
#summary(new[,tt])

# Outlier replacement with 99 percintile values
# library(scales)
# new[,tt[tt %in% names(new)]] = sapply(new[,tt[tt %in% names(new)]], function(x){squish(x, round(quantile(x, c(.01, .99))))})
# 
# t(sapply(vam[,tt[tt %in% names(vam)]], function(x) {quantile(x, probs = c(0.1, 0.2, 0.3, 0.4, 0.5,0.6,0.7,0.8,0.9,.93,.96,.97,.98,.99,.9999,1.0),na.rm = T)  }))

#-----------------------------------------------------------------------------------------------------------
#all columns which all having counts in transactions******
#checking outliers in multiple columns and replacing with 99% quantile values******

dput(as.character(paste("BRN_CW_Cnt_prev",c(1:6),sep = "")))

tt2 = c("count_C_prev1", "count_C_prev2", "count_C_prev3", "count_C_prev4", 
        "count_C_prev5", "count_C_prev6","count_D_prev1", "count_D_prev2", "count_D_prev3", "count_D_prev4", 
        "count_D_prev5", "count_D_prev6","COUNT_ATM_C_prev1", "COUNT_ATM_C_prev2", "COUNT_ATM_C_prev3", 
        "COUNT_ATM_C_prev4", "COUNT_ATM_C_prev5", "COUNT_ATM_C_prev6","COUNT_ATM_D_prev1", "COUNT_ATM_D_prev2", "COUNT_ATM_D_prev3", 
        "COUNT_ATM_D_prev4", "COUNT_ATM_D_prev5", "COUNT_ATM_D_prev6","COUNT_BRANCH_C_prev1", "COUNT_BRANCH_C_prev2", "COUNT_BRANCH_C_prev3", 
        "COUNT_BRANCH_C_prev4", "COUNT_BRANCH_C_prev5", "COUNT_BRANCH_C_prev6","COUNT_BRANCH_D_prev1", "COUNT_BRANCH_D_prev2", "COUNT_BRANCH_D_prev3", 
        "COUNT_BRANCH_D_prev4", "COUNT_BRANCH_D_prev5", "COUNT_BRANCH_D_prev6","COUNT_IB_C_prev1", "COUNT_IB_C_prev2", "COUNT_IB_C_prev3", 
        "COUNT_IB_C_prev4", "COUNT_IB_C_prev5", "COUNT_IB_C_prev6","COUNT_IB_D_prev1", "COUNT_IB_D_prev2", "COUNT_IB_D_prev3", 
        "COUNT_IB_D_prev4", "COUNT_IB_D_prev5", "COUNT_IB_D_prev6","COUNT_MB_C_prev1", "COUNT_MB_C_prev2", "COUNT_MB_C_prev3", 
        "COUNT_MB_C_prev4", "COUNT_MB_C_prev5", "COUNT_MB_C_prev6","COUNT_MB_D_prev1", "COUNT_MB_D_prev2", "COUNT_MB_D_prev3", 
        "COUNT_MB_D_prev4", "COUNT_MB_D_prev5", "COUNT_MB_D_prev6","COUNT_POS_C_prev1", "COUNT_POS_C_prev2", "COUNT_POS_C_prev3", 
        "COUNT_POS_C_prev4", "COUNT_POS_C_prev5", "COUNT_POS_C_prev6","COUNT_POS_D_prev1", "COUNT_POS_D_prev2", "COUNT_POS_D_prev3", 
        "COUNT_POS_D_prev4", "COUNT_POS_D_prev5", "COUNT_POS_D_prev6","ATM_CW_Cnt_prev1", "ATM_CW_Cnt_prev2", "ATM_CW_Cnt_prev3", 
        "ATM_CW_Cnt_prev4", "ATM_CW_Cnt_prev5", "ATM_CW_Cnt_prev6","BRN_CW_Cnt_prev1", "BRN_CW_Cnt_prev2", "BRN_CW_Cnt_prev3", 
        "BRN_CW_Cnt_prev4", "BRN_CW_Cnt_prev5", "BRN_CW_Cnt_prev6")


summary(new[,tt2])
describe(new[,tt2])

t(summarise_at(group_by(new, Responders), vars(c(tt2)), funs(mean(., na.rm = TRUE))))

data.frame(sapply(new[,tt2], function(x)  { mean(x) } ))

data.frame(colSums(is.na(new[,tt2])))

#outlier replacement with 99 percintile values*****
library(scales)

t(sapply(new[,tt2], function(x) {quantile(x, probs = c(0.1, 0.2, 0.3, 0.4, 0.5,0.6,0.7,0.8,0.9,.93,.96,.97,.98,.99,.9950,.9990,.9999,1.0),na.rm = T) }))

new[,tt2] = sapply(new[,tt2], function(x){squish(x, round(quantile(x, c(.01, .9990))))})

describe(new[,tt2])



#-----------------------------------------------------------------------------------------------------

#Removing redundant columns not now in the end ***********
dput(as.character(paste("custinit_DR_cnt_prev",c(1:6),sep = "")))

tt3 = c("custinit_CR_amt_prev1", "custinit_CR_amt_prev2", "custinit_CR_amt_prev3", 
        "custinit_CR_amt_prev4", "custinit_CR_amt_prev5", "custinit_CR_amt_prev6","custinit_DR_amt_prev1", "custinit_DR_amt_prev2", "custinit_DR_amt_prev3", 
        "custinit_DR_amt_prev4", "custinit_DR_amt_prev5", "custinit_DR_amt_prev6","custinit_CR_cnt_prev1", "custinit_CR_cnt_prev2", "custinit_CR_cnt_prev3", 
        "custinit_CR_cnt_prev4", "custinit_CR_cnt_prev5", "custinit_CR_cnt_prev6","custinit_DR_cnt_prev1", "custinit_DR_cnt_prev2", "custinit_DR_cnt_prev3", 
        "custinit_DR_cnt_prev4", "custinit_DR_cnt_prev5", "custinit_DR_cnt_prev6"
)

tt3[tt3 %in% names(new)]  

#removing redundanr columns
new[,tt3[tt3 %in% names(new)]] = NULL

dim(new)
#---------------------------------------------------------------------------------------------
#all columns which all having amounts in rupees*** 

dput(as.character(paste("D_prev",c(1:6),sep = "")))

tt4 = c("ATM_amt_prev1", "ATM_amt_prev2", "ATM_amt_prev3", "ATM_amt_prev4", 
        "ATM_amt_prev5", "ATM_amt_prev6","ATM_CW_Amt_prev1", "ATM_CW_Amt_prev2", "ATM_CW_Amt_prev3", 
        "ATM_CW_Amt_prev4", "ATM_CW_Amt_prev5", "ATM_CW_Amt_prev6","BRN_CW_Amt_prev1", "BRN_CW_Amt_prev2", "BRN_CW_Amt_prev3", 
        "BRN_CW_Amt_prev4", "BRN_CW_Amt_prev5", "BRN_CW_Amt_prev6","BRN_CASH_Dep_Amt_prev1", "BRN_CASH_Dep_Amt_prev2", "BRN_CASH_Dep_Amt_prev3", 
        "BRN_CASH_Dep_Amt_prev4", "BRN_CASH_Dep_Amt_prev5", "BRN_CASH_Dep_Amt_prev6","BRN_CASH_Dep_Cnt_prev1", "BRN_CASH_Dep_Cnt_prev2", "BRN_CASH_Dep_Cnt_prev3", 
        "BRN_CASH_Dep_Cnt_prev4", "BRN_CASH_Dep_Cnt_prev5", "BRN_CASH_Dep_Cnt_prev6","CNR_prev1", "CNR_prev2", "CNR_prev3", "CNR_prev4", "CNR_prev5", 
        "CNR_prev6","BAL_prev1", "BAL_prev2", "BAL_prev3", "BAL_prev4", "BAL_prev5", 
        "BAL_prev6","EOP_prev1", "EOP_prev2", "EOP_prev3", "EOP_prev4", "EOP_prev5", 
        "EOP_prev6","CR_AMB_Prev1", "CR_AMB_Prev2", "CR_AMB_Prev3", "CR_AMB_Prev4", 
        "CR_AMB_Prev5", "CR_AMB_Prev6","C_prev1", "C_prev2", "C_prev3", "C_prev4",
        "C_prev5", "C_prev6","D_prev1", "D_prev2", "D_prev3", "D_prev4", "D_prev5", "D_prev6")

summary(new[,tt4])

describe(new[,tt4])

#mean of each column
data.frame(sapply(new[,tt4], function(x) {mean(x)})) 

#mean corresponding to target type of each column
t(summarise_at(group_by(new, Responders), vars(c(tt4)),funs(round(mean(., na.rm = F)))))



t(summarise_at(group_by(new, Responders), vars(c(tt4)),
               funs(round(mean(., na.rm = F)),quantile(., probs = c(.96,.97,.98,.99,.9999,1.0),na.rm = T)  )))


t(sapply(new[,tt4], function(x) {quantile(x, probs = c(0.1, 0.2, 0.3, 0.4, 0.5,0.6,0.7,0.8,0.9,.93,.96,.97,.98,.99,.9999,1.0),na.rm = T)  }))



# "log" transformation of numeric variable are in rupees ***********
data.frame(sapply(new[,tt4], function(x) {sd(x)>50}))

temp1 = c("ATM_amt_prev1","ATM_amt_prev2","ATM_amt_prev3","ATM_amt_prev4","ATM_amt_prev5","ATM_amt_prev6","ATM_CW_Amt_prev1","ATM_CW_Amt_prev2","ATM_CW_Amt_prev3","ATM_CW_Amt_prev4","ATM_CW_Amt_prev5","ATM_CW_Amt_prev6","BRN_CASH_Dep_Amt_prev1","BRN_CASH_Dep_Amt_prev2","BRN_CASH_Dep_Amt_prev3","BRN_CASH_Dep_Amt_prev4","BRN_CASH_Dep_Amt_prev5","BRN_CASH_Dep_Amt_prev6","BRN_CASH_Dep_Cnt_prev3","BRN_CASH_Dep_Cnt_prev5","CNR_prev1","CNR_prev2","CNR_prev3","CNR_prev4","CNR_prev5","CNR_prev6","BAL_prev1","BAL_prev2","BAL_prev3","BAL_prev4","BAL_prev5","BAL_prev6","EOP_prev1","EOP_prev2","EOP_prev3","EOP_prev4","EOP_prev5","EOP_prev6","CR_AMB_Prev1","CR_AMB_Prev2","CR_AMB_Prev3","CR_AMB_Prev4","CR_AMB_Prev5","CR_AMB_Prev6","C_prev1","C_prev2","C_prev3","C_prev4","C_prev5","C_prev6","D_prev1","D_prev2","D_prev3","D_prev4","D_prev5","D_prev6")

describe(new[,temp1])
t(sapply(new[,temp1], function(x) {quantile(x, probs = c(0.5,0.6,0.7,0.8,0.9,.93,.96,.97,.98,.99,.9990,.9999,1.0),na.rm = T)  }))

A <- function(x) log(x+(1-min(x)))
new[,temp1] <- apply(new[,temp1], 2, A ) 

describe(new[,tt4])


#===============================================================================
#preprocessing from col 246 to col 377
#================================================================================

#all variables for understanding******no preprocessing
tt5 = c("FRX_PrevQ1","EFT_SELF_TRANSFER_PrevQ1","Billpay_Active_PrevQ1","Billpay_Reg_ason_Prev1","FD_AMOUNT_BOOK_PrevQ1","FD_AMOUNT_BOOK_PrevQ2","NO_OF_FD_BOOK_PrevQ1","NO_OF_FD_BOOK_PrevQ2","NO_OF_RD_BOOK_PrevQ1","NO_OF_RD_BOOK_PrevQ2","RD_AMOUNT_BOOK_PrevQ1","RD_AMOUNT_BOOK_PrevQ2","Total_Invest_in_MF_PrevQ1","Total_Invest_in_MF_PrevQ2","count_No_of_MF_PrevQ1","count_No_of_MF_PrevQ2","Dmat_Investing_PrevQ1","Dmat_Investing_PrevQ2","AGRI_PREM_CLOSED_PREVQ1","AL_CNC_PREM_CLOSED_PREVQ1","AL_PREM_CLOSED_PREVQ1","BL_PREM_CLOSED_PREVQ1","CC_PREM_CLOSED_PREVQ1","CE_PREM_CLOSED_PREVQ1","CV_PREM_CLOSED_PREVQ1","EDU_PREM_CLOSED_PREVQ1","OTHER_LOANS_PREM_CLOSED_PREVQ1","PL_PREM_CLOSED_PREVQ1","RD_PREM_CLOSED_PREVQ1","FD_PREM_CLOSED_PREVQ1","TL_PREM_CLOSED_PREVQ1","TWL_PREM_CLOSED_PREVQ1","AGRI_Closed_PrevQ1","AL_CNC_Closed_PrevQ1","AL_Closed_PrevQ1","BL_Closed_PrevQ1","CC_CLOSED_PREVQ1","CE_Closed_PrevQ1","CV_Closed_PrevQ1","EDU_Closed_PrevQ1","GL_Closed_PrevQ1","OTHER_LOANS_Closed_PrevQ1","PL_Closed_PrevQ1","RD_CLOSED_PREVQ1","FD_CLOSED_PREVQ1","TL_Closed_PrevQ1","TWL_Closed_PrevQ1","DEMAT_CLOSED_PREV1YR","SEC_ACC_CLOSED_PREV1YR","AGRI_TAG_LIVE","AL_CNC_TAG_LIVE","AL_TAG_LIVE","BL_TAG_LIVE","CC_TAG_LIVE","CE_TAG_LIVE","CV_TAG_LIVE","DEMAT_TAG_LIVE","EDU_TAG_LIVE","GL_TAG_LIVE","HL_TAG_LIVE","SEC_ACC_TAG_LIVE","INS_TAG_LIVE","LAS_TAG_LIVE","MF_TAG_LIVE","OTHER_LOANS_TAG_LIVE","PL_TAG_LIVE","RD_TAG_LIVE","FD_TAG_LIVE","TL_TAG_LIVE","TWL_TAG_LIVE","lap_tag_live","AGRI_DATE","AL_CNC_DATE","AL_DATE","BL_DATE","CE_DATE","CV_DATE","EDU_DATE","GL_DATE","LAP_DATE","LAS_DATE","OTHER_LOANS_DATE","PL_DATE","TL_DATE","TWL_DATE","Charges_PrevQ1","Charges_cnt_PrevQ1","NO_OF_COMPLAINTS","CASH_WD_AMT_Last6","CASH_WD_CNT_Last6","Billpay_Active_PrevQ1_N","Billpay_Reg_ason_Prev1_N","Charges_cnt_PrevQ1_N","FRX_PrevQ1_N","brn_code","RBI_Class_Audit","age","gender_bin","Recency_of_CR_TXN","Recency_of_DR_TXN","Recency_of_IB_TXN","Recency_of_ATM_TXN","Recency_of_BRANCH_TXN","Recency_of_POS_TXN","Recency_of_MB_TXN","Recency_of_Activity","I_AQB_PrevQ1","I_AQB_PrevQ2","I_CR_AQB_PrevQ1","I_CR_AQB_PrevQ2","I_CNR_PrevQ1","I_CNR_PrevQ2","I_NRV_PrevQ1","I_NRV_PrevQ2","CR_AMB_Drop_Build_1","CR_AMB_Drop_Build_2","CR_AMB_Drop_Build_3","CR_AMB_Drop_Build_4","CR_AMB_Drop_Build_5","Req_Logged_PrevQ1","Req_Resolved_PrevQ1","Query_Logged_PrevQ1","Query_Resolved_PrevQ1","Complaint_Logged_PrevQ1","Complaint_Resolved_PrevQ1","NO_OF_CHEQUE_BOUNCE_V1","Percent_Change_in_Credits","Percent_Change_in_FT_Bank","Percent_Change_in_FT_outside","Percent_Change_in_Self_Txn","Percent_Change_in_Big_Expenses")

summary(new[,tt5])
describe(new[,tt5])

#mean of each column
data.frame(sapply(new[,tt5], function(x) {mean(x)})) 

#mean corresponding to target type of each column
t(summarise_at(group_by(new,Responders), vars(tt5),funs(round(mean(., na.rm = F)))))


t(sapply(new[,tt5], function(x) {quantile(x, probs = c(0.5,0.6,0.7,0.8,0.9,.93,.96,.97,.98,.99,.9990,.9999,1.0),na.rm = T)  }))


# counts related columns all in no.of times withdrawl/deposits*****
#----------------------------------------------------------------
tt6 = c("NO_OF_FD_BOOK_PrevQ1","NO_OF_FD_BOOK_PrevQ2","count_No_of_MF_PrevQ1","count_No_of_MF_PrevQ2","SEC_ACC_CLOSED_PREV1YR","Charges_cnt_PrevQ1","CASH_WD_CNT_Last6","Charges_cnt_PrevQ1_N")

describe(new[,tt6])

#mean of each column
data.frame(sapply(new[,tt6], function(x) {mean(x)})) 
#mean corresponding to target type of each column
t(summarise_at(group_by(new, Responders), vars(c(tt6)),funs(round(mean(., na.rm = F)))))
t(sapply(new[,tt6], function(x) {quantile(x, probs = c(0.5,0.6,0.7,0.8,0.9,.93,.96,.97,.98,.99,.9990,.9999,1.0),na.rm = T)  }))

#count_No_of_MF_PrevQ1,count_No_of_MF_PrevQ2 looking didfferent

#outlier replacement with 99.99 percintile values*****
temp2 = c("count_No_of_MF_PrevQ1","count_No_of_MF_PrevQ2")
library(scales)
new[,temp2] = sapply(new[,temp2], function(x){squish(x, round(quantile(x, c(.01, .9999))))})
describe(new[,tt6])



#all numeric columns in rupees********
#------------------------------------------
tt7 = c("RD_AMOUNT_BOOK_PrevQ2","RD_AMOUNT_BOOK_PrevQ1","FRX_PrevQ1","EFT_SELF_TRANSFER_PrevQ1","Billpay_Active_PrevQ1","Billpay_Reg_ason_Prev1","FD_AMOUNT_BOOK_PrevQ1","FD_AMOUNT_BOOK_PrevQ2","NO_OF_RD_BOOK_PrevQ1","NO_OF_RD_BOOK_PrevQ2","Total_Invest_in_MF_PrevQ1","Total_Invest_in_MF_PrevQ2","Dmat_Investing_PrevQ1","Dmat_Investing_PrevQ2","AGRI_PREM_CLOSED_PREVQ1","AL_CNC_PREM_CLOSED_PREVQ1","AL_PREM_CLOSED_PREVQ1","BL_PREM_CLOSED_PREVQ1","CC_PREM_CLOSED_PREVQ1","CE_PREM_CLOSED_PREVQ1","CV_PREM_CLOSED_PREVQ1","EDU_PREM_CLOSED_PREVQ1","OTHER_LOANS_PREM_CLOSED_PREVQ1","PL_PREM_CLOSED_PREVQ1","RD_PREM_CLOSED_PREVQ1","FD_PREM_CLOSED_PREVQ1","TL_PREM_CLOSED_PREVQ1","TWL_PREM_CLOSED_PREVQ1","AGRI_Closed_PrevQ1","AL_CNC_Closed_PrevQ1","AL_Closed_PrevQ1","BL_Closed_PrevQ1","CC_CLOSED_PREVQ1","CE_Closed_PrevQ1","CV_Closed_PrevQ1","EDU_Closed_PrevQ1","GL_Closed_PrevQ1","OTHER_LOANS_Closed_PrevQ1","PL_Closed_PrevQ1","RD_CLOSED_PREVQ1","FD_CLOSED_PREVQ1","TL_Closed_PrevQ1","TWL_Closed_PrevQ1","DEMAT_CLOSED_PREV1YR","AGRI_TAG_LIVE","AL_CNC_TAG_LIVE","AL_TAG_LIVE","BL_TAG_LIVE","CC_TAG_LIVE","CE_TAG_LIVE","CV_TAG_LIVE","DEMAT_TAG_LIVE","EDU_TAG_LIVE","GL_TAG_LIVE","HL_TAG_LIVE","SEC_ACC_TAG_LIVE","INS_TAG_LIVE","LAS_TAG_LIVE","MF_TAG_LIVE","OTHER_LOANS_TAG_LIVE","PL_TAG_LIVE","RD_TAG_LIVE","FD_TAG_LIVE","TL_TAG_LIVE","TWL_TAG_LIVE","lap_tag_live","AGRI_DATE","AL_CNC_DATE","AL_DATE","BL_DATE","CE_DATE","CV_DATE","EDU_DATE","GL_DATE","LAP_DATE","LAS_DATE","OTHER_LOANS_DATE","PL_DATE","TL_DATE","TWL_DATE","Charges_PrevQ1","NO_OF_COMPLAINTS","CASH_WD_AMT_Last6","Billpay_Active_PrevQ1_N","Billpay_Reg_ason_Prev1_N","FRX_PrevQ1_N","brn_code","RBI_Class_Audit","age","gender_bin","Recency_of_CR_TXN","Recency_of_DR_TXN","Recency_of_IB_TXN","Recency_of_ATM_TXN","Recency_of_BRANCH_TXN","Recency_of_POS_TXN","Recency_of_MB_TXN","Recency_of_Activity","I_AQB_PrevQ1","I_AQB_PrevQ2","I_CR_AQB_PrevQ1","I_CR_AQB_PrevQ2","I_CNR_PrevQ1","I_CNR_PrevQ2","I_NRV_PrevQ1","I_NRV_PrevQ2","CR_AMB_Drop_Build_1","CR_AMB_Drop_Build_2","CR_AMB_Drop_Build_3","CR_AMB_Drop_Build_4","CR_AMB_Drop_Build_5","Req_Logged_PrevQ1","Req_Resolved_PrevQ1","Query_Logged_PrevQ1","Query_Resolved_PrevQ1","Complaint_Logged_PrevQ1","Complaint_Resolved_PrevQ1","NO_OF_CHEQUE_BOUNCE_V1","Percent_Change_in_Credits","Percent_Change_in_FT_Bank","Percent_Change_in_FT_outside","Percent_Change_in_Self_Txn","Percent_Change_in_Big_Expenses","Responders")

summary(new[,tt7])
describe(new[,tt7])

#mean of each column
data.frame(sapply(new[,tt7], function(x) {mean(x)})) 
#mean corresponding to target type of each column
t(summarise_at(group_by(new, Responders), vars(c(tt7)),funs(round(mean(., na.rm = F)))))
t(sapply(new[,tt7], function(x) {quantile(x, probs = c(0.5,0.6,0.7,0.8,0.9,.93,.96,.97,.98,.99,.9990,.9999,1.0),na.rm = T)  }))


data.frame(sapply(new[,tt7], function(x) {sd(x)>100}))
temp3 = c("RD_AMOUNT_BOOK_PrevQ2","RD_AMOUNT_BOOK_PrevQ1","FD_AMOUNT_BOOK_PrevQ1","FD_AMOUNT_BOOK_PrevQ2","Total_Invest_in_MF_PrevQ1","Total_Invest_in_MF_PrevQ2","Dmat_Investing_PrevQ1","Dmat_Investing_PrevQ2","AL_DATE","Charges_PrevQ1","CASH_WD_AMT_Last6","brn_code","Recency_of_Activity","I_AQB_PrevQ1","I_AQB_PrevQ2","I_CR_AQB_PrevQ1","I_CR_AQB_PrevQ2","I_CNR_PrevQ1","I_CNR_PrevQ2","I_NRV_PrevQ1","I_NRV_PrevQ2","CR_AMB_Drop_Build_1","CR_AMB_Drop_Build_2","CR_AMB_Drop_Build_3","CR_AMB_Drop_Build_4","CR_AMB_Drop_Build_5","Percent_Change_in_Credits","Percent_Change_in_FT_outside","Percent_Change_in_Self_Txn","Percent_Change_in_Big_Expenses")


describe(new[,temp3])
t(sapply(new[,temp3], function(x) {quantile(x, probs = c(0.5,0.6,0.7,0.8,0.9,.93,.96,.97,.98,.99,.9990,.9999,1.0),na.rm = T)  }))

# "log" transformation of numeric variable are in rupees
A <- function(x) log(x+(1-min(x)))
new[,temp3] <- apply(new[,temp3], 2, A ) 
describe(new[,tt7])

#===========================================================================================





































