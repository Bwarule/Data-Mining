## Future past data 
#############################################################################################################
## Project : xyz                                                                                       #####
## Date : 26 Feb 2016                                                                                   #####
## Purpose : Data processing & cleaning information                                                     #####
## Author : Bharat Warule                                                                               #####
#############################################################################################################

## Required Library ##
library(RJDBC) 
library(DBI)
library(reshape)
library(dplyr)
library(tidyr)

url <- "jdbc:redshift://xprize-infosys-en.cv7rw2nnt3rz.ap-southeast-2.redshift.amazonaws.com:5439/dp"
user='sa'
password='4ED45F0F-rt'
## connect to Amazon Redshift
driver <- JDBC("com.amazon.redshift.jdbc41.Driver", 
               "C:\\JSQL\\RedshiftJDBC41-1.1.2.0002.jar", identifier.quote="'")
conn <- dbConnect(driver, url, user, password)
options( java.parameters = "-Xmx4g" )


## ---------------------------------------------------------------------------------------------------------- ##
## Get The customer's MOB: How long customer is working with Rio Tinto
## ---------------------------------------------------------------------------------------------------------- ##
## where kna1.customernumber in (select distinct customernumber from dbo.infydriver where infydriver.fiscalyear = '2014'
kna1_query <- "select customernumber, 
countrykey, 
city, 
CAST(dateonwhichtherecordwascreated as date) as dateonwhichtherecordwascreated  
from dbo.kna1"
kna1_info <- dbGetQuery(conn, kna1_query)
kna1_info$dateonwhichtherecordwascreated <- 
  as.Date(kna1_info$dateonwhichtherecordwascreated)
kna1_info$MOB <- as.numeric(round((as.Date('2014-12-31')-kna1_info$dateonwhichtherecordwascreated)/30))
table(kna1_info$MOB >= 6 )

# FALSE  TRUE 
##  3753 29262
kna1_info_MOB62014 <- kna1_info[which(kna1_info$MOB >=  6),]

## ---------------------------------------------------------------------------------------------------------- ##
## Define Defaulter/Non Defaulter
## ---------------------------------------------------------------------------------------------------------- ##
infyoutstanding_query <- "select customernumber, flag,
duedate,fiscalperiod,fiscalyear, 
paymentdefault from dbo.infyinvoice
where CAST(infyinvoice.duedate as date) <= '2014-12-31'
and CAST(infyinvoice.documentdateindocument as date) <= '2014-12-31'"
infyoutstanding <- dbGetQuery(conn, infyoutstanding_query)
infyoutstanding$duedate <- as.Date(infyoutstanding$duedate)
## kna1_info_MOB62015
infyoutstanding_all_final <-
  infyoutstanding[which(infyoutstanding$customernumber %in% kna1_info_MOB62014$customernumber),]
paymentdefaultData <- 
  infyoutstanding_all_final[,c("customernumber","fiscalperiod","fiscalyear","paymentdefault")]

paymentdefaultData1 <- paymentdefaultData %>%
  dplyr::group_by(customernumber,fiscalperiod,fiscalyear) %>%
  dplyr::summarize(paymentdefaultY = sum(paymentdefault %in% c('Y')),
                   paymentdefaultN = sum(paymentdefault %in% c('N')))


## ---------------------------------------------------------------------------------------------------------- ##
## Define finalize the Defaulter/Non Defaulter for year 2014
## ---------------------------------------------------------------------------------------------------------- ##
customernumber <- as.character(unique(paymentdefaultData1$customernumber))
defaultData1_2014 <- paymentdefaultData1[which(paymentdefaultData1$fiscalyear == 2014),]
defaultData1_2014_info <- defaultData1_2014 %>%
  dplyr::group_by(customernumber,fiscalyear) %>%
  dplyr::summarize(paymentdefaultY = sum(paymentdefaultY),
                   paymentdefaultN = sum(paymentdefaultN))

defaultData1_2014_info$paymentdefault_rate <- 
  defaultData1_2014_info$paymentdefaultY/(defaultData1_2014_info$paymentdefaultN + 
                                            defaultData1_2014_info$paymentdefaultY)

## DataIn <- data.frame(customernumber=customernumber, defaultor = 0) ## 8713 rows
DataIn <- defaultData1_2014_info[,c("customernumber","paymentdefault_rate")]

DataIn$defaultor <- DataIn$paymentdefault_rate
# DataIn$paymentdefault_rate[is.na(DataIn$paymentdefault_rate)] <- 0
DataIn$defaultor <- round(DataIn$defaultor)

## ---------------------------------------------------------------------------------------------------------- ##
## Define paymentdefault_rate for year 2013
## ---------------------------------------------------------------------------------------------------------- ##
defaultData1_2013 <- paymentdefaultData1[which(paymentdefaultData1$fiscalyear == 2013),]
defaultData1_2013_info <- defaultData1_2013 %>%
  dplyr::group_by(customernumber,fiscalyear) %>%
  dplyr::summarize(paymentdefaultY = sum(paymentdefaultY),
                   paymentdefaultN = sum(paymentdefaultN))

defaultData1_2013_info$paymentdefault_rate <- 
  defaultData1_2013_info$paymentdefaultY/(defaultData1_2013_info$paymentdefaultN + defaultData1_2013_info$paymentdefaultY)

## Merge DataIn from the Mycode_4march.R code
names(defaultData1_2013_info) <- c(names(defaultData1_2013_info)[1:4],"paymentdefault_rate_Lastyear")
DataInfinal <- merge(x = DataIn, y = defaultData1_2013_info[c( "customernumber", "paymentdefault_rate_Lastyear")] , 
                     by = c("customernumber"),  all.x = TRUE)
names(DataInfinal) 


## ---------------------------------------------------------------------------------------------------------- ##
## Define terms of payment key used by customer
## ---------------------------------------------------------------------------------------------------------- ##
query <- "select customernumber,fiscalyear,fiscalperiod, termsofpaymentkey 
from dbo.infyinvoice
where CAST(infyinvoice.duedate as date) <= '2014-12-31'
and CAST(infyinvoice.documentdateindocument as date) <= '2014-12-31'"
termsofpaymentkey <- dbGetQuery(conn, query)
dim(termsofpaymentkey) ##641201      4
head(termsofpaymentkey)

library(dplyr)
library(tidyr)
termsofpaymentkey$termsofpaymentkey <- as.character(termsofpaymentkey$termsofpaymentkey)

## Payment term used for model development which occurs more than 5000
## 1000 1002 1005 100X 1020 1050 1070 1100 110A 1140 1150 1200 1250 1300 1450 1600
## 4050 6140  ZZZZ 7000 
##  tab_terms_payment$Var1[which(tab_terms_payment$Freq > 5000)]

termsofpaymentkey_new <- termsofpaymentkey %>%
  ## dplyr::arrange(customernumber, fiscalyear, fiscalperiod) %>%
  dplyr::group_by(customernumber, fiscalyear, fiscalperiod) %>%
  dplyr::summarize(payterm_1000_key = sum(termsofpaymentkey %in% c('1000')), 
                   payterm_1005_key = sum(termsofpaymentkey %in% c('1005')), 
                   payterm_100X_key = sum(termsofpaymentkey %in% c('100X')), 
                   payterm_1020_key = sum(termsofpaymentkey %in% c('1020')), 
                   payterm_1050_key = sum(termsofpaymentkey %in% c('1050')), 
                   payterm_1070_key = sum(termsofpaymentkey %in% c('1070')), 
                   payterm_1100_key = sum(termsofpaymentkey %in% c('1100')), 
                   payterm_111A_key = sum(termsofpaymentkey %in% c('110A')), 
                   payterm_1140_key = sum(termsofpaymentkey %in% c('1140')), 
                   payterm_1150_key = sum(termsofpaymentkey %in% c('1150')), 
                   payterm_1200_key = sum(termsofpaymentkey %in% c('1200')), 
                   payterm_1250_key = sum(termsofpaymentkey %in% c('1250')), 
                   payterm_1300_key = sum(termsofpaymentkey %in% c('1300')), 
                   payterm_1450_key = sum(termsofpaymentkey %in% c('1450')), 
                   payterm_1600_key = sum(termsofpaymentkey %in% c('1600')), 
                   payterm_4050_key = sum(termsofpaymentkey %in% c('4050')), 
                   payterm_6140_key = sum(termsofpaymentkey %in% c('6140')), 
                   payterm_7000_key = sum(termsofpaymentkey %in% c('7000')), 
                   payterm_zzzz_key = sum(termsofpaymentkey %in% c('ZZZZ')),	
                   payterm_1002_key = sum(termsofpaymentkey %in% c('1002')))

query <- "select customernumber,fiscalyear,fiscalperiod, count(*) as max_termsofpaymentkey 
from dbo.infyinvoice
where CAST(infyinvoice.duedate as date) <= '2014-12-31'
and CAST(infyinvoice.documentdateindocument as date) <= '2014-12-31'
group by  customernumber,fiscalyear,fiscalperiod"
max_termsofpaymentkey <- dbGetQuery(conn, query)

termsofpaymentkey_new <- merge(x = termsofpaymentkey_new, y = max_termsofpaymentkey , 
                               by = c("customernumber", "fiscalyear", "fiscalperiod"),  all.x = TRUE)
dim(termsofpaymentkey_new) ## 81646 

## merge this with defaulter & non-defaulter data
##  termsofpaymentkey_new : (156922,23) & infy :  (156922,5)
termsofpaymentkey_newold <- termsofpaymentkey_new ## backup
termsofpaymentkey_new <- termsofpaymentkey_new[which(termsofpaymentkey_new$fiscalyear == '2014'),]
termsofpaymentkey_new$fiscalyear <- NULL
termsofpaymentkey_new$fiscalperiod <- NULL

termsofpaymentkey_new  <- termsofpaymentkey_new  %>%
  group_by(customernumber) %>% summarise_each(funs(sum))
DataInfinal <- merge(x = DataInfinal, y = termsofpaymentkey_new, by = c("customernumber"),  all.x = TRUE)
dim(DataInfinal) 

## ---------------------------------------------------------------------------------------------------------- ##
## Get The customer's credit limit
## ---------------------------------------------------------------------------------------------------------- ##
knkk_query <- "select customernumber,
creditcontrolarea,
customerscreditlimit,
creditmanagementriskcategory from dbo.knkk"
knkk_info <- dbGetQuery(conn, knkk_query)
## [1] "2600" "3100" "9040" "2040" "2041" "9190" "9300" "9500" "3000" "9000" "8100"
## [12] "9050" "2602" "6600" "1000" "4020" "3600" "1300" "6170" "4500" "4000" "9100"
## [23] "5000" "2000" "2601" "9610" "6000" "8000" "6500" "1001" "6200" "4600" "C000"
## [34] "2170" "2700" "1100" "8800" "1050" "4400" "4900" "4700" "2702" "8151" "X000"
## [45] "4510" "9400" "1170" "2001" "2500"
## 22234 unique customernumber id present

knkk_info_credit <- knkk_info[,c( "customernumber","creditcontrolarea","customerscreditlimit")]
knkk_info_credit_spread <- spread(knkk_info_credit, creditcontrolarea, customerscreditlimit)
## dim  22234   50
## Replace all missing values 0, if there is no credit limit for any creditcontrolarea then it will be Zero
knkk_info_credit_spread[is.na(knkk_info_credit_spread)] <- 0
names(knkk_info_credit_spread)[-1] <- paste( "creditlimit",names(knkk_info_credit_spread)[-1],"controlarea",sep="_")

## Z00  Z01  Z02  Z03  Z04  Z05  Z06  Z99 this are creditmanagementriskcategory present in table
knkk_info_riskcat <- knkk_info[,c( "customernumber","creditmanagementriskcategory")]
knkk_info_riskcat <- knkk_info_riskcat %>%
  dplyr::group_by(customernumber) %>% 
  dplyr::summarize(riskcategoryZ00 = sum(creditmanagementriskcategory %in% c('Z00')), 
                   riskcategoryZ01 = sum(creditmanagementriskcategory %in% c('Z01')), 
                   riskcategoryZ02 = sum(creditmanagementriskcategory %in% c('Z02')), 
                   riskcategoryZ03 = sum(creditmanagementriskcategory %in% c('Z03')), 
                   riskcategoryZ04 = sum(creditmanagementriskcategory %in% c('Z04')),
                   riskcategoryZ05 = sum(creditmanagementriskcategory %in% c('Z05')), 
                   riskcategoryZ06 = sum(creditmanagementriskcategory %in% c('Z06')),
                   riskcategoryZ99 = sum(creditmanagementriskcategory %in% c('Z99')))
## final data contains knkk_info_riskcat : 22234    9
## final data contains knkk_info_credit_spread : 22234    50

## ------------------------------------------------------------------------------------------------------------------ ##
dim(DataInfinal) ## 3180   25
DataInfinal <- merge(x = DataInfinal, y = knkk_info_riskcat, by = c("customernumber"),  all.x = TRUE)
DataInfinal <- merge(x = DataInfinal, y = knkk_info_credit_spread, by = c("customernumber"),  all.x = TRUE)


## ---------------------------------------------------------------------------------------------------------- ##
## Get The customer's countrykey,  city, MOB
## ---------------------------------------------------------------------------------------------------------- ##
DataInfinal <- merge(x = DataInfinal, y = kna1_info_MOB62014, by = c("customernumber"),  all.x = TRUE)
DataInfinal$dateonwhichtherecordwascreated <- NULL
#cust_data$paymentdefault_rate <- round(cust_data$paymentdefault_rate)
country <- data.frame(table(DataInfinal$countrykey,DataInfinal$defaultor))
country_non_defaultor <- country[which(country$Var2 == 0),]
names(country_non_defaultor) <- c("country_name","resp","count_non_defaultor")
country_defaultor <- country[which(country$Var2 == 1),]
names(country_defaultor) <- c("country_name","resp","count_defaultor")
country_non_defaultor$resp <- NULL
country_defaultor$resp <- NULL
country_data <- merge(x = country_defaultor, y = country_non_defaultor, 
                      by = c("country_name"))
country_data[with(country_data,order(-count_defaultor)),]
##    country_name count_defaultor count_non_defaultor
#            AU            1117                 439
#           US             289                 246
#           CA             228                 172
#           MG              94                   5
#           CN              72                 139
#           GB              45                  33
#           IN              32                  52

top_country <- c("AU", "US", "CA", "MG")
DataInfinal$countrykey <- as.character(DataInfinal$countrykey)
DataInfinal$countrykey[!(DataInfinal$countrykey %in% top_country)] <- "other_country"

## ---------------------------------------------------------------------------------------------------------- ##
## Define customer transaction time
## ---------------------------------------------------------------------------------------------------------- ##
infyoutstanding_query <- "select customernumber,
CAST( baselinedateforduedatecalculation as date) as BaseDate
from dbo.infyinvoice
where CAST(infyinvoice.duedate as date) <= '2014-12-31'
and CAST(infyinvoice.documentdateindocument as date) <= '2014-12-31'"

infy_BaseDate <- dbGetQuery(conn, infyoutstanding_query)

uniq_cust <- length(unique(infy_BaseDate$customernumber))
cust_id <- as.character(unique(infy_BaseDate$customernumber))
final_transaction_time <- data.frame()

for(i in 1:uniq_cust){
  cust_idinfo <- cust_id[i]
  infy_BaseDateIn <- infy_BaseDate[which(infy_BaseDate$customernumber == cust_idinfo),]
  infy_BaseDateIn <- unique(infy_BaseDateIn)
  if(nrow(infy_BaseDateIn)<=1){
    next;
  }
  infy_BaseDateIn <- infy_BaseDateIn[order(infy_BaseDateIn$basedate),]
  infy_BaseDateIn$nextDate <- c(infy_BaseDateIn$basedate[-1],NA)  
  infy_BaseDateIn <- infy_BaseDateIn[!(is.na(infy_BaseDateIn$nextDate)),]
  time_diff <- as.numeric(as.Date(infy_BaseDateIn$nextDate) -as.Date(infy_BaseDateIn$basedate))
  new_data <- data.frame(customernumber= cust_idinfo,
                         min_trns = min(time_diff),
                         averge= mean(time_diff,na.rm=TRUE),
                         median_time = median(time_diff,na.rm=TRUE),
                         max_trns = max(time_diff),
                         count = length(time_diff))
  if(nrow(final_transaction_time) == 0){
    final_transaction_time <- new_data
  }else{
    final_transaction_time <- rbind(final_transaction_time, new_data)
  }
  print(i)
  
}

## averge
val <- quantile(final_transaction_time$averge,c(0.05,0.95))
final_transaction_time$averge[which(final_transaction_time$averge > 300.04)] <- 300.05

## min_trns
val <- quantile(final_transaction_time$min_trns,c(0.05,0.95))
final_transaction_time$min_trns[which(final_transaction_time$min_trns > 190)] <- 190

## max_trns
val <- quantile(final_transaction_time$max_trns,c(0.05,0.95))
final_transaction_time$max_trns[which(final_transaction_time$max_trns > 689)] <- 689

val <- quantile(final_transaction_time$count,c(0.05,0.95))
final_transaction_time$count[which(final_transaction_time$count > 177.05)] <- 177.05

## Merge DataIn from the Mycode_4march.R code
DataInfinal <- merge(x = DataInfinal, y = final_transaction_time , 
                     by = c("customernumber"),  all.x = TRUE)

## impute the missing value mean
DataInfinal$min_trns[is.na(DataInfinal$min_trns)] <- median(DataInfinal$min_trns,na.rm=TRUE)
DataInfinal$max_trns[is.na(DataInfinal$max_trns)] <- median(DataInfinal$max_trns,na.rm=TRUE)
DataInfinal$averge[is.na(DataInfinal$averge)] <- median(DataInfinal$averge,na.rm=TRUE)
DataInfinal$median_time[is.na(DataInfinal$median_time)] <- median(DataInfinal$median_time,na.rm=TRUE)
DataInfinal$count[is.na(DataInfinal$count)] <- median(DataInfinal$count,na.rm=TRUE)
DataInfinal$log_MOB <- log(DataInfinal$MOB)

## ---------------------------------------------------------------------------------------------------------- ##
## Define customertype transaction time
## ---------------------------------------------------------------------------------------------------------- ##

query <- "select  customertype, customernumber from dbo.infyinvoice 
where CAST(infyinvoice.duedate as date) <= '2014-12-31'
and CAST(infyinvoice.documentdateindocument as date) <= '2014-12-31'
and fiscalyear = '2014'"
customertypein <- dbGetQuery(conn, query)
customertypein <- unique(customertypein)
DataInfinal <- merge(x = DataInfinal, y = customertypein,
                     by = c("customernumber"),  all.x = TRUE)

## ---------------------------------------------------------------------------------------------------------- ##
## Define customer Days_paid_late time
## ---------------------------------------------------------------------------------------------------------- ##
conn <- dbConnect(driver, url, user, password)
query <- "select  customernumber, 
                       duedate,
            CAST(infyinvoice.clearingdate as date) as clearingdate,
             fiscalyear,fiscalperiod from dbo.infyinvoice 
       where CAST(infyinvoice.duedate as date) <= '2014-12-31'      
and CAST(infyinvoice.documentdateindocument as date) <= '2014-12-31'
and infyinvoice.clearingdate != '00000000'"
Days_paid_late <- dbGetQuery(conn, query)
Days_paid_late$Dayspaid_late <- 
  as.Date(Days_paid_late$clearingdate) - as.Date(Days_paid_late$duedate)
Days_paid_late$Dayspaid_late <- 
  as.numeric(Days_paid_late$Dayspaid_late )
Days_paid_late$Dayspaid_early <- Days_paid_late$Dayspaid_late
Days_paid_late$Dayspaid_early[which(Days_paid_late$Dayspaid_early >= 0)] <- 0
Days_paid_late$Dayspaid_early <- -1*Days_paid_late$Dayspaid_early
Days_paid_late$Dayspaid_ontime <- "not_intime"
Days_paid_late$Dayspaid_ontime[which(Days_paid_late$Dayspaid_early == 0)] <- "intime"
Days_paid_late$Dayspaid_latein <- Days_paid_late$Dayspaid_late
Days_paid_late$Dayspaid_latein[which(Days_paid_late$Dayspaid_latein < 0)] <- 0

intimepayment_data <- Days_paid_late %>%
  dplyr::group_by(customernumber,fiscalyear) %>% 
  dplyr::summarize(not_intime_count = sum(Dayspaid_ontime %in% c('not_intime')), 
                   intime_count = sum(Dayspaid_ontime %in% c('intime')))

intimepayment_data2013 <-
  intimepayment_data[which(intimepayment_data$fiscalyear==2013),]
names(intimepayment_data2013) <- c("customernumber","fiscalyear",
                                   "not_intime_count_lastyear","intime_count_lastyear")
#intimedata2013 <- intimepayment_data2013 %>%
#  dplyr::group_by(customernumber) %>% 
#  dplyr::summarize(not_intime_count_sum_lastyear = sum(not_intime_count,na.rm=TRUE),
#                   not_intime_count_avg_lastyear = mean(not_intime_count,na.rm=TRUE),
#                   not_intime_count_min_lastyear = min(not_intime_count,na.rm=TRUE),
#                   not_intime_count_max_lastyear = max(not_intime_count,na.rm=TRUE),
#                   intime_count_sum_lastyear = sum(intime_count,na.rm=TRUE),
#                   intime_count_avg_lastyear = mean(intime_count,na.rm=TRUE),
#                   intime_count_min_lastyear = min(intime_count,na.rm=TRUE),
#                   intime_count_max_lastyear = max(intime_count,na.rm=TRUE))

Days_paid_latesummary <-   Days_paid_late %>%
  dplyr::group_by(customernumber,fiscalyear) %>% 
  dplyr::summarize(sum_Dayspaid_latein = sum(Dayspaid_latein,na.rm=TRUE),
                   min_Dayspaid_latein = min(Dayspaid_latein,na.rm=TRUE),
                   max_Dayspaid_latein = max(Dayspaid_latein,na.rm=TRUE),
                   mean_Dayspaid_latein = mean(Dayspaid_latein,na.rm=TRUE),
                  sum_Dayspaid_early = sum(Dayspaid_early,na.rm=TRUE),
                   min_Dayspaid_early = min(Dayspaid_early,na.rm=TRUE),
                   max_Dayspaid_early = max(Dayspaid_early,na.rm=TRUE),
                   mean_Dayspaid_early = mean(Dayspaid_early,na.rm=TRUE))
     
Days_paid_latesummary2013 <-
  Days_paid_latesummary[which(Days_paid_latesummary$fiscalyear==2013),]

names(Days_paid_latesummary2013) <- c("customernumber","fiscalyear",
    "sum_Dayspaid_latein_lastyear","min_Dayspaid_latein_lastyear","max_Dayspaid_latein_lastyear",
    "mean_Dayspaid_latein_lastyear", "sum_Dayspaid_early_lastyear","min_Dayspaid_early_lastyear",  
    "max_Dayspaid_early_lastyear",   "mean_Dayspaid_early_lastyear") 
                                       
#Dayspaid_2013 <- Days_paid_latesummary2013 %>%
#  dplyr::group_by(customernumber) %>% 
#  dplyr::summarize(
#    mean_sum_Dayspaid_latein_lastyear = mean(sum_Dayspaid_latein,na.rm=TRUE),
#    mean_min_Dayspaid_latein_lastyear = mean(min_Dayspaid_latein,na.rm=TRUE),
#    mean_max_Dayspaid_latein_lastyear = mean(max_Dayspaid_latein,na.rm=TRUE),
#    mean_sum_Dayspaid_early_lastyear = mean(sum_Dayspaid_early,na.rm=TRUE),
#    mean_min_Dayspaid_early_lastyear = mean(min_Dayspaid_early,na.rm=TRUE),
#    mean_max_Dayspaid_early_lastyear = mean(max_Dayspaid_early,na.rm=TRUE),
#    sum_mean_Dayspaid_early_lastyear = sum(mean_Dayspaid_early,na.rm=TRUE),
#    mean_mean_Dayspaid_early_lastyear = mean(mean_Dayspaid_early,na.rm=TRUE),
#    min_mean_Dayspaid_early_lastyear = min(mean_Dayspaid_early,na.rm=TRUE),
#    max_mean_Dayspaid_early_lastyear = max(mean_Dayspaid_early,na.rm=TRUE),
#    sum_mean_Dayspaid_latein_lastyear = sum(mean_Dayspaid_latein,na.rm=TRUE),
#    mean_mean_Dayspaid_latein_lastyear = mean(mean_Dayspaid_latein,na.rm=TRUE),
#    min_mean_Dayspaid_latein_lastyear = min(mean_Dayspaid_latein,na.rm=TRUE),
#    max_mean_Dayspaid_latein_lastyear = max(mean_Dayspaid_latein,na.rm=TRUE))

## Dayspaid_2013 3762   15
Days_paid_latesummary2013$fiscalyear <- NULL
DataInfinal <- merge(x = DataInfinal, y = Days_paid_latesummary2013,
                     by = c("customernumber"),  all.x = TRUE)

intimepayment_data2013$fiscalyear <- NULL
DataInfinal <- merge(x = DataInfinal, y = intimepayment_data2013,
                     by = c("customernumber"),  all.x = TRUE)

## Missing value imputation
new_vars <- c(names(Days_paid_latesummary2013),names(intimepayment_data2013))
new_vars <- new_vars[!(new_vars %in% "customernumber")]
for(i in 1:length(new_vars)){
  variable_name <- new_vars[i]
  DataInfinal[is.na(DataInfinal[,variable_name]),variable_name] <- 
                mean(DataInfinal[,variable_name],na.rm=TRUE)
}

### Need to perform outlier analysis
# [1] "sum_Dayspaid_latein_lastyear"  "min_Dayspaid_latein_lastyear"  "max_Dayspaid_latein_lastyear" 
# [4] "mean_Dayspaid_latein_lastyear" "sum_Dayspaid_early_lastyear"   "min_Dayspaid_early_lastyear"  
# [7] "max_Dayspaid_early_lastyear"   "mean_Dayspaid_early_lastyear"  "not_intime_count_lastyear"    
# [10] "intime_count_lastyear"


quantile(DataInfinal$sum_Dayspaid_early_lastyear,c(0.05,0.95))
DataInfinal$sum_Dayspaid_early_lastyear[which(DataInfinal$sum_Dayspaid_early_lastyear > 977.8)] <- 977.8

quantile(DataInfinal$min_Dayspaid_early_lastyear,c(0.05,0.95))
DataInfinal$min_Dayspaid_early_lastyear[which(DataInfinal$min_Dayspaid_early_lastyear > 0.6373)] <- 0.6373

quantile(DataInfinal$max_Dayspaid_early_lastyear,c(0.05,0.95))
DataInfinal$max_Dayspaid_early_lastyear[which(DataInfinal$max_Dayspaid_early_lastyear > 60)] <- 60

quantile(DataInfinal$mean_Dayspaid_early_lastyear,c(0.05,0.95))
DataInfinal$mean_Dayspaid_early_lastyear[which(DataInfinal$mean_Dayspaid_early_lastyear > 20)] <- 20

quantile(DataInfinal$not_intime_count_lastyear,c(0.05,0.95))
DataInfinal$not_intime_count_lastyear[which(DataInfinal$not_intime_count_lastyear > 51)] <- 51 

quantile(DataInfinal$intime_count_lastyear,c(0.05,0.95))
DataInfinal$intime_count_lastyear[which(DataInfinal$intime_count_lastyear > 97.9)] <- 97.9 

quantile(DataInfinal$sum_Dayspaid_latein_lastyear,c(0.05,0.95))
DataInfinal$sum_Dayspaid_latein_lastyear[which(DataInfinal$sum_Dayspaid_latein_lastyear > 3465.1)] <- 3465.1 

quantile(DataInfinal$min_Dayspaid_latein_lastyear,c(0.05,0.95))
DataInfinal$min_Dayspaid_latein_lastyear[which(DataInfinal$min_Dayspaid_latein_lastyear > 30)] <- 30

quantile(DataInfinal$max_Dayspaid_latein_lastyear,c(0.05,0.95))
DataInfinal$max_Dayspaid_latein_lastyear[which(DataInfinal$max_Dayspaid_latein_lastyear > 422)] <- 422 

quantile(DataInfinal$mean_Dayspaid_latein_lastyear,c(0.05,0.95))
DataInfinal$mean_Dayspaid_latein_lastyear[which(DataInfinal$mean_Dayspaid_latein_lastyear > 118.44)] <- 118.44

## ---------------------------------------------------------------------------------------------------------- ##
## Define customer Days_paid_late time
## ---------------------------------------------------------------------------------------------------------- ##
# 2013 
conn <- dbConnect(driver, url, user, password)
query <- "select * from dbo.infybharat where fiscalyear in ('2011','2012','2013')"
materialgroup <- dbGetQuery(conn, query)

materialgroupData <- 
  materialgroup[,c("customernumber","fiscalperiod","fiscalyear","materialgroup","paymentdefault")]

materialgroupDatanew <- materialgroupData %>%
  dplyr::group_by(customernumber,materialgroup) %>%
  dplyr::summarize(paymentdefaultY = sum(paymentdefault %in% c('Y')),
                   paymentdefaultN = sum(paymentdefault %in% c('N')))
materialgroupDatanew$paymentdefaultYNsum <- 
  materialgroupDatanew$paymentdefaultY + materialgroupDatanew$paymentdefaultN
materialgroupDatanew$paymentdefault <-
  materialgroupDatanew$paymentdefaultY/materialgroupDatanew$paymentdefaultYNsum

materialvars <- c("customernumber","materialgroup","paymentdefault")
              
materialgroupDatanew <- materialgroupDatanew[,materialvars]
materialgroup_defaultrate <-
  spread(materialgroupDatanew, materialgroup, paymentdefault)
names(materialgroup_defaultrate)[-1] <- 
  paste("defaultrate_materialgroup_",names(materialgroup_defaultrate)[-1] ,"_last3year",sep="")


DataInfinal <- merge(x = DataInfinal, y = materialgroup_defaultrate,
                     by = c("customernumber"),  all.x = TRUE)

## Do the missing value Imputation ; replace the missing with zero
imp_vars <- names(materialgroup_defaultrate)[-1]
imp_vars <- imp_vars[!(imp_vars %in% "customernumber")]
for(i in 1:length(imp_vars)){
  variable_name <- imp_vars[i]
  DataInfinal[is.na(DataInfinal[,variable_name]),variable_name] <- 0
  ##mean(materialgroup_defaultrate[,variable_name],na.rm=TRUE)
}


info <- data.frame(t(cor(DataInfinal[,c('paymentdefault_rate')],DataInfinal[,c(new_vars)])))
model <-  glm(paste("defaultor ~ ",paste(new_vars,collapse=" +"),sep=" "),
              family=binomial(link = "logit"),data =DataInfinal)

vars <- c("paymentdefault_rate_Lastyear",
          "max_trns",
          "countrykey",
          "customertype",
          #"payterm_1000_key",
          "ratio_payterm_1000_key_max",
          "ratio_payterm_1300_key_max",
          #"ratio_payterm_1450_key_max",
          "MOB")

pointvars <- c(#"mean_sum_Dayspaid_early_lastyear",
               "sum_mean_Dayspaid_early_lastyear",
               "sum_mean_Dayspaid_latein_lastyear",
               #"not_intime_count_avg_lastyear",
               "intime_count_avg_lastyear",
               "min_trns","averge")
## mean_sum_Dayspaid_early_lastyear   0.0064908  0.0011712   5.542 2.99e-08 ***
#3 sum_mean_Dayspaid_early_lastyear  -0.0107620  0.0019319  -5.571 2.54e-08 ***
#  sum_mean_Dayspaid_latein_lastyear  0.0013840  0.0003743   3.698 0.000218 ***
#  not_intime_count_avg_lastyear     -0.1555845  0.0263374  -5.907 3.48e-09 ***
#  intime_count_avg_lastyear          0.0605942  0.0152925   3.962 7.42e-05 ***
  
dev_sample <- 0.8
x3 <- nrow(DataInfinal)
x2 <- as.integer(x3*dev_sample)
sub <- c(sample(1:x3,x2))
train_sample <- DataInfinal[sub,]
test_sample <- DataInfinal[-sub,]

model <-  glm(paste("defaultor ~ ",paste(c(pointvars,vars),collapse = "+"),sep=""),
              family=binomial(link = "logit"),data =train_sample)
summary(model)
Ffit_LR <- model
