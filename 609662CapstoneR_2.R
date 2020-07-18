###adding comments

## Setting Working Directory
setwd("c:\\capstone")

######## Installing and Loading the required packages/Data ##########

#install.packages("MASS")
#install.packages("car")
#install.packages("e1071")
#install.packages("caret", dependencies = c("Depends", "Suggests"))
#install.packages("cowplot")
#install.packages("GGally")

#library(MASS)
#library(car)
#library(e1071)
#library(caret)
#library(cowplot)
#library(caTools)
#library(janitor)
#library(GGally)
#library(ROCR)
library(ggplot2)
library(data.table) 
library(stringr)
library(lubridate)
library(Matrix)
library(reshape)
library(stargazer)
library(dplyr)
library(scales)
library(corrplot)
library(plyr)
library(xlsx)
library("zoo")
library(cowplot)
library(GGally)





## Importing the csv files into dataframe 
df_consumer_electronics<-read.csv("ConsumerElectronics.csv",stringsAsFactors =FALSE)



################################################################
#################### Data Understanding ######################
################################################################

# Dimension
dim(df_consumer_electronics)  # 1648824      20

head(df_consumer_electronics)

str(df_consumer_electronics)

## Checking for NA values across df_consumer_electronics dataframe
sum(is.na(df_consumer_electronics))  #14712 values

## Fetching the NA values for each cloumn of df_consumer_electronics data frame
sapply(df_consumer_electronics, function(x) sum(is.na(x)))
## Observed Values: 

#gmv 4904                      
#cust_id 4904                         
#pincode  4904   

## Fetching the Zero values for each cloumn of df_consumer_electronics data frame
sapply(df_consumer_electronics, function(x) sum(x==0))

#deliverybdays 2359 
#deliverycdays  538
#sla 5979 
#product_mrp 5308
#product_procurement_sla 42461 

#Observing unique values in Category variables - to find any data mismatch due to spellings 
sort(unique(df_consumer_electronics$product_analytic_super_category)) #1 
sort(unique(df_consumer_electronics$product_analytic_category)) #5
sort(unique(df_consumer_electronics$product_analytic_sub_category)) #14
sort(unique(df_consumer_electronics$product_analytic_vertical)) #74

# Reviewing GMV Values
summary(df_consumer_electronics$gmv) 
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#0     340     750    2461    1999  226947    4904 

#Reviewing units Vlaues
summary(df_consumer_electronics$units) 
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#1.000   1.000   1.000   1.022   1.000  50.000 
summary(df_consumer_electronics$sla) #Outlier Noticed - Clear Outlier
#Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#0.000    4.000    6.000    5.688    7.000 1006.000 

summary(df_consumer_electronics$product_procurement_sla) 
count(df_consumer_electronics, 'product_procurement_sla')# outlier noticed -1 & 100

#Reviewing Payment Method
sort(unique(df_consumer_electronics$s1_fact.order_payment_type)) # "COD"     "Prepaid"

#Reviewing Payment Method
summary(df_consumer_electronics$product_mrp) # "COD"     "Prepaid"
count(df_consumer_electronics, 'product_mrp') #0   5308
boxplot(df_consumer_electronics$product_mrp) 
df_high_mrp<-subset(df_consumer_electronics,product_mrp>4108) # Not outlier or error 
View(df_high_mrp)


# Check max and min of "Order.Date"
max(df_consumer_electronics$order_date)    # "2016-07-25 01:19:45"
min(df_consumer_electronics$order_date)    # 2015-05-19 13:42:09
df_consumer_electronics$order_date <- as.Date (df_consumer_electronics$order_date,format = "%Y-%m-%d")
df_consumer_electronics$Month<-as.numeric(df_consumer_electronics$Month) 
df_consumer_electronics$Year<-as.numeric(df_consumer_electronics$Year) 

## As per the requirement, we have to consider data from July 2015 to June 2016.Let's remove other data from dataset
df_consumer_electronics <- 
  subset(df_consumer_electronics, !(Month ==5 & Year==2015|Month ==6 & Year==2015|Month ==7 & Year==2016))

## sanity Check 
min(df_consumer_electronics$order_date) # "2015-07-01" - July 2015
max(df_consumer_electronics$order_date) # "2016-06-30" - June 2016

## Lets create a new column for week number
df_consumer_electronics$week_number <- week(df_consumer_electronics$order_date)
summary(df_consumer_electronics$week_number)
min(filter(df_consumer_electronics,Year==2015)$week_number) ## 26
max(filter(df_consumer_electronics,Year==2015)$week_number) ## 53

min(filter(df_consumer_electronics,Year==2016)$week_number) ## 1
max(filter(df_consumer_electronics,Year==2016)$week_number) ## 26


#Serializing week number 
df_consumer_electronics$week_number_new<- 0
df_consumer_electronics$week_number<-as.numeric(df_consumer_electronics$week_number) 

df_consumer_electronics$week_number_new<-ifelse( df_consumer_electronics$Year==2015,df_consumer_electronics$week_number-25,df_consumer_electronics$week_number+28)

View(df_consumer_electronics)


################################################################
####################### Data Cleaning #######################
################################################################

head(df_consumer_electronics)

#REmvoign unwanted columns
df_consumer_electronics<-df_consumer_electronics[ , -which(names(df_consumer_electronics) %in% c("ï..fsn_id","order_id","order_item_id" , "product_analytic_super_category"))]

#Clearing outlier on sla
count(df_consumer_electronics, 'sla')
boxplot(df_consumer_electronics$sla) 
#Removing SLA > 3rd Qu value 7
df_consumer_electronics$sla[df_consumer_electronics$sla >15] <- 7
df_consumer_electronics$sla<-as.numeric(df_consumer_electronics$sla) 
boxplot(df_consumer_electronics$sla) 

#Clearing outlier on product_procurement_sla

count(df_consumer_electronics, 'product_procurement_sla')

boxplot(df_consumer_electronics$product_procurement_sla)
#Replacing -1 to 0
df_consumer_electronics$product_procurement_sla[df_consumer_electronics$product_procurement_sla ==-1] <- 0
df_consumer_electronics$product_procurement_sla[df_consumer_electronics$product_procurement_sla >15] <- 3
boxplot(df_consumer_electronics$product_procurement_sla)                   

#Deliverybday --> The dispatch delay from wearhouse, "\n" means no delay recorded, 
#0 means a delay was anticipated, but ultimately no delay has occurred.

count(df_consumer_electronics, 'deliverybdays') # \\N 1312972 & 23 -values in the field ,
#replace this values to Zero to show there is no delay
#Replacing "\n" & <0 to 0
df_consumer_electronics$deliverybdays[df_consumer_electronics$deliverybdays == "\\N"] <- "0"
df_consumer_electronics$deliverybdays[df_consumer_electronics$deliverybdays <=0] <- "0"
#Rechecking 
count(df_consumer_electronics, 'deliverybdays') #  1315369


#Deliverycday --> The delivery delay to customer,
#"\n" means no delay recorded, 0 means a delay was anticipated, however, ultimately no delay happened.

count(df_consumer_electronics, 'deliverycdays')  #\\N 1312971
#Replacing "\n" & <0 to 0
df_consumer_electronics$deliverycdays[df_consumer_electronics$deliverycdays == "\\N"] <- "0"
df_consumer_electronics$deliverycdays[df_consumer_electronics$deliverycdays <=0] <- "0"
#Rechecking 
count(df_consumer_electronics, 'deliverycdays')

# Removing rows with NA vlues in gmv
df_consumer_electronics<-df_consumer_electronics[!(is.na(df_consumer_electronics$gmv)), ]


################################################################
####################### EDA #######################
################################################################


NPSS <- read.xlsx2("Media data and other information.xlsx",
                              sheetName="NPS")
View(NPSS)
head(NPSS)
MEDIA <- read.xlsx2("Media data and other information.xlsx",
                   sheetName="MEDIA")
View(MEDIA)

HOLIDAY <- read.csv("holiday_calendar.csv",stringsAsFactors =FALSE)

View(HOLIDAY)
HOLIDAY$DATE_HOLIDAY<-as.Date(HOLIDAY$DATE_HOLIDAY,format = "%Y-%m-%d")
str(HOLIDAY)

SALE_WEEK<- read.xlsx2("Media data and other information.xlsx",
                    sheetName="SALE_WEEK")
View(SALE_WEEK)
  
df_consumer_electronics_01<-merge(df_consumer_electronics,NPSS,by="week_number_new")
df_consumer_electronics_01<-merge(df_consumer_electronics_01,MEDIA,by="week_number_new")
df_consumer_electronics_01<-merge(df_consumer_electronics_01,HOLIDAY,by.x ="order_date",by.y="DATE_HOLIDAY",all.x = TRUE)
df_consumer_electronics_01<- df_consumer_electronics_01[ ,-which(names(df_consumer_electronics_01) %in% c("week_number_new.y"  ))]
colnames(df_consumer_electronics_01)[which(names(df_consumer_electronics_01) == "week_number_new.x")] <- "week_number_new"
df_consumer_electronics_01<-merge(df_consumer_electronics_01,SALE_WEEK,by="week_number_new",all.x = TRUE)

df_consumer_electronics_01$SALE_WEEK<-as.numeric(df_consumer_electronics_01$SALE_WEEK) 
df_consumer_electronics_01$SALE_WEEK[is.na(df_consumer_electronics_01$SALE_WEEK)] <- 0
df_consumer_electronics_01$HOLIDAY_SALE_NAME[is.na(df_consumer_electronics_01$HOLIDAY_SALE_NAME)] <- "NO SALE"

df_consumer_electronics_01$NPS<-as.numeric(df_consumer_electronics_01$NPS) 
df_consumer_electronics_01$Total.Investment<-as.numeric(df_consumer_electronics_01$Total.Investment) 
df_consumer_electronics_01$TV<-as.numeric(df_consumer_electronics_01$TV) 
df_consumer_electronics_01$Digital<-as.numeric(df_consumer_electronics_01$Digital) 
df_consumer_electronics_01$Sponsorship<-as.numeric(df_consumer_electronics_01$Sponsorship) 
df_consumer_electronics_01$Content.Marketing<-as.numeric(df_consumer_electronics_01$Content.Marketing) 
df_consumer_electronics_01$Online.marketing<-as.numeric(df_consumer_electronics_01$Online.marketing) 
df_consumer_electronics_01$X.Affiliates<-as.numeric(df_consumer_electronics_01$X.Affiliates) 
df_consumer_electronics_01$SEM<-as.numeric(df_consumer_electronics_01$SEM) 
df_consumer_electronics_01$Radio<-as.numeric(df_consumer_electronics_01$Radio) 
df_consumer_electronics_01$Other<-as.numeric(df_consumer_electronics_01$Other) 

head(df_consumer_electronics_01)
dim(df_consumer_electronics_01)
#View(df_consumer_electronics_01)
df_consumer_electronics_02<- df_consumer_electronics_01[ ,-which(names(df_consumer_electronics_01) %in% c("Year" , "Month" ,"cust_id" ,"pincode" ))]
head(df_consumer_electronics_02)
View(df_consumer_electronics_02)


## Dummy variable creation

##Variables with 2 levels

colnames(df_consumer_electronics_02)[which(names(df_consumer_electronics_02) == "s1_fact.order_payment_type")] <- "payment_type"


#1. payment_type
df_consumer_electronics_02$payment_type<-factor(df_consumer_electronics_02$payment_type)
levels(df_consumer_electronics_02$payment_type)<-c(0,1)
## assigning 0 to No and 1 to Yes
df_consumer_electronics_02$payment_type<- as.numeric(levels(df_consumer_electronics_02$payment_type))[df_consumer_electronics_02$payment_type]


#Plotting GMV per week highlighting sales week or not 
plot1<-ggplot(data=df_consumer_electronics_02, aes(x=week_number_new, y=gmv, fill=SALE_WEEK)) +
  geom_bar(stat="identity")
#Plotting gmv per week highlighting category
plot2<-ggplot(data=df_consumer_electronics_02, aes(x=week_number_new, y=gmv, fill=product_analytic_sub_category)) +
  geom_bar(stat="identity")
#plotting units per week highlighting sales week or not 
plot3<-ggplot(data=df_consumer_electronics_02, aes(x=week_number_new, y=units, fill=SALE_WEEK)) +
  geom_bar(stat="identity")
#plotting units per week highliting category
plot4<-ggplot(data=df_consumer_electronics_02, aes(x=week_number_new, y=units, fill=product_analytic_sub_category)) +
  geom_bar(stat="identity")

plot_grid(plot1, plot2, plot3, plot4,  align = "h")





#Correlation
#Extracting all numeric field data from  dataframe to find correlation
df_consumer_electronics_numeric <- Filter(is.numeric, df_consumer_electronics_02)
View(df_consumer_electronics_numeric)
df_consumer_electronics_cor<- cor(df_consumer_electronics_numeric)
corrplot(df_consumer_electronics_cor, method = "ellipse")

#ggpairs(df_consumer_electronics_02[,c("week_number_new","gmv","units","payment_type","sla","product_mrp","product_procurement_sla")])



