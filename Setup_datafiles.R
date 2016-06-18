####Set Up for data clean up####
setwd("~/Documents/UCD/BA Prac/CrowdAnalytics-R-Comp/Raw Files")
train<-read.csv("CAX_Startup_Train.csv",header = T, sep= ",")
test<- read.csv("CAX_Startup_Test.csv",header =T, sep = ",")
datadic<- read.csv("CAX_Startup_DataDictionary.csv",header =T, sep = ",")

#Bind train and test
train$Category<- c("train")
test$Category<- c("test")
d<-rbind(train,test)

#set default contrast treatment for ordinal factors
options(contrasts = rep ("contr.treatment", 2))

# recoding ordinal variables in proper order
str(d$Founders_previous_company_employee_count) #Large = 1 to be changed to 3
library(plyr)
d$employee_count_code<- as.numeric(revalue(d$Founders_previous_company_employee_count,
                                           c("Small"=1, "Medium"=2, "Large"=3)))
d$Founders_previous_company_employee_count = NULL

#Check the mix of classes
table(d$Dependent) #118 Os and 116 1s

#Partition into train and validation set
require(caret)
train<-d[d$Category=="train",]
test<-d[d$Category=="test",] #Final test set
set.seed(666)
tr<-createDataPartition(train$Dependent,p=.90,list= FALSE)
trnew<-train[tr,]
tstnew<-train[-tr,]

#Check the mix of classes
prop.table(table(trnew$Dependent))
prop.table(table(tstnew$Dependent))

setwd("~/Documents/UCD/BA Prac/CrowdAnalytics-R-Comp")
