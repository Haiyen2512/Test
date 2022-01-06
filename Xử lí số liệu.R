library(MASS)
library(party)
library(e1071)
library(caret)
library(caTools)
library(dplyr)
library(rpart)
library(arules)
library(randomForest)
library(pROC)
library(ggplot2)
library(rpart)
library(rpart.plot)
library(gmodels)
library(gridExtra)
library(rugarch)
library(aTSA)
library(astsa)
library(forecast)
library(stats)
library(fGarch)
library(tseries) 
library(simEd)
library(urca)
library(ggfortify)
library(TSSS)
library(readxl)
library(writexl)


NewData<-DATA
sum(is.na(NewData))
  # There are a total of 11 missing values in the dataset
    # Checking the attribute(s) with the missing values
    sapply(NewData, function(x) sum(is.na(x)))
    # The attribute TotalCharges has 11 missing values
    # create new dataset without missing data
    Dataset <- na.omit(NewData)
    # Checking if the missing values attribute was removed successfully
    sum(is.na(Dataset))
    
# Removing Customer ID attribute since it is of no use for the analysis

    Dataset <- mutate(Dataset, TenureYear = tenure)
    Dataset$TenureYear[Dataset$TenureYear >=0 &
                         Dataset$TenureYear <= 12] <- '0-1 Year'
    Dataset$TenureYear[Dataset$TenureYear > 12 &
                         Dataset$TenureYear <= 24] <- '1-2 Years'
    Dataset$TenureYear[Dataset$TenureYear > 24 &
                         Dataset$TenureYear <= 36] <- '2-3 Years'
    Dataset$TenureYear[Dataset$TenureYear > 36 &
                         Dataset$TenureYear <= 48] <- '3-4 Years'
    Dataset$TenureYear[Dataset$TenureYear > 48 &
                         Dataset$TenureYear <= 60] <- '4-5 Years'
    Dataset$TenureYear[Dataset$TenureYear > 60 &
                         Dataset$TenureYear <= 72] <- '5-6 Years'   
Dataset[Dataset=="No phone service"]<- "No"
Dataset[Dataset=="No internet service"]<- "No"
  
Dataset$Churn<-as.factor(Dataset$Churn)
Dataset<-mutate_if(Dataset,is.character,as.factor)
Telcomchurn<-Dataset[,c(2,3,13,16:23,27:30,32)]
write_xlsx(Telcomchurn,"Telcomchurn.xlsx")

write_xlsx(Dataset,"Dataset.xlsx")
corr1<-Dataset[,c(16,17)]  
MT1<-cor(corr1)
ggcorrplot(MT1,hc.order = TRUE, lab=TRUE)
corr2<-Dataset[,c(22,28)]  
MT2<-cor(corr2)
ggcorrplot(MT2,hc.order = TRUE, lab=TRUE)
corr3<-Dataset[,c(23,27)]  
MT3<-cor(corr3)
ggcorrplot(MT3,hc.order = TRUE, lab=TRUE)