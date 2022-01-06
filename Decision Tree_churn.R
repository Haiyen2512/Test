
# Load libraries
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
library(tree)
#Decision Tree
set.seed(123)
Dtree<-Telcomchurn
     Dtree$churn_rate <- NULL
      str(Dtree)
    # Decision Tree
    # For easier interpretation, we can convert the tenure attribute from months to year for the decision tree.
    n2 = nrow (Dtree)
indexes2 = sample (n2 , n2 * 80/100)
Trainset2 = Dtree[indexes2,]
Testset2 = Dtree[-indexes2,]

sapply(Dtree,class)

CustomerChurn_ctree1 <- ctree(Churn ~ Contract + TenureYear + Total.Revenue +Avg.Monthly.Long.Distance.Charges +MonthlyCharges  ,Trainset2,weights = NULL, 
                              controls = ctree_control(),  scores = NULL)

plot(CustomerChurn_ctree1, type="simple")


    # Testset Predictions
Pred2<-predict(CustomerChurn_ctree1,Testset2)
    confusionMatrix_Dtree = table(pred = Pred2 , true = Testset2$Churn)
    pred_dtree = mean (Pred2 == Testset2$Churn)
confusionMatrix(confusionMatrix_Dtree, positive = "Yes")
pred_dtree
CustomerChurn_ctree1
Dtree<-Telcomchurn
Dtree$churn_rate <- NULL
str(Dtree)
# Decision Tree
# For easier interpretation, we can convert the tenure attribute from months to year for the decision tree.
set.seed(123)

n2 = nrow (Dtree)
indexes2 = sample (n2 , n2 * 80/100)
Trainset2 = Dtree[indexes2,]
Testset2 = Dtree[-indexes2,]

sapply(Dtree,class)

CustomerChurn_ctree2 <- ctree(Churn ~ as.factor(Group)+Contract + TenureYear + Total.Revenue  +MonthlyCharges  ,Trainset2,weights = NULL, 
                              controls = ctree_control(), xtrafo = ptrafo, ytrafo = ptrafo, 
                              scores = NULL)

plot(CustomerChurn_ctree2, type = "simple")
# Testset Predictions
Pred2<-predict(CustomerChurn_ctree2,Testset2)
confusionMatrix_Dtree = table(pred = Pred2 , true = Testset2$Churn)
pred_dtree = mean (Pred2 == Testset2$Churn)

confusionMatrix(confusionMatrix_Dtree, positive = "Yes")
set.seed(123)
Nhom1<- Telcomchurn[Telcomchurn$Group =="3",]
write_xlsx(Nhom1,"Nhom1.xlsx")
n2 = nrow (Nhom1)
indexes2 = sample (n2 , n2 * 80/100)
Trainset2 = Nhom1[indexes2,]
Testset2 = Nhom1[-indexes2,]

n2
CustomerChurn_ctree1 <- ctree(as.factor(Churn) ~ Contract + TenureYear + Total.Revenue +Avg.Monthly.Long.Distance.Charges +MonthlyCharges  ,Trainset2)

plot(CustomerChurn_ctree1, type="simple")
dataset<-Dataset[Dataset$Churn.Category=="0"]

# Testset Predictions
Pred2<-predict(CustomerChurn_ctree1,Testset2)
confusionMatrix_Dtree = table(pred = Pred2 , true = Testset2$Churn)
pred_dtree = mean (Pred2 == Testset2$Churn)
confusionMatrix(confusionMatrix_Dtree, positive = "Yes")
summary(Nhom1$MonthlyCharges)
