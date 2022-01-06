SVM<-Telcomchurn
set.seed(123)
SVM$Churn <- dplyr::recode(SVM$Churn,"Yes"= "1", "No"= "0")
n3 = nrow (SVM)
indexes = sample (n3 , n3 * (80/100))
Trainset3 = SVM[indexes,]
Testset3 = SVM[-indexes,]

# Creating Random Forest model with default parameters
SVM = svm(as.factor(Churn) ~ Contract +TenureYear + Total.Revenue +Avg.Monthly.Long.Distance.Charges +MonthlyCharges,Trainset3,type = 'C-classification',kernel = 'linear' )
pred1 <- predict(SVM, Testset3)
sv<-table(pred1,Testset3$Churn)
sv
CrossTable(pred1,Testset3$Churn)
confusionMatrix(sv,positive = "1")

SVM<-Telcomchurn
set.seed(123)
SVM$Churn <- dplyr::recode(SVM$Churn,"Yes"= "1", "No"= "0")
n3 = nrow (SVM)
indexes = sample (n3 , n3 * (80/100))
Trainset3 = SVM[indexes,]
Testset3 = SVM[-indexes,]

# Creating Random Forest model with default parameters
SVM = svm(as.factor(Churn) ~ as.factor(Group)+Contract +TenureYear + Total.Revenue +Avg.Monthly.Long.Distance.Charges +MonthlyCharges,Trainset3,type = 'C-classification',kernel = 'linear' )
pred1 <- predict(SVM, Testset3)
sv<-table(pred1,Testset3$Churn)
sv
CrossTable(pred1,Testset3$Churn)
confusionMatrix(sv,positive = "1")
      plot(SVM, SVM)
SVM<-svm(as.factor(Churn) ~Total.Revenue + Contract +Avg.Monthly.Long.Distance.Charges, Trainset3,type = 'C-classification',kernel = 'linear' )
      pred1 <- predict(SVM, Testset3)
      sv<-table(pred1,Testset3$Churn)
      sv
      CrossTable(pred1,Testset3$Churn)
      confusionMatrix(sv,positive = "1")
      plot(SVM, SVM)
      