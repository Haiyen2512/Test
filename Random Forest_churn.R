Random<-Telcomchurn
Ran1<-Random[,c(1:5,7:12,14)]
Ran<-Random[,c(1:5,7:12,14,15)]

set.seed(123)
n12 = nrow (Ran1)
indexes12 = sample (n12 , n12 * (80/100))
Trainset12 = Ran1[indexes12,]
Testset12 = Ran1[-indexes12,]

# Fine tuning parameters of Random Forest model
RandomForest1 <- randomForest(as.factor(Churn)~., data = Trainset12, ntree = 500, mtry = 3,
                               importance = TRUE)
RandomForest1
# Predicting on Training Set
pred12 <- predict(RandomForest1, Testset12, type = "class")
# Checking classification accuracy
table(pred12, Testset12$Churn)
confusionMatrix(table(pred12, Testset12$Churn), positive = "Yes")
dim(pred12)
# Checking classification accuracy
mean(pred12 == Testset1$Churn)
# Checking Important Variables
importance(RandomForest1)
# Plotting Important Variables
varImpPlot(RandomForest1)

RandomForest13 <- randomForest(as.factor(Churn)~ Contract + TenureYear + Total.Revenue +Avg.Monthly.Long.Distance.Charges +MonthlyCharges , data = Trainset12, ntree = 500, mtry = 3,
                               importance = TRUE)
RandomForest13
# Predicting on Training Set
pred13 <- predict(RandomForest13, Testset12, type = "class")
# Checking classification accuracy
table(pred13, Testset12$Churn)
confusionMatrix(table(pred13, Testset12$Churn),positive = "Yes")

set.seed(123)
n1 = nrow (Ran)
indexes1 = sample (n1 , n1 * (80/100))
Trainset1 = Ran[indexes1,]
Testset1 = Ran[-indexes1,]

# Fine tuning parameters of Random Forest model
RandomForest2 <- randomForest(as.factor(Churn)~., data = Trainset1, ntree = 500, mtry = 3,
                                   importance = TRUE)
RandomForest2
# Predicting on Training Set
pred <- predict(RandomForest2, Testset1, type = "class")
# Checking classification accuracy
table(pred, Testset1$Churn)
confusionMatrix(table(pred, Testset1$Churn),positive = "Yes")
dim(pred)
# Checking classification accuracy
mean(pred == Testset1$Churn)
# Checking Important Variables
importance(RandomForest2)
# Plotting Important Variables
varImpPlot(RandomForest2)
# Predicting on Training Set
pred <- predict(RandomForest2, Testset1, type = "class")
# Checking classification accuracy
table(pred, Testset1$Churn)
confusionMatrix(table(pred, Testset1$Churn),positive = "Yes")

RandomForest21 <- randomForest(as.factor(Churn)~ Group +Contract + TenureYear + Total.Revenue +Avg.Monthly.Long.Distance.Charges +MonthlyCharges , data = Trainset1, ntree = 500, mtry = 3,
                              importance = TRUE)
RandomForest21
# Predicting on Training Set
pred21 <- predict(RandomForest21, Testset1, type = "class")
# Checking classification accuracy
table(pred21, Testset1$Churn)
confusionMatrix(table(pred21, Testset1$Churn),positive = "Yes")



