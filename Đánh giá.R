LOGIT1<-Dataset[,c(4:12,14,15,18,24:26,33)]
LOGIT<-LOGIT1[LOGIT1$Group=="3",]
set.seed(123)
n1 = nrow (LOGIT)
indexes1 = sample (n1 , n1 * (80/100))
Trainset1 = LOGIT[indexes1,]
Testset1 = LOGIT[-indexes1,]
Logit <- glm(Churn ~ ., LOGIT, family = "binomial")
summary(Logit)
anova(Logit, test="Chisq")
Logit$coefficients
100*(1-exp(Logit$coefficients))
pred1 <- predict(Logit, Testset1)
pred1 <- ifelse(pred1 > 0.5,"Yes","No")
confusionMatrix(table(pred1,Testset1$Churn),positive = "Yes")
confint(Logit)
Logit1 <- glm(Churn ~ Contract+ as.factor(Group) +TenureYear+ Total.Revenue +Avg.Monthly.Long.Distance.Charges +MonthlyCharges , Trainset1, family = "binomial")
summary(Logit1)
anova(Logit,Logit1, test="Chisq")
Logit1$coefficients
library(pscl)
pR2(Logit1)
100*(1-exp(Logit1$coefficients))
pred <- predict(Logit1, Testset1)
pred <- ifelse(pred > 0.5,"Yes","No")
confusionMatrix(table(pred,Testset1$Churn),positive = "Yes")
