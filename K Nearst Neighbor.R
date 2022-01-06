KNN<-Ran
library(class)
library(hexView)
KNN$gender <- dplyr::recode(KNN$gender,"Male"= "1", "Female"= "0")
KNN$Dependents <- dplyr::recode(KNN$Dependents,"Yes"= "1", "No"= "0")
KNN$Married <- dplyr::recode(KNN$Married,"Yes"= "1", "No"= "0")
KNN$Churn <- dplyr::recode(KNN$Churn,"Yes"= "1", "No"= "0")
KNN$Contract <- dplyr::recode(KNN$Contract,"Month-to-month"= "0", "One year"= "1","Two year"="2")
KNN$TenureYear <- dplyr::recode(KNN$TenureYear,"0-1 Year"= "0", "1-2 Years"= "1","2-3 Years"= "2","3-4 Years"= "3","4-5 Years"= "4","5-6 Years"= "5")
KNN<-mutate_if(KNN,is.factor,as.character)
####
KNN1<-KNN[,c(1:14)]
set.seed(123)
str(KNN1)
n12 = nrow (KNN1)
indexes = sample (n12 , n12 * (80/100))
Trainset12 = KNN1[indexes,]
Testset12 = KNN1[-indexes,]
cl=Trainset12$Churn

i=1
k.optm=1
for (i in 1:76){
  knn.mod <- knn(train=Trainset12, test=Testset12, cl, k=i)
  k.optm[i] <- 100 * sum(Testset12$Churn == knn.mod)/NROW(Testset12)
  k=i
  cat(k,'=',k.optm[i],'')}
plot(k.optm, type="b", xlab="K- Value",ylab="Accuracy level-Original data")
knn.34 <- knn(train = Trainset12, test = Testset12,cl, k=35)
confusionMatrix(table(knn.34,Testset12$Churn),positive = "1")


######
set.seed(123)
KNN2<-KNN[,c(4:6,9,12,13)]
n13 = nrow (KNN2)
indexes = sample (n13 , n13 * (80/100))
Trainset13 = KNN2[indexes,]
Testset13 = KNN2[-indexes,]
cl1=Trainset13$Churn


accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(knn.761)
i=1
k.optm=1
for (i in 1:76){
  knn.mod <- knn(train=Trainset13, test=Testset13, cl1, k=i)
  k.optm[i] <- 100 * sum(Testset13$Churn == knn.mod)/NROW(Testset13)
  k=i
  cat(k,'=',k.optm[i],'')}
plot(k.optm, type="b", xlab="K- Value",ylab="Accuracy level- 5 variable data")
knn.761 <- knn(train = Trainset13, test = Testset13,cl1, k=9)
confusionMatrix(table(knn.761,Testset13$Churn),positive = "1")
###
KNN3<-KNN
set.seed(123)
str(KNN3)
n14 = nrow (KNN3)
indexes = sample (n14 , n14 * (80/100))
Trainset14 = KNN3[indexes,]
Testset14 = KNN3[-indexes,]
cl2=Trainset14$Churn

knn.762 <- knn(train = Trainset14, test = Testset14,cl2, k=76)
confusionMatrix(table(knn.762,Testset14$Churn),positive = "1")

accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(Knn.76)
i=1
k.optm=1
for (i in 1:76){
  knn.mod <- knn(train=Trainset14, test=Testset14, cl2, k=i)
  k.optm[i] <- 100 * sum(Testset14$Churn == knn.mod)/NROW(Testset14)
  k=i
  cat(k,'=',k.optm[i],'')}
plot(k.optm, type="b", xlab="K- Value",ylab="Accuracy level-clustered data")
knn.762 <- knn(train = Trainset14, test = Testset14,cl2, k=35)
confusionMatrix(table(knn.762,Testset14$Churn),positive = "1")
######
set.seed(123)
KNN4<-KNN[,c(4:6,9,12,13,15)]
n15 = nrow (KNN2)
indexes = sample (n15 , n15 * (80/100))
Trainset15 = KNN4[indexes,]
Testset15 = KNN4[-indexes,]
cl3=Trainset15$Churn

knn.763 <- knn(train = Trainset15, test = Testset15,cl3, k=76)
confusionMatrix(table(knn.763,Testset15$Churn),positive = "1")

accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(knn.763)

i=1
k.optm=1
for (i in 1:76){
  knn.mod <- knn(train=Trainset15, test=Testset15, cl3, k=i)
  k.optm[i] <- 100 * sum(Testset15$Churn == knn.mod)/NROW(Testset15)
  k=i
  cat(k,'=',k.optm[i],'')}
plot(k.optm, type="b", xlab="K- Value",ylab="Accuracy level- 6 variable data")
knn.763 <- knn(train = Trainset15, test = Testset15,cl3, k=9)
confusionMatrix(table(knn.763,Testset15$Churn),positive = "1")
