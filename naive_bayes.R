finalproject<-read.csv("C:/Users/User/Desktop/AIUB/Semester 8/Introduction to data science/Final/TasteTrios-Sheet1.csv", header=TRUE, sep=",")
options(max.print = 5000)
finalproject
summary(finalproject)
colSums(is.na(finalproject))


tableIng1 <- table(finalproject$Ingredient1,finalproject$ClassificationOutput)


chi_1<-chisq.test(tableIng1)
chi_1
pVal_1<-chi$p.value
if(pVal_1<0.05){
  
  print("Ingredient 1 is Singnificat")
  
}else{
  
  
  print("Ingredient 1 is Not Singnificat ")
  
  
  
}


tableIng2 <- table(finalproject$Ingredient2,finalproject$ClassificationOutput)


chi_2<-chisq.test(tableIng1)
chi_2
pVal_2<-chi$p.value
if(pVal_2<0.05){
  
  print("Ingredient 2 is Singnificat")
  
}else{
  
  
  print("Ingredient 2 is Not Singnificat ")
  
  
  
}

tableIng3 <- table(finalproject$Ingredient3,finalproject$ClassificationOutput)


chi_3<-chisq.test(tableIng3)
chi_3
pVal_3<-chi$p.value
if(pVal_3<0.05){
  
  print("Ingredient 3 is Singnificat")
  
}else{
  
  
  print("Ingredient3 is Not Singnificat ")
  
}


install.packages("e1071")
library(e1071)
nb <- naiveBayes(ClassificationOutput ~ Ingredient1 + Ingredient2 + Ingredient3, data = finalproject)

multi_unknown_instance <- data.frame(
  Ingredient1 = c("Mushroom", "Pumpkin", "Mango"),
  Ingredient2 = c("Butter", "Apples", "Honey"),
  Ingredient3 = c("Sage", "Curry", "Lime")
)
predicted_class <- predict(nb, newdata = multi_unknown_instance)
print("Predicted Class:")
print(predicted_class)


nbs <- naiveBayes(ClassificationOutput ~ Ingredient1 + Ingredient2 + Ingredient3, data = finalproject)
single_unknown_instance <- data.frame(
  Ingredient1 = "Mushroom",
  Ingredient2 = "Butter",
  Ingredient3 = "Sage"
)
predicted_class <- predict(nbs, newdata = single_unknown_instance)
print("Predicted Class:")
print(predicted_class)
finalproject

install.packages("caret") 
library(caret)
train_index <- sample(1:nrow(finalproject), size = 0.8 * nrow(finalproject)) 
train_data <- finalproject[train_index, ] 
test_data <- finalproject[-train_index, ]
train_data
test_data

num_of_test_data <- nrow(test_data)
print("No of Test Data set: ")
num_of_test_data


num_of_train_data <- nrow(train_data)
print("No of Tranning Data set: ")
num_of_train_data

Test_dataset <- read.csv("C:/Users/User/Desktop/AIUB/Semester 8/Introduction to data science/Final/test_data.csv", header=TRUE, sep=",")
Test_dataset

Train_csv_data <- read.csv("C:/Users/User/Desktop/AIUB/Semester 8/Introduction to data science/Final/train_data.csv", header=TRUE, sep=",")
Train_csv_data

install.packages("naivebayes")
library(naivebayes)

ML_nb_model <- naive_bayes(ClassificationOutput ~ ., data = train_data) 
ML_nb_model

predicted_class <- predict(ML_nb_model, newdata = Test_dataset)
correctly_classified <- sum(predicted_class == Test_dataset$ClassificationOutput)
cat("Number of correctly classified instances :", correctly_classified, "\n")

accuracy <- correctly_classified / nrow(Test_dataset)
cat("accuracy is:")
accuracy


CrossValidation <- trainControl(method = "cv", number = 10) 
CrossValidation
cv_model <- train(ClassificationOutput ~ ., data = finalproject, method = "naive_bayes", trControl = CrossValidation)
cv_model

all_fold <- cv_model$resample
all_fold

accuracy <- cv_model$results$Accuracy
accuracy


cm <- confusionMatrix(cv_model)
cm


cat("For  Compatible  Class (TP,FN,FP,TN): ")
TP_Compatible <- cm$table[1, 1]
cat("True Positives (TP):", TP_Compatible, "\n")


FN_Compatible <- cm$table[1, 2]+ cm$table[1, 3]
cat("False Negatives (FN):", FN_Compatible, "\n")


FP_Compatible <- cm$table[2, 1]+cm$table[3, 1]
cat("False Positives (FP):", FP_Compatible, "\n")


TN_Compatible <- cm$table[2, 2]+cm$table[2,3]+cm$table[3,2]+cm$table[3, 3]
cat("True Negatives (TN):", TN, "\n")


cat("Precision For Compatible  Class :")
Precision_Compatible = TP_Compatible / (TP_Compatible + FP_Compatible)
Precision_Compatible

cat("Recall For Compatible  Class :")
recall_Compatible=TP_Compatible/(TP_Compatible+FN_Compatible)
recall_Compatible

cat("F-Measure Compatible  Class :")
F_Compatible = 2 * (Precision_Compatible * recall_Compatible) / (Precision_Compatible + recall_Compatible)
F_Compatible



cat("For  Highly Compatible  Class (TP,FN,FP,TN): ")
TP_HighlyCompatible <- cm$table[2, 2]
cat("True Positives (TP):", TP, "\n")


FN_HighlyCompatible <- cm$table[2, 1]+ cm$table[2, 3]
cat("False Negatives (FN):", FN, "\n")


FP_HighlyCompatible <- cm$table[1, 2]+cm$table[3, 2]
cat("False Positives (FP):", FP, "\n")


TN_HighlyCompatible <- cm$table[1, 1]+cm$table[1,3]+cm$table[3,1]+cm$table[3, 3]
cat("True Negatives (TN):", TN, "\n")


cat("Precision For Highly Compatible  Class :")
Precision_HighlyCompatible = TP_HighlyCompatible / (TP_HighlyCompatible + FP_HighlyCompatible)
Precision_HighlyCompatible

cat("Recall For Highly Compatible  Class :")
recall_HighlyCompatible=TP_HighlyCompatible/(TP_HighlyCompatible+FN_HighlyCompatible)
recall_HighlyCompatible

cat("F-Measure Highly Compatible  Class :")
F_HighlyCompatible = 2 * (Precision_HighlyCompatible * recall_HighlyCompatible) / (Precision_HighlyCompatible + recall_HighlyCompatible)
F_HighlyCompatible





cat("For  Moderately  Compatible  Class (TP,FN,FP,TN): ")
TP_ModeratelyCompatible <- cm$table[3, 3]
cat("True Positives (TP):", TP,"\n")


FN_ModeratelyCompatible <- cm$table[3, 1]+ cm$table[3, 2]
cat("False Negatives (FN):", FN, "\n")


FP_ModeratelyCompatible <- cm$table[1, 3]+cm$table[2, 3]
cat("False Positives (FP):", FP, "\n")


TN_ModeratelyCompatible <- cm$table[1, 1]+cm$table[1,2]+cm$table[2,1]+cm$table[2, 2]
cat("True Negatives (TN):", TN, "\n")


cat("Precision ForModerately  Compatible  Class :")
Precision_ModeratelyCompatible = TP_ModeratelyCompatible / (TP_ModeratelyCompatible + FP_ModeratelyCompatible)
Precision_ModeratelyCompatible

cat("Recall For Moderately Compatible  Class :")
recall_ModeratelyCompatible=TP_ModeratelyCompatible/(TP_ModeratelyCompatible+FN_ModeratelyCompatible)
recall_ModeratelyCompatible

cat("F-Measure Moderately Compatible  Class :")
F1_ModeratelyCompatible = 2 * (Precision_ModeratelyCompatible * recall_ModeratelyCompatible) / (Precision_ModeratelyCompatible + recall_ModeratelyCompatible)
F1_ModeratelyCompatible









