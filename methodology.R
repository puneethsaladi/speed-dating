### First job - predict if match depending on responses from both the partners
# We need to remove the decisions as they determine if there was a match

speed_dating_1 = speed_dating[,-c(64,65)]

## Split the data into set1 (training set), set2 (validation set) and set3 (test set)
train_indices = sample(1:8378,5378,replace=FALSE)
set1 = speed_dating_1[train_indices,]
validation_and_test_data = speed_dating_1[-train_indices,]
validation_indices = sample(1:3000,1500,replace=FALSE)
set2 = validation_and_test_data[validation_indices,]
set3 = validation_and_test_data[-validation_indices,]

## QDA

## Logistic Regression

## Naive Bayes

## KNN

## GAM

## Ramdom Forest

## SVM

## Neural Nets

### Second job - predict individual decisions [This should have a higher accuracy as each record has more data regarding the primary person]

### Third job - predict decisions only based on the 6 attributes collected [differences in decision criteria for men and women]
