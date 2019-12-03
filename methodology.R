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

set1.numeric = set1
set1.numeric[,-64] = as.numeric(unlist(set1[,-64]))
set2.numeric = set2
set2.numeric[,-64] = as.numeric(unlist(set2[,-64]))
set3.numeric = set3
set3.numeric[,-64] = as.numeric(unlist(set3[,-64]))

##### QDA ######


library(MASS)
qda.fit <- qda(x=set1.numeric[,-64], grouping=set1.numeric$match)

qda.pred.train <- predict(qda.fit, newdata=set1.numeric[,-64])$class
qda.pred.valid <- predict(qda.fit, newdata=set2.numeric[,-64])$class
qda.pred.test <- predict(qda.fit, newdata=set3.numeric[,-64])$class
(qmisclass.train <- mean(ifelse(qda.pred.train == set1.numeric$match, yes=0, no=1)))
(qmisclass.valid <- mean(ifelse(qda.pred.valid == set2.numeric$match, yes=0, no=1)))
(qmisclass.test <- mean(ifelse(qda.pred.test == set3.numeric$match, yes=0, no=1)))

# Test set confusion matrix
table(set3.numeric$match, qda.pred.test, dnn=c("Obs","Pred"))

####### Logistic Regression ##############

rescale <- function(x1,x2){
  for(col in 1:ncol(x1)){
    a <- min(x2[,col])
    b <- max(x2[,col])
    x1[,col] <- (x1[,col]-a)/(b-a)
  }
  x1
}

set1.rescale <- data.frame(cbind(rescale(set1.numeric[,-64], set1.numeric[,-64]), class=set1.numeric$match))
set2.rescale <- data.frame(cbind(rescale(set2.numeric[,-64], set1.numeric[,-64]), class=set2.numeric$match))
set3.rescale <- data.frame(cbind(rescale(set3.numeric[,-64], set1.numeric[,-64]), class=set3.numeric$match))

library(glmnet)

logit.cv <- cv.glmnet(x=as.matrix(set1.rescale[,1:63]), y=set1[,64], family="multinomial")
logit.cv
x11()
plot(logit.cv)

lascv.pred.train <- predict(object=logit.cv, newx=as.matrix(set1.rescale[,1:63]), s=logit.cv$lambda.min, type="class")
lascv.pred.valid <- predict(logit.cv, newx=as.matrix(set2.rescale[,1:63]), s=logit.cv$lambda.min, type="class")
lascv.pred.test <- predict(logit.cv, newx=as.matrix(set3.rescale[,1:63]), s=logit.cv$lambda.min, type="class")
(lascvmisclass.train <- mean(ifelse(lascv.pred.train == set1$match, yes=0, no=1)))
(lascvmisclass.valid <- mean(ifelse(lascv.pred.valid == set2$match, yes=0, no=1)))
(lascvmisclass.test <- mean(ifelse(lascv.pred.test == set3$match, yes=0, no=1)))

################# Naive Bayes ########################

library(klaR)

pc <-  prcomp(x=set1.numeric[,-64], scale.=TRUE)

xi.1 <- data.frame(pc$x,class = as.factor(set1$match))
xi.2 <- data.frame(predict(pc, newdata=set2.numeric), class = as.factor(set2.numeric$match))
xi.3 <- data.frame(predict(pc, newdata=set3.numeric), class = as.factor(set3.numeric$match))

NB.pc <- NaiveBayes(x=xi.1[,-64], grouping=xi.1[,64], usekernel=FALSE)

NBpc.pred.train <- predict(NB.pc, newdata=xi.1[,-64], type="class")
NBpc.pred.valid <- predict(NB.pc, newdata=xi.2[,-64], type="class")
NBpc.pred.test <- predict(NB.pc, newdata=xi.3[,-64], type="class")

# Error rates
(NBmisclass.train <- mean(ifelse(NBpc.pred.train$class == xi.1$class, yes=0, no=1)))
(NBmisclass.valid <- mean(ifelse(NBpc.pred.valid$class == xi.2$class, yes=0, no=1)))
(NBmisclass.test <- mean(ifelse(NBpc.pred.test$class == xi.3$class, yes=0, no=1)))

################ Random Forest  #####################

library(randomForest)

rf.1 <- randomForest(data=set1.numeric, match~., importance=TRUE, ntree=2500, keep.forest=TRUE)

win.graph(h=7,w=6,pointsize=12)
plot(rf.1)

round(importance(rf.1),3) # Print out importance measures
win.graph(h=7,w=12)
varImpPlot(rf.1) # Plot of importance measures; more interesting with more variables

# Predict results of classification. 
pred.rf.1.train <- predict(rf.1, newdata=set1.numeric, type="response")
pred.rf.1.val <- predict(rf.1, newdata=set2.numeric, type="response")
pred.rf.1.test <- predict(rf.1, newdata=set3.numeric, type="response")

(misclass.train.4 <- mean(ifelse(pred.rf.1.train == set1.numeric$match, yes=0, no=1)))
(misclass.val.4 <- mean(ifelse(pred.rf.1.val == set2.numeric$match, yes=0, no=1)))
(misclass.test.4 <- mean(ifelse(pred.rf.1.test == set3.numeric$match, yes=0, no=1)))

################ SVM  #####################

library(e1071)

svm.tune <-  tune.svm(data=set1, match ~ ., kernel="radial", 
                          tunecontrol=tune.control(sampling="fix"), 
                          validation.x=set2[,1:63], validation.y=set2[,64], 
                          gamma = 10^(-2:2), cost = 10^(2:6))  ## gamma = 0.01 cost = 100 performance = 0.16
summary(svm.tune) 
aav <- summary(svm.tune)$performances
aav[order(aav[,3]),]
x11(h=7, w=6, pointsize=12)
plot(svm.tune, type="contour", transform.x=log10, transform.y=log10)
x11(h=7, w=6, pointsize=12)
plot(svm.tune, type="perspective", transform.x=log10, transform.y=log10, theta=150)

svm.01.100 <- svm(data=set1, match ~ ., kernel="radial", gamma=.01, cost=100)
summary(svm.01.100)

svm.pred.train <- predict(svm.01.100, newdata=set1, type="vector")
(svm.misclass.train <- mean(ifelse(svm.pred.train == set1$match, yes=0, no=1)))

svm.pred.val <- predict(svm.01.100, newdata=set2, type="vector")
(svm.misclass.val <- mean(ifelse(svm.pred.val == set2$match, yes=0, no=1)))

svm.pred.test <- predict(svm.01.100, newdata=set3, type="vector")
(svm.misclass.test <- mean(ifelse(svm.pred.test == set3$match, yes=0, no=1)))

######################## Neural Nets #######################

rescale.set1 <- function(x1,x2){
  minx <- apply(X=x1, MARGIN=2, FUN=min)
  maxx <- apply(X=x1, MARGIN=2, FUN=max)
  x3 <- matrix (nrow=nrow(x2), ncol=ncol(x2))
  for(i in c(1:ncol(x2))){
    x3[,i] <- (x2[,i] - minx[i])/(maxx[i] - minx[i])
  }
  x3
}


x.1.unscaled <- as.matrix(set1.numeric[,-64])
x.1 <- rescale.set1(x.1.unscaled, x.1.unscaled)
x.2.unscaled <- as.matrix(set2.numeric[,-64])
x.2 <- rescale.set1(x.1.unscaled, x.2.unscaled)
x.3.unscaled <- as.matrix(set3.numeric[,-64])
x.3 <- rescale.set1(x.1.unscaled, x.3.unscaled)

library(nnet)

y.1 <- class.ind(set1[,64])
y.2 <- class.ind(set2[,64])
y.3 <- class.ind(set3[,64])

nn.1.0 <- nnet(x=x.1, y=y.1, size=1, maxit=1000, softmax=TRUE)

p1.nn.1.0 <-predict(nn.1.0, newdata=x.1, type="class")
table(p1.nn.1.0, as.factor(set1.numeric$match),  dnn=c("Predicted","Observed"))
(misclass1.1.0 <- mean(ifelse(p1.nn.1.0 == as.factor(set1.numeric$match), yes=0, no=1)))
# Val set error
p2.nn.1.0 <-predict(nn.1.0, newdata=x.2, type="class")
table(p2.nn.1.0, as.factor(set2.numeric$match),  dnn=c("Predicted","Observed"))
(misclass2.1.0 <- mean(ifelse(p2.nn.1.0 == set2.numeric$match, yes=0, no=1)))
# Test set error
p3.nn.1.0 <-predict(nn.1.0, newdata=x.3, type="class")
table(p3.nn.1.0, as.factor(set3.numeric$match),  dnn=c("Predicted","Observed"))
(misclass3.1.0 <- mean(ifelse(p3.nn.1.0 == as.factor(set3.numeric$match), yes=0, no=1)))


################## xgboost ################

library(xgboost)

xg <- xgboost(data=as.matrix(set1.numeric[,-64]), label=as.numeric(set1.numeric[,64])-1, 
                  max_depth=5, eta=.001, subsample=.8,
                  nrounds=1000, num_class=2, objective="multi:softprob")

pred.xg.train <- predict(xg, newdata=as.matrix(set1.numeric[,-64]), reshape=TRUE)
pred.xg.val <- predict(xg, newdata=as.matrix(set2.numeric[,-64]), reshape=TRUE)
pred.xg.test <- predict(xg, newdata=as.matrix(set3.numeric[,-64]), reshape=TRUE)

class.xg.train <- apply(pred.xg.train[,], 1, which.max)
class.xg.val <- apply(pred.xg.val[,], 1, which.max)
class.xg.test <- apply(pred.xg.test[,], 1, which.max)
head(cbind(pred.xg.test[,],class.xg.test))
head(class.xg.test)
(as.numeric(set1.numeric$match))

(misclass.boost.xg.train <- mean(ifelse(class.xg.train == as.numeric(set1.numeric$match), yes=0, no=1)))
(misclass.boost.xg.val <- mean(ifelse(class.xg.val == as.numeric(set2.numeric$match), yes=0, no=1)))
(misclass.boost.xg.test <- mean(ifelse(class.xg.test == as.numeric(set3.numeric$match), yes=0, no=1)))

xgb.importance(model=xg)
xgb.plot.importance(xgb.importance(model=xg)[1:15,])

#################  Kernel Density ###############################

## Lets estimate kernel density for the 6 most important variables according to xgboost

library(sm)
x11(h=12, w=18)
par(mfrow=c(2,3))
sm.density.compare(x=set1.numeric$like, group=set1$match, lwd=2)
sm.density.compare(x=set1.numeric$attractive_o, group=set1$match, lwd=2)
sm.density.compare(x=set1.numeric$shared_interests_o, group=set1$match, lwd=2)
sm.density.compare(x=set1.numeric$funny_o, group=set1$match, lwd=2)
sm.density.compare(x=set1.numeric$attractive_partner, group=set1$match, lwd=2)
sm.density.compare(x=set1.numeric$funny_partner, group=set1$match, lwd=2)

####################  Nearest Neighbour #########################

library(FNN)

scale.set1 <- function(x1,x2){
  mean <- apply(X=x1, MARGIN=2, FUN=mean)
  stdev <- apply(X=x1, MARGIN=2, FUN=sd)
  scale(x=x2, center=mean, scale=stdev)
}


x.1.unscaled <- as.matrix(set1.numeric[,-64])
x.1 <- scale.set1(x.1.unscaled,x.1.unscaled)
x.2.unscaled <- as.matrix(set2.numeric[,-64])
x.2 <- scale.set1(x.1.unscaled,x.2.unscaled)
x.3.unscaled <- as.matrix(set3.numeric[,-64])
x.3 <- scale.set1(x.1.unscaled,x.3.unscaled)

kmax <- 25
k <- matrix(c(1:kmax), nrow=kmax)
runknn <- function(x){
  knnfit <- knn(train=x.1, test=x.2, cl=set1[,64], k=x)
  mean(ifelse(knnfit == set2[,64], yes=0, no=1))
}

mis <- apply(X=k, MARGIN=1, FUN=runknn)
mis.se <- sqrt(mis*(1-mis)/nrow(set2))

plot(x=k, y=mis, type="b", ylim=c(.10,.22), ylab="Missclassification error") 
for(ii in c(1:kmax)){
  lines(x=c(k[ii],k[ii]), y=c(mis[ii]-mis.se[ii], mis[ii]+mis.se[ii]), col=colors()[220])
}
abline(h=min(mis + mis.se), lty="dotted")

#Trying the value of k with the lowest validation error on test data set.
knnfit12.3 <- knn(train=x.1, test=x.3, cl=set1[,64], k=12)

table(knnfit12.3, set3[,64],  dnn=c("Predicted","Observed"))
(misclass.3.knn12 <- mean(ifelse(knnfit12.3 == set3[,64], yes=0, no=1)))

#Trying the 1-se value of k with the (largest k with validation error within 1 se).
knnfit25.3 <- knn(train=x.1, test=x.3, cl=set1[,64], k=25)

table(knnfit25.3, set3[,64],  dnn=c("Predicted","Observed"))
(misclass.3.knn25 <- mean(ifelse(knnfit25.3 == set3[,64], yes=0, no=1)))

##################################################################
### Second job - Predict yes from primary person's attributes ####
#################################################################

# We need to remove the decisions as they determine if there was a match
# We also remove all the scores that the partner gave to the primary person

speed_dating_2 = speed_dating[,-c(66,65,57,11:22)]

## Split the data into set1 (training set), set2 (validation set) and set3 (test set)
train_indices = sample(1:8378,5378,replace=FALSE)
set1 = speed_dating_2[train_indices,]
validation_and_test_data = speed_dating_2[-train_indices,]
validation_indices = sample(1:3000,1500,replace=FALSE)
set2 = validation_and_test_data[validation_indices,]
set3 = validation_and_test_data[-validation_indices,]

set1.numeric = set1
set1.numeric[,-51] = as.numeric(unlist(set1[,-51]))
set2.numeric = set2
set2.numeric[,-51] = as.numeric(unlist(set2[,-51]))
set3.numeric = set3
set3.numeric[,-51] = as.numeric(unlist(set3[,-51]))

################# xgboost ##############################

library(xgboost)

xg <- xgboost(data=as.matrix(set1.numeric[,-51]), label=as.numeric(set1.numeric[,51])-1, 
              max_depth=5, eta=.001, subsample=.8,
              nrounds=1000, num_class=2, objective="multi:softprob")

pred.xg.train <- predict(xg, newdata=as.matrix(set1.numeric[,-51]), reshape=TRUE)
pred.xg.val <- predict(xg, newdata=as.matrix(set2.numeric[,-51]), reshape=TRUE)
pred.xg.test <- predict(xg, newdata=as.matrix(set3.numeric[,-51]), reshape=TRUE)

class.xg.train <- apply(pred.xg.train[,], 1, which.max)
class.xg.val <- apply(pred.xg.val[,], 1, which.max)
class.xg.test <- apply(pred.xg.test[,], 1, which.max)
head(cbind(pred.xg.test[,],class.xg.test))

(misclass.boost.xg.train <- mean(ifelse(class.xg.train == as.numeric(set1.numeric$decision), yes=0, no=1)))
(misclass.boost.xg.val <- mean(ifelse(class.xg.val == as.numeric(set2.numeric$decision), yes=0, no=1)))
(misclass.boost.xg.test <- mean(ifelse(class.xg.test == as.numeric(set3.numeric$decision), yes=0, no=1)))

############################## knn ##############################

library(FNN)

scale.set1 <- function(x1,x2){
  mean <- apply(X=x1, MARGIN=2, FUN=mean)
  stdev <- apply(X=x1, MARGIN=2, FUN=sd)
  scale(x=x2, center=mean, scale=stdev)
}


x.1.unscaled <- as.matrix(set1.numeric[,-51])
x.1 <- scale.set1(x.1.unscaled,x.1.unscaled)
x.2.unscaled <- as.matrix(set2.numeric[,-51])
x.2 <- scale.set1(x.1.unscaled,x.2.unscaled)
x.3.unscaled <- as.matrix(set3.numeric[,-51])
x.3 <- scale.set1(x.1.unscaled,x.3.unscaled)

kmax <- 25
k <- matrix(c(1:kmax), nrow=kmax)
runknn <- function(x){
  knnfit <- knn(train=x.1, test=x.2, cl=set1[,51], k=x)
  mean(ifelse(knnfit == set2[,51], yes=0, no=1))
}

mis <- apply(X=k, MARGIN=1, FUN=runknn)
mis.se <- sqrt(mis*(1-mis)/nrow(set2))

plot(x=k, y=mis, type="b", ylim=c(.20,.30), ylab="Missclassification error") 
for(ii in c(1:kmax)){
  lines(x=c(k[ii],k[ii]), y=c(mis[ii]-mis.se[ii], mis[ii]+mis.se[ii]), col=colors()[220])
}
abline(h=min(mis + mis.se), lty="dotted")

#Trying the value of k with the lowest validation error on test data set.
knnfit3.3 <- knn(train=x.1, test=x.3, cl=set1[,51], k=3)

table(knnfit3.3, set3[,51],  dnn=c("Predicted","Observed"))
(misclass.3.knn3 <- mean(ifelse(knnfit3.3 == set3[,51], yes=0, no=1)))

#Trying the 1-se value of k with the (largest k with validation error within 1 se).
knnfit25.3 <- knn(train=x.1, test=x.3, cl=set1[,51], k=25)

table(knnfit25.3, set3[,51],  dnn=c("Predicted","Observed"))
(misclass.3.knn25 <- mean(ifelse(knnfit25.3 == set3[,51], yes=0, no=1)))

###################################################
##################################################
# Interesting - The misclassification rate for 
# individual decision prediction is worse then 
# match prediction
####################################################
##################################################

###################################################################
################ Third job - Gender differences  ##################
###################################################################

# We only keep the 6 attributes collected and decision.

speed_dating_3 = speed_dating[,c(1,34:39,64)]

## Split the data into male and female observations
male = speed_dating_3[speed_dating_3$gender=="male",-1]
female = speed_dating_3[speed_dating_3$gender=="female",-1]

library(randomForest)

rf.male <- randomForest(data=male, decision~., importance=TRUE, ntree=2500, keep.forest=TRUE)
rf.female <- randomForest(data=female, decision~., importance=TRUE, ntree=2500, keep.forest=TRUE)

var.imp.male = rf.male$importance[,3]
var.imp.female = rf.female$importance[,3]
plot(var.imp.male, xaxt="n",type = "o",col = "red", xlab = "Attributes", ylab = "Mean Decrease in Accuracy", 
     main = "Differences in predictive importance of various attributes", ylim=c(0,0.15), lwd=2)

axis(1, at = c(1:6), labels=c("attractive","sincere","intelligence","funny","abmitious","shared interest")) 

lines(var.imp.female, type = "o", col = "blue", lwd=2)
legend("topright",c("Male", "Female"),lty=c(1,1),lwd=c(2,2),col=c("red","blue"))

###################################################
####### Fourth job - Racial differences ###########
###################################################

speed_dating_4 = speed_dating[,c(5,34:39,64)]

## Split the data into male and female observations
apac = speed_dating_4[speed_dating_4$race=="'Asian/Pacific Islander/Asian-American'",-1]
european = speed_dating_4[speed_dating_4$race=="European/Caucasian-American",-1]
others = speed_dating_4[speed_dating_4$race=="Other",-1]
latino = speed_dating_4[speed_dating_4$race=="'Latino/Hispanic American'",-1]
african = speed_dating_4[speed_dating_4$race=="'Black/African American'",-1]


library(randomForest)

rf.apac <- randomForest(data=apac, decision~., importance=TRUE, ntree=2500, keep.forest=TRUE)
rf.european <- randomForest(data=european, decision~., importance=TRUE, ntree=2500, keep.forest=TRUE)
rf.others <- randomForest(data=others, decision~., importance=TRUE, ntree=2500, keep.forest=TRUE)
rf.latino <- randomForest(data=latino, decision~., importance=TRUE, ntree=2500, keep.forest=TRUE)
rf.african <- randomForest(data=african, decision~., importance=TRUE, ntree=2500, keep.forest=TRUE)

var.imp.apac = rf.apac$importance[,3]
var.imp.european = rf.european$importance[,3]
var.imp.others = rf.others$importance[,3]
var.imp.latino = rf.latino$importance[,3]
var.imp.african = rf.african$importance[,3]
plot(var.imp.apac, xaxt="n",type = "o",col = "red", xlab = "Attributes", ylab = "Mean Decrease in Accuracy", 
     main = "Differences in predictive importance of various attributes", ylim=c(0,0.15), lwd=2)

axis(1, at = c(1:6), labels=c("attractive","sincere","intelligence","funny","abmitious","shared interest")) 

lines(var.imp.european, type = "o", col = "blue", lwd=2)
lines(var.imp.latino, type = "o", col = "orange", lwd=2)
lines(var.imp.african, type = "o", col = "green", lwd=2)
lines(var.imp.others, type = "o", col = "brown", lwd=2)
legend("topright",c("Asian/Pacific Islander/Asian-American", "European/Caucasian-American", "Latino/Hispanic American", "Black/African American", "Other"),lty=c(1,1,1,1,1),lwd=c(2,2,2,2,2),col=c("red","blue","orange","green","brown"))
