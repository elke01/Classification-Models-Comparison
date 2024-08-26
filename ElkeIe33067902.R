setwd("C:/Users/USER/Desktop/FIT3152/Assignments/A2")

library(tree)
library(e1071)
library(ROCR)
library(adabag)
library(rpart)
library(randomForest)


rm(list = ls()) 
Phish <- read.csv("PhishingData.csv") 
set.seed(33067902) # Your Student ID is the random seed 
L <- as.data.frame(c(1:50)) 
L <- L[sample(nrow(L), 10, replace = FALSE),] 
Phish <- Phish[(Phish$A01 %in% L),] 
PD <- Phish[sample(nrow(Phish), 2000, replace = FALSE),] # sample of 2000 rows 


#===================================
#QUESTION 1
#===================================
# Getting to know the data
View(PD)
dim(PD)

#Label 0 corresponds to a legitimate URL, label 1 to a phishing URL
#Proportion of phishing to legitimate
phishing = sum(PD$Class == 1)
phishing
legit = sum(PD$Class == 0)
legit

# Seeing the data types of each variables
str(PD)

# See the unique values in each columns
unique_values <- lapply(PD, unique)
unique_values

# Get the real valued attributes
real_valued = c(1, 2, 4, 5, 8, 11, 12, 17, 18, 22, 23, 24, 25)

# Create a data frame for real valued attributes
real_valued_PD = subset(PD, select = real_valued)

# Create visualization of distribution of real valued attributes
boxplot(PD[,c(1, 2, 4, 5, 8, 11, 17, 22, 24, 25)], las = 2, main = "Distribution of real valued predictors")
boxplot(PD[,c(12, 18, 23)], las = 2, main = "Distribution of real valued predictors")


# Determining which variable can be omitted (seeing the distribution of values)
boxplot(PD[,c(3:11, 14:17, 19:22, 24:25)], las = 2, main = "Distribution value of predictors")
boxplot(PD[,c(1:2, 12:13, 18, 23)], las = 2, main = "Distribution value of predictors")
boxplot(PD[,c(2,13)], las = 2, main = "Distribution value of predictors")
boxplot(PD[,c(22, 25)], las = 2, main = "Distribution value of predictors")


#===================================
#QUESTION 2
#===================================
# Remove NAs
PD = na.omit(PD)
# Change dependent variable to factor type
PD$Class = factor(PD$Class)


#===================================
#QUESTION 3
#===================================
set.seed(33067902) #Student ID as random seed  
# Split into 70% training data and 30% testing data
train.row = sample(1:nrow(PD), 0.7*nrow(PD)) 
PD.train = PD[train.row,] 
PD.test = PD[-train.row,]

#===================================
#QUESTION 4
#===================================
# Decision Tree
tree.fit = tree(Class ~., data = PD.train)
# Plot the decision tree
plot(tree.fit, main = "Decision Tree")
text(tree.fit, pretty = 0)

# Naïve Bayes 
naive.fit = naiveBayes(Class~., data = PD.train)

# Bagging
bag.fit = bagging(Class~., data = PD.train, mfinal = 10)

# Boosting 
boost.fit = boosting(Class~., data = PD.train, mfinal = 10)

# Random Forest 
rf.fit <- randomForest(Class ~ ., data = PD.train)


#===================================
#QUESTION 5
#===================================
# Decision Tree
# Make Prediction
tree.predict = predict(tree.fit, PD.test, type = "class")
# Create confusion matrix
tree.table = table(actual = PD.test$Class, predicted = tree.predict)
tree.table
# Calculate accuracy
tree.acc = (sum(diag(as.matrix(tree.table))) / nrow(PD.test))*100
tree.acc


# =====================================================
# Naïve Bayes 
# Make Prediction
naive.predict = predict(naive.fit, PD.test, type = "class")
# Create confusion matrix
naive.table = table(actual = PD.test$Class, predicted = naive.predict)
naive.table
# Calculate accuracy
naive.acc = (sum(diag(as.matrix(naive.table))) / nrow(PD.test))*100
naive.acc


# =====================================================
# Bagging
# Make Prediction
bag.predict = predict.bagging(bag.fit, newdata = PD.test)
# Create confusion matrix
bag.predict$confusion
# Calculate accuracy
bagging.acc = (sum(diag(as.matrix(bag.predict$confusion))) / nrow(PD.test))*100
bagging.acc


# =====================================================
# Boosting 
# Make Prediction
boost.predict = predict.boosting(boost.fit, newdata = PD.test)
# Create confusion matrix
boost.predict$confusion
# Calculate accuracy
boost.acc = (sum(diag(as.matrix(boost.predict$confusion))) / nrow(PD.test))*100
boost.acc


# =====================================================
# Random Forest 
# Make Prediction
rf.predict <- predict(rf.fit, PD.test)
# Create confusion matrix
rf.table = table(observed = PD.test$Class, predicted = rf.predict)
rf.table
# Calculate accuracy
rf.acc = (sum(diag(as.matrix(rf.table))) / nrow(PD.test))*100
rf.acc


#===================================
#QUESTION 6
#===================================
# Decision Tree
# Calculate confidence
tree_conf = predict(tree.fit, PD.test, type = "vector")
# Create prediction object (choose 2nd bcs we only use the probability of yes)
tree_prediction = prediction(tree_conf[,2], PD.test$Class)
# Calculate TPR AND FPR
tree_ROC = performance(tree_prediction, "tpr", "fpr")
# Plot ROC
plot(tree_ROC, col = "blue", main = "ROC Curve of Classification Models")
abline(0,1)

# Calculate AUC
tree_auc = performance(tree_prediction, "auc")
tree_auc = as.numeric(tree_auc@y.values)
tree_auc

#==============================================
# Naïve Bayes 
# Calculate confidence
naive_conf = predict(naive.fit, PD.test, type = "raw")
# Create prediction object (choose 2nd bcs we only use the probability of yes)
naive_prediction = prediction(naive_conf[,2], PD.test$Class)
# Calculate TPR AND FPR
naive_ROC = performance(naive_prediction, "tpr", "fpr")
# Plot ROC
plot(naive_ROC, add = TRUE,  col = "red")


# Calculate AUC
naive_auc = performance(naive_prediction, "auc")
naive_auc = as.numeric(naive_auc@y.values)
naive_auc

#==============================================
# Bagging
# Create prediction object (choose 2nd bcs we only use the probability of yes)
bagging_prediction = prediction(bag.predict$prob[,2], PD.test$Class)
# Calculate TPR AND FPR
bagging_ROC = performance(bagging_prediction, "tpr", "fpr")
# Plot ROC
plot(bagging_ROC, add = TRUE,  col = "green")


# Calculate AUC
bagging_auc = performance(bagging_prediction, "auc")
bagging_auc = as.numeric(bagging_auc@y.values)
bagging_auc

#==============================================
# Boosting 
# Create prediction object (choose 2nd bcs we only use the probability of yes)
boosting_prediction = prediction(boost.predict$prob[,2], PD.test$Class)
# Calculate TPR AND FPR
boosting_ROC = performance(boosting_prediction, "tpr", "fpr")
# Plot ROC
plot(boosting_ROC, add = TRUE,  col = "blueviolet")


# Calculate AUC
boosting_auc = performance(boosting_prediction, "auc")
boosting_auc = as.numeric(boosting_auc@y.values)
boosting_auc

#==============================================
# Random Forest 
rf_conf = predict(rf.fit, PD.test, type = "prob")
# Create prediction object (choose 2nd bcs we only use the probability of yes)
rf_prediction = prediction(rf_conf[,2], PD.test$Class)
# Calculate TPR AND FPR
rf_ROC = performance(rf_prediction, "tpr", "fpr")
# Plot ROC
plot(rf_ROC, add = TRUE,  col = "black")
# Give legend to plot
legend("bottomright", legend =c("Decision Tree", "Naive Bayes", "Bagging"
                                , "Boosting", "Random Forest"), 
       col = c("blue", "red", "green", "blueviolet", "black"), lty = c(1,1), 
       lwd = 2, cex = 1.5)


# Calculate AUC 
rf_auc = performance(rf_prediction, "auc")
rf_auc = as.numeric(rf_auc@y.values)
rf_auc


#===================================
#QUESTION 7
#===================================
# Create data frame for the performance of each models
performance = data.frame(Model = c("Tree", "Naive Bayes", "Bagging", "Boosting"
                                , "Random Forest"),
                    Accuracy = c(tree.acc, naive.acc, bagging.acc, boost.acc, rf.acc),
                    AUC = c(tree_auc, naive_auc, bagging_auc, boosting_auc, rf_auc))
performance


#===================================
#QUESTION 8
#===================================
# Decision Tree
summary(tree.fit)

#==============================================
# Bagging
bag.fit$importance
# Make Bar Plot
barplot(bag.fit$importance[order(bag.fit$importance, decreasing = TRUE)], ylim = c(0, 100),
        main = "Bagging Variable Importance")

#==============================================
# Boosting
boost.fit$importance
# Make Bar Plot
barplot(boost.fit$importance[order(boost.fit$importance, decreasing = TRUE)], ylim = c(0, 100),
        main = "Boosting Variable Importance")

#==============================================
# Random forest
rf.importance = rf.fit$importance
# Sort in order from most improtant to least
rf.importance = rf.importance[order(rf.importance[,1], decreasing = TRUE), , drop = FALSE]
# Make Bar Plot
barplot(rf.importance[,1], ylim = c(0, max(rf.importance[,1])), names.arg = rownames(rf.importance),
        main = "Random Forest Variable Importance")


#===================================
#QUESTION 9
#===================================
# Perform cross validation
cvtest = cv.tree(tree.fit, FUN = prune.misclass)
cvtest
# Prune the decision tree model
pruned.Dfit = prune.misclass(tree.fit, best = 3)
summary(pruned.Dfit)

# Make prediction for simple Decision Tree
treeSimple.predict = predict(pruned.Dfit, PD.test, type = "class")
# Calculate accuracy by confusion matrix
treeSimple.table = table(actual = PD.test$Class, predicted = treeSimple.predict)
treeSimple.table
treeSimple.acc = (sum(diag(as.matrix(treeSimple.table))) / nrow(PD.test))*100
treeSimple.acc


# Calculate confidence
tree_conf_s = predict(pruned.Dfit, PD.test, type = "vector")

# Create prediction object (choose 2nd bcs we only use the probability of yes)
tree_prediction_s = prediction(tree_conf_s[,2], PD.test$Class)

# Calculate TPR AND FPR
tree_ROC_s= performance(tree_prediction_s, "tpr", "fpr")

# Calculate AUC
tree_auc_s= performance(tree_prediction_s, "auc")
tree_auc_s = as.numeric(tree_auc_s@y.values)
tree_auc_s

# Comparison of the simple tree and the original tree
summary(pruned.Dfit)
summary(tree.fit)

# Plot the simple tree
plot(pruned.Dfit, main = "Simple Decision Tree")
text(pruned.Dfit, pretty = 0)

#===================================
#QUESTION 10
#===================================
# Making the improved decision tree (changing the mincut)
treeImproved.fit = tree(Class ~., PD.train, mincut = 15)

# Make prediction for imrpved Decision Tree
treeImproved.predict = predict(treeImproved.fit, PD.test, type = "class")
# Calculate accuracy by confusion matrix
treeImproved.table = table(actual = PD.test$Class, predicted = treeImproved.predict)
treeImproved.table
treeImproved.acc = (sum(diag(as.matrix(treeImproved.table))) / nrow(PD.test))*100
treeImproved.acc

# Check the summary of the improved tree
summary(treeImproved.fit)

# Calculate confidence
tree_conf_i = predict(treeImproved.fit, PD.test, type = "vector")

# Create prediction object (choose 2nd bcs we only use the probability of yes)
tree_prediction_i = prediction(tree_conf_i[,2], PD.test$Class)

# Calculate TPR AND FPR
tree_ROC_i= performance(tree_prediction_i, "tpr", "fpr")

# Calculate AUC 
tree_auc_i= performance(tree_prediction_i, "auc")
tree_auc_i = as.numeric(tree_auc_i@y.values)
tree_auc_i

# Plot the improved tree
plot(treeImproved.fit, main = "Improved Decision Tree")
text(treeImproved.fit, pretty = 0)

#===================================
#QUESTION 11
#===================================
# Import needed library
library(neuralnet)

# Make 80% training data and 20% testing data
set.seed(33067902) #Student ID as random seed  
train.row.net = sample(1:nrow(PD), 0.8*nrow(PD)) 
PD.train.net = PD[train.row.net,] 
PD.test.net = PD[-train.row.net,]


# Create Artificial Neural Network
net.PD <- neuralnet(Class == 1~ A01 + A18 + A22 + A23, PD.train.net, hidden=5, linear.output = FALSE, stepmax = 1e7)

# Make prediction to the testing data
net.pred = compute(net.PD, PD.test.net[c(1, 18, 22, 23)])

# Round the result
net.predr = round(net.pred$net.result,0)
# Change to dataframe
net.predrdf = as.data.frame(net.predr)

# Create a confusion matrix
net.table = table(observed = PD.test.net$Class, predicted =
        net.predrdf$V1)
net.table

# Calculate Accuracy
net.acc = (sum(diag(as.matrix(net.table))) / nrow(PD.test.net))*100
net.acc

# Calculate confidence
net_conf = predict(net.PD, PD.test.net, type = "response")

# Create prediction object (choose 2nd bcs we only use the probability of yes)
net_prediction = ROCR::prediction(net.pred$net.result[,1], PD.test.net$Class)

# Calculate AUC
net_auc = performance(net_prediction, "auc")
net_auc = as.numeric(net_auc@y.values)
net_auc

# Plot ANN
plot(net.PD, rep="best") 


#===================================
#QUESTION 12
#===================================
# MAKING SVM MODEL
# Only choose the important variables
PD.train.svm = PD.train[, c(1,  18, 22, 23, 26)]
PD.test.svm = PD.test[, c(1, 18, 22, 23, 26)]

# Scale independent variables
PD.train.svm[-5] = scale(PD.train.svm[-5])
PD.test.svm[-5] = scale(PD.test.svm[-5])

# Fitting SVM to the Training set 
svm.model = svm(formula = Class ~ ., 
                 data = PD.train.svm, 
                 type = 'C-classification', 
                 kernel = 'radial',
                probability = TRUE) 

# Make prediction to the test data
svm.pred = predict(svm.model, newdata = PD.test.svm) 

# Create Confusion Matrix
svm.table = table(PD.test.svm$Class, svm.pred) 
svm.table

# Calculate Accuracy
svm.acc = (sum(diag(as.matrix(svm.table))) / nrow(PD.test.svm))*100
svm.acc

# Calculate confidence
svm.pred.prob = predict(svm.model, newdata = PD.test.svm, probability = TRUE)
svm.pred.prob = attr(svm.pred.prob, "probabilities")

# Create prediction object (choose 2nd bcs we only use the probability of yes)
net_prediction = ROCR::prediction(svm.pred.prob[, 2], PD.test.svm$Class)

# Calculate AUC
svm_auc = performance(net_prediction, "auc")
svm_auc = as.numeric(svm_auc@y.values)
svm_auc
