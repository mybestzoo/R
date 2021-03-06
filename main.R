#read data
data <- read.csv2("base_train.csv", header=T, na.strings=c(""))

#table(data$X3)

#visualize missing values
library(Amelia)
missmap(data, main = "Missing values vs observed")

#clean data
data <- data[,3:ncol(data)]
data$Y3 <- NULL
data <- na.omit(data)

# generate training and test sets
index <- sample(1:nrow(data),round(0.75*nrow(data)))
train <- data[index,]
test <- data[-index,]

#fit the log regression
lm.fit <- glm(X3~., data=train, family=binomial(link='logit'))
# run next line to check the summary
#summary(lm.fit)

# predict on test set
pr.lm <- predict(lm.fit,test,type='response')

#convert results into binary output with decision boundary 0,5
pr.lm <- ifelse(pr.lm > 0.5,1,0)

#calculate missclassification error
E.lm <- mean(pr.lm != test$X3)
#print(paste('Accuracy',1-E.lm))

#achieved result depends on the manual split of data into
# test and training sets, so we need to perform some validation
# at this point. We skip it for simplicity.

#now we plot the ROC curve and calculate the AUC 
#(area under the curve) which are typical performance 
#measurements for a binary classifier.
#The ROC is a curve generated by plotting the 
#true positive rate (TPR) against the false positive 
#rate (FPR) at various threshold settings while 
#he AUC is the area under the ROC curve. 
#As a rule of thumb, a model with good predictive 
#ability should have an AUC closer to 1 (1 is ideal) than to 0.5.
library(ROCR)
pr <- prediction(pr.lm, test$X3)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
#run to plot the ROC curve
#plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
#auc

#now we fit the same data with a neural network
# prepare data
#test[ is.na(test) ] <- NA
#test <- na.omit(test)
test.val <- test$X3
test$X3 <- NULL

# to fit the model we construct 3 hidden layers
library(neuralnet) 
n <- names(train)
f <- as.formula(paste("X3 ~ ", paste(n[!n %in% "X3"], collapse = " + ")))
nn <- neuralnet(f,data=train,hidden=c(11,9),linear.output=F)

#predict on test set
pr.nn <- compute(nn,test)
#convert results to binary
pr.result <- ifelse(pr.nn$net.result > 0.5,1,0)
#compute missclassification error
E.nn <- mean(test.val != pr.result)

# Now the ROC AUC test for neural net
#first detach neuralnet package as it's conflicting with ROCR
detach("package:neuralnet", unload=TRUE)
library(ROCR)
pr.net <- prediction(pr.result,test.val)
prf.nn <- performance(pr.net,measure = "tpr", x.measure = "fpr")
# run to plot ROC curve
#plot(prf.nn)

auc.nn <- performance(pr.net, measure ="auc")
auc.nn <- auc.nn@y.values[[1]]

#print results
print(paste('Accuracy nn:',1-E.nn))
print(paste('Accuracy lm:',1-E.lm))
print(paste('AUC nn:',auc.nn))
print(paste('AUC lm:',auc))