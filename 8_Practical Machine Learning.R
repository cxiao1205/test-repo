# covers the basic ideas behind machine learning/prediction: study design, conceptual issues, and practical implementation.
setwd("C:/Users/Xiao.Cheng/Desktop/R Code/Data Science Specialization_Coursera")

# prediction motivation
        # Who predicts? 
        # Local governments: pension payments. Google: whether you will click on an ad. Amazon: what movies you will watch. Insurance companies: what your risk of death is. Johns Hopkins: who will succeed in their programs. 
        # Choosing the right dataset and knowing what the specific question is paramount.
        # Components of a predictor: question -> input data -> features -> algorithm -> parameters -> evaluation
library(kernlab) 
data(spam) # load the spam dataset.
head(spam) # layout shows you the variables available in the datdaset: frequencies of certain words
plot(density(spam$your[spam$type=="nonspam"]),
     col = "blue",main = "", xlab = "Frequency of 'your'") # distribution of the word "your" within nonspam category
lines(density(spam$your[spam$type=="spam"]),col='red') # distribution of the word "your" within spam category
abline(v=0.5, col = "black")  # what if we choose 0.5 as a cutoff point?
prediction <- ifelse(spam$your > 0.5, "spam","nonspam")
table(prediction,spam$type)/length(spam$type) # accuracy = 0.459 + 0.292 = 75.1% .Optimistic estimate of the overall error rate
        # do not try to automize the features selection process. a semi-auto version is deep learning, which might be useful. 
        # "Best" Machine Learnign Method: Interpretable, Simple, Accurate, Fast and Scalable (trade-offs)
# Sample error
        # In sample error: the error rate you get on the same data set you used to buid your predictor. Sometimes called resubstitution error.
        # Out of Sample error: the error rate you get on a new data set. Sometimes called generalization error. 
        # Key: out of sample error is what you care about. In sample error < out of sample error. The reason is overfitting. 
library(kernlab) 
data(spam)
set.seed(333)
smallSpam <- spam[sample(dim(spam)[1],size = 10),] # take a small sample with 10 obs
spamLabel <- (smallSpam$type == "spam")*1 + 1
plot(smallSpam$capitalAve,col=spamLabel)  # the idea is you may able to fit the data extremely well, if we only look at the training set. e.g. choose a certain threshold value of capitalAve.
        # ok the seed simulation seems differ than the one on coursera. Anyway, consider about two rules. Assume rule 1 is a more complicated one: if x>2.7, or x between 2,4 and 2.45, then we classify x as spam. if x<2.4, or x is between 2.45 and 2.7, we classify x as nonspam. Rule 2: simply use 2.8 as a cutoff point. The rule 1 may fit small sample perfectly, but works worse than rule 2 when dealing with the full sample. This is called verfitting problem. 
        # the key is data have two parts, signal and noise. We want to find signal and ignore the noise. 

# Prediction study design:
        # 1. Define your error rate
        # 2. Split data into training, testing, and validation subsets
        # 3. on the training set pick features, use cross-validation
        # 4. on the training set pick prediction function, use cross-validation
        # 5. if no validation, apply 1 time to test set (don't apply multiple times ---otherwise we are still fit the model using test set)
        # 6. if Validation, apply to test set and refine. apply 1 time to validation
        # Rules of thumb: avoid using small dataset. 60% training, 20% test, and 20% validation if large sample size. 60% training and 40% testing if you have a medium sample size. if you only have a small sample size, do cross validation, and report caveat of small sample size. 
        # don't look at test/validation set. randomly sample. note the structure of the problem. 

# Type of errors. 
        # In general, Positive = identified, negative = rejected. Therefore:
        # True positive = correctly identified 
        # False positive = incorrectly identified
        # True negative = correctly rejected
        # False negative = incorrectly rejected
        # Sensitivity: P(positive test | disease)= TP/(TP + FN)
        # Specificity: P(negative test | no disease) = TN/(FP + TN)
        # Positive Predictive Value: P(disease | positive test) = TP /(TP + FP)
        # Negative Predictive value: P(no disease | negative test) = TN / (FN + TN)
        # Accuracy: P(correct outcome) = True positive + True negative = (TP + TN)/(TP + FP + FN + TN)
# Now Assume some disease has a 0.1% prevalence in the population. Assume our test kit works with 99% sensitivity and 99% specificity. So the confusion matrix is like following:
#                    Positive Disease             Negative Disease
# Positive Test          TP(99)                        Fp(999)
# Negative Test          FN(1)                         TN(98901)
# In this case, sensitivity = specificity = 99%. However, Positive Predictive Value = 99/(99+999) = 9% (if your test result shows positive, you only have 9% of the chance that you have desease). Negative predictive value = 98901/(1+98901) > 99.9%. Accuracy=(99+98901)/100000=99%. 
# Now if a disease is prevalence at 10% in the population, numbers make better sense. 
        # MSE: Mean squared error: 1/n*sum(prediction-truth)^2. Also the RMSE is the root mean squared error. sensitive to outliers.
        # Other common error measures: Median absolute deviation, more robust. Sensitivity, specificity, accuracy, concordance. 
# ROC (Receiver Operating Characteristic) curves: x axis: P(FP) or P(1-specificity), y axis: P(TP) or P(sensitivity). the bigger area under the curve (AUC), the better.AUC = 0.5, random guesssing. AUC = 1, perfect classification. 

# Cross validation
        # The key idea is the accuracy on the training set is too optimistic. We need a better estimate comes from an independent set. But we don't want the test set to be a part of the training set. So we estimate the test set accuracy with the traning set, which is cross validation. 
        # split the training set into sub-traning and sub-test set. repeat and average the estimated errors. 
                # but how? consider random subsampling, k-fold (k equal sized subsetting), leave one out (just pick one as sub-test set). 
                # for time series data, must be in "chunks". For k-fold cross validation, larger k means less bias but more variance. Smaller k means more bias but less variance. random sampling must be done without replacement. random sampling with replacement is the bootstrap---and bootstrap will underestimates of the error (may be corrected but more complicated).

# So now, what data should you use?
        # Consider polling data (combine datasets together), weighting the data, use data closed to x to predict x. 

# Caret package
library(caret)   # Machine learning package in r. 
        # is compatible with several objects:
        # lda from MASS: predict(obj)
        # glm from stats: predict(obj, type = "response")
        # gbm from gbm: predict(obj, type = "response", n.trees)
        # mda from mda: predict(obj, type = "posterior")
        # rpart from rpart, predict(obj, type = "prob)
        # Weka from RWeka, predict(obj, type = "probability")
        # LogitBoost from caTools, predict(obj, type = "raw", nIter)
library(kernlab);data(spam)
inTrain <- createDataPartition(y=spam$type,p=0.75, list=FALSE) # 75% of data to train the model
training <- spam[inTrain,] # subset as training set
testing <- spam[-inTrain,] # subset as testing set
dim(training) # dimension of the training set
set.seed(32343)
modelFit <- train(type ~., data=training, method="glm")  # training the dataset traning: type is the predicted variable, use all other variables as independent variables, method is glm (generalized linear model)
modelFit # so this model including 3451 samples, 57 predictors, 2 predicting clasees, no pre-processing, resampling: bootstrapped (25 replicates, correct the problem of underestimating errors)
modelFit$finalModel  # this is the coefficient for glm model. 
predictions <- predict(modelFit, newdata=testing)
predictions # predicting the test set
confusionMatrix(predictions,testing$type)  # The no information rate is the error rate when the input and output are independent. 

# Data slicing
        # Partition(data splitting)
library(kernlab);data(spam)
inTrain <- createDataPartition(y=spam$type,p=0.75, list=FALSE) # 75% of data to train the model
training <- spam[inTrain,] # subset as training set
testing <- spam[-inTrain,] # subset as testing set
dim(training) # dimension of the training set
        # K-fold
set.seed(32323)
folds <- createFolds(y=spam$type,k=10,list=TRUE,returnTrain=TRUE) # create a folds list. Each item in the list represent a sample.
sapply(folds,length)
folds[[1]][1:10]

folds <- createFolds(y=spam$type,k=10,list=TRUE,returnTrain=FALSE) # return the test set
sapply(folds,length)
folds[[1]][1:10]
        # Resampling/bootstrapping
folds <- createResample(y=spam$type,times=10,list=TRUE) # resampling/bootstrapping 10 times. Remember it means everytime you draw an obs from the raw sample you put it back after drawing. Since you may pick one obs several times, the variance would be too small.
sapply(folds,length)
        # Time Slices
tme <- 1:1000
folds <- createTimeSlices(y=tme,initialWindow = 20,horizon=10) # we want to create several windows with 20 sample size each (so each training set consists of 20 obs), and forecasting sample size is 10 (so each test set size is 10)
names(folds)
folds$train[[1]]

# Training Options
# LogitBoost from caTools, predict(obj, type = "raw", nIter)
library(kernlab);data(spam);library(caret)
inTrain <- createDataPartition(y=spam$type,p=0.75, list=FALSE) # 75% of data to train the model
training <- spam[inTrain,] # subset as training set
testing <- spam[-inTrain,] # subset as testing set
dim(training) # dimension of the training set
set.seed(32343)
modelFit <- train(type ~., data=training, method="glm")  # the bottom line is we could use train default and trainControl for setting up the model. seed is very important for reproducible research. Check caret tutorial for more information

# plotting Predictors
library(ISLR); library(ggplot2);library(caret);
data(Wage)
summary(Wage)
inTrain <- createDataPartition(y=Wage$wage, p = 0.7, list= FALSE)
training <- Wage[inTrain,]
testing <- Wage[-inTrain,]
dim(training); dim(testing)
featurePlot(x=training[, c("age","education","jobclass")],  # scatter plot between any two of the variables. y on the top right corner
            y = training$wage,
            plot="pairs")

qplot(age,wage,data=training)   # plot age against wage. However, we can see some points significant higher than others (with higher wage)
qplot(age,wage,colour=jobclass,data=training)  # we can colour the plotting based on jobclass. for the significant higher cohert, we can see majority of them comes from the information sector
qq <- qplot(age,wage,colour=education, data=training)  
qq + geom_smooth(method='lm',formula=y~x) # we can actually add regression smoothers
library(Hmisc)
cutWage <- cut2(training$wage, g=3) # cut wage variable into 3 different categories
table(cutWage)
p1 <- qplot(cutWage,age,data=training,fill=cutWage,geom=c("boxplot"))  # boxplot actually shows 5 statistics: min, Q1, median, Q3, and max
p1
p2 <- qplot(cutWage,age,data=training,fill=cutWage, geom=c("boxplot","jitter")) # show the scatter plot also
library(gtable);library(gridExtra)
grid.arrange(p1,p2,ncol=2)
t1 <- table(cutWage,training$jobclass)
t1
prop.table(t1,1)  # proportion of each row. 2 means proportion of each column

qplot(wage,colour=education,data=training,geom="density") # plot the distribution of wage breakdown by education. Please note we should only explorate the training set, but ignore the testing set

# Basic preprocessing (note only for the training set)
library(kernlab);data(spam);library(caret)
inTrain <- createDataPartition(y=spam$type,p=0.75, list=FALSE) # 75% of data to train the model
training <- spam[inTrain,] # subset as training set
testing <- spam[-inTrain,] # subset as testing set
hist(training$capitalAve,main="",xlab="ave. capital run length") # this is very skewed data----although majority clustered on one side of the histogram, we also have few data points significantly higher than the normal. It is very hard to deal with to model the predictors.
mean(training$capitalAve)
sd(training$capitalAve)  # standard deviation is much larger than the mean. 
trainCapAve <- training$capitalAve
trainCapAveS <- (trainCapAve - mean(trainCapAve))/sd(trainCapAve) # standardize
mean(trainCapAveS)
sd(trainCapAveS)
        # note we should also standardize the test set using the mean and sd from the training set. however, the standardized test set may have non N(0,1) distribution because we use diff parameters than own generates
preObj <- preProcess(training[, -58], method= c("center","scale"))  # passing all training variables except the 58th one (spam/nospam). to center every variable and scale each variable
trainCapAveS <- predict(preObj, training[,-58])$capitalAve  
mean(trainCapAveS)
testCapAveS <- predict(preObj, testing[,-58])$capitalAve   # pass the preProcess data from training set to test set. 
mean(testCapAveS)        # the mean may differ from 0

set.seed(32343)
modelFit <- train(type~., data=training, 
                  preProcess=c("center","scale"),method="glm") # pass the preProcess function to the train function
modelFit
PreObj <- preProcess(training[,-58],method=c("BoxCox"))  # Box-Cox transformation. It is used to transform non-normal variables into a normal shape. 
trainCapAveS <- predict(PreObj,training[,-58])$capitalAve
par(mfrow=c(1,2)); hist(trainCapAveS);qqnorm(trainCapAveS)  # histogram look much nicer. QQ plot is closed to normal

library(RANN)
set.seed(13343)
training$capAve <- training$capitalAve
selectNA <- rbinom(dim(training)[1],size=1,prob=0.05)==1
training$capAve[selectNA] <- NA # we set 5% of the variable to be missing. How can we deal with the missing variables?
preObj <- preProcess(training[,-58],method="knnImpute")  # try to impute the missing data. knnImpute is the k nearest neighbours imputing, with default k=10
capAve <- predict(preObj,training[,-58])$capAve
capAveTruth <- training$capAve
capAveTruth <- (capAveTruth-mean(capAveTruth))/sd(capAveTruth)
quantile(capAve -capAveTruth)
quantile((capAve - capAveTruth)[selectNA])
quantile((capAve - capAveTruth)[!selectNA])

# Covariate Creation
        # Two levels of covariate creation: 
        # level 1 is from raw data to covariate. 
        # Depends heavily on application. The balancing act is summarization vs. information loss

        # level 2 is transforming tidy covariates
        # The best approach is through exploratory analysis (plotting/tables): do it only on the training set!
library(ISLR);library(caret);data(Wage); # load wage dataset
inTrain <- createDataPartition(y=Wage$wage,  
                               p=0.7,list=FALSE)
training <- Wage[inTrain,]; testing <- Wage[-inTrain,]
table(training$jobclass)
dummies <- dummyVars(wage ~ jobclass, data=training) # this function creates a full set of dummy variables. We could generate dummies to describe the categorical information 
head(predict(dummies,newdata=training)) # predict the wage (which is in the formula when generate dummies)

nsv <- nearZeroVar(training,saveMetrics = FALSE)  # when a variable has almost no variability (almost the same for all observations). 
nsv  # think about the nzv value as some hypothesis test result. TRUE means there is almost no variability hence we could ignore the variable. However, if the variable with limitied variability is your variable of interest, it is a different story.

        # Spline basis
library(splines)   # fit curvy lines
bsBasis <- bs(training$age,df=3)  # 3rd degree polinomial. Generate the B-spline basis matrix for a polynomial spline
bsBasis  # cubic fitting
lm1 <- lm(wage ~ bsBasis, data=training)
plot(training$age,training$wage,pch=19,cex=0.5)
points(training$age,predict(lm1,newdata=training),col="red",pch=19,cex=0.5)
predict(bsBasis,age=testing$age)  # test on testing set.

# Preprocessing with PCA
library(kernlab);data(spam);library(caret)
inTrain <- createDataPartition(y=spam$type,p=0.75, list=FALSE) # 75% of data to train the model
training <- spam[inTrain,] # subset as training set
testing <- spam[-inTrain,] # subset as testing set
M <- abs(cor(training[,-58]))  # correlation matrix
diag(M) <- 0  # remove diagnoal cor (to it self, =1)
which(M > 0.8, arr.ind=T) # return the variables have high correlation with each other (locations)
names(spam)[c(34,32)]
plot(spam[,34],spam[,32]) 

X <- 0.71*(training$num415 + training$num857)  # we can rotate the plot above. 
Y <- 0.71*(training$num415 - training$num857)
plot(X,Y) # most of the variability happens on X axis (Y values are mostly 0). In this case, X bring much more information than Y
smallSpam <- spam[,c(34,32)] # select two columns
prComp <- prcomp(smallSpam)  # PCA. returns a list
plot(prComp$x[,1],prComp$x[,2])  # this plot shows a very similar picture as the XY plot. The first component is like X, and second component is like Y
prComp$rotation  # rotation matrix. PC1 = 0.7081*num415 + 0.7061*num857. PC2 = 0.7061*num415 - 0.7081*num857
typeColor <- ((spam$type=="spam")*1 + 1)   # colar vector. 
prComp <- prcomp(log10(spam[,-58]+1))  # transfer the variables a little to make the data (whole dataset) more Gaussian.
plot(prComp$x[,1],prComp$x[,2],col=typeColor,xlab="PC1",ylab="PC2")  # plot principle component 1 against principle component 2. In this plot, we also differentiate the observations (like the position in the eigen vector) by black and red----and, in this plot, we could see the plotting of red points ("spam") are generally to the right of black points ("ham", or non-spam obs)

preProc <- preProcess(log10(spam[,-58]+1),method="pca",pcaComp=2) # PCA, return a list. 
spamPC <- predict(preProc,log10(spam[,-58]+1))  # predict the two principal component
plot(spamPC[,1],spamPC[,2],col=typeColar) # you can see the difference between ham and spam obs.

trainPC <- predict(preProc,log10(training[,-58]+1)) # only apply to the training set
trainPC$type <- training$type
modelFit <- train(type ~ ., method="glm",data=trainPC)
testPC <- predict(preProc,log10(testing[,-58]+1))  # take the PCA process from the training dataset and pass it to the testing data set
confusionMatrix(testing$type,predict(modelFit,testPC))  # check the training model performance in test set

# predicting with regression
library(caret);data(faithful); set.seed(333)
inTrain <- createDataPartition(y=faithful$waiting,
                               p=0.5, list=FALSE)
trainFaith <- faithful[inTrain,]; testFaith <- faithful[-inTrain,] # split the dataset into training set and test set.
head(trainFaith) # use old faithful geyser eruption as an example. eruptions is the eruption time, waiting time is the waiting time
plot(trainFaith$waiting,trainFaith$eruptions,pch=19,col="blue",xlab="Waiting",ylab="Duration") # plot duration vs. waiting time
lm1 <- lm(eruptions ~ waiting,data=trainFaith)
summary(lm1)

plot(trainFaith$waiting,trainFaith$eruptions,pch=19,col="blue",xlab="Waiting",ylab="Duration")
lines(trainFaith$waiting,lm1$fitted,lwd=3)  # plot the fit line also
coef(lm1)[1] + coef(lm1)[2]*80  # predict the eruption time if we have waiting time of 80
newdata <- data.frame(waiting=80) # another way to do the prediction
predict(lm1,newdata)

par(mfrow=c(1,2)) # plot of training data fit and test data fit
plot(trainFaith$waiting,trainFaith$eruptions,pch=19,col="blue",xlab="Waiting",ylab="Duration")
lines(trainFaith$waiting,predict(lm1),lwd=3)
plot(testFaith$waiting,testFaith$eruptions,pch=19,col="blue",xlab="Waiting",ylab="Duration")
lines(testFaith$waiting,predict(lm1,newdata=testFaith),lwd=3)

sqrt(sum((lm1$fitted-trainFaith$eruptions)^2))  # RMSE on training set
sqrt(sum((predict(lm1,newdata=testFaith)-testFaith$eruptions)^2)) # RMSE on test set. More realistic estimate for new data set.

par(mfrow=c(1,1),mar=c(4,4,4,4)) # plot the confidence interval also
pred1 <- predict(lm1,newdata=testFaith,interval="prediction")
ord <- order(testFaith$waiting)
plot(testFaith$waiting,testFaith$eruptions,pch=19,col="blue")
matlines(testFaith$waiting[ord],pred1[ord,],type="l",col=c(1,2,2),lty = c(1,1,1), lwd=3)

modFit <- train(eruptions ~ waiting,data=trainFaith,method="lm")
summary(modFit$finalModel) # use caret package train function for regression models.

# Predicting with regression multiple covariates---how to choose predictors.
library(ISLR); library(ggplot2); library(caret);
data(Wage); Wage <- subset(Wage,select=-c(logwage)) # remove logwage variable
summary(Wage)

inTrain <- createDataPartition(y=Wage$wage,  # again, training vs. test data set
                               p=0.7, list=FALSE)
training <- Wage[inTrain,]; testing <- Wage[-inTrain,]
dim(training); dim(testing)

featurePlot(x=training[,c("age","education","jobclass")],  # feature plot
            y = training$wage,
            plot="pairs")

qplot(age,wage,data=training)
qplot(age,wage,colour=jobclass,data=training) # jobclass tells you some info
qplot(age,wage,colour=education,data=training) # education also tells you some variation.

modFit<- train(wage ~ age + jobclass + education,
               method = "lm",data=training)
finMod <- modFit$finalModel
print(modFit) 

plot(finMod,1,pch=19,cex=0.5,col="#00000010") # plot the residuals. we can see some outliers. Think about is there any other predictors could capture the features of these outliers?
qplot(finMod$fitted,finMod$residuals,colour=race,data=training) 
plot(finMod$residuals,pch=19) # against index. if see trend, then consider serial correlation

pred <- predict(modFit, testing)
qplot(wage,pred,colour=year,data=testing) # heat map. does year tell us some trend?

modFitAll<- train(wage ~ .,data=training,method="lm") # use all variables.
pred <- predict(modFitAll, testing)
qplot(wage,pred,data=testing)

# Predicting with trees.
        # Iteratively split variables into groups
        # Evaluate "homogeneity" within each group (leaves)
        # split again if necessary
        # easy to set up and better performance in nonlinear settings
        # but may have overfitting problem and harder to estimate uncertainty.

        # start with all variables in one group. find the best predictor -> two leaves. within each leaf, find the best predictor-> continue, until the groups are too small or sufficiently pure.

        # error measures (Measures of impurity): misclassification error(0 means perfect purity, 0.5 means no purity, 1 means perfect on the other side) or gini index (0 means perfect purity, 0.5 means no purity), Deviance/information gain (0 means perfect purity, 1 means no purity)
data(iris); library(ggplot2)
names(iris)
table(iris$Species) # predict species. 
inTrain <- createDataPartition(y=iris$Species,
                               p=0.7, list=FALSE) # training set and test set.
training <- iris[inTrain,]
testing <- iris[-inTrain,]
dim(training); dim(testing)
qplot(Petal.Width,Sepal.Width,colour=Species,data=training)  # a little challenging with linear model (we can observe 3 clusters.)
library(caret)
modFit <- train(Species ~ .,method="rpart",data=training) # rpart is a package to do the regression and decision trees. Use this method to get a decission tree
print(modFit$finalModel)

plot(modFit$finalModel, uniform=TRUE,  # this is a way to plot the decision tree
     main="Classification Tree")
text(modFit$finalModel, use.n=TRUE, all=TRUE, cex=.8)
library(rattle) # finer printing
fancyRpartPlot(modFit$finalModel)
predict(modFit,newdata=testing)  # predict the test set with the decision tree. 

# Bagging: Bootstrap aggregating
        # Resample cases and recalculate predictions. Average or majority vote.
library(ElemStatLearn); data(ozone,package="ElemStatLearn")
ozone <- ozone[order(ozone$ozone),]
head(ozone)    # predict temperature as a function of ozone.

ll <- matrix(NA,nrow=10,ncol=155)
for(i in 1:10){   # resample the dataset 10 times. 
        ss <- sample(1:dim(ozone)[1],replace=T)   # for each time of the loop (10 times total), first resample the dataset
        ozone0 <- ozone[ss,]; ozone0 <- ozone0[order(ozone0$ozone),] # subset the resampled data, order them based on the value of ozone
        loess0 <- loess(temperature ~ ozone,data=ozone0,span=0.2) # curve (smooth) fit of temperature vs. ozone. like spline 
        ll[i,] <- predict(loess0,newdata=data.frame(ozone=1:155)) # predict the temperature value based on the fit and ozone value 1:155
}

plot(ozone$ozone,ozone$temperature,pch=19,cex=0.5) # original dataset scatter plot
for(i in 1:10){lines(1:155,ll[i,],col="grey",lwd=2)}  # plot the 10 resampled curve fit
lines(1:155,apply(ll,2,mean),col="red",lwd=2) # plot the average fit
        # may also use bagEarth, treebag, bagFDA method in train function for bagging
predictors = data.frame(ozone=ozone$ozone) # predictor as ozone
temperature = ozone$temperature # outcome as temperature
library(party) # required for bag function
treebag <- bag(predictors, temperature, B = 10, # bag function in caret package
               bagControl = bagControl(fit = ctreeBag$fit, # function used to fit. note ctreeBag is a function 
                                       predict = ctreeBag$pred, # function to predict from train model
                                       aggregate = ctreeBag$aggregate)) # way to put predictions together

plot(ozone$ozone,temperature,col='lightgrey',pch=19) # scatter plot of ozone vs. temperature
points(ozone$ozone,predict(treebag$fits[[1]]$fit,predictors),pch=19,col="red") # fit from a single conditional regression tree (the first fit)
points(ozone$ozone,predict(treebag,predictors),pch=19,col="blue") # fit from averaged trees.
ctreeBag$fit

# Random Forests: an extension to bagging
        # Bootstrap samples -> at each split, bootstrap variables -> Grow multiple trees and vote/average
        # accurate, but may be slow, hard to interpret, and overfitting problem.
        # each tree is based on a bootstrap sample (resampled), each node we allowed different subset of the variables to potentially contribute to the splits. For each new observation, we find a prediction in in each tree. then we average these predictions to get the predictive probabilities of each class across all the different trees (I guess use test data)

data(iris); library(ggplot2);library(caret);library(randomForest)
inTrain <- createDataPartition(y=iris$Species,
                               p=0.7, list=FALSE)
training <- iris[inTrain,]
testing <- iris[-inTrain,]

modFit <- train(Species~ .,data=training,method="rf",prox=TRUE)  # try to classify species of leaves by random forest. prox = TRUE 
modFit
getTree(modFit$finalModel,k=2) # get the 2nd tree in the random forest model

irisP <- classCenter(training[,c(3,4)], training$Species, modFit$finalModel$prox) # center of the class predictions, or what the predictions would be. 
irisP <- as.data.frame(irisP); irisP$Species <- rownames(irisP)
p <- qplot(Petal.Width, Petal.Length, col=Species,data=training)   # scatter plot of plot petal length and petal width. colored by species
p + geom_point(aes(x=Petal.Width,y=Petal.Length,col=Species),size=5,shape=4,data=irisP) # add centers of each category

pred <- predict(modFit,testing); testing$predRight <- pred==testing$Species # prediction using test data
table(pred,testing$Species)
qplot(Petal.Width,Petal.Length,colour=predRight,data=testing,main="newdata Predictions") # scatter plot in test data. colored by predRight---whether the prediction model classifies leaves right or not.
        # check rfcv for cross validation. train function in caret package also provide CV

# Boosting
        # Take lots of weak predictors (could be from varieties of methods, like random forest, regression and bagging), weight them and add them up, get a stronger predictor
        # e.g. gbm: boosting with trees
        # mboost: model based boosting
        # ada: statistical boosting based on additive logistic regression
        # gamBoost: boosting generalized additive models
library(ISLR); data(Wage); library(ggplot2); library(caret);library(gbm)
Wage <- subset(Wage,select=-c(logwage))
inTrain <- createDataPartition(y=Wage$wage,
                               p=0.7, list=FALSE)
training <- Wage[inTrain,]; testing <- Wage[-inTrain,] # wage data as an example
modFit <- train(wage ~ ., method="gbm",data=training,verbose=FALSE)  # modeling wage using boosting with trees. turn verbose off to avoid too many output things
print(modFit)

qplot(predict(modFit,testing),wage,data=testing) # print the predicted wage vs. real wage in the test data

# Model based prediction
        # Assume the data follow a prob model (e.g. normal distribution). Use Bayes' theorem to identify optimal classifiers
        # can take advantage of structure of the data. Need to make additional assumptions about the data.
        # Linear discriminant analysis assume prob function is multivariate Gaussian with same covariances
        # Quadratic discriminant analysis assume prob function is multivariate Gaussian with different covariances
        # Model based prediction assumes more complicated versions for the covariance matrix
        # Naive Bayes assumes independence between features for model building. use in text/document classification---when you have to deal with a lot of binary predictors
        # predictors will be like decision boundaries
data(iris); library(ggplot2)
names(iris)
table(iris$Species)  # use leaves species dataset as an example
inTrain <- createDataPartition(y=iris$Species,
                               p=0.7, list=FALSE)
training <- iris[inTrain,]
testing <- iris[-inTrain,]
dim(training); dim(testing)

modlda = train(Species ~ .,data=training,method="lda") # linear discriminant model
modnb = train(Species ~ ., data=training,method="nb") # naive bayes model
plda = predict(modlda,testing); pnb = predict(modnb,testing) # predictions in test set
table(plda,pnb) # predictions agree for all but one (or a little bit more) values

equalPredictions = (plda==pnb)
qplot(Petal.Width,Sepal.Width,colour=equalPredictions,data=testing)

# Regularized regression
        # Fit a regression model, penalize (or shrink) large coefficients
        # ccan helo with the bias/variance tradeoff (prediction ability vs. prediction error). can help with model selection
        # doesn't perform as well as random forests and boosting
        # e.g. remove one of the variable when it is highly correlated to another in the regression model. In this case, we may get slightly biased coefficients, but reduce the variance in the estimate.

library(ElemStatLearn); data(prostate) 
str(prostate) # prostate cancer example
        # When the number of predictors increase (model becomes more complex), the train error usually decrease. However, the test error shows some sort of quadratic form (due to overfitting problem).
        # model selection approach: split samples. decomposing expected prediction error. 
small = prostate[1:5,]
lm(lpsa ~ .,data =small)  # an example if you have more predictors than observations. 

        # Ridge regression. We add lambda * sum(beta^2) to the objection function (RSS). You can imagine that when the value of lambda grows, the penalty term also increase----if the lambda is too big than all coefficients should go to 0. When lambda is 0, the model reduced to linear regression model. 
        # Lasso regression. shrink some of the coefficients and set some of them to exactly 0 (automatically provide you with model selection rules). Also have Relaxo Lasso regression.

# Combining predictors
        # combine classifiers by averaging/voting (e.g. combine random forest together with linear regression, etc.)
        # improve accuracy, but may reduce interpretability. 
        # e.g. the winner of Netflix prize actually combine 107 predictors together.
        # Basic intuition is majority vote
        # We may combine similar classifiers (e.g. trees), or model stacking, model ensembling (different classifiers)
library(ISLR); data(Wage); library(ggplot2); library(caret);
Wage <- subset(Wage,select=-c(logwage))

# Create a building data set and validation set
inBuild <- createDataPartition(y=Wage$wage,
                               p=0.7, list=FALSE)
validation <- Wage[-inBuild,]; buildData <- Wage[inBuild,]

inTrain <- createDataPartition(y=buildData$wage,
                               p=0.7, list=FALSE)
training <- buildData[inTrain,]; testing <- buildData[-inTrain,] # wage data set as an example
dim(training)
dim(testing)
dim(validation)

mod1 <- train(wage ~.,method="glm",data=training)  # generalized linear model
mod2 <- train(wage ~.,method="rf", # random forest
              data=training, 
              trControl = trainControl(method="cv"),number=3)

pred1 <- predict(mod1,testing); pred2 <- predict(mod2,testing) # predict the testing set using each model 
qplot(pred1,pred2,colour=wage,data=testing)# neither of the two methods perfectly correlate with the true wage variable.

predDF <- data.frame(pred1,pred2,wage=testing$wage) # new data set. predict 1, predict 2 and true value of wage
combModFit <- train(wage ~.,method="gam",data=predDF) # train a model---outcome is the true wage, input are the predictions.
combPred <- predict(combModFit,predDF) # predict wage using the combined model
sqrt(sum((pred1-testing$wage)^2))
sqrt(sum((pred2-testing$wage)^2))
sqrt(sum((combPred-testing$wage)^2)) # you can see the RSS decrease

pred1V <- predict(mod1,validation); pred2V <- predict(mod2,validation) # prediction of model 1 & 2 on validation dataset
predVDF <- data.frame(pred1=pred1V,pred2=pred2V) # data frame contain two predictions
combPredV <- predict(combModFit,predVDF) # prediction from the combined model
sqrt(sum((pred1V-validation$wage)^2))
sqrt(sum((pred2V-validation$wage)^2))
sqrt(sum((combPredV-validation$wage)^2)) # again, you can see the error reduce if we use the combined model. Improve accuracy. 
        # scalability vs. accuracy is a very important tradoff in terms of doing business

# Forecasting
        # time series: beware of serial correlations and spurious correlations. beware extrapolation!
library(quantmod)
from.dat <- as.Date("01/01/08", format="%m/%d/%y")
to.dat <- as.Date("12/31/13", format="%m/%d/%y")
getSymbols("GOOG", src="yahoo", from = from.dat, to = to.dat)
head(GOOG) # Open high low close volume info

mGoog <- to.monthly(GOOG) # adjsut frequency
googOpen <- Op(mGoog) # subset the open
ts1 <- ts(googOpen,frequency=12) # timeseries
plot(ts1,xlab="Years+1", ylab="GOOG")
plot(decompose(ts1),xlab="Years+1") # decompose the time series into trend, seasonal, and cyclic

ts1Train <- window(ts1,start=1,end=5)
ts1Test <- window(ts1,start=5,end=(7-0.01))
ts1Train

plot(ts1Train)
lines(ma(ts1Train,order=3),col="red") # simple moving average

ets1 <- ets(ts1Train,model="MMM")
fcast <- forecast(ets1)
plot(fcast); lines(ts1Test,col="red") # exponential smoothing

accuracy(fcast,ts1Test) # the the accuracy
        # Use Forecasting: principles and practice as a very good start

# Unsupervised Prediction
        # You don't know the labels for prediction (You don't know what is y)
        # To build a predictor: Create clusters, Name clusters, and Build predictor for clusters. In a new data set, Predict clusters. (Pure data mining???)
data(iris); library(ggplot2)
inTrain <- createDataPartition(y=iris$Species,
                               p=0.7, list=FALSE)
training <- iris[inTrain,]
testing <- iris[-inTrain,]
dim(training); dim(testing) # use leave species as an example, if we ignore the species clusters. 

kMeans1 <- kmeans(subset(training,select=-c(Species)),centers=3) # create 3 different clusters, ignore the species info
training$clusters <- as.factor(kMeans1$cluster)
qplot(Petal.Width,Petal.Length,colour=clusters,data=training) # scatter plot and colored the observations based on clusters

table(kMeans1$cluster,training$Species)

modFit <- train(clusters ~.,data=subset(training,select=-c(Species)),method="rpart")
table(predict(modFit,training),training$Species) # build predictor

testClusterPred <- predict(modFit,testing) 
table(testClusterPred ,testing$Species) # apply on test data set
        # Be very awre of the over-interpretation of clusters!
