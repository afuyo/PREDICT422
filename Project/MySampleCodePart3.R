########################################
## PREDICT 422
## Charity Project - Part 3 (The Classification Problem)
##
## SampleCodePart3.R
########################################

# Load packages required for this code.
library(pROC)
library(lift)
library(MASS)
library(rpart)
library(caret)

########################################
## Exercise 1
## Read Data from CSV File
########################################

# This path is specified wrt a Mac, the Windows path will start differently
inPath = file.path("/Users","JLW","Documents","Northwestern MSPA","PREDICT 422",
                   "Project - Charity Mailing","Project Data Files")

charityData = read.csv(file.path(inPath,"trainSample.csv"),na.strings=c("NA"," "))

charityData = read.csv(file.choose(),na.strings=c("NA"," "))
# Convert categorical variables to factors
# This is highly recommended so that R treats the variables appropriately.
# The lm() method in R can handle a factor variable without us needing to convert 
# the factor to binary dummy variable(s).
charityData$DONR = as.factor(charityData$DONR)
charityData$HOME = as.factor(charityData$HOME)
charityData$HINC = as.factor(charityData$HINC)

# For this assignment, we will use the full dataset.
# Rename the dataset to classData for clarity.
classData = charityData

# Remove charityData from R session environment.
# This reduces memory used (minor point) and helps to ensure that downstream
# code doesn't reference charityData by mistake (will cause an error and 
# should catch your attention).
rm(charityData)

########################################
## Exercise 2
## Data Preparation
########################################

## Part A - Resolve Missing Values

## Check for Missing Values
which(sapply(classData,anyNA))

# HOME - Make a level 0 and code missing values as 0
levels(classData$HOME) = c(levels(classData$HOME),"0")
classData$HOME[is.na(classData$HOME)] = "0"
table(classData$HOME,useNA="ifany")

# HINC - Make a level 0 and code missing values as 0
levels(classData$HINC) = c(levels(classData$HINC),"0")
classData$HINC[is.na(classData$HINC)] = "0"
table(classData$HINC,useNA="ifany")

# GENDER - Assign A, J, and NA to category U
idxMF = classData$GENDER %in% c("M","F")
classData$GENDER[!idxMF] = "U"
classData$GENDER = factor(classData$GENDER)
table(classData$GENDER)

## Part B - Derived or Transformed Variables

# Add your own code here (optional).
#
# Note: Applying a transform to the response variable DAMT is an all-or-none 
# proposition. Transforming the response changes the scale of the y (response) and 
# yhat (predicted) values. Therefore, you cannot compare the MSEs of a model fit to 
# DAMT and a model fit to f(DAMT) where "f" is some transformation function such as
# log or sqrt. If you make the mistake of comparing MSEs in such a way, one MSE value
# may be much smaller than the other. Yet, that will not be a sign that one model
# fits much better than the other; it will be an indication of the y and yhat values
# being on a different scale due to the transformation.
#
# The solution is to either use NO transformation of the response or to apply the 
# same transformation to the response for EVERY model that you fit.

## Part C - Re-categorize Variables

# Separate RFA Values (R = recency, F = frequency, A = amount)
# Note: I wrote a function (separateRFA) to perform these steps.
separateRFA = function(xData,varName)
{
  bytes = c("R","F","A")
  newVarNames = paste(varName,bytes, sep="_")
  
  for (ii in 1:length(bytes)) # Loop over 1 to 3 (corresponding to R, F, and A)
  {
    # Find the unique values for current byte
    byteVals = unique(substr(levels(xData[,varName]),ii,ii))
    
    for (jj in 1:length(byteVals)) # Loop over unique byte values
    {
      rowIdx = substr(xData[,varName],ii,ii) == byteVals[jj]
      xData[rowIdx,newVarNames[ii]] = byteVals[jj]
    }
    
    xData[,newVarNames[ii]] = factor(xData[,newVarNames[ii]])
  }
  
  return(xData)
}

# Apply separateRFA to the variables RFA_96 and check results.
# Note that the output from this section is for error-checking purposes.
# You do not need to include this output in your assignment write-up.
classData = separateRFA(classData,"RFA_96")
table(classData$RFA_96,classData$RFA_96_R)
table(classData$RFA_96,classData$RFA_96_F)
table(classData$RFA_96,classData$RFA_96_A)

## Part D - Drop Variables

# This part is optional. However, there are several reasons one might want to drop
# variables from the dataset. A few reasons are listed here.
#
# - In EDA, you may find that some variables have no (or negligible) predictive value.
# Some variables that you have access to may prove to be irrelevant to the modeling
# problem at hand. You are permitted to eliminate these from consideration in your
# models. One way to do this is to drop them from the dataset.
# 
# - Transformed variables should replace the original variables. Typically, you 
# would not use both a variable and its transformed version.
#
# - Derived variables might need to replace base variables. For example, if you 
# compute a ratio between two variables, then you may run into problems including
# both the original variables and the ration in your model (due to multi-collinearity
# concerns).
#
# - In the case of RFA variables that we have broken down into separate R, F, and A
# variables, you should not include both the combined and the separated variables in
# your models. Make your choice between using the RFA variable and the separated
# variables and drop the unused one(s) from the dataset. My recommendation is to
# use the separated variables since there will be fewer dummy variables generated,
# and it might be the case that some of R, F, and A have less predictive value (and
# can be left out of your models).
#
# - Factor variables can cause problems with some of the R methods. Specifically,
# let's suppose that GENDER does not have much predictive ability and you do not plan
# to include GENDER in your models. You can write the model formula in such a way
# that GENDER is excluded. However, if your test set happens to be a sample that does
# not contain any observations in a particular category (GENDER = U, perhaps), then 
# you will run into trouble with R making predictions on the test set, despite the
# fact that GENDER is not included in your model. In my opinion, this is a weakness 
# in the way some methods are implemented in R. However, if you run into this problem,
# then the most direct solution is to remove the problem variable from your dataset.

# Index of variables to drop from dataset. You can identify the column numbers
# manually, or you can search by variable name as shown below.
# - Remove DONR since it only has one level in the regression problem. DONR is not
# meant to be used for the regression problem anyway.
# - Remove RFA_96 and RFA_97 in favor or keeping the separate R, F, and A variables.
# - Remove RFA_97_R since there is only one level expressed. No information is added
# and it may cause problems with the code.
dropIdx = which(names(classData) %in% c("DAMT","RFA_96"))

# Drop the variables indicated by dropIdx.
classData2 = classData[,-dropIdx]
names(classData2)   # check that the result is as expected

########################################
## Exercise 3
## Dataset Partitioning
########################################

# Specify the fraction of data to use in the hold-out test.
testFraction = 0.25   
set.seed(123)

# Sample training subset indices.
# - the index vector has length equal to the size of the sampled set
# - the index values are integer, representing the row numbers to use for the sample
trainIdx = sample(nrow(classData2),size=(1-testFraction)*nrow(classData2),
                  replace=FALSE)

########################################
## Exercise 4
## Model Fitting
########################################

# Note: In the following sub-sections I give one example of each kind of model. 
# The examples are meant to illustrate the necessary coding for each model type. 
# I intentionally build models that are intended to be adequate and may be based on 
# somewhat arbitrary choices. Hence, there are plenty of better models left for you 
# to build on your own.

## Part A - Simple Logistic Regression
#modelA1 = glm(DONR ~ MAXRAMNT,data=classData2,subset=trainIdx,family=binomial)
modelA1 = glm(DONR ~ RFA_96_F,data=classData2,subset=trainIdx,family=binomial)
summary(modelA1)
par(mfrow=c(2,2))
plot(modelA1)
par(mfrow=c(1,1))

# Note that there are two primary ways to use a classification model.
# 1) As a classifier. That is, classify each observation as a 1 or 0 (for
# binary classification problems). For a model such as logistic regression, 
# one will build a ROC curve and select a threshold for splitting scores into
# classes 1 and 0.
# 2) As a ranking model. That is, generate a score such that observations
# with higher scores are more likely to be Class 1 and observations with lower
# scores are more likely to be Class 0. For a model such as logistic regression,
# the value produced by the regression equation (after estimating coefficients
# beta) are the scores.
#
# These two applications use different means for measuring success.
# 1) For a given threshold (chosen from a ROC curve), a classifier is obtained.
# The confusion matrix, true positive rate, false positive rate, and overall 
# accuracy of the classifier can be used to evaluate performance.
# 2) For a ranking model, a lift chart or something similar is used to measure
# "lift". What we want to see is that actual 1s are lifted higher in the
# ranking produced by the model.
#
# For the charity problem, we are more directly interested in application 2
# (ranking individuals). In Part 4 of the project, ranking individuals will be
# the sole focus of your classification model. However, in Part 3 you will also 
# evaluate your models as classifiers (application 1), more for the exercise 
# of working with classifiers than for the direct applicability to the 
# mailing list problem.
# 
# Throughout the rest of the sample code provided here, you will see me 
# addressing both aspects of the classification problem (classification and
# ranking). I will make the code as applicable to these two tasks.

# Classification: Fitting the logistic model is only the first part of the 
# classification task. The logistic model provides a score for the likelihood
# that an individual will donate. We must select a cutoff or threshold value 
# to use for translating the score to a 0/1 classification result.
#
# In general, a default threshold of 0.5 can be used. However, there are two 
# problems with this strategy.
#  1) A default value of 0.5 assumes that the 0-class and 1-class are 
#  represented equally in the training dataset. If that assumption fails, 
#  then 0.5 will not work well as a cutoff.
#  2) The default value of 0.5 is not necessarily optimal. We should generate
#  a ROC curve in order to assess the potential for a more optimal threshold 
#  and to evaluate the cost-benefit of a particular threshold in terms of 
#  FP-TP rates.
#
# As you may have observed in EDA, the DONR=1 class represents approximately 
# 5% of the dataset. This explains why the logistic regression scores are on 
# the scale of 0.05. As such, using the default cutoff of 0.5 would result in
# all individuals being classified as non-donors. While this would be a very
# unhelpful classifcation model, it would have 95% accuracy since the model 
# would be incorrect only 5% of the time.
trnProbsA1 = predict(modelA1,type="response")
hist(trnProbsA1,col="gray")   # Note that scores are distributed around 0.05.
hist(trnProbsA1,col="gray",xlim=c(0,1))   # Rescale to make obvious.

# Classification: ROC Curve for Model A1 - Use methods from pROC package.
rocA1 = roc(response=classData2$DONR[trainIdx],predictor=trnProbsA1)
par(pty="s")  # sets plotting region to a square, useful for ROC curves
# Use par(pty="m") to return to default of rectangular plotting region.
plot(rocA1,col="blue",
     main=paste("ROC curve for Model A1\nAUC = ",round(rocA1$auc,digits=3),sep=""))
par(pty="m")

# Classification: Determine "optimal" threshold.
# Note: There is no single rule to define an "optimal" threshold. We must 
# apply our own judgment within the context of the classification problem.
# 
# One rule of thumb for determining an optimal point on the ROC curve is to 
# select the point closest to the upper-left corner (coordinates (0,1)). This 
# rule gives equal weight to TP and FP. We know that is not appropriate in 
# some cases.
#
# Note: Since the ROC curve is plotted with Specificity on the horizontal axis
# (instead of FP, where FP = 1-Specificity) and the horizontal axis goes
# from 1 down to 0, I will be using the coordinates (1,1) in the distance 
# formula.
dist01 = sqrt((rocA1$specificities-1)^2 + (rocA1$sensitivities-1)^2)
optIdxA1 = which.min(dist01)  # index corresponding to minimum distance
threshA1 = rocA1$thresholds[optIdxA1]  # threshold corresponding to min. distance
points(rocA1$specificities[optIdxA1],rocA1$sensitivities[optIdxA1],col="red",pch=7)

# A few notes on the ROC curve.
#  1) Given the ROC curve we get for the charity classification problem, there don't
#  appear to be a lot of great choices for the threshold. Recall that we are 
#  not going to get a particularly good classification model on this problem 
#  (though it won't matter so much when we get to Part 3 of the project).
#  2) A better ROC curve would go higher into the top-left corner. Therefore, 
#  you could find a threshold that provides a much higher TP for much lower 
#  FP. We don't see that scenario with this project.
#  3) Depending on the context of the classification problem, you may only be 
#  able to tolerate a certain amount of FP or you may be able to tolerate 
#  quite a bit of FP.
#  4) For the mailing problem, FPs are cheap (we mail to a non-donor at a cost of
#  $0.68) and FNs (or missed TPs) are comparably more expensive (we lose out 
#  on a donation that has an average value of $15.62). If you are interested 
#  in incorporating these relative weights into the classification model, 
#  then you could replace the dist01 calculation above with the following:
#     sqrt((0.68*(rocA1$specificities-1))^2 + (15.62*(rocA1$sensitivities-1)^2))
#  Note that if you do so, you will end up with a tremendous number of FPs but you
#  should end up capturing most of the TPs.

# Ranking: Generate lift chart on training subset and measure top-decile lift.
plotLift(trnProbsA1,classData2$DONR[trainIdx])
TopDecileLift(trnProbsA1,classData2$DONR[trainIdx])

## Part B - Linear Discriminant Analysis

# IMPORTANT NOTE: The model notation "DONR ~ .-ID" (namely using all variables minus
# a select few) is valid and should work. However, I have encountered some cases 
# where an error was caused using this notation. I have not figured out why the error
# shows up, but I have found that changing the model specification to the form
# "DONR ~ AGE + HOME + HINC" and so on (adding all variables that you want using plus
# signs) makes the error go away. If you encounter an error in building your model,
# try making this change to see if the error goes away. If the error remains, then
# you have something else wrong with your model and should keep troubleshooting
# (including posting a question to the Q & A).

# LDA on a Subset of Variables
# Note that my selection of variables for this model is arbitrary. I can
# get away with this since my model is to be used for illustration purposes
# only. You should provide some justification for the variables that you
# select in building your models.

classData_D=subset(classData2[trainIdx,],select=c(DONR,AGE,MEDAGE,MEDPPH,MEDHVAL,MEDINC,MEDEDUC,NUMPROM,NUMPRM12,RAMNTALL,NGIFTALL,MAXRAMNT,LASTGIFT,TDON,RFA_96_R,RFA_96_F,RFA_96_A))
c_1 <- trainControl(method = "none")

maxvar     <-(4) 
direction <-"forward"
tune_1     <-data.frame(maxvar,direction)
tr <- train(DONR~., data=classData_D, method = "stepLDA", trControl=c_1, tuneGrid=tune_1)

modelB1 = lda(DONR~ AGE+MEDAGE+MEDHVAL+MEDINC+MEDEDUC+NUMPROM+MAXRAMNT +MEDINC+MEDEDUC+ NUMPROM+NUMPRM12+RAMNTALL+NGIFTALL+MAXRAMNT+TDON+LASTGIFT+RFA_96_R+RFA_96_F+RFA_96_A, data=classData2,
              subset=trainIdx)
modelB1 = lda(DONR~ AGE, data=classData2,
              subset=trainIdx)
#tr2 <- train(DONR~., data=classData_D, method = 'stepLDA', trControl=trainControl(method = 'cv'))

#classData_C=subset(classData2[trainIdx,],select=c(DONR))
sc.obj=stepclass(classData_D,classData_C,"lda",start.vars="AGE")

var_selection=stepclass(DONR~ AGE+MEDAGE+MEDHVAL+MEDINC+MEDEDUC+NUMPROM+MAXRAMNT +MEDINC+MEDEDUC+ NUMPROM+NUMPRM12+RAMNTALL+NGIFTALL+MAXRAMNT+TDON+LASTGIFT+RFA_96_R+RFA_96_F+RFA_96_A,data=classData2,
                        subset=trainIdx,method="lda",start.vars="AGE")


#forwards=step(nothing,scope=list(lower=formula(nothing),upper=formula(modelB1)),direction="forward")
modelB1
predB1 = predict(modelB1,classData2[trainIdx,])
trnProbsB1 = predB1$posterior[,2]   # column 2 corresponds to Pr(DONR = 1)

# Classification: While predB1 contains a field called 'class' that contains
# classification results, those classification results are for a default 
# threshold of 0.5. As such, all individuals are classified as non-donors
# since none of the LDA probabilities exceed 0.5.
#
# Similar to modelA1, we explore the probabilities and build a ROC curve 
# for modelB1.
hist(trnProbsB1,col="gray")   # Note that scores are distributed around 0.05.
hist(trnProbsB1,col="gray",xlim=c(0,1))   # Rescale to make obvious.

# Classification: ROC Curve for Model B1 - Use methods from pROC package.
rocB1 = roc(response=classData2$DONR[trainIdx],predictor=trnProbsB1)
par(pty="s")  # sets plotting region to a square, useful for ROC curves
# Use par(pty="m") to return to default of rectangular plotting region.
plot(rocB1,col="blue",
     main=paste("ROC curve for Model B1\nAUC = ",round(rocB1$auc,digits=3),sep=""))
par(pty="m")

# Classification: Determine "optimal" threshold.
#dist01 = sqrt((rocB1$specificities-1)^2 + (rocB1$sensitivities-1)^2)
dist01=  sqrt((0.68*(rocA1$specificities-1))^2 + (15.62*(rocA1$sensitivities-1)^2))
optIdxB1 = which.min(dist01)  # index corresponding to minimum distance
threshB1 = rocB1$thresholds[optIdxB1]  # threshold corresponding to min. distance
points(rocB1$specificities[optIdxB1],rocB1$sensitivities[optIdxB1],col="red",pch=7)

# As before, if you are interested in incorporating the relative weights 
# into the classification model, then you could replace the dist01 
# calculation above with the following:
#     sqrt((0.68*(rocA1$specificities-1))^2 + (15.62*(rocA1$sensitivities-1)^2))
# Note that if you do so, you will end up with a tremendous number of FPs but you
# should end up capturing most of the TPs.

# Ranking: Generate lift chart on training subset and measure top-decile lift.
plotLift(trnProbsB1,classData2$DONR[trainIdx])
TopDecileLift(trnProbsB1,classData2$DONR[trainIdx])

## Part C - Tree-Based Models
# A few notes about fitting a classification tree to the charity data.
#  1) tree(DONR ~ .-ID,data=classData2,subset=trainIdx) gives you a single node tree
#  This is not desirable for a classification model. Basically, every individual is
#  labelled a non-donor (as with logistic regression and a cutoff of 0.5).
#  2) One thing we can do to result in the tree method building more
#  than a single node tree, is set split="gini". See pp. 311-12 of ISLR for 
#  discussion of the Gini index.
#  3) When we set split="gini", we next get an error that maximum tree depth has
#  been reached. We can ameliorate this error by selecting fewer variables in the
#  model formula (I started with DONR ~ .-ID). I believe this error results from
#  limitations in the way the tree method is implemented.
#  4) Having said all that, with the formula 
#     DONR ~  NGIFTALL + MAXRAMNT + LASTGIFT + TDON
#  I got a tree with 2185 nodes. When I tried to run cv.tree to look at pruning 
#  the tree, I got an error about a singlenode tree that I was unable to resolve.
#  5) The tree package is not the most widely used package for building trees; rpart
#  is much more common. Therefore, I am switching to using the rpart package.
#  6) The rpart method requires some parameter settings in order for it to build 
#  a tree of more than a single node. The parameters are: 
#     method="class" (should be the default setting when DONR is a factor variable, 
#       but at this point it doesn't hurt to be explicit)
#     split="gini" (same as with the tree method)
#     loss=matrix(0,15.62,0.68,0) (sets cost parameters: $0.68 for FP and $15.62 
#       for FN)
fullTree = rpart(DONR ~  NGIFTALL + MAXRAMNT + LASTGIFT + TDON,
                data=classData2,subset=trainIdx,method="class",
                parms=list(split="gini",loss=matrix(c(0,15.62,0.68,0),nrow=2,ncol=2)))
#fullTree = rpart(DONR ~ AGE + MEDAGE + MEDHVAL + MEDINC + NUMPROM + NUMPRM12 + RAMNTALL + TDON + RFA_96_F + RFA_96_A,
 #                data=classData2,subset=trainIdx,method="class",
  #               parms=list(split="gini",loss=matrix(c(0,15.62,0.68,0),nrow=2,ncol=2)))
summary(fullTree)
plot(fullTree)
text(fullTree)

# Prune the tree
printcp(fullTree)
cpBest = fullTree$cptable[which.min(fullTree$cptable[,"xerror"]),"CP"]
modelC1 = prune(fullTree,cp=cpBest) # In this case, the optimal tree is the unpruned tree
summary(modelC1)
plot(modelC1)
text(modelC1,pretty=0)

# Classification: We do not need a ROC curve for a classification tree. The 
# tree provides the classification results directly.

# Ranking: Generate lift chart on training subset and measure top-decile lift.
trnProbsC1 = predict(modelC1,newdata=classData2[trainIdx,],type="prob")[,2]
plotLift(trnProbsB1,classData2$DONR[trainIdx])
TopDecileLift(trnProbsB1,classData2$DONR[trainIdx])

## Part D - Model of your choice.
classData_sub=subset(classData,select=c(DONR,AGE,MEDAGE,MEDPPH,MEDHVAL,MEDINC,MEDEDUC,NUMPROM,NUMPRM12,RAMNTALL,NGIFTALL,MAXRAMNT,LASTGIFT,TDON))
summary(classData_sub)
str(classData_sub)
cor(classData_sub)
glm.fit=glm(DONR~ AGE+MEDAGE+MEDHVAL+MEDINC+MEDEDUC+NUMPROM+MAXRAMNT +MEDINC+MEDEDUC+ NUMPROM+NUMPRM12+RAMNTALL+NGIFTALL+MAXRAMNT+TDON+LASTGIFT+RFA_96_R+RFA_96_F+RFA_96_A, data=classData2,subset=trainIdx,family=binomial )
backwards=step(glm.fit)
formula(backwards)
summary(backwards)
# I would be inclined to drop RFA_96_A
modelD1=glm(DONR ~ AGE + MEDAGE + MEDHVAL + MEDINC + NUMPROM + NUMPRM12 + RAMNTALL + TDON + RFA_96_F + RFA_96_A,data=classData2,subset=trainIdx,family=binomial )
#modelD1=glm(DONR ~ AGE + MEDAGE + MEDHVAL + MEDINC + NUMPROM + NUMPRM12 + RAMNTALL + TDON + RFA_96_F ,data=classData2,subset=trainIdx,family=binomial )
modelD1.probs=predict(modelD1,type="response")
head(modelD1.probs)
summary(modelD1)
# I would be inclined to drop RFA_96_A


trnProbsD1 = predict(modelD1,type="response")
hist(trnProbsD1,col="gray")   # Note that scores are distributed around 0.05.
hist(trnProbsD1,col="gray",xlim=c(0,1))   # Rescale to make obvious.

# Classification: ROC Curve for Model D1 - Use methods from pROC package.
rocD1 = roc(response=classData2$DONR[trainIdx],predictor=trnProbsD1)
par(pty="s")  # sets plotting region to a square, useful for ROC curves
# Use par(pty="m") to return to default of rectangular plotting region.
plot(rocD1,col="blue",
     main=paste("ROC curve for Model D1\nAUC = ",round(rocD1$auc,digits=3),sep=""))
par(pty="m")


dist01 = sqrt((rocD1$specificities-1)^2 + (rocD1$sensitivities-1)^2)
optIdxD1 = which.min(dist01)  # index corresponding to minimum distance
threshD1 = rocD1$thresholds[optIdxD1]  # threshold corresponding to min. distance
points(rocD1$specificities[optIdxD1],rocD1$sensitivities[optIdxD1],col="red",pch=7)

# Build a model of your choice here.

########################################
## Exercise 5
## Model Validation
########################################

# For each model, I will generate predictions for all data (Train and Test). 
# I will then calculate the Train and Test Classification Accuracy, 
# Confusion Matrices, TP and FP Rates, and Top Decile Lift by subsetting the
# predictions accordingly. The following functions will perform those tasks 
# for me.
assignClass = function(probVals,threshVal)
{
  predVals = rep(0,length(probVals))
  predVals[probVals > threshVal] = 1
  predVals = factor(predVals)
  
  return(predVals)
}

calcMetrics = function(targetVals,predVals)
{
  confMat = table(targetVals,predVals,dnn=c("Target","Predicted"))
  
  classResults = list(
    confMat = confMat,
    TPrate = round(confMat[2,2] / sum(confMat[2,]),digits=4),
    FPrate = round(confMat[1,2] / sum(confMat[1,]),digits=4),
    accuracy = round(mean(targetVals == predVals),digits=2),
    topDecileLift = TopDecileLift(predVals,targetVals)
  )
  
  return(classResults)
}

calcResults = function(model,modelLabel,dataSet,trainIdx,threshVal=NULL)
{
  if (!is.null(threshVal) & "glm" %in% class(model)) {
    # Predict for glm models
    probVals = predict(model,dataSet,type="response")
    predVals = assignClass(probVals,threshVal)
  } else if (length(intersect(class(model),c("tree","rpart","randomForest")) > 0)) {
    # Predict for tree, rpart, randomForest models
    predVals = predict(model,dataSet,type="class")
  } else if (length(intersect(class(model),c("lda")) > 0)) {
    # Predict for lda models
    predVals = predict(model,dataSet)$class
  } else if (length(intersect(class(model),c("svm")) > 0)) {
    # Predict for svm models
    predVals = predict(model,dataSet)
  }
    
  results = list(
    name = modelLabel,
    train = calcMetrics(classData2$DONR[trainIdx],predVals[trainIdx]),
    test = calcMetrics(classData2$DONR[-trainIdx],predVals[-trainIdx])
  )
  
  return(results)
}

nModels = 4 # Number of models you fit. I fit 3 models in this sample code.
naTmp = rep(NA,nModels) # Code short-hand.
nanTmp = rep(NaN,nModels)
modelMetrics = data.frame(
  Model = naTmp,
  Train.Accuracy = nanTmp, Train.TP = nanTmp, Train.FP = nanTmp, Train.Lift = nanTmp,
  Test.Accuracy = nanTmp, Test.TP = nanTmp, Test.FP = nanTmp, Test.Lift = nanTmp
  )

resultsA1 = calcResults(modelA1,"A1",classData2,trainIdx,threshA1)
print(resultsA1$test$confMat)
modelMetrics[1,] = c(resultsA1$name,
                     resultsA1$train$accuracy,resultsA1$train$TPrate,resultsA1$train$FPrate,resultsA1$train$topDecileLift,
                     resultsA1$test$accuracy,resultsA1$test$TPrate,resultsA1$test$FPrate,resultsA1$test$topDecileLift)

resultsB1 = calcResults(modelB1,"B1",classData2,trainIdx,threshB1)
print(resultsB1$test$confMat)
modelMetrics[2,] = c(resultsB1$name,
                     resultsB1$train$accuracy,resultsB1$train$TPrate,resultsB1$train$FPrate,resultsB1$train$topDecileLift,
                     resultsB1$test$accuracy,resultsB1$test$TPrate,resultsB1$test$FPrate,resultsB1$test$topDecileLift)

resultsC1 = calcResults(modelC1,"C1",classData2,trainIdx)
print(resultsC1$test$confMat)
modelMetrics[3,] = c(resultsC1$name,
                     resultsC1$train$accuracy,resultsC1$train$TPrate,resultsC1$train$FPrate,resultsC1$train$topDecileLift,
                     resultsC1$test$accuracy,resultsC1$test$TPrate,resultsC1$test$FPrate,resultsC1$test$topDecileLift)

resultsD1 = calcResults(modelD1,"D1",classData2,trainIdx,threshD1)
print(resultsD1$test$confMat)
modelMetrics[4,] = c(resultsD1$name,
                     resultsD1$train$accuracy,resultsD1$train$TPrate,resultsD1$train$FPrate,resultsD1$train$topDecileLift,
                     resultsD1$test$accuracy,resultsD1$test$TPrate,resultsD1$test$FPrate,resultsD1$test$topDecileLift)
print(modelMetrics)

