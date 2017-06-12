########################################
## PREDICT 422
## Charity Project - Part 4 (The Mailing List Problem)
##
## DataPreparation.R
########################################

# If you find yourself copying large chunks of code over again, it is often a good
# idea to convert that code into a function. Converting code to a function form allows
# you to use the same code multiple times without being as vulnerable to copy-paste
# errors.
#
# For example, you have a sequence of data processing steps that you applied to the
# data in Part 2 of the project. [Note: You may have applied the exact same steps in Part 3 
# of the project, but I am going to assume here that the Part 3 data processing steps differ
# from the Part 2 steps (even just a little).] For Part 4, you will need to apply the
# same sequence of data processing steps to the data in valSample.csv and 
# testSample.csv. Now suppose you copy all of the data processing code over and 
# change the dataset name from regData to valData in one case and from regrData to 
# testData in the other case. That's all fine, if a little bit repetitive. But what if
# you eventually decide to go back and work more on your models and modeling process?
#
# [Note: That is a bit of a tricky proposition from the viewpoint that your validation
# process is supposed to be independent of your modeling process. Circling back can 
# end up violating good modeling practices. However, strictly from a code development
# standpoint, a situation such as this could happen.]
#
# Let's say you make a few changes to your original data processing steps. For example,
# you apply a transformation to one predictor variable, and you create a new derived
# variable. Now, if you forget to make the same changes to the copies applied to the 
# validation and test data, then you will run into problems. For the specific changes
# I mention, you are likely to get errors, since you won't have the transformed or 
# derived variables as part of your validation and test dataframes. When it comes time
# to apply your model to those datasets, R won't know what to do. In this case, you're
# likely to detect your mistake and fix it. Suppose instead that you had transformed a
# variable by raising it to the 2nd power, and then you changed your code to raise the
# variable to the third power instead. If you don't make the same change to the 
# validation and test copies, then those datasets won't match the training data. 
# However, you won't necessarily get an error, since the transformed variables would
# exist (just of the wrong power). The moral of the story is that making copies of code
# can lead to mistakes; it is a much better coding practice to write functions that can
# handle multiple situations.

# Separate RFA Values (R = recency, F = frequency, A = amount)
# Note: The same separateRFA function can be called by the functions processPart1
# and processPart2.
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

# Function to perform Part 2 data processing steps on a data frame.
# I copied the data processing steps from Exercise 2 of Part 2 and renamed regrData
# to myData.
processPart2 = function(myData)
{
  ## Part A - Resolve Missing Values
  
  ## Check for Missing Values
  which(sapply(myData,anyNA))
  
  # HOME - Make a level 0 and code missing values as 0
  levels(myData$HOME) = c(levels(myData$HOME),"0")
  myData$HOME[is.na(myData$HOME)] = "0"
  table(myData$HOME,useNA="ifany")
  
  # HINC - Make a level 0 and code missing values as 0
  levels(myData$HINC) = c(levels(myData$HINC),"0")
  myData$HINC[is.na(myData$HINC)] = "0"
  table(myData$HINC,useNA="ifany")
  
  # GENDER - Assign A, J, and NA to category U
  idxMF = myData$GENDER %in% c("M","F")
  myData$GENDER[!idxMF] = "U"
  myData$GENDER = factor(myData$GENDER)
  table(myData$GENDER,useNA="ifany")
  
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
  
  # Apply separateRFA to the variables RFA_96 and check results.
  # Note that the output from this section is for error-checking purposes.
  # You do not need to include this output in your assignment write-up.
  myData = separateRFA(myData,"RFA_96")

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
  dropIdx = which(names(myData) %in% c("DONR","RFA_96"))
  
  # Drop the variables indicated by dropIdx.
  myData2 = myData[,-dropIdx]
  names(myData2)   # check that the result is as expected
  
  return(myData2)
}

# Function to perform Part 3 data processing steps on a data frame.
# I copied the data processing steps from Exercise 2 of Part 3 and renamed classData
# to myData.
processPart3 = function(myData)
{
  ## Part A - Resolve Missing Values
  
  ## Check for Missing Values
  which(sapply(myData,anyNA))
  
  # HOME - Make a level 0 and code missing values as 0
  levels(myData$HOME) = c(levels(myData$HOME),"0")
  myData$HOME[is.na(myData$HOME)] = "0"
  table(myData$HOME,useNA="ifany")
  
  # HINC - Make a level 0 and code missing values as 0
  levels(myData$HINC) = c(levels(myData$HINC),"0")
  myData$HINC[is.na(myData$HINC)] = "0"
  table(myData$HINC,useNA="ifany")
  
  # GENDER - Assign A, J, and NA to category U
  idxMF = myData$GENDER %in% c("M","F")
  myData$GENDER[!idxMF] = "U"
  myData$GENDER = factor(myData$GENDER)
  table(myData$GENDER)
  
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

  # Apply separateRFA to the variables RFA_96 and check results.
  # Note that the output from this section is for error-checking purposes.
  # You do not need to include this output in your assignment write-up.
  myData = separateRFA(myData,"RFA_96")

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
  dropIdx = which(names(myData) %in% c("DAMT","RFA_96"))
  
  # Drop the variables indicated by dropIdx.
  myData2 = myData[,-dropIdx]
  names(myData2)   # check that the result is as expected
  
  return(myData2)
}