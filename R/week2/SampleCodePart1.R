########################################
## PREDICT 422
## Charity Project - Part 1 (Exploratory Data Analysis)
##
## SampleCodePart1.R
########################################

########################################
## Exercise 1
## Read Data from CSV File
########################################

# This path is specified wrt a Mac, the Windows path will start differently
inPath = file.path("/Users","JLW","Documents","Northwestern MSPA","PREDICT 422",
                   "Project - Charity Mailing","Project Data Files")

charityData = read.csv(file.path(inPath,"trainSample.csv"),na.strings=c("NA"," "))

# You can also try the following command for browsing to the csv file instead.
# charityData = read.csv(file.choose(),na.strings=c("NA"," "))

# Convert categorical variables to factors
# This is highly recommended so that R treats the variables appropriately.
# The lm() method in R can handle a factor variable without us needing to convert 
# the factor to binary dummy variable(s).
charityData$DONR = as.factor(charityData$DONR)
charityData$HOME = as.factor(charityData$HOME)
charityData$HINC = as.factor(charityData$HINC)

########################################
## Exercise 2
## Data Quality Check
########################################

dim(charityData)      # dimensions of data
names(charityData)    # variable names
str(charityData)      # one form of summary of data
summary(charityData)  # another form of summary

## Check for Missing Values
which(sapply(charityData,anyNA))

# Missing values identified in HINC, GENDER, and RFA_96
# Get counts of missing values for each variable
table(charityData$HOME,useNA="ifany")
table(charityData$HINC,useNA="ifany")
table(charityData$GENDER,useNA="ifany")

########################################
## Exercise 3
## Exploratory Data Analysis
########################################

## Part A - General EDA

# Histogram of the response variable DAMT
# (first with 0s included, second with 0s dropped)
hist(charityData$DAMT,col="blue",breaks=20,xlab="DAMT",main="")
hist(charityData$DAMT[charityData$DAMT > 0],col="blue",breaks=20,xlab="DAMT > 0",main="")

# Counts of the response variable DONR
table(charityData$DONR)
barplot(table(charityData$DONR),xlab="DONR")

## Part B - Regression Problem EDA

# Boxplot of DAMT amount by categories for GENDER
plot(charityData$GENDER,charityData$DAMT,xlab="Gender",ylab="Donation ($)")

# Plot DAMT against a quantitative predictor variable
plot(charityData$AGE,charityData$DAMT,xlab="Age",ylab="Donation ($)")
# add regression line (optional)
lm_age = lm(DAMT ~ AGE, data=charityData)
abline(lm_age,col="red")

## Part C - Classification Problem EDA

# Boxplot of AGE by DONR status
# In order for R to make this into a boxplot, DONR needs to be a factor variable
# and DONR needs to be plotted on the horizontal axis.
plot(charityData$DONR,charityData$AGE,xlab="DONR",ylab="AGE")

# Plot response against a categorical variable
# "Wrong" Way
# The following barplot is an accurate presentation of the data. I call it the 
# "wrong" way because students have a tendency to draw the wrong conclusions from
# this graph.
barplot(table(charityData$GENDER[charityData$DONR == 1]),
        xlab="GENDER",main="Barplot of GENDER for DONR = 1")
# This graph shows that there are more female donors than male donors. Therefore, 
# it seems reasonable to conclude that a female is more likely to donate. However,
# if you were to look at the non-donors, you would see that there are more female 
# non-donors than male non-donors. What this graph is showing us is that females
# outnumber males in the dataset as a whole, not that females are more likely to 
# donate.

# "Right" Way
# A mosaic plot is obtained when we plot one factor variable against another. The
# mosaic plot represents the counts as proportions to the whole. A deviation in
# overall proportion of females donating compared to males donating is meaningful
# whereas the absolute count of females donating compared to males donating was not.
plot(charityData$DONR,charityData$GENDER,xlab="DONR",ylab="GENDER",main="Mosaic Plot")
# Or
plot(charityData$GENDER,charityData$DONR,xlab="GENDER",ylab="DONR",main="Mosaic Plot")
# These graphs show that M/F doesn't show any difference in DONR status.

# Note: You may want to separate RFA Values (R = recency, F = frequency, A = amount)
# into separate R, F, and A variables. I wrote a function (separateRFA) to perform 
# these steps.
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

# Apply separateRFA to the variables RFA_96
charityData = separateRFA(charityData,"RFA_96")

# Check the results
table(charityData$RFA_96,charityData$RFA_96_R)
table(charityData$RFA_96,charityData$RFA_96_F)
table(charityData$RFA_96,charityData$RFA_96_A)

########################################
## Exercise 4
## Principal Component Analysis
########################################

# See ISLR Section 10.4 for guidance on performing PCA in R.
