######################################
#Exercise 5 5.4 ISLR
library(ISLR)
summary(Default)

#(a) Fit a logistic regression model that uses income and balance to
#predict default.
set.seed(1)
glm.fit = glm(default ~ income + balance, data = Default, family = binomial)

#(b) i. Split the sample set into a training set and a validation set.







###################################### 
#Excercise 8 ISLR 5.4 ###############


###################################### 
#Excercise 8 ISLR 6.8 ###############


###################################### 
#Excercise 9 ISLR 6.8 ###############