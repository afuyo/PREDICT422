######################################
#Exercise 5 5.4 ISLR
library(ISLR)
summary(Default)

#(a) Fit a logistic regression model that uses income and balance to
#predict default.
set.seed(1)
glm.fit = glm(default ~ income + balance, data = Default, family = binomial)

fiveB = function() {
#(b) i. Split the sample set into a training set and a validation set.
#train= sample(10000,7000)
train=sample(dim(Default)[1],dim(Default)[1]*0.5)
#test=Default[)-train]

##ii. Fit a multiple logistic regression model using only the training
##observations.
glm.fit = glm(default ~ income + balance, data = Default, family = binomial, 
              subset = train)

##iii. Obtain a prediction of default status for each individual in
#the validation set by computing the posterior probability of
#default for that individual, and classifying the individual to
#the default category if the posterior probability is greater
#than 0.5.

# iii.
glm.pred = rep("No", dim(Default)[1]/2)
glm.probs = predict(glm.fit, Default[-train, ], type = "response")
glm.pred[glm.probs > 0.5] = "Yes"
##iv
##Compute the validation set error, which is the fraction of
#the observations in the validation set that are misclassified.
return (mean(glm.pred != Default[-train, ]$default))
}
fiveB()

#(c) Repeat the process in (b) three times, using three different splits
#of the observations into a training set and a validation set. Comment
#on the results obtained.
for (i in 1:3)
  print(fiveB())
#(d)

train = sample(dim(Default)[1], dim(Default)[1]/2)
glm.fit = glm(default ~ income + balance + student, data = Default, family = binomial, 
              subset = train)
glm.pred = rep("No", dim(Default)[1]/2)
glm.probs = predict(glm.fit, Default[-train, ], type = "response")
glm.pred[glm.probs > 0.5] = "Yes"
mean(glm.pred != Default[-train, ]$default)
###################################### 
#Excercise 8 ISLR 5.4 ###############
#(a)
set.seed(1)
y = rnorm(100)
x = rnorm(100)
y = x - 2 * x^2 + rnorm(100)

#(b)
plot(x, y)

#(c)

library(boot)
Data = data.frame(x, y)
set.seed(1)
# i.
glm.fit = glm(y ~ x)
cv.glm(Data, glm.fit)$delta

# ii.
glm.fit = glm(y ~ poly(x, 2))
cv.glm(Data, glm.fit)$delta

# iii.
glm.fit = glm(y ~ poly(x, 3))
cv.glm(Data, glm.fit)$delta


#############################33333
#(d)


set.seed(10)
# i.
glm.fit = glm(y ~ x)
cv.glm(Data, glm.fit)$delta

# ii.
glm.fit = glm(y ~ poly(x, 2))
cv.glm(Data, glm.fit)$delta

# iii.
glm.fit = glm(y ~ poly(x, 3))
cv.glm(Data, glm.fit)$delta


# iv.
glm.fit = glm(y ~ poly(x, 4))
cv.glm(Data, glm.fit)$delta

#(f)
summary(glm.fit)



###################################### 
#Excercise 8 ISLR 6.8 ###############

#(a)
set.seed(1)
X = rnorm(100)
eps = rnorm(100)
            
#(b)
beta0 = 3
beta1 = 2
beta2 = -3
beta3 = 0.3
Y = beta0 + beta1 * X + beta2 * X^2 + beta3 * X^3 + eps

#(c)
library(leaps)
data.full = data.frame(y = Y, x = X)
mod.full = regsubsets(y ~ poly(x, 10, raw = T), data = data.full, nvmax = 10)
mod.summary = summary(mod.full)

# Find the model size for best cp, BIC and adjr2
which.min(mod.summary$cp)           

            
            
            

###################################### 
#Excercise 9 ISLR 6.8 ###############