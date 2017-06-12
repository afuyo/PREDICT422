library(ISLR)
set.seed(1)
train=sample(392,196)
lm.fit =lm(mpg~horsepower ,data=Auto ,subset =train )
#plot(lm.fit)
attach(Auto)
mean((mpg -predict (lm.fit ,Auto))[-train ]^2)
#using quadratic horsepower
lm.fit2=lm(mpg~poly(horsepower ,2) ,data=Auto ,subset =train )
mean((mpg -predict (lm.fit2 ,Auto))[-train ]^2)
#using cubic horsepower
lm.fit3=lm(mpg~poly(horsepower ,3) ,data=Auto ,subset =train )
mean((mpg -predict (lm.fit3 ,Auto))[-train ]^2)
# the same wiht different seed

set.seed(2)
lm.fit =lm(mpg~horsepower ,data=Auto ,subset =train )
#plot(lm.fit)
attach(Auto)
mean((mpg -predict (lm.fit ,Auto))[-train ]^2)
#using quadratic horsepower
lm.fit2=lm(mpg~poly(horsepower ,2) ,data=Auto ,subset =train )
mean((mpg -predict (lm.fit2 ,Auto))[-train ]^2)
#using cubic horsepower
lm.fit3=lm(mpg~poly(horsepower ,3) ,data=Auto ,subset =train )
mean((mpg -predict (lm.fit3 ,Auto))[-train ]^2)