#PREDICT 422
#Programming Assignment 2
#

###################################
#Excercise 9 (ISLR Section 10.7)
library(ISLR)
set.seed(2)
#9(a)
#Using hierarchical clustering with complete linkage and
#Euclidean distance, cluster the states.
hc.complete = hclust(dist(USArrests), method="complete")
plot(hc.complete)
#9(b)
#Cut the dendrogram at a height that results in three distinct
#clusters. Which states belong to which clusters?
cut<-cutree(hc.complete, 3)
table(cutree(hc.complete, 3))
#9(c)
#Hierarchically cluster the states using complete linkage and Euclidean
#distance, after scaling the variables to have standard deviation
#one.
dsc = scale(USArrests)
hc.s.complete = hclust(dist(dsc), method="complete")
plot(hc.s.complete)
#9(d)
#What effect does scaling the variables have on the hierarchical
#clustering obtained? In your opinion, should the variables be
#scaled before the inter-observation dissimilarities are computed?
#Provide a justification for your answer.
cutree(hc.s.complete, 3)
table(cutree(hc.s.complete, 3))
table(cutree(hc.s.complete, 3), cutree(hc.complete, 3))

#10
#(a)
set.seed(2)
x = matrix(rnorm(20*3*50, mean=0, sd=0.001), ncol=50)
x[1:20, 2] = 1
x[21:40, 1] = 2
x[21:40, 2] = 2
x[41:60, 1] = 1
#b
######################33
pca.out = prcomp(x)
summary(pca.out)
pca.out$x[,1:2]
plot(pca.out$x[,1:2], col=2:4, xlab="Z1", ylab="Z2", pch=19) 
#c
################################33
km.out = kmeans(x, 3, nstart=20)
table(km.out$cluster, c(rep(1,20), rep(2,20), rep(3,20)))
#d
################3333333
km.out = kmeans(x, 2, nstart=20)
km.out$cluster
table(km.out$cluster)
#e
###################333
km.out = kmeans(x, 4, nstart=20)
km.out$cluster
table(km.out$cluster)
#f
################3333333
km.out = kmeans(pca.out$x[,1:2], 3, nstart=20)
table(km.out$cluster, c(rep(1,20), rep(2,20), rep(3,20)))
#g
#######################3333
km.out = kmeans(scale(x), 3, nstart=20)
km.out$cluster
table(km.out$cluster, c(rep(1,20), rep(2,20), rep(3,20)))

#Chapter 3
############
#9
#(a)
pairs(Auto)
#(b)
################3333
cor(subset(Auto, select=-name))

#(c)
#################333333
lm.fit1 = lm(mpg~.-name, data=Auto)
summary(lm.fit1)
#(d)
##################3
par(mfrow=c(2,2))
plot(lm.fit1)
#(e)
lm.fit2 = lm(mpg~cylinders*displacement+displacement*weight, data=Auto[,1:8])
summary(lm.fit2)
#(f)
attach(Auto)
lm.fit3 = lm(mpg~log(weight)+sqrt(horsepower)+acceleration+I(acceleration^2))
summary(lm.fit3)
par(mfrow=c(2,2))
plot(lm.fit3)
plot(predict(lm.fit3), rstudent(lm.fit3))

#second log model

lm.fit2<-lm(log(mpg)~cylinders+displacement+horsepower+weight+acceleration+year+origin,data=Auto)
summary(lm.fit2)
par(mfrow=c(2,2)) 
plot(lm.fit2)
plot(predict(lm.fit2),rstudent(lm.fit2))
#chapter 3 
#10
#(a)
################################333

summary(Carseats)
attach(Carseats)
lm.fit = lm(Sales~Price+Urban+US)
summary(lm.fit)

#(e)
lm.fit2 = lm(Sales ~ Price + US)
summary(lm.fit2)
#(g)
confint(lm.fit2)
#(h)
plot(predict(lm.fit2), rstudent(lm.fit2))
