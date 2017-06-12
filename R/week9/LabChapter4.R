library(ISLR)
names(Smarket)
dim(Smarket)
summary(Smarket)
pairs(Smarke)
cor(Smarket)
cor(Smarket[,-9])
str(Smarket)
attach(Smarket)
plot(Volume)
#fitt the model Lag1 through Lag5 and Volume
glm.fit=glm(Direction???Lag1+Lag2+Lag3+Lag4+Lag5+Volume , data=Smarket ,family =binomial )
summary(glm.fit)
coef(glm.fit)
summary(glm.fit)$coef
#prediction
glm.probs=predict(glm.fit,type="response")
glm.probs[1:10]
contrasts(Direction)
#confusion matrix
glm.pred=rep("Down",1250)
glm.pred[glm.probs > .5]="Up"
table(glm.pred,Direction)
(507+145) /1250
mean(glm.pred==Direction)
train=(Year<2005)
Smarket.2005 = Smarket[!train,]
dim(Smarket.2005)
Direction.2005= Direction [! train]
glm.fit=glm(Direction???Lag1+Lag2+Lag3+Lag4+Lag5+Volume ,data=Smarket ,family =binomial ,subset =train )
glm.probs =predict (glm.fit ,Smarket.2005 , type="response")
#reduced model
glm.fit=glm(Direction ~ Lag1+Lag2 ,data=Smarket ,family =binomial , subset =train)
glm.probs =predict (glm.fit ,Smarket.2005 , type="response")
glm.pred=rep ("Down" ,252)
glm.pred[glm.probs >.5]="Up"
table(glm.pred ,Direction.2005)
mean(glm.pred== Direction.2005)
106/(106+76)
predict (glm.fit ,newdata =data.frame(Lag1=c(1.2 ,1.5) , Lag2=c(1.1 , -0.8) ),type ="response")
#Linear Discriminant Analysis
library (MASS)
lda.fit=lda(Direction ~ Lag1+Lag2 ,data=Smarket ,subset =train)
lda.fit
plot(lda.fit)
lda.pred=predict (lda.fit , Smarket.2005)
lda.class =lda.pred$class
table(lda.class ,Direction.2005)
mean(lda.class == Direction.2005)
sum(lda.pred$posterior [ ,1] >=.5)
sum(lda.pred$posterior [,1]<.5)
lda.pred$posterior [1:20 ,1]
lda.class [1:20]
sum(lda.pred$posterior [,1]>.9)
