# PREDICT 422
# Programming Assignment 1
# 

########################################
# Exercise 8 (ISLR Section 2.4)

# 8 (a)
inPath = file.path("/Users","JLW","Documents","Northwestern MSPA","PREDICT 422",
                   "Programming Assignments","Assignment 1")
college = read.csv(file.path(inPath,"College.csv"))

# 8 (b)
fix(college)
rownames(college) = college[,1]
fix(college)
college = college[,-1]
fix(college)

# 8 (c)

# 8 (c) i.
summary(college)

# 8 (c) ii.
pairs(college[,1:10])

# 8 (c) iii.
plot(college$Private,college$Outstate,xlab="Private",ylab="Outstate")

# 8 (c) iv.
Elite = rep("No",nrow(college))
Elite[college$Top10perc > 50] = "Yes"
Elite = as.factor(Elite)
college = data.frame(college,Elite)
summary(college$Elite)
plot(college$Elite,college$Outstate,xlab="Elite",ylab="Outstate")

# 8 (c) v.
par(mfrow=c(2,2))
hist(college$Apps,breaks=50,col="gray",xlab="Apps",main="")
hist(college$Accept,breaks=20,col="gray",xlab="Accept",main="")
hist(college$Enroll,breaks=15,col="gray",xlab="Enroll",main="")
hist(college$Top10perc,breaks=15,col="gray",xlab="Top10perc",main="")

# 8 (c) vi.
# additional EDA

########################################
# Exercise 9 (ISLR Section 2.4)

# Load data and remove missing values per the lab (Section 2.3.4)
Auto = read.table(file.path(inPath,"Auto.data"),header=TRUE,na.strings="?")
dim(Auto)
Auto = na.omit(Auto)
dim(Auto)

# 9 (a)
str(Auto)
summary(Auto)

# 9 (b)
for (ii in 1:7)
{
  rng = range(Auto[,ii])
  print(paste(names(Auto)[ii],": ",rng[1]," to ",rng[2],sep=""))
}

# 9 (c)
apply(Auto[,1:7],2,mean)
apply(Auto[,1:7],2,sd)

# 9 (d)
AutoSubset = Auto[-(10:85),]
sapply(AutoSubset[,1:7],range)
sapply(AutoSubset[,1:7],mean)
sapply(AutoSubset[,1:7],sd)

# 9 (e) 
pairs(Auto[,1:8])
Auto$cylinders = as.factor(Auto$cylinders)
Auto$origin = as.factor(Auto$origin)
pairs(Auto[,c(1,3:7)])
par(mfrow=c(1,2))
barplot(table(Auto$cylinders),xlab="cylinders")
barplot(table(Auto$origin),xlab="origin")

# 9 (f)
par(mfrow=c(2,3))
plot(Auto$displacement,Auto$mpg,xlab="displacement",ylab="mpg")
plot(Auto$horsepower,Auto$mpg,xlab="horsepower",ylab="mpg")
plot(Auto$weight,Auto$mpg,xlab="weight",ylab="mpg")
plot(Auto$acceleration,Auto$mpg,xlab="acceleration",ylab="mpg")
plot(Auto$year,Auto$mpg,xlab="year",ylab="mpg")

par(mfrow=c(1,2))
plot(Auto$cylinders,Auto$mpg,xlab="cylinders",ylab="mpg")
plot(Auto$origin,Auto$mpg,xlab="origin",ylab="mpg")
