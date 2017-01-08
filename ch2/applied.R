# Applied Exercises

# 8
# 8.(a)
college = read.csv("College.csv")

# 8.(b)
fix(college)
rownames(college) = college[,1]
college = college[,-1]
fix(college)

# 8.(c)
# i
summary(college)
# ii
pdf("PairsCollege.pdf")
pairs(college[,1:10])
dev.off()
# iii
pdf("OutstateVSPrivate.pdf")
plot(college$Private, college$Outstate, xlab="Private", ylab="Outstate", main="Outstate VS Private")
dev.off()
# iv
Elite = rep("No", nrow(college))
Elite[college$Top10perc>50] = "Yes"
Elite = as.factor(Elite)
college = data.frame(college, Elite)
summary(college$Elite) # Yes 78, No 699
pdf("EliteVSPrivate.pdf")
plot(college$Elite, college$Outstate, xlab="Elite", ylab="Outstate", main="Elite VS Private")
dev.off()
# v
pdf("Histograms.pdf")
par(mfrow=c(2,2))
hist(college$Apps, main="Apps")
hist(college$Accept, main="Accept")
hist(college$Enroll, main="Enroll")
hist(college$Outstate, main="Outstate")
dev.off()

# 9
# 9.(a)
Auto = read.csv("Auto.csv", header=T, na.strings="?")
Auto = na.omit(Auto)
Auto$origin = as.factor(Auto$origin)
dim(Auto)
summary(Auto) 
# origin and name are qualitative
# others are quantitative

# 9.(b)
sapply(Auto[,1:7], range)
#       mpg cylinders displacement horsepower weight acceleration year
# [1,]  9.0         3           68         46   1613          8.0   70
# [2,] 46.6         8          455        230   5140         24.8   82

#9.(c)
sapply(Auto[,1:7], mean)
#       mpg    cylinders displacement   horsepower       weight acceleration 
# 23.445918     5.471939   194.411990   104.469388  2977.584184    15.541327 
#      year 
# 75.979592 
sapply(Auto[,1:7], sd)
#         mpg    cylinders displacement   horsepower       weight acceleration 
#    7.805007     1.705783   104.644004    38.491160   849.402560     2.758864 
#        year 
#    3.683737 

# 9.(d)
newAuto = Auto[-c(10:85),]
sapply(newAuto[,1:7], range)
#       mpg cylinders displacement horsepower weight acceleration year
# [1,] 11.0         3           68         46   1649          8.5   70
# [2,] 46.6         8          455        230   4997         24.8   82
sapply(newAuto[,1:7], mean)
#          mpg    cylinders displacement   horsepower       weight acceleration 
#    24.404430     5.373418   187.240506   100.721519  2935.971519    15.726899 
#         year 
#    77.145570 
sapply(newAuto[,1:7], sd)
#          mpg    cylinders displacement   horsepower       weight acceleration 
#     7.867283     1.654179    99.678367    35.708853   811.300208     2.693721 
#         year 
#     3.106217 

#9.(e)
pdf("PairsAuto.pdf")
pairs(Auto)
dev.off()

#9.(f)
pdf("mpgVScylinders.pdf")
plot(cylinders, mpg, xlab="cylinders", ylab="mpg", main="mpg VS cylinders")
dev.off()
# less cylinders, more mpg

# 10
# 10.(a)
library(MASS)
Boston
?Boston
dim(Boston) # 506 rows and 14 columns
fix(Boston)

# 10.(b)
Boston$chas = as.factor(Boston$chas)
pdf("PriceVSCharlesRiver.pdf")
plot(Boston$chas, Boston$medv, xlab="chas", ylab="medv", main="Price VS Charles River")
dev.off()
# pricer is higher is the house bounds river
pdf("PriceVSDistance.pdf")
plot(Boston$dis, Boston$medv, xlab="dis", ylab="medv", main="Price VS Distance")
dev.off()
# the farther the distance, the higher the price

# 10.(c)
pdf("CrimeVSDistance.pdf")
plot(Boston$dis, Boston$crim, xlab="dis", ylab="crime", main="Crime VS Distance")
dev.off()
# the closer it is to work area, the higher the per capita crime rate

# 10.(d)
summary(Boston)
range(Boston$crim)
range(Boston$tax)
range(Boston$ptratio)

# 10.(e)
summary(Boston$chas) # 35 suburbs bound the Charles River

# 10.(f)
median(Boston$ptratio) # the median of pupil-teacher ratio is 19.05

# 10.(g)
min(Boston$medv) 
# lowest median value of owner-occupied homes is 5
nrow(Boston[Boston$medv==min(Boston$medv) ,])
# two suburbs have the same lowest median value of 5
Boston[Boston$medv==5,]
#        crim zn indus chas   nox    rm age    dis rad tax ptratio  black lstat medv
# 399 38.3518  0  18.1    1 0.693 5.453 100 1.4896  24 666    20.2 396.90 30.59    5
# 406 67.9208  0  18.1    1 0.693 5.683 100 1.4254  24 666    20.2 384.97 22.98    5

# 10.(h)
nrow(subset(Boston, Boston$rm>7)) # 64 suburbs
nrow(subset(Boston, Boston$rm>8)) # 13 suburbs
Boston[Boston$rm>8,]
summary(Boston[Boston$rm>8,])