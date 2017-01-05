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