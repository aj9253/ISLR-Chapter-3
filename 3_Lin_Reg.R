# Chaptrer 3 ISLR

library("MASS")

library("ISLR")
library(ggplot2)

names(Boston)
fix(Boston)
attach(Boston)

lm.fit=lm(medv~lstat,data=Boston)
plot(medv,lstat,pch=20)
abline(lm.fit,lwd=2,col="red")
plot(1:20,1:20,pch=1:20)
print(qplot(medv, lstat)+ylim(0,15)+abline(lm.fit))
plot(lm(medv~lstat,data=Boston))


par(mfrow=c(2,2))
plot(lm.fit)
plot(predict(lm.fit),residuals(lm.fit))
?par
dev.off()


# Chapter 3 - Question 8

summary(Auto)
fix(Auto)

lm.fit=lm(mpg~horsepower,data=Auto)
summary(lm.fit)
attach(Auto)
plot(horsepower,mpg)
abline(lm.fit)
plot(lm.fit)
predict(lm.fit,data.frame(horsepower=98),interval="confidence")

# Chapter 3 - Question 9
 pairs(Auto,cex = .5, col = "dark red")


pairs(Auto, labels = colnames(Auto), main = "Auto Pairs matrix", pch = 21, 
      bg = c("red", "green3", "blue", "yellow", "green3", "red", "green3", "black")[unclass(Auto$cylinders)], upper.panel = NULL)

Auto2 = Auto[,(1:8)]
head(Auto2)
cor(Auto2)
pairs(Auto2, labels = colnames(Auto2), main = "Correlation matrix", pch = 21, 
      bg = c("red", "green3", "blue", "yellow", "green3", "red", "green3", "black")[unclass(Auto$cylinders)], lower.panel = NULL)
lm.fit=lm(mpg~.-name,data=Auto)
summary(lm.fit)


plot(lm.fit)

dev.off()
lev = hat(model.matrix(lm.fit))
plot(lev)
Auto[lev>0.1,]
