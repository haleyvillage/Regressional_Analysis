library("plyr")

africa <- read.csv("AFRICA.csv", header=TRUE)

summary(africa)

beta <- lm(Corrupt~ GDP , data = africa)

plot(africa$GDP, africa$Corrupt, main="GDP Per Capita as a Linear Predictor of Corruption Level", xlab="GDP per capita", ylab="corruption level")
abline(beta,col="blue",lwd=2,lty=2)

summary(beta)

confint(beta,parm='GDP', level = 0.95)

predict(beta,data.frame(GDP=25000), interval="prediction")

beta <- lm(Corrupt~ PolR , data = africa)

plot(africa$PolR, africa$Corrupt, main="Degree of Freedom in Political Rights as a Linear Predictor of Corruption Level", xlab="degree of freemdom in political rights", ylab="corruption level", cex.main=.99)
abline(beta,col="blue",lwd=2,lty=2)

summary(beta) 

confint(beta,parm='PolR', level = 0.95)

predict(beta,data.frame(PolR=5.5), interval="prediction")
