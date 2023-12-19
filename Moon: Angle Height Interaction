library("plyr")
moon <- read.csv("MOON.csv", header=TRUE)
summary(moon)

beta1 <- lm(ANGLE ~ HEIGHT, data = moon)
plot(moon$HEIGHT, moon$ANGLE, main="Plot of moon data and coefficients", 
    xlab="Angle", 
    ylab="Height")
abline(beta1,col="blue",lwd=2,lty=2)

beta1$residuals
plot(moon$HEIGHT,beta1$residuals, main="Residuals versus moon data", 
    xlab="moon data", 
    ylab="Residuals")

subsample_greater50 <- subset(moon, HEIGHT > 50)
subsample_less50 <- subset(moon, HEIGHT <= 50)

model_greater50 <- lm(ANGLE ~ HEIGHT, data = subsample_greater50)
model_less50 <- lm(ANGLE ~ HEIGHT, data = subsample_less50)

mse_greater50 <- mean(model_greater50$residuals^2)
mse_less50 <- mean(model_less50$residuals^2)

mse_greater50
mse_less50

subsample_greater50 <- subset(moon, HEIGHT > 50)
subsample_less50 <- subset(moon, HEIGHT <= 50)

model_greater50 <- lm(ANGLE ~ HEIGHT, data = subsample_greater50)
model_less50 <- lm(ANGLE ~ HEIGHT, data = subsample_less50)

mse_greater50 <- mean(model_greater50$residuals^2)
mse_less50 <- mean(model_less50$residuals^2)

mse_greater50
mse_less50

residuals_greater50 <- model_greater50$residuals^2
residuals_less50 <- model_less50$residuals^2
combined_data <- data.frame(
  residuals = c(residuals_greater50, residuals_less50),
  subsample_indicator = rep(c("Greater_50", "Less_50"), c(length(residuals_greater50), length(residuals_less50))))
hetero_test <- lmtest::bptest(residuals ~ subsample_indicator, data = combined_data)
print(hetero_test)
