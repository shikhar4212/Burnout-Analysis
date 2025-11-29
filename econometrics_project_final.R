defaultW <- getOption("Warn")
options(warn = -1)
##-------------------------------------##

library(car)
library(lmtest)

#Loading data
data <- read.csv("econometrics_data.csv")

#Creating composite scales
data$JobPressure  <- rowMeans(data[, grep("^JP[1-5]$", names(data))], na.rm = TRUE)
data$TaskStress   <- rowMeans(data[, grep("^DTF[1-4]$", names(data))], na.rm = TRUE)
data$TimePressure <- rowMeans(data[, grep("^DTT[1-4]$", names(data))], na.rm = TRUE)

#Regression model
model <- lm(JobPressure ~ TaskStress + TimePressure, data = data)
resid <- residuals(model)
summary(model)

#Check assumptions

#Autocorrelation - Durbin-Watson Test
dwtest(model)
acf(resid)

#Linearity of Parameters
plot(JobPressure ~ TaskStress, data=data)
plot(JobPressure ~ TimePressure, data=data)

crPlots(model)


#Errors are White Noise - Ljung Box Test
Box.test(resid)


#Normality of Errors - Shapiro-Wilk Test
shapiro.test(resid)

#Homoscedastiticity Test - Breusch-Pagan Test
bptest(model)

#Multicollinearity Test
vif(model)
##--------------------------------------##
gc()

