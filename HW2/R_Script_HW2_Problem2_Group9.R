#install.packages("locfit")

library(locfit)

data = airquality
data = na.omit(data)

dim(data)

View(data)

plot(airquality$Temp, airquality$Ozone, 
     xlab = "Daily Maximum Temperature (in degree fahrenheit)",
     ylab = "Mean ozone (in parts per bollion)",
     col = "blue",
     pch = 16, 
     main = "Daily Measurement of Ozone vs Temperature for 05.01.1973 to 30.09.1973 in New York")

#regression by local polynomial mean
fit <- locfit.raw(data$Temp , data$Ozone, deg = 2)


dim(data)

lines(fit, col = "red", lwd = 2)
legend("topleft",legend = "Estimated regression function using local polynomial mean approach",
       lwd = 2, col = "red")


fitted <- predict(fit, data$Temp)
##########
fitted_loc_mean <- predict(fit, data$Temp)
##########

fit.deriv1 <- locfit.raw(data$Temp , data$Ozone, deg = 2, deriv = 1)
#first_deriv <- fit.deriv1$trans(data$Temp)

#lines(fit.deriv1, col = "red")

first_deriv <- predict(fit.deriv1, data$Temp)

points(data$Temp, first_deriv, col = "green", pch = 16)

fit.deriv2 <- locfit.raw(data$Temp , data$Ozone, deg = 2, deriv = c(1,1))
#second_deriv <- fit.deriv2$trans(data$Temp)

second_deriv <- predict(fit.deriv2, data$Temp)


df <- data.frame(data$Temp, data$Ozone, fitted, first_deriv, second_deriv )
View(df[sample(1:111, size = 10, replace = FALSE),])


plot(data$Temp, first_deriv, 
     xlab = "Daily Maximum Temperature (in degree fahrenheit)",
     ylab = "Derivative of the regression function",
     col = "blue",
     pch = 16, 
     ylim = c(-0.4, 5), cex = 1.5,
     main = "Derivative of the regression function vs Temperature for 05.01.1973 to 30.09.1973 in New York")

points(data$Temp, second_deriv, col = "green", pch = 17, cex = 1.5)
legend("topleft",legend = c("Estimated first order derivative of the regression function",
                            "Estimated second order derivative of the regression function"),
       lwd = 2, col = c("blue", "green"), pch = c(16,17))


#############################################################################3
#regrrssion by local polynomial median


plot(airquality$Temp, airquality$Ozone, 
     xlab = "Daily Maximum Temperature (in degree fahrenheit)",
     ylab = "Mean ozone (in parts per bollion)",
     col = "blue",
     pch = 16, 
     main = "Daily Measurement of Ozone vs Temperature for 05.01.1973 to 30.09.1973 in New York")

#regression by local polynomial mean
fit <- locfit.robust(data$Temp , data$Ozone, deg = 2)


dim(data)

lines(fit, col = "green", lwd = 2)
legend("topleft",legend = "Estimated regression function using local polynomial median approach",
       lwd = 2, col = "red")

legend("topleft",legend = c("Estimated regression function using local polynomial mean approach",
                            "Estimated regression function using local polynomial median approach"),
       lwd = c(2,2), col = c("red", "green"))



fitted <- predict(fit, data$Temp)
##########
fitted_loc_median <- predict(fit, data$Temp)
##########


fit.deriv1 <- locfit.robust(data$Temp , data$Ozone, deg = 2, deriv = 1)
#first_deriv <- fit.deriv1$trans(data$Temp)

#lines(fit.deriv1, col = "red")

first_deriv <- predict(fit.deriv1, data$Temp)


fit.deriv2 <- locfit.robust(data$Temp , data$Ozone, deg = 2, deriv = c(1,1))
#second_deriv <- fit.deriv2$trans(data$Temp)

second_deriv <- predict(fit.deriv2, data$Temp)


df <- data.frame(data$Temp, data$Ozone, fitted, first_deriv, second_deriv )
View(df[sample(1:111, size = 5, replace = FALSE),])


plot(data$Temp, first_deriv, 
     xlab = "Daily Maximum Temperature (in degree fahrenheit)",
     ylab = "Derivative of the regression function",
     col = "blue",
     pch = 16, 
     ylim = c(-0.4, 5), cex = 1.5,
     main = "Derivative of the regression function vs Temperature for 05.01.1973 to 30.09.1973 in New York")

points(data$Temp, second_deriv, col = "green", pch = 17, cex = 1.5)
legend("topleft",legend = c("Estimated first order derivative of the regression function",
                            "Estimated second order derivative of the regression function"),
       lwd = 2, col = c("blue", "green"), pch = c(16,17))

############ comparison ############
mse_loc_mean <- mean((data$Ozone - fitted_loc_mean)^2)
rmse_mean <- sqrt(mse_loc_mean)


r2_mean <- 1- (mse_loc_mean /var(data$Ozone))

mse_loc_median <- mean((data$Ozone - fitted_loc_median)^2)
rmse_median <- sqrt(mse_loc_median)


r2_median <- 1- (mse_loc_median /var(data$Ozone))
