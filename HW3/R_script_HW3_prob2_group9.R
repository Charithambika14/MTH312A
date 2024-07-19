#-------
#=============================================================================

#Question 2
#============
rm(list = ls())
set.seed(1234)
brownianMotion = function(steps, dt) {     ## n = no. of data points, u = dt
  # Generate random normally distributed increments
  increments = rnorm(n = steps, mean = 0, sd = sqrt(dt))
  
  # Calculate cumulative sum to get the Brownian motion
  std_brownian_motion = c(0 , cumsum(increments))
  
  return(std_brownian_motion)
}

# Parameters
steps = 100 # Number of time steps
dt = 0.1 
n = 100 #number of data points to be generated

X = matrix(NA, steps+1, n)
for(i in 1:n){
  X[,i] = brownianMotion(steps = steps, dt = dt)
}
time = seq(0, steps*dt, dt)


Y = colMeans(X^2) + rnorm(ncol(X))   ## generating Y
Y


#dividing into testing and training data sets
train_index <- sample(1:n, 0.75*n)  # 65% of data for training

Y_train = Y[train_index]
X_train = X[, train_index]

Y_test = Y[-train_index]
X_test = X[, -train_index]

m_hat = function(f, h, Y, X){
  norms = apply(f-X, 2, FUN = norm, type = '2') 
  kd = dnorm(norms/h)
  return(sum(kd*Y)/sum(kd))
}

#----------------CHANGE THIS BANDWIDTH ACC. TO YOUR DATA-------------
h = 10 #bandwidth 

#fitting the model on training data
m_hat_values_train = numeric(length = length(Y_train))

for(i in 1:length(Y_train)){
  m_hat_values_train[i] = m_hat(X_train[,i], h, Y_train, X_train)
}

#RMSE of the training dataset
RMSE1 = sqrt(sum((m_hat_values_train - Y_train)^2))

#fitting the model on testing data
m_hat_values_test = numeric(length = length(Y_test))
for(i in 1:length(Y_test)){
  m_hat_values_test[i] = m_hat(X_test[,i], h, Y_train, X_train)
}

#RMSE of the testing dataset
RMSE2 = sqrt(sum((m_hat_values_test - Y_test)^2))

plot(Y_test, pch = 19, xlab = "no. of data points")
lines(m_hat_values_test, col = "blue", lwd = 2)
legend(15,10, legend = "Fitted line", lty = 1, col = 'blue')


