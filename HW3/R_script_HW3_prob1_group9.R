#Question 1
#============

# Function to generate Brownian motion
set.seed(12345)
brownianMotion = function(steps, dt) {
  # Generate random normally distributed increments
  increments = rnorm(n = steps, mean = 0, sd = sqrt(dt))
  
  # Calculate cumulative sum to get the Brownian motion
  std_brownianMotion = c(0 , cumsum(increments))
  
  return(std_brownianMotion)
}
# Function to generate drifted Brownian motion
generate_drift_brownianMotion = function(steps, dt, drift){
  
  std_brownianMotion = brownianMotion(steps, dt)
  
  time_points = seq(0, steps*dt, dt)
  
  drift_brownianMotion = drift * time_points + std_brownianMotion
  
  return(drift_brownianMotion)
}

generate_data = function(steps, dt, drift){
  index = rbinom(1, 1, 0.85)
  output = if (index == 1) {
    output = brownianMotion(steps, dt)
  } else {
    output = generate_drift_brownianMotion(steps, dt, drift)
  }
  return(output)
}

# Parameters
steps = 100  # Number of time steps
dt = 0.1         # Time step size
drift = 2
n = 100 #number of data points to be generated
# Generate Brownian motion data
data = matrix(NA, steps+1, n)
for(i in 1:n){
  data[,i] = generate_data(steps = steps, dt = dt, drift = drift)
}
time = seq(0, steps*dt, dt)


upper_band = apply(data, 1, quantile, prob = 0.90)
lower_band = apply(data, 1, quantile, prob = 0.10)


#Plotting the data
final_data = cbind(time, reshape2::melt(data))
final_data[,3] = as.factor(final_data[,3])
library(ggplot2)
ggplot()+
  geom_line(data = final_data, aes(x = time, y = value, col = Var2))+
  theme(legend.position = 'none')+
  labs(x = 't',
       y = 'X(t)',
       title = 'Data Generated from a Mixture of Brownian Motions')


#--------------Outlier Detection Method---------------

#detecting outliers
id = NULL

for(i in 1 : n){
  id[i] = sum(data[,i] > upper_band | data[,i] < lower_band)
}

outlier_id = which(id > 85)
length(outlier_id)
#-------------------------------------------------------------------------

#plotting the outliers and bands
g = ggplot()+
  geom_line(data = final_data, aes(x = time, y = value, col = Var2))+
  theme(legend.position = 'none')
outlier_data = cbind(time, reshape2::melt(data[, outlier_id]))
outlier_data[,3] = as.factor(outlier_data[,3])

ggplot()+ geom_line(data = outlier_data, aes(x = time, y = value, col = Var2), linewidth = 1, show.legend = FALSE)+
  geom_point(aes(x = time, y = upper_band, shape = 'Band'), col = 'darkblue')+
  geom_point(aes(x = time, y = lower_band, shape = 'Band'), col = 'darkblue')+
  labs(shape = 'Index',
       x = 't',
       y = 'X(t)',
       title = 'Outliers in the data')

