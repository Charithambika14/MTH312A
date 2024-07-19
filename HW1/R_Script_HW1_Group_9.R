
############### Problem 1 ####################

#install.packages("ddalpha")
library(ddalpha)

head(iris)
library(dplyr)

 
### For different pair of species we need to update the following line ###
data_set_vir <- iris %>% filter(Species %in% c("setosa","virginica"))
dim(data_set_vir)

X <- as.matrix(data_set_vir[,1:4])
data_set <- data_set_vir %>% filter(Species == "setosa")
data_set <- data_set[,1:4]
hf_Set <- depth.halfspace(X, data_set)
hf_Set
which.max(hf_Set)


data_vir <- data_set_vir %>% filter(Species == "virginica")
data_vir <- data_vir[,1:4]
hf_vir <- depth.halfspace(X, data_vir)
hf_vir
which.max(hf_vir)


plot(hf_Set, hf_vir, col = "blue", pch = 16,
     xlab = "Half-space depth w.r.t. Setosa",
     ylab = "Half-space depth w.r.t. Virginica",
     ylim = c(0,0.20),
     main = "DD-plot (w.r.t. Half Space depth) of Setosa vs Virginica")
lines(x = seq(0,0.35, 0.001), y = seq(0,0.35, 0.001), col = "red", lwd = 2)
legend("topright", legend = "Straight Line at 45 degree angle", col = "red",lty = 1)

######## for centralization ###########
data_set$Sepal.Length = data_set$Sepal.Length - data_set_vir$Sepal.Length[which.max(hf_Set)]
data_set$Sepal.Width = data_set$Sepal.Width - data_set_vir$Sepal.Width[which.max(hf_Set)]
data_set$Petal.Length = data_set$Petal.Length - data_set_vir$Petal.Length[which.max(hf_Set)]
data_set$Petal.Width = data_set$Petal.Width - data_set_vir$Petal.Width[which.max(hf_Set)]


data_vir$Sepal.Length = data_vir$Sepal.Length - data_set_vir$Sepal.Length[which.max(hf_vir)]
data_vir$Sepal.Width = data_vir$Sepal.Width - data_set_vir$Sepal.Width[which.max(hf_vir)]
data_vir$Petal.Length = data_vir$Petal.Length - data_set_vir$Petal.Length[which.max(hf_vir)]
data_vir$Petal.Width = data_vir$Petal.Width - data_set_vir$Petal.Width[which.max(hf_vir)]

X_central <- rbind(data_set, data_vir)
X_central <- as.matrix(X_central)

hf_Set <- depth.halfspace(X_central, data_set)
hf_Set


hf_vir <- depth.halfspace(X_central, data_vir)
hf_vir

plot(hf_Set, hf_vir, col = "blue", pch = 16,
     xlab = "Half-space depth w.r.t. Setosa",
     ylab = "Half-space depth w.r.t. Virginica",
     ylim = c(0,0.35),
     main = "DD-plot (w.r.t. Half Space depth) of the Centralized Data of Setosa vs Virginica")
lines(x = seq(0,1, 0.0001), y = seq(0,1, 0.0001), col = "red", lwd = 2)
legend("topright", legend = "Straight Line at 45 degree angle", col = "red",lty = 1)



########## Scale adjustment ##########



S_X <- cov(data_set)
eig_Sx <- eigen(S_X)
Dx <- diag(eig_Sx$values)
Px <- eig_Sx$vectors
Sx_half_inv <- Px %*% diag((eig_Sx$values)^(-0.5)) %*% t(Px) 

for(i in 1:dim(data_set)[1]){
 data_set[i,] = Sx_half_inv %*% t(as.matrix(data_set[i,]))
}




S_Y <- cov(data_vir)
eig_Sy <- eigen(S_Y)
Dy <- diag(eig_Sy$values)
Py <- eig_Sy$vectors
Sy_half_inv <- Py %*% diag((eig_Sy$values)^(-0.5)) %*% t(Py) 

for(i in 1:dim(data_vir)[1]){
  data_vir[i,] = Sy_half_inv %*% t(as.matrix(data_vir[i,]))
}


X_stand <- rbind(data_set, data_vir)
X_stand <- as.matrix(X_stand)

hf_Set <- depth.halfspace(X_stand, data_set)
hf_Set


hf_vir <- depth.halfspace(X_stand,data_vir)
hf_vir

plot(hf_Set, hf_vir, col = "blue", pch = 16,
     xlab = "Half-space depth w.r.t. Setosa",
     ylab = "Half-space depth w.r.t. Virginica",
     ylim = c(0,0.35),
     main = "DD-plot (w.r.t. Half Space depth) of the Standardized Data of Setosa vs Virginica")
lines(x = seq(0,1, 0.0001), y = seq(0,1, 0.0001), col = "red", lwd = 2)
legend("topright", legend = "Straight Line at 45 degree angle", col = "red",lty = 1)








######################## Problem 2 #################################


first_step <- function(n,u,X){
  Q_u <- NULL
  found <- FALSE
  i <- 1
  while(!found & i <= n){
    lhs <- (n-1)*u
    
    for(j in 1:n){
      if(j != i){
        lhs <- lhs + (X[j,] - X[i,])/norm(X[j,] - X[i,], type = "2")
      }
    }
    first_step_lhs <- norm(lhs , type = "2")
    
    if(first_step_lhs <= (1 + norm(u,type = "2"))){
      Q_u <- X[i,]
      found <- TRUE
    }else{
      i <- i+1
    }
    
  }
  return(c(found,Q_u))
}

foo <- function(n,u,X,Q){
  lhs <- n*u
  for (i in 1:n) {
    lhs <- lhs + (X[i,] - Q)/norm(X[i,] - Q , type = "2")
  }
  lhs <- norm(lhs , type = "2")
  return(lhs)
}


second_step <- function(n,u,X){
  count <- 0
  found <- F
  while(!found & count < 10){
    Q <- optim(runif(2), foo, X = X, u = u, n = n, method = "BFGS")
    par <- Q$par
    
    if(norm(par, "2") < 7){
      found <- T
    }
    count <- count + 1
  }
  
  return(par)
}

find_Q <- function(n,u,X){
  #ans <- first_step(n,u,X)
  #temp <- ans[2]
  #found <- ans[1]
  #if(found == TRUE){
  #  Q <- temp
  #  return(Q)
  #}
  
  Q <- second_step(n,u,X)
  return(Q)
}

multi_quantile <- function(r , num_points = 10, X){
  
  n <- dim(X)[1]
  
  theta <- seq(0, 2*pi, len = num_points)
  u <- matrix(data = c(r*cos(theta), r*sin(theta)), ncol = 2, byrow = F)
  
  Q <- matrix(0, nrow = num_points, ncol = 2)
  for(i in 1:num_points){
    Q[i, ] <- as.vector(find_Q(X = X, n = n, u = u[i, ]))
  }
  
  ## Adjustment for large norm of Q
  for(i in 2:(num_points-1)){
    if( norm(Q[i,], "2") > norm(Q[i-1,], "2") + norm(Q[i+1,], "2")){
      Q[i, 1] <- mean(Q[i-1,1], Q[i+1,1])
      Q[i, 2] <- mean(Q[i-1,2], Q[i+1,2])
    }
  }
  
  ## for first point of Q
  if( norm(Q[1,], "2") > norm(Q[2,], "2") + norm(Q[num_points,],"2")){
    Q[1, 1] <- mean(Q[2,1], Q[num_points,1])
    Q[1, 2] <- mean(Q[2,2], Q[num_points,2])
  }
  
  ## for last point of Q
  if( norm(Q[num_points,], "2") > norm(Q[num_points-1,], "2") + norm(Q[1,],"2")){
    Q[num_points, 1] <- mean(Q[num_points-1,1], Q[1,1])
    Q[num_points, 2] <- mean(Q[num_points-1,2], Q[1,2])
  }
  
  return(Q)
}

Q_plot <- function(Q, col_index)
{
  Q <- rbind(Q, Q[1, ])
  lines(Q[, 1], Q[, 2], col = col_index, lwd = 2)
}


###################################
library(MASS)

library(dplyr)

data_vir <- iris 

plot(data_vir$Sepal.Length, data_vir$Sepal.Width, pch = 19, col = "blue",
     xlab = "Sepal Length", ylab = "Sepal Width",
     main = "Quantile contour Plot of Sepal Length vs Sepal Width of Iris Dataset")




df <- data.frame(SL = data_vir$Sepal.Length,
                 SW = data_vir$Sepal.Width)


data <- scale(df)
data <- as.data.frame(data)



plot(data$SL, data$SW, pch = 19, col = "blue",
     xlab = "Sepal Length", ylab = "Sepal Width",
     main = "Quantile contour Plot of Sepal Length vs Sepal Width of Iris Dataset")

for(i in 1:9){
  r <- i/10
  Q <- multi_quantile(r = r, num_points = 10, data)
  Q_plot(Q, col_index = i)
  print(i)
}



############# Bivariate KDE Plot ############
rm(list = ls())

#dataset loading ......
library(MASS)

library(dplyr)

data <- iris
data <- data[,1:2]
#kernel density estimate
#install.packages("ks")
library(ks)
kd <- kde(data)
attach(kd)


#density estmate at the diven data points
k =(predict(kd , x = as.matrix(data)))






#3D plot of the bivariate density with quantile contours....looks cool....:) 
library(plotly)
density = data.frame(Sepal_Length = data$Sepal.Length , Sepal_Width = data$Sepal.Width , Density = k)

plot_ly(density, x = ~Sepal_Length , y = ~Sepal_Width , z = ~Density , type = "scatter3d" , color = k , opacity = 0.5 , marker = list(size = 6))




