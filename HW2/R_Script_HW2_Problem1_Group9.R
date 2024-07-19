
#===========================
setwd("C:/Users/Rajdeep Adhya/OneDrive/Desktop/4th_Sem_IITK/Subhra_Sankar_Dhar/Home_Work")

## Library used #######

library(Matrix)
library(ks)



#checking independence using the definition (using KDE to estimate the PDFs)
########################

log.joint.kde = function(dat){     ### function to evaluate density using KDE ##
  marginal.kde = array(0)
  for (i in 1:ncol(dat)){
    x = dat[,i]
    y = log(kde(x)$estimate)
    marginal.kde <- cbind(marginal.kde,y)
  }
  #here we are trying to use product kernel
  final.kde = apply(marginal.kde, 1, sum)
  return(final.kde)
}



#checking independence through characteristic function
#############

#function to calculate characteristic function
cf.calc = function(t.cf, data){
  terms = apply(data, 1, function(x){exp(1i * (t.cf) %*% t(x))})
  output = mean(terms)
  return(output)
}

#===================================================

# data application

#-------------------------------------
#Data adjustment
#----------------
urban <- read.csv("Urban_layer.csv")
table(urban$class)

library(tidyverse)
library(dplyr)
urban$class <- as.factor(urban$class)
urban$class <- as.vector(urban$class)

########## DATA Reduction using PCA ##########
#--------------------------------------

data_urban_red <- prcomp(urban[,-1])$x[,1:10]
asp.i <- which(urban$class == "asphalt ")
shad.i <- which(urban$class == "shadow ") 

urban.asph.red <- data_urban_red[asp.i,]
urban.shad.red <- data_urban_red[shad.i,]

full.urban.red <- cbind(urban.asph.red,urban.shad.red)



#########################
##### finding the marginal density and joint density ########
###################

log.kde.full.data = log.joint.kde(full.urban.red)
log.kde.asph = log.joint.kde(urban.asph.red)
log.kde.shad = log.joint.kde(urban.shad.red)

condition1 = (log.kde.full.data) - (log.kde.asph +log.kde.shad)
plot.ts(condition1)

 

#-------------------------------------
### checking independence using characteristics function ###########
##########################

p = ncol(full.urban.red)
# t.cf.marginal = rep(1, p)

t.cf.full = matrix(NA, ncol = 2*p, nrow = 1e3)

t_calc = function(){
  r = 10
  theta = c(runif(1, 0, 2*pi), runif(2*p-2, 0, pi))
  
  t.full = numeric(2*p)
  
  t.full[1] = r * prod(sin(theta))
  for(i in 2 : (2*p)){
    t.full[i] = r * cos(theta[i-1]) * prod(sin(theta[i : length(theta)]))
  }
  
  t.full[2*p] = r * cos(theta[length(theta)])
  
  return(t.full)
}


for(i in 1:nrow(t.cf.full)){
  t.cf.full[i, ] = t_calc()
}

t.X = t.cf.full[ ,1:p]
t.Y = t.cf.full[ ,(p + 1):(2*p)]



# for(i in 1:1)
cf.asp = apply(t.X,  1, FUN = cf.calc, data = urban.asph.red)
cf.shad = apply(t.Y,  1, FUN = cf.calc, data = urban.shad.red)

cf.fulldata = apply(t.cf.full, 1, FUN = cf.calc, data = full.urban.red)

#checking the condition of independence

condition2 = cf.fulldata - cf.asp * cf.shad
Norm_ch = sapply(condition2, FUN = norm, type = '2')

plot.ts(condition2,ylim = c(-1,1))
abline(a = 0, b = 0, lwd = 2, col = 'red')

max(Norm_ch)

plot(density(Norm_ch))


