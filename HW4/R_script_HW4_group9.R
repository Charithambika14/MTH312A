#Breast Cancer Data Classification
#================================

#Reading the data
brst_data = read.csv('breast_cancer.csv')[,-c(1,33)]
n = nrow(brst_data)
brst_data[,1] = as.factor(brst_data[,1])

#dividing into testing and training data sets
set.seed(54321) 
train_indices = sample(1:n, 0.8 * n)
train_data = brst_data[train_indices, ]
test_data = brst_data[-train_indices, ]


Y_train = train_data$diagnosis
X_train = train_data[,-1]

Y_test = test_data$diagnosis
X_test = test_data[,-1]

#####################################################
#1. Depth Based Classifier
library(ddalpha)
model_depth = ddalpha.train(formula = diagnosis~.,
                            train_data) #By delfault it uses Half-Space Depth

Y_test_pred = unlist(ddalpha.classify(model_depth, X_test))

(accuracy_d = mean(Y_test == Y_test_pred))
(misc_prob_D = 1 - accuracy_d)
######################################################

#2. SVM Based Classifier
library(e1071) 

model_SVM = svm(formula = diagnosis ~ ., 
                 data = train_data, 
                 type = 'C-classification', 
                 kernel = 'linear',
                probability = T) 

Y_test_pred = predict(model_SVM, X_test)

(accuracy_svm = mean(Y_test == Y_test_pred))
(misc_prob_svm = 1 - accuracy_svm)
#####################################################3

#3. KNN Based classifier

#finding optimal k
find_optimal_k = function(X_train, y_train, max_k = 20, num_folds = 5) {
  accuracies = numeric(max_k)
  for (k in 1:max_k) {
    folds = cut(seq(1, nrow(X_train)), breaks = num_folds, labels = FALSE)
    for (i in 1:num_folds) {
      validation_indices = which(folds == i, arr.ind = TRUE)
      X_val = X_train[validation_indices, ]
      y_val = y_train[validation_indices]
      X_train_fold = X_train[-validation_indices, ]
      y_train_fold = y_train[-validation_indices]
      predictions = knn(X_train_fold, X_val, y_train_fold, k = k)
      accuracies[k] = accuracies[k] + sum(predictions == y_val)
    }
    accuracies[k] = accuracies[k] / nrow(X_train)
  }
  return(which.max(accuracies))
}

k_optim = find_optimal_k(X_train, Y_train)

knn_model = knn(train = X_train, test = X_test, cl = Y_train, k = k_optim)

Y_test_pred = knn_model
(accuracy_knn = mean(Y_test == Y_test_pred))
(misc_prob_knn = 1 - accuracy_knn)

plot(1:20, k_optim, type = "o", xlab = "Values of K",
     ylab = "Accuracy",
     main = "Plot of Finding Optimal K (For KNN for Breast Cancer Dataset)", col = "blue",
     pch = 19, cex = 1.2, lwd = 2)


#####################################################3

#4. KDE Based Classifier
library(ks)
library(mvtnorm)

brst_data_pca =  cbind(brst_data[,1], prcomp(brst_data[,-1], scale = TRUE,
                   center = TRUE, retx = T)$x[,1:6])



train_data_pca = brst_data_pca[train_indices, ]
test_data_pca = brst_data_pca[-train_indices, ]


Y_train_pca = train_data_pca[,1]
X_train_pca = train_data_pca[,-1]

Y_test_pca = test_data_pca[,1]
X_test_pca = test_data_pca[,-1]

B_train_pca = X_train_pca[Y_train_pca == 1,]
M_train_pca = X_train_pca[Y_train_pca == 2,]

kde_B = numeric(length = length(Y_test_pca))
kde_M = numeric(length = length(Y_test_pca))

for(i in 1 : length(Y_test_pca)){
  kde_B[i] = kde(B_train_pca, eval.points = X_test_pca[i,])$estimate
  kde_M[i] = kde(M_train_pca, eval.points = X_test_pca[i,])$estimate
  print(paste0('Done : ', i))
}

Y_test_pred = ifelse(kde_B>kde_M, 1, 2)
(accuracy_KDE = mean(Y_test_pca == Y_test_pred))
(misc_prob_KDE = 1 - accuracy_KDE)








########################################################3

#===============================================================
#Heart Disease Data Classification
#================================

#####################################################
#1. Depth Based Classifier



depth = function(train_data, testX = X_test, testY = Y_test,V14){
  library(ddalpha)
  model_depth = ddalpha.train(formula = V14 ~ .,
                              train_data, outsider.methods = "depth.Mahalanobis") #By default it uses Half-Space Depth
  Y_test_pred = unlist(ddalpha.classify(model_depth, testX))
  
  accuracy_d = mean(testY == Y_test_pred)
  misc_prob_D = 1 - accuracy_d
  A = c(accuracy_d , misc_prob_D)
  names(A) = c('Accuracy' , 'Missclassification Prob')
  return(A)
}




#################################################

#2. SVM Based Classifier

SVM = function(train =train_data, testX = X_test, testY = Y_test, V14 = V14){
  library(e1071) 
  
  model_SVM = svm(formula = V14 ~ ., 
                  data = train, 
                  type = 'C-classification', 
                  kernel = 'linear',
                  probability = T) 
  
  Y_test_pred = predict(model_SVM, testX)
  
  accuracy_svm = mean(testY == Y_test_pred)
  misc_prob_svm = 1 - accuracy_svm
  A = c(accuracy_svm , misc_prob_svm)
  names(A) = c('Accuracy' , 'Missclassification Prob')
  return(A)
}

########################################################333

#3. KNN Based classifier

#finding optimal k
find_optimal_k = function(trainX = X_train, trainY = Y_train, max_k = 20, num_folds = 5) {
  accuracies = numeric(max_k)
  for (k in 1:max_k) {
    folds = cut(seq(1, nrow(trainX)), breaks = num_folds, labels = FALSE)
    for (i in 1:num_folds) {
      validation_indices = which(folds == i, arr.ind = TRUE)
      X_val = trainX[validation_indices, ]
      y_val = trainY[validation_indices]
      trainX_fold = trainX[-validation_indices, ]
      trainY_fold = trainY[-validation_indices]
      predictions = knn(trainX_fold, X_val, trainY_fold, k = k)
      accuracies[k] = accuracies[k] + sum(predictions == y_val)
    }
    accuracies[k] = accuracies[k] / nrow(trainX)
  }
  return(which.max(accuracies))
}

KNN = function(trainX = X_train, trainY = Y_train, testX = X_test, testY = Y_test){
  k_optim = find_optimal_k(trainX, trainY)
  
  knn_model = knn(train = trainX, test = testX, cl = trainY, k = k_optim)
  
  Y_test_pred = knn_model
  accuracy_knn = mean(Y_test == Y_test_pred)
  misc_prob_knn = 1 - accuracy_knn
  A = c(accuracy_knn , misc_prob_knn)
  names(A) = c('Accuracy' , 'Missclassification Prob')
  return(A)
}

KDE = function(data){
  data_pca =  cbind(data[,ncol(data)], prcomp(data[,-ncol(data)], scale = TRUE,
                                              center = TRUE, retx = T)$x[,1:6])
  
  
  
  train_data_pca = data_pca[train_indices, ]
  test_data_pca = data_pca[-train_indices, ]
  
  
  Y_train_pca = train_data_pca[,1]
  X_train_pca = train_data_pca[,-1]
  
  Y_test_pca = test_data_pca[,1]
  X_test_pca = test_data_pca[,-1]
  
  p_train_pca = X_train_pca[Y_train_pca == 1,]
  a_train_pca = X_train_pca[Y_train_pca == 0,]
  
  kde_p = numeric(length = length(Y_test_pca))
  kde_a = numeric(length = length(Y_test_pca))
  
  for(i in 1 : length(Y_test_pca)){
    kde_p[i] = kde(p_train_pca, eval.points = X_test_pca[i,])$estimate
    kde_a[i] = kde(a_train_pca, eval.points = X_test_pca[i,])$estimate
    print(paste0('Done : ', i))
  }
  
  Y_test_pred = ifelse(kde_p > kde_a, 1, 2)
  (accuracy_KDE = mean(Y_test_pca == Y_test_pred))
  (misc_prob_KDE = 1 - accuracy_KDE)
  return(accuracy_KDE)
}

####################################################


#Cleveland Data
c_data = read.csv("processed.cleveland.data", sep=",",header =F)

#data pre-processing
c_data$V14[c_data$V14 > 0] = 1
c_data = c_data[-which(c_data == "?", arr.ind = T)[,1],]
n = nrow(c_data)
c_data[,12] = as.numeric(c_data[,12])
c_data[,13] = as.numeric(c_data[,13])

#dividing into testing and training data sets
set.seed(54321) 
train_indices = sample(1:n, 0.8 * n)
train_data = c_data[train_indices, ]
test_data = c_data[-train_indices, ]


Y_train = train_data$V14
X_train = train_data[,-14]

Y_test = test_data$V14
X_test = test_data[,-14]


(cleveland = data.frame(depth = depth(), SVM = SVM(), KNN = KNN()))

clevelnad_KDE = KDE(c_data)
clevelnad_KDE

#################################################################################

