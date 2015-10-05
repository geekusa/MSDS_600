# Practical session: Machine learning for computational biology
# 2014 http://cbio.ensmp.fr/~ebernard/teaching_material/mlbio.R

##############################
## Preliminary code to run
##############################
library(ggplot2) # plot nice graphics
library(kernlab) # solve the SVM
library(ROCR)    # plot performance

# Function to generate a (n, p) matrix of draws from a random variable
RandomMatrix <- function( dist, n, p, ... ) {
  rs <- dist( n*p, ... )
  matrix( rs, n, p )
}

##############################
## 1 Linear SVM
##############################

#### Section A: linearly separable dataset
##########################################

## i) Function to generate a linearly separable dataset
GenerateDatasetLinear <- function( n, p ) 
{
  n.pos <- floor( n/2 )
  x.pos <- RandomMatrix( rnorm, n.pos, p, mean=0, sd=1 )
  x.neg <- RandomMatrix( rnorm, n-n.pos, p, mean=3, sd=1 )
  y <- c( rep( 1, n.pos ), rep( -1, n-n.pos ) )
  n.train <- floor( 0.8 * n )
  idx.train <- sample( n, n.train )
  is.train <- rep( 0, n )
  is.train[idx.train] <- 1
  
  return(data.frame( x=rbind( x.pos, x.neg ), y=y, train=is.train ))
}

## ii) Generate the dataset (150 points in dimension 2)
linear.data <- GenerateDatasetLinear( 150, 2 )

## iii) Extract train and test subsets of the dataset
linear.train <- linear.data[linear.data$train==1, -4 ]
linear.test <- linear.data[linear.data$train==0, -4 ]

## iv) Plot the data
qplot( data=linear.data, x.1, x.2, colour=factor(y), shape=factor(train) )

## v) Train a linear SVM
linear.svm <- ksvm( y ~ ., data=linear.train, type='C-svc', kernel='vanilladot',scaled=c(),C=100 )

# Look what is inside
linear.svm
attributes(linear.svm)

## vi) Plot the model
plot( linear.svm, data=linear.train )

# vii) Predicting class on the test set: Class= sign(t(w)*x_test +b) 
linear.prediction <- predict( linear.svm, linear.test )

# viii) Accuracy 
Accuracy_test <- sum(linear.prediction==linear.test$y)/nrow(linear.test) *100
print(paste0('The accuracy of the prediction on the test dataset is : ', Accuracy_test,'%'))

# ix) Confusion matrix
table(linear.test$y,linear.prediction,dnn=c("REAL","PREDICTION"))

#### Section B: linearly overlapping dataset
#############################################

## x) Function to generate a linearly overlapping dataset
GenerateDatasetOverlapping <- function( n, p ) 
{
  n.pos <- floor( n/2 )
  x.pos <- RandomMatrix( rnorm, n.pos, p, mean=0, sd=1 )
  x.neg <- RandomMatrix( rnorm, n-n.pos, p, mean=1.5, sd=1 )
  y <- c( rep( 1, n.pos ), rep( -1, n-n.pos ) )
  n.train <- floor( 0.8 * n )
  idx.train <- sample( n, n.train )
  is.train <- rep( 0, n )
  is.train[idx.train] <- 1
  data.frame( x=rbind( x.pos, x.neg ), y=y, train=is.train )
}

## xi) Generate the dataset (150 points in dimension 2)
linear.data.overlap <- GenerateDatasetOverlapping( 150, 2 )

## xii) Extract train and test subsets of the dataset
linear.train.overlap <- linear.data.overlap[linear.data.overlap$train==1, -4 ]
linear.test.overlap <- linear.data.overlap[linear.data.overlap$train==0, -4 ]

## xiii) Plot the data
qplot( data=linear.data.overlap, x.1, x.2, colour=factor(y), shape=factor(train) )

# xiv) Train linear SVM with different C
linear.svm.overlap <- ksvm( y ~ ., data=linear.train.overlap, type='C-svc', kernel='vanilladot',scaled=c(),C=100 )
linear.svm.overlap.1 <- ksvm( y ~ ., data=linear.train.overlap, type='C-svc', kernel='vanilladot',scaled=c(),C=0.01 )
linear.svm.overlap.2 <- ksvm( y ~ ., data=linear.train.overlap, type='C-svc', kernel='vanilladot',scaled=c(),C=1000000)

# Plot the model
x11()
plot( linear.svm.overlap.1, data=linear.train.overlap )

x11()
plot( linear.svm.overlap.2, data=linear.train.overlap )

# xv) Predictions for test set
linear.prediction.overlap <- predict( linear.svm.overlap, linear.test.overlap )

# xvi) Accuracy 
Accuracy_test <- sum(linear.prediction.overlap==linear.test.overlap$y)/nrow(linear.test.overlap) *100
print(paste0('The accuracy of the prediction on the test dataset is : ', Accuracy_test,'%'))

# xvii) Confusion matrix
table(linear.test.overlap$y,linear.prediction.overlap,dnn=c("REAL","PREDICTION"))

# xviii) Prediction scores
linear.prediction.score <- predict( linear.svm, linear.test, type='decision' )

#### PAUSE HERE AND WAIT FOR THE OTHERS : EXPLANATION ABOUT ROC CURVES ####

# xix) Compute ROC curves
linear.roc.curve <- performance( prediction( linear.prediction.score, linear.test.overlap$y ),measure='tpr', x.measure='fpr' )
plot( linear.roc.curve )

#### Section C: Not linearly separable 
#############################################
# xx) Generate a 'cross-shaped' dataset (not linearly separable)
GenerateDatasetNonlinear <- function( n, p ) {
  bottom.left <- RandomMatrix( rnorm, n, p, mean=0, sd=1 )
  upper.right <- RandomMatrix( rnorm, n, p, mean=4, sd=1 )
  tmp1 <- RandomMatrix( rnorm, n, p, mean=0, sd=1 )
  tmp2 <- RandomMatrix( rnorm, n, p, mean=4, sd=1 )
  upper.left <- cbind( tmp1[,1], tmp2[,2] )
  bottom.right <- cbind( tmp2[,1], tmp1[,2] )
  y <- c( rep( 1, 2 * n ), rep( -1, 2 * n ) )
  idx.train <- sample( 4 * n, floor( 3.5 * n ) )
  is.train <- rep( 0, 4 * n )
  is.train[idx.train] <- 1
  data.frame( x=rbind( bottom.left, upper.right, upper.left, bottom.right ), y=y, train=is.train )
}

# xxi) Extract train and test datasets
nonlinear.data <- GenerateDatasetNonlinear( 100, 2 )
nonlinear.train <- nonlinear.data[nonlinear.data$train==1, -4 ]
nonlinear.test <- nonlinear.data[nonlinear.data$train==0, -4 ]

# xxii) Plot the data
qplot( data=nonlinear.data, x.1, x.2, colour=factor(y), shape=factor(train) )

# xxiii) Train a nonlinear SVM
nonlinear.svm <- ksvm( y ~ ., data=nonlinear.train, type='C-svc', kernel='rbf', kpar=list(sigma=1), C=100, scale=c() )
plot( nonlinear.svm, data=nonlinear.train )

# xxiv) Look at the importance of the parameter C here ! 
cgrid <- 2^(-10:10)
for (i in 1:length(cgrid)) {
  nonlinear.svm <- ksvm(y ~. , data=nonlinear.train,type="C-svc",kernel='rbfdot',C=cgrid[i],scaled=c())
  x11()
  plot(nonlinear.svm,data=nonlinear.train)
}
graphics.off()

# xxv) Compute the error by cross-validation for several values of C
err <- numeric(length(cgrid))
for (i in 1:length(cgrid)) {
  cross.nonlinear.svm <- ksvm(y ~. , data=nonlinear.train,type="C-svc",kernel='rbfdot',C=cgrid[i],scaled=c(),cross=3)
  err[i] <- cross(cross.nonlinear.svm)
}
err

# xxvi) Plot the "error versus C" curve
BiasVarianceTradeoff <- data.frame(cgrid=cgrid,error=err)
ggplot(BiasVarianceTradeoff,aes(x=cgrid,y=error)) + geom_point() + geom_line() + scale_x_log10()


#### BONUS SECTION: APPLICATION TO REAL DATA
############################################
source( 'http://bioconductor.org/biocLite.R' )
biocLite( 'ALL' )

require( 'ALL' )
data( 'ALL' )

# Inspect the data
?ALL
show(ALL)
print(summary(pData(ALL)))

# Look at the type ans stage of each patient
print(ALL$BT)

# Predict type of disease from expression data
all.data <- data.frame( x=t( exprs(ALL) ), y=substr( ALL$BT, 1, 1 ) )
# ........ TO DO 
  matrix( rs, n, p )
}

GenerateDatasetOverlapping <- function( n, p ) 
{
  n.pos <- floor( n/2 )
  x.pos <- RandomMatrix( rnorm, n.pos, p, mean=0, sd=1 )
  x.neg <- RandomMatrix( rnorm, n-n.pos, p, mean=1.5, sd=1 )
  y <- c( rep( 1, n.pos ), rep( -1, n-n.pos ) )
  n.train <- floor( 0.8 * n )
  idx.train <- sample( n, n.train )
  is.train <- rep( 0, n )
  is.train[idx.train] <- 1
  data.frame( x=rbind( x.pos, x.neg ), y=y, train=is.train )
}

## xi) Generate the dataset (150 points in dimension 2)
linear.data.overlap <- GenerateDatasetOverlapping( 150, 2 )

## xii) Extract train and test subsets of the dataset
linear.train.overlap <- linear.data.overlap[linear.data.overlap$train==1, -4 ]
linear.test.overlap <- linear.data.overlap[linear.data.overlap$train==0, -4 ]

## xiii) Plot the data
qplot( data=linear.data.overlap, x.1, x.2, colour=factor(y), shape=factor(train) )

# xiv) Train linear SVM with different C
linear.svm.overlap <- ksvm( y ~ ., data=linear.train.overlap, type='C-svc', kernel='vanilladot',scaled=c(),C=100 )
linear.svm.overlap.1 <- ksvm( y ~ ., data=linear.train.overlap, type='C-svc', kernel='vanilladot',scaled=c(),C=0.01 )
linear.svm.overlap.2 <- ksvm( y ~ ., data=linear.train.overlap, type='C-svc', kernel='vanilladot',scaled=c(),C=1000000)

# Plot the model
x11()
plot( linear.svm.overlap.1, data=linear.train.overlap )

x11()
plot( linear.svm.overlap.2, data=linear.train.overlap )

# xv) Predictions for test set
linear.prediction.overlap <- predict( linear.svm.overlap, linear.test.overlap )

# xvi) Accuracy 
Accuracy_test <- sum(linear.prediction.overlap==linear.test.overlap$y)/nrow(linear.test.overlap) *100
print(paste0('The accuracy of the prediction on the test dataset is : ', Accuracy_test,'%'))

# xvii) Confusion matrix
table(linear.test.overlap$y,linear.prediction.overlap,dnn=c("REAL","PREDICTION"))

# xviii) Prediction scores
linear.prediction.score <- predict( linear.svm, linear.test, type='decision' )

#### PAUSE HERE AND WAIT FOR THE OTHERS : EXPLANATION ABOUT ROC CURVES ####

# xix) Compute ROC curves
linear.roc.curve <- performance( prediction( linear.prediction.score, linear.test.overlap$y ),measure='tpr', x.measure='fpr' )
plot( linear.roc.curve )
