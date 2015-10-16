library(e1071)
#data(iris)
#attach(iris)

# alternatively the traditional interface:
#I find the alternative much easier to understand, x will be
#the data.frame iris EXCEPT for the column "Species" and y will be "Species"
#x <- subset(iris, select = -Species)
#y <- Species
#model <- svm(x, y) 

# test with train data
# I think here is where the example falls apart, testing with training data 
#feels like no test at all
#pred <- predict(model, x)

#instead I am going to take a sample of the iris data.frame (70%) and use that as the 
#training data, and keep the the remaining 30% to see how it did, the following is 
#just creating random row numbers in order to use as a subset
train.sample.iris <- sample(nrow(iris), ceiling(nrow(iris) * 0.7))
test.sample.iris <- (1:nrow(iris)) [- train.sample.iris]

#train with 70% data
#set x as the train subset without the Species column
x <- subset(iris[train.sample.iris, ], select = -Species)
#same data set only the Species column
y <- subset(iris[train.sample.iris, ], select = Species)
model <- svm(x, y)

#now lets pull out the Species column from our test.sample.iris subset
test.iris <- subset(iris[test.sample.iris, ], select = -Species)
#later we will need just the Species column from the test set to test the accuracy
test.iris.Species <- subset(iris[test.sample.iris, ], select = Species)

#run this through the prediction
pred <- predict(model, test.iris)

#check the accuracy of the model using a confusion matrix
conf.mat <- table("Predictions" = pred, Actual = test.iris.Species)

