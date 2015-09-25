library(e1071)
x <- array(data = c(0,0,1,1,0,1,0,1),dim=c(4,2))
x
y <- factor(c(0,1,1,0))
y
model <- svm(x,y,type = "C-classification")
summary(model)
print(model)
predict(model,x)