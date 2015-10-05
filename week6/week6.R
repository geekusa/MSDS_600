#recreate assignment
library(e1071)
setwd("/Users/worshamn/Dropbox/Documents/Regis/MSDS600/week6/")
srd1 <- read.csv("srd1.csv",header=FALSE)
srd2 <- read.csv("srd2.csv",header=FALSE)
y1 <- rep(0,1000)
y2 <- rep(1,1000)
y <- factor(c(y1,y2))
x <- rbind(srd1,srd2)
model <- svm(x,y,type="C-classification")
p <- predict(model,x)
plot(array(p))

#accuracy test
conf.mat <- table("Predictions" = p, Actual = y)
print(conf.mat)
(accuracy <- sum(diag(conf.mat)) / length(y) * 100)

#recreate the scatter graph and plot the prediction next to it
library(ggplot2)
library(Rmisc)
df=data.frame(x,y=as.numeric(as.character(y)))
df.0 <- df[df$y==0,]
df.1 <- df[df$y==1,]
srd_plot1 <- ggplot() + 
  geom_point(data=srd1, aes(x=V1,y=V2), color = 'blue', size=1) + 
  geom_point(data=srd2, aes(x=V1,y=V2), color = 'red', size=1)
srd_plot2 <- ggplot() + 
  geom_point(data=df.0, aes(x=V1,y=V2), color = 'orange', size=1) + 
  geom_point(data=df.1, aes(x=V1,y=V2), color = 'green', size=1) 
multiplot(srd_plot1,srd_plot2)

#3dscatterplot
library(scatterplot3d)
scatterplot3d(df, col.axis="blue", col.grid="lightblue",color=2+2*df$y, pch=16, angle=55, scale.y=0.7)
scatterplot3d(df$V1,df$V2,df$y,color=2+2*df$y,pch=df$y)

#ROC curve
library(ROCR)
p <- predict(model,x, type='decision')
roc.curve1 <- performance( prediction(as.numeric(as.character(p)),as.numeric(as.character(y))),measure='tpr', x.measure='fpr' )
plot(roc.curve1)


#add in rd data as the test data
library(e1071)
setwd("/Users/worshamn/Dropbox/Documents/Regis/MSDS600/week6/")
srd1 <- read.csv("srd1.csv",header=FALSE)
srd2 <- read.csv("srd2.csv",header=FALSE)
rd1 <- read.csv("rd1.csv",header=FALSE)
rd2 <- read.csv("rd2.csv",header=FALSE)
y1 <- rep(0,1000)
y2 <- rep(1,1000)
a1 <- rep(0,10000)
a2 <- rep(1,10000)
y <- factor(c(y1,y2))
a <- factor(c(a1,a2))
x <- rbind(srd1,srd2)
z <- rbind(rd1,rd2)
wts <- table(y)
wts[1] <- 1.21
wts[2] <- 0.89
model <- svm(x,y,type="C-classification",cross=10,cost=100,tolerance=1,class.weights=wts,gamma=0.5)
#model <- svm(x,y,type="C-classification",cross=10,cost=4,tolerance=1,gamma=2)
p <- predict(model,z)
plot(array(p))
conf.mat <- table("Predictions" = p, Actual = a)
print(conf.mat)
(accuracy <- sum(diag(conf.mat)) / sum(conf.mat) * 100)

#play with tune function
obj <- tune(svm, x,y, 
            ranges = list(gamma = 2^(-1:1), cost = 2^(2:6)),
            tunecontrol = tune.control(sampling = "fix")
)
summary(obj)

#could never get this to work
DF=data.frame(x,y=as.numeric(as.character(y)))
obj <- tune.svm(x,y, gamma = 2^(-2:2), cost = 2^(2:12))
summary(obj)

#see how well hold-out compares
DF=data.frame(x,y=as.numeric(as.character(y)))
index <- sample(1:dim(DF)[1])
train <- DF[index[1:floor(dim(DF)[1]/2)], ]
test <- DF[index[((ceiling(dim(DF)[1]/2)) + 1):dim(DF)[1]], ]
filter <- svm(train[,-3],train[,3],type="C-classification")
cvpred <- predict(filter,test[,-3])
conf.mat <- table("Predictions" = pred, Actual = test.DF.testVariable)
print(conf.mat)
(accuracy <- sum(diag(conf.mat)) / length(test.sample.DF) * 100)

#ksvm
library(kernlab)
df=data.frame(x,y=as.numeric(as.character(y)))
newmodel <- ksvm(y~.,data=df,type="C-svc",cross=10)
newmodel
p <- predict(newmodel,z)
conf.mat <- table("Predictions" = p, Actual = a)
print(conf.mat)
(accuracy <- sum(diag(conf.mat)) / length(a) * 100)

#RMSE and MAE
library(hydroGOF)
sim <- strtoi(array(p))
obs <- strtoi(array(a))
rmse(sim,obs)
mae(sim,obs)

#cross valicadation using ROC curve
p <- predict(model,x, type='decision')
p.list <- list(array(p))
a.list <- list(array(a))
roc.curve2 <- performance( prediction(as.numeric(as.character(p)),as.numeric(as.character(a))),measure='tpr', x.measure='fpr' )
plot(roc.curve2)

#plot rd data
library(Rmisc)
multiplot(roc.curve1,roc.curve2)
ggplot() + 
  geom_point(data=srd1, aes(x=V1,y=V2), color = 'blue', size=1.5) + 
  geom_point(data=srd2, aes(x=V1,y=V2), color = 'red', size=1.5)
big.df=data.frame(z,y=as.numeric(as.character(a)))
df.0 <- df[df$y==0,]
df.1 <- df[df$y==1,]
rd_plot1 <- ggplot() + 
  geom_point(data=rd1, aes(x=V1,y=V2), color = 'blue', size=0.5) + 
  geom_point(data=rd2, aes(x=V1,y=V2), color = 'red', size=0.5)
rd_plot2 <- ggplot() + 
  geom_point(data=df.0, aes(x=V1,y=V2), color = 'orange', size=0.5) + 
  geom_point(data=df.1, aes(x=V1,y=V2), color = 'green', size=0.5) 
multiplot(rd_plot1,rd_plot2)
ggplot() + 
  geom_point(data=rd1, aes(x=V1,y=V2), color = 'blue', size=0.5) + 
  geom_point(data=rd2, aes(x=V1,y=V2), color = 'red', size=0.5)
  geom_point(data=df.0, aes(x=V1,y=V2), color = 'orange', size=0.5) + 
  geom_point(data=df.1, aes(x=V1,y=V2), color = 'green', size=0.5)
  
#don't know what I was doing here  
  model=svm(x,y,type="C-classification",probability=T)
  summary(model)
  svm.results=predict(model,big.df[,1:2],probability=T)
  ##convert from factor to numeric tags
  svm.pred=as.numeric(as.character(svm.results)) 
  ##get probablities for "1" Good class 
  svm.probs=attr(svm.results,'probabilities')[,"1"]