BN1 <- read.csv ("BN1.csv", header = FALSE)
head(BN1)
summary(BN1)
BN1_mean = mean(BN1 [,1])
BN1_mean
hist(BN1 [,1], col="green")
boxplot(BN1, pars = list(boxwex = 0.6), range=0)

BN2 <- read.csv ("BN2.csv", header = FALSE)
hist(BN2 [,1], col="yellow")
summary(BN2)
boxplot(BN2, pars = list(boxwex = 0.6), range=0)

data <- data.frame(x=BN1,y=BN2)
boxplot(data, names = c('BN1','BN2'), pars = list(boxwex = 0.6), range=0)

boxplot(BN1 [,1],BN2 [,1], names = c('BN1','BN2'), pars = list(boxwex = 0.6), range=0)

hist(N2 [,1], col="blue", main='Overlapping Histogram', xlab = 'N1 vs N2')
hist(N1 [,1], col='red', add=T)
box()

top10 <- function(x)
{
  counts <- table(x, useNA = "always")
  head(sort(counts, decreasing = TRUE), 10)
}
top10(BN1[,1])

IQR(BN1[,1])
IQR(BN2[,1])

max(BN1[,1]) - min(BN1[,1])
max(BN2[,1]) - min(BN2[,1])

mde <- density(BN1[,1])
mde$x[which(mde$y == max(mde$y))]

outliers <- function(x){
  lowerq = quantile(x[,1])[2]
  upperq = quantile(x[,1])[4]
  iqr <- IQR(x[,1])
  mild.lower.Thresh <- lowerq - (iqr *1.5)
  mild.upper.Thresh <- upperq + (iqr *1.5)
  extreme.lower.Thresh <- lowerq - (iqr *3)
  extreme.upper.Thresh <- upperq + (iqr *3)
  mild.lower.outliers <- x[,1][ which(x[,1] < mild.lower.Thresh) ]
  extreme.lower.outliers <- x[,1][ which(x[,1] < extreme.lower.Thresh) ]
  mild.upper.outliers <- x[,1][ which(x[,1] > mild.upper.Thresh) ]
  extreme.upper.outliers <- x[,1][ which(x[,1] > extreme.upper.Thresh) ]
  mild.upper.outliers.count <- length(mild.upper.outliers)
  mild.lower.outliers.count <- length(mild.lower.outliers)
  mild.outliers.count <- mild.upper.outliers.count + mild.lower.outliers.count
  extreme.upper.outliers.count <- length(extreme.upper.outliers)
  extreme.lower.outliers.count <- length(extreme.lower.outliers)
  extreme.outliers.count <- extreme.upper.outliers.count + extreme.lower.outliers.count 
  message1 <- print(paste0("\\# of Mild Outliers & ",mild.outliers.count))
  message2 <- print(paste0("\\# of Upper Mild Outliers & ",mild.upper.outliers.count))
  message3 <- print(paste0("\\# of Lower Mild Outliers & ",mild.lower.outliers.count))
  message4 <- print(paste0("\\# of Extreme Outliers & ",extreme.outliers.count))
  message2 <- print(paste0("\\# of Upper Extreme Outliers & ",extreme.upper.outliers.count))
  message3 <- print(paste0("\\# of Lower Extreme Outliers & ",extreme.lower.outliers.count))
}