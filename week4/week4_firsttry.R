boxplot(BN1, names = c('BN1','BN2'), at = 1, xlim = c(0.5, 2.5), 
        ylim = range(c(BN1,BN2)))
boxplot(BN2, at = 2, add = TRUE)