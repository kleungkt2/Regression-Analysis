# Set working directory
setwd('/Users/xinwei/math3424_data/')

# Input the dataset
data <- read.table('multiple.dat')
# dimension of data
dim(data)

# calculate some descriptive statistics
# (e.g., mean, mode, median, variance, standard deviation, skewness, kurtosis, etc)
y = data$V1

mean(y)

getmode <- function(v) {
    uniqv <- unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
}
getmode(y)

median(y)
var(y)
sd(y)

# install R packages
install.packages('moments')
# import R packages
library(moments)
# or
require(moments)

skewness(y)
kurtosis(y)

# histogram
hist(y, breaks=30)

# Fit a linear regression model
reg = lm(V1 ~ V2 + V3 + V4, data)
summary(reg)
