# Set working directory
setwd('/Users/xinwei/math3424_data/')

# Input the dataset
data <- read.table('multiple.dat')
summary(data)

# Fit a linear regression model
lm_full = lm(V1 ~ V2 + V3 + V4, data = data)
summary(lm_full)

##################### With known parameters ##################### 
## Given \beta_1 = 1
## Model: y - \beta_1 * x_1 = \beta_0 + \beta_2 * x_2 + \beta_3 * x_3
### Transform the response
y1 = data$V1 - 1 * data$V2
data$y1 = y1
### Regression
lm1 = lm(y1 ~ V3 + V4, data = data)
summary(lm1)


##################### Model selection ##################### 
# Sequential variable selection
# step(): AIC based
## Backward selection
backward <- step(lm_full, trace = 1)
formula(backward)
summary(backward)
## Forward selection
lm_null <- lm(V1 ~ 1, data = data)
forward <- step(lm_null, scope = list(upper = formula(lm_full)), 
                direction = "forward", trace = 1)
formula(forward)
summary(forward)

## Packages: olsrr, leaps, etc.


##################### Categorical variables ##################### 
########### One-way ANOVA ########### 
data1 <- read.table('oneway.dat')
data1$V1 <- as.factor(data1$V1)
aov1 = aov(V2 ~ V1, data = data1)
summary(aov1)
aov1 = aov(V3 ~ V1, data = data1)
summary(aov1)

########### Two-way ANOVA ########### 
data2 <- read.csv('twoway.csv', header = F)
## Transform the categorical variables into factors
data2$V1 <- as.factor(data2$V1)
data2$V2 <- as.factor(data2$V2)
aov2 = aov(V3 ~ V1 + V2 + V1*V2, data = data2)
summary(aov2)


########### Logistic regression ###########
## Simulate data
library(mvtnorm)
# set random seed
set.seed(1)
# prior P(y=1)
p <- 0.4
# dimension
d <- 4
# sample size
n <- 5000
# generate response
y <- c(rep(1, p*n), rep(0, (1-p)*n))
# generate x from x|y=1~N(mu1, sigma1), x|y=0~N(mu2, sigma2)
x <- matrix(nrow = n, ncol = d)
mu1 <- c(rep(1, d/2), rep(0, d/2))
mu2 <- rep(0, d)
sigma1 <- diag(d)
sigma2 <- diag(d)
x[1:(p*n),] <- rmvnorm(p*n, mean = mu1, sigma = sigma1)
x[-(1:(p*n)),] <- rmvnorm((1-p)*n, mean = mu2, sigma = sigma2)
# True coefficient: (1,1,0,0)

# optional: construct a dataset
data_binary <- data.frame(y,x)

# Fit a Logistic regression model
logistic <- glm(y ~ x, family = binomial)
# or
logistic <- glm(y ~ ., family = binomial, data = data_binary)
summary(logistic)

