## Q1
x = cars$speed # Define x
y = cars$dist  # Define y
## Q2
Sxy = sum((x-mean(x)) * (y-mean(y)))
Sxx = sum((x-mean(x)) ^ 2)
Syy = sum((y-mean(y)) ^ 2)
beta_1_hat = Sxy/Sxx
beta_0_hat = mean(y) - beta_1_hat * mean(x)
beta_hat = c(beta_0_hat,beta_1_hat)
## Q3
y.hat = beta_0_hat + beta_1_hat * x
sigma2_hat <- sum((y.hat-y)^2) / (length(y) - 2)
## Q4
R2 <- 1 - sum((y.hat-y)^2) / Syy
## Q5
X = cbind(1,x)
## Q6
XtX = t(X)%*% X
Xty = t(X)%*% y
beta.hat  = solve(XtX) %*%Xty
beta.hat  = as.numeric(beta.hat) 
all(beta_hat==beta.hat)
all.equal(beta_hat,beta.hat,
          tolerance = 1.5e-8)