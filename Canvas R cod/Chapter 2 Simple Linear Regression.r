############### Simple linear regression
#  Let’s consider a simple example of how the speed of a car affects its stopping distance, that is, how far it travels 
#  before it comes to a stop. To examine this relationship, we will use the cars dataset which, is a default R dataset. 
#  Thus, we don’t need to load a package first; it is immediately available

View(cars)
names(cars)      ## the dataframe has two variables, i.e., two columns

### scatter plot to check the linearity
plot(dist ~ speed, data = cars,
     xlab = "Speed (in Miles Per Hour)",
     ylab = "Stopping Distance (in Feet)",
     main = "Stopping Distance vs Speed",
     pch  = 20,
     cex  = 2,
     col  = "grey")

### Least squares estimator by implementing the math formula
x = cars$speed
y = cars$dist
Sxy = sum((x - mean(x)) * (y - mean(y)))
Sxx = sum((x - mean(x)) ^ 2)
Syy = sum((y - mean(y)) ^ 2)
c(Sxy, Sxx, Syy)
beta_1_hat = Sxy / Sxx
beta_0_hat = mean(y) - beta_1_hat * mean(x)
c(beta_0_hat, beta_1_hat)                        # the estimated beta_0 and beta_1 from calculation

### make predictions
# We can now use this line to make predictions. First, let’s see the possible x values in the cars dataset. Since some x
# values may appear more than once, we use the unique() to return each unique value only once.
unique(cars$speed)
beta_0_hat + beta_1_hat * 8     # Let’s make a prediction for the stopping distance of a car traveling at 8 miles per hour.

# Now let’s make a prediction for the stopping distance of a car traveling at 21 miles per hour. This is considered interpolation as 21 is not an observed 
# value of x. (But is in the data range.) We can use the special %in% operator to quickly verify this in R.
8 %in% unique(cars$speed)
21 %in% unique(cars$speed)
min(cars$speed) < 21 & 21 < max(cars$speed)
beta_0_hat + beta_1_hat * 21        ## make the prediction
range(cars$speed)                   ## the range of data


## Residual at x=8     ---- still based on direct calcualtion by the math formula
cars[which(cars$speed == 8), ]
16 - (beta_0_hat + beta_1_hat * 8)          ## calculate the residual

## Variance estimation    ---- still based on direct calcualtion by the math formula
y_hat = beta_0_hat + beta_1_hat * x
e     = y - y_hat
n     = length(e)
s2_e  = sum(e^2) / (n - 2)
s2_e
sqrt(s2_e)         ## standard error as an estimate of the error standard deviation


## Decomposition of variances   ---- still based on direct calcualtion by the math formula
SST   = sum((y - mean(y)) ^ 2)                  # Sum of squares total
SSReg = sum((y_hat - mean(y)) ^ 2)              # Sum of squares regression
SSE   = sum((y - y_hat) ^ 2)                    # Sum of squares error
c(SST = SST, SSReg = SSReg, SSE = SSE)
R2 = SSReg / SST
R2




########### Now we automate the above procedure using the lm package
stop_dist_model = lm(dist ~ speed, data = cars)       # lm means linear model, y ~ x
stop_dist_model      # output the result of the simple linear regression
summary(stop_dist_model)  #display the point estimate, t-statistics, p-value, R-squared Adjusted R-squared, F-statistic
names(stop_dist_model)    #check which results are stored in the fitted model

##  scatter plot and display the linear fit
plot(dist ~ speed, data = cars,
     xlab = "Speed (in Miles Per Hour)",
     ylab = "Stopping Distance (in Feet)",
     main = "Stopping Distance vs Speed",
     pch  = 20,
     cex  = 2,
     col  = "grey")
abline(stop_dist_model, lwd = 3, col = "darkorange")

stop_dist_model$residuals       ## check the residuals
coef(stop_dist_model)           ## extract the estimated coefficients
resid(stop_dist_model)          ## extract the residuals
fitted(stop_dist_model)         ## extract the fitted value
summary(stop_dist_model)$r.squared   ## exact the R-squared
summary(stop_dist_model)$sigma       ## extract the standard error, ie., the estimated noise standard deviation
predict(stop_dist_model, newdata = data.frame(speed = 8))      ## prediction at a new point with speed=8
predict(stop_dist_model, newdata = data.frame(speed = c(8, 21, 50)))   ## simultaneuously predict at the points 8, 21, 50
predict(stop_dist_model)        ## which also outputs the fitted values



#######  Now let us do the inference and hypothesis testing
## We first recall the fitted model as above
stop_dist_model = lm(dist ~ speed, data = cars)
summary(stop_dist_model)

# Last chapter, we only discussed the Estimate, Residual standard error, and Multiple R-squared values. In this chapter, we will discuss all of the 
# information under Coefficients as well as F-statistic.
summary(stop_dist_model)$coefficients[1,]
summary(stop_dist_model)$coefficients[2,]  # the t-statistic and p-value for the coefficient

confint(stop_dist_model, level = 0.99)     # 99% confidence interval of the coefficients
confint(stop_dist_model, level = 0.95)     # 95% confidence interval of the coefficients
confint(stop_dist_model, level = 0.99)[1, 1]   # extract the row 1 and column 1 entry from the matrix output from confint(stop_dist_model, level = 0.99)
confint(stop_dist_model, level = 0.99)[2, 1]


# We can also construct the confidence intervals directly from the math formula
beta_1_hat = coef(stop_dist_model)[2]    # store the estimate
beta_1_hat_se = summary(stop_dist_model)$coefficients[2, 2]  # store the standard error
crit = qt(0.995, df = length(resid(stop_dist_model)) - 2)    # get the critical value
c(beta_1_hat - crit * beta_1_hat_se, beta_1_hat + crit * beta_1_hat_se)  # [est-margin,  est+margin]


# Confidence interval for the mean response at speed=5 or 21
new_speeds = data.frame(speed = c(5, 21))
predict(stop_dist_model, newdata = new_speeds, 
        interval = c("confidence"), level = 0.99)      ## 99% confidence interval for the mean response



# Prediction interval at a new observation
predict(stop_dist_model, newdata = new_speeds, 
        interval = c("prediction"), level = 0.99)      ## 99% prediction interval at speed=5 or 21





#####  Confidence and Prediction Bands 
speed_grid = seq(min(cars$speed), max(cars$speed), by = 0.01)
dist_ci_band = predict(stop_dist_model, 
                       newdata = data.frame(speed = speed_grid), 
                       interval = "confidence", level = 0.99)
dist_pi_band = predict(stop_dist_model, 
                       newdata = data.frame(speed = speed_grid), 
                       interval = "prediction", level = 0.99) 

plot(dist ~ speed, data = cars,
     xlab = "Speed (in Miles Per Hour)",
     ylab = "Stopping Distance (in Feet)",
     main = "Stopping Distance vs Speed",
     pch  = 20,
     cex  = 2,
     col  = "grey",
     ylim = c(min(dist_pi_band), max(dist_pi_band)))
abline(stop_dist_model, lwd = 5, col = "darkorange")

lines(speed_grid, dist_ci_band[,"lwr"], col = "dodgerblue", lwd = 3, lty = 2)
lines(speed_grid, dist_ci_band[,"upr"], col = "dodgerblue", lwd = 3, lty = 2)
lines(speed_grid, dist_pi_band[,"lwr"], col = "dodgerblue", lwd = 3, lty = 3)
lines(speed_grid, dist_pi_band[,"upr"], col = "dodgerblue", lwd = 3, lty = 3)
points(mean(cars$speed), mean(cars$dist), pch = "+", cex = 3)





#### Goodness-of-fit test or significance of regression,  F-test
summary(stop_dist_model)        # The F-statistic and P-value is automatically provided 
anova(stop_dist_model)          # Provide the ANOVA talbe


# Note that there is another equivalent way to do this in R, which we will return to often to compare two models.
anova(lm(dist ~ 1, data = cars), lm(dist ~ speed, data = cars))








###################
###################              Simulations:    Y = 3 + 6x_1 + error
###################
set.seed(42)
sample_size = 100 # this is n
x = seq(-1, 1, length = sample_size)
Sxx = sum((x - mean(x)) ^ 2)

beta_0 = 3
beta_1 = 6
sigma  = 2

num_samples = 10000                   ## repeat it for 10,000 times 
beta_0_hats = rep(0, num_samples)
beta_1_hats = rep(0, num_samples)

for (i in 1:num_samples) {
        eps = rnorm(sample_size, mean = 0, sd = sigma)
        y   = beta_0 + beta_1 * x + eps
        
        sim_model = lm(y ~ x)
        
        beta_0_hats[i] = coef(sim_model)[1]
        beta_1_hats[i] = coef(sim_model)[2]
}
mean(beta_1_hats)     # empirical mean
var(beta_1_hats)      # empirical variance

#### draw the histogram for the empirical distribution of the estimated coefficient
hist(beta_1_hats, prob = TRUE, breaks = 20, 
     xlab = expression(hat(beta)[1]), main = "", border = "dodgerblue")
curve(dnorm(x, mean = beta_1, sd = sqrt(var_beta_1_hat)), 
      col = "darkorange", add = TRUE, lwd = 3)












