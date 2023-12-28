############           Ways to graph and summarize data
library(ggplot2)  ## load the library
mean(mpg$cty)     ## sample mean of a vector
median(mpg$cty)   ## sample median of a vector
var(mpg$cty)      ## sample variance of a vector
sd(mpg$cty)       ## sample standard deviation of a vector
min(mpg$cty)
max(mpg$cty)
range(mpg$cty)



## For categorical variables, counts and percentages can be used for summary.
table(mpg$drv)
table(mpg$drv) / nrow(mpg)   ## relative frequency


## histograms for a numerical variable
hist(mpg$cty)

hist(mpg$cty,
     xlab   = "Miles Per Gallon (City)",
     main   = "Histogram of MPG (City)",
     breaks = 12,
     col    = "dodgerblue",
     border = "darkorange")





## bar plots for a categorical variable
barplot(table(mpg$drv))

barplot(table(mpg$drv),
        xlab   = "Drivetrain (f = FWD, r = RWD, 4 = 4WD)",
        ylab   = "Frequency",
        main   = "Drivetrains",
        col    = "dodgerblue",
        border = "darkorange")



#### Box Plots
unique(mpg$drv)    ## returns the unique values of a variable
boxplot(mpg$hwy)

#However, more often we will use boxplots to compare a numerical variable for different values of a categorical variable.
boxplot(hwy ~ drv, data = mpg)

boxplot(hwy ~ drv, data = mpg,
        xlab   = "Drivetrain (f = FWD, r = RWD, 4 = 4WD)",
        ylab   = "Miles Per Gallon (Highway)",
        main   = "MPG (Highway) vs Drivetrain",
        pch    = 20,
        cex    = 2,
        col    = "darkorange",
        border = "dodgerblue")



##### Scatter plot
plot(hwy ~ displ, data = mpg)

plot(hwy ~ displ, data = mpg,
     xlab = "Engine Displacement (in Liters)",
     ylab = "Miles Per Gallon (Highway)",
     main = "MPG (Highway) vs Engine Displacement",
     pch  = 20,
     cex  = 2,
     col  = "dodgerblue")









##############    Basics of Probability and Statistics
##### We typically want to know one of four things:
#     (1) The density (pdf) at a particular value.
#     (2) The distribution (cdf) at a particular value.
#     (3) The quantile value corresponding to a particular probability.
#     (4) A random draw of values from a particular distribution.

##### The general naming structure of the relevant R functions is:
#     (1) dname: calculates density (pdf) at input x.
#     (2) pname: calculates distribution (cdf) at input x.
#     (3) qname calculates the quantile at an input probability.
#     (4) rname generates a random draw from a particular distribution.
#     where the name represents the name of a given distribution, like 'norm' for normal distribution, 'binom' for binomial distribution
#     't' for t distribution, 'pois' for poisson distribution, 'chisq' for chi-squared distribution

dnorm(x = 3, mean = 2, sd = 5)    # the pdf at x=3 for N(2, 25)
pnorm(q = 3, mean = 2, sd = 5)    # the cdf at x=3 for N(2, 25)
qnorm(p=0.975, mean=2, sd = 5)    # the quantile for probability 0.975
rnorm(n = 10, mean = 2, sd = 5)   # generate a random sample of 10 of N(2, 25)

dbinom(x=6, size=10,prob=0.75)




####### One-sample T test
capt_crisp = data.frame(weight = c(15.5, 16.2, 16.1, 15.8, 15.6, 16.0, 15.8, 15.9, 16.2))
n <- length(capt_crisp$weight)
x_bar = mean(capt_crisp$weight)
s     = sd(capt_crisp$weight)
mu_0  = 16
t = (x_bar - mu_0) / (s / sqrt(n))    ### t-statistic
pt(t, df = n - 1)                     ### p-value

## we can do the above test in one command, one-sided test
t.test(x = capt_crisp$weight, mu = 16, alternative = c("less"), conf.level = 0.95)

## Since the test was one-sided, R returned a one-sided confidence interval. If instead we wanted a two-sided interval for 
## the mean weight of boxes of Captain Crisp cereal we could modify our code.
capt_test_results = t.test(capt_crisp$weight, mu = 16,
                           alternative = c("two.sided"), conf.level = 0.95)
names(capt_test_results)












