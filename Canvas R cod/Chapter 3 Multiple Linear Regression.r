#################  Multiple Linear Regression
# So in this chapter, we will extend our current linear model to allow a response to depend on multiple predictors
# We use the autompg data downloadable from the UCI website
autompg = read.table(
  "http://archive.ics.uci.edu/ml/machine-learning-databases/auto-mpg/auto-mpg.data",
  quote = "\"",
  comment.char = "",
  stringsAsFactors = FALSE)                     # read the data from the web

colnames(autompg) = c("mpg", "cyl", "disp", "hp", "wt", "acc", "year", "origin", "name")   # give the dataframe new headers
autompg = subset(autompg, autompg$hp != "?")    # remove the missing data, which is stored as "?"
autompg = subset(autompg, autompg$name != "plymouth reliant")  # remove the plymouth reliant, as it causes some issues
rownames(autompg) = paste(autompg$cyl, "cylinder", autompg$year, autompg$name)  # give the dataset row names, based on the engine, year and name
autompg = subset(autompg, select = c("mpg", "cyl", "disp", "hp", "wt", "acc", "year"))  # remove the variable for name, as well as origin
autompg$hp = as.numeric(autompg$hp) # # change horsepower from character to numeric
str(autompg)  # check final structure of data


## fit a linear model with response mpg and predictors wt and year
mpg_model = lm(mpg ~ wt + year, data = autompg)
coef(mpg_model)
summary(mpg_model)



## Use matrix algebra to directly sovle mlr using the math formula
n = nrow(autompg)
p = length(coef(mpg_model))
X = cbind(rep(1, n), autompg$wt, autompg$year)
y = autompg$mpg

(beta_hat = solve(t(X) %*% X) %*% t(X) %*% y)    ## solve for the coefficient estimate

## standard error
summary(mpg_model)$sigma        ## use the fitted model
y_hat = X %*% solve(t(X) %*% X) %*% t(X) %*% y
e     = y - y_hat
sqrt(t(e) %*% e / (n - p))      ## use the math formula to derive the standard error



##  Test whether the parameters are zero or not
summary(mpg_model)$coef       # t-statistic and p-value are provided

confint(mpg_model, level = 0.99)  # 99% confidence interval for the coefficients

new_cars = data.frame(wt = c(3500, 5000), year = c(76, 81))
predict(mpg_model, newdata = new_cars, interval = "confidence", level = 0.99)     ## confidence interval of mean responses at new observations

plot(year ~ wt, data = autompg, pch = 20, col = "dodgerblue", cex = 1.5)
points(new_cars, col = "darkorange", cex = 3, pch = "X")

predict(mpg_model, newdata = new_cars, interval = "prediction", level = 0.99)     ## prediction interval at new observations



###### Goodness-of-fit test, significance of regression and F-test
null_mpg_model = lm(mpg ~ 1, data = autompg)
full_mpg_model = lm(mpg ~ wt + year, data = autompg)
anova(null_mpg_model, full_mpg_model)

sum((fitted(full_mpg_model) - fitted(null_mpg_model)) ^ 2)           # Sum of squares regression
sum(resid(full_mpg_model) ^ 2)                                       # Sum of squares error
sum(resid(null_mpg_model) ^ 2)                                       # Sum of squares total
length(coef(full_mpg_model)) - length(coef(null_mpg_model))          # Degrees of freedom regression
length(resid(full_mpg_model)) - length(coef(full_mpg_model))         # Degrees of freedom error
length(resid(null_mpg_model)) - length(coef(null_mpg_model))         # Degrees of freedom total









###################
###################              Simulations:    Y = 5 - 2x_1 + 6x_2 + error
###################
set.seed(1337)
n = 100 # sample size
p = 3
beta_0 = 5
beta_1 = -2
beta_2 = 6
sigma  = 4

x0 = rep(1, n)
x1 = sample(seq(1, 10, length = n))       # uniform sampling from 1, 2, 3, ..., 10
x2 = sample(seq(1, 10, length = n))
X = cbind(x0, x1, x2)
C = solve(t(X) %*% X)
eps      = rnorm(n, mean = 0, sd = sigma)
y        = beta_0 + beta_1 * x1 + beta_2 * x2 + eps
sim_data = data.frame(x1, x2, y)         # simulated data

(beta_hat = C %*% t(X) %*% y)            # solve directly by math formula


##### Now repeat the simulation for many times and check the distribution of the estimated coefficient
num_sims = 10000   # repeat form 10,000 times
beta_hat_2 = rep(0, num_sims)
for(i in 1:num_sims) {
  eps           = rnorm(n, mean = 0 , sd = sigma)
  sim_data$y    = beta_0 * x0 + beta_1 * x1 + beta_2 * x2 + eps
  fit           = lm(y ~ x1 + x2, data = sim_data)
  beta_hat_2[i] = coef(fit)[3]
}
mean(beta_hat_2)     # is close to the truth
beta_2

var(beta_hat_2)      # variance
sigma ^ 2 * C[2 + 1, 2 + 1]    # estimated variance of the the coefficient

##### check the distribution of the estimated coefficents
hist(beta_hat_2, prob = TRUE, breaks = 20, 
     xlab = expression(hat(beta)[2]), main = "", border = "dodgerblue")
curve(dnorm(x, mean = beta_2, sd = sqrt(sigma ^ 2 * C[2 + 1, 2 + 1])), 
      col = "darkorange", add = TRUE, lwd = 3)






############   Example on Lecture Slides -- Supervisor Dataset
supervisor_dat<-read.table('data/P060.txt',header=TRUE)   ## read the data
y<-supervisor_dat$Y
X<-as.matrix(supervisor_dat[,-1])
X<-cbind(rep(1,30),X)
colnames(X)<-c("Const.","X1","X2","X3","X4","X5","X6")
hat_beta <- solve(t(X) %*% X) %*% t(X) %*% y         ## LSE by direct computation using the formula
hat_y <- X %*% hat_beta
Rsquared <- sum((hat_y-mean(y))^2)/sum((y-mean(y))^2)
R2adj<-1-sum((hat_y-y)^2)/sum((y-mean(y))^2)*(n-1)/(n-p-1)
n<-30
p<-6
hat_sig2<-sum((hat_y-y)^2)/(n-p-1)

### use lm function to obtain the regression table
supervisor_model<-lm(Y ~ . , data = supervisor_dat)



#supervisor_center <- supervisor_dat
#supervisor_center$Y <- supervisor_center$Y-mean(supervisor_center$Y)

supervisor_center <- apply(supervisor_dat,2,function(x) x-mean(x))    ## center the variables
y_center<-supervisor_center[,1]
X_center<-as.matrix(supervisor_center[,-1])
X_center<-cbind(rep(1,30),X_center)
colnames(X_center)<-c("Const.","X1","X2","X3","X4","X5","X6")
hat_beta_center <- solve(t(X_center) %*% X_center) %*% t(X_center) %*% y_center         ## LSE by direct computation using the formula


supervisor_center_stand <- apply(supervisor_dat,2,function(x) (x-mean(x))/sd(x))    ## center and standardize the variables
y_center_stand<-supervisor_center_stand[,1]
X_center_stand<-as.matrix(supervisor_center_stand[,-1])
X_center_stand<-cbind(rep(1,30),X_center_stand)
colnames(X_center_stand)<-c("Const.","X1","X2","X3","X4","X5","X6")
hat_beta_center_stand <- solve(t(X_center_stand) %*% X_center_stand) %*% t(X_center_stand) %*% y_center_stand        ## LSE by direct computation using the formula
hat_y <- X_center_stand %*% hat_beta_center_stand
Rsquared <- sum((hat_y-mean(y_center_stand))^2)/sum((y_center_stand-mean(y_center_stand))^2)
R2adj<-1-sum((hat_y-y_center_stand)^2)/sum((y_center_stand-mean(y_center_stand))^2)*(n-1)/(n-p-1)

sd_all<-apply(supervisor_dat,2,sd)   ## find the column-wise standard deviation





##### Confidence intervals of regression coefficients
supervisor_dat<-read.table('data/P060.txt',header=TRUE)   ## read the data
fmodel<-lm(Y~.,data=supervisor_dat)
confint(fmodel,level=0.95)




######################## Test of Hypothesis for Linear Models --1
##  H0: Y ~ X_1 + X_2
##  H1: full model
supervisor_dat<-read.table('data/P060.txt',header=TRUE)   ## read the data
y<-supervisor_dat$Y
X<-as.matrix(supervisor_dat[,-1])
X<-cbind(rep(1,30),X)
colnames(X)<-c("Const.","X1","X2","X3","X4","X5","X6")
hat_beta <- solve(t(X) %*% X) %*% t(X) %*% y 
haty <- X %*% hat_beta
SSE_fm  <- sum((y-haty)^2)     ## SSE for full model
# now fit the reduced model
X1<-X[,c(1,2,3)]
hat_beta_rm <- solve(t(X1) %*% X1) %*% t(X1) %*% y
haty_rm <- X1 %*% hat_beta_rm
SSE_rm <- sum((y-haty_rm)^2)
n<-30;p<-6;k<-3;
F_val <- (SSE_rm-SSE_fm)/SSE_fm*(n-p-1)/(p+1-k)

### automatically using anova
full_mod <- lm(Y~., data=supervisor_dat)
red_mod <- lm(Y~X1+X2, data=supervisor_dat)
anova(red_mod,full_mod)






######################## Test of Hypothesis for Linear Models --2
##  H0: Y = beta_0
##  H1: full model
supervisor_dat<-read.table('data/P060.txt',header=TRUE)   ## read the data
y<-supervisor_dat$Y
X<-as.matrix(supervisor_dat[,-1])
X<-cbind(rep(1,30),X)
colnames(X)<-c("Const.","X1","X2","X3","X4","X5","X6")
hat_beta <- solve(t(X) %*% X) %*% t(X) %*% y 
haty <- X %*% hat_beta
SSE_fm  <- sum((y-haty)^2)     ## SSE for full model
# now fit the reduced model
X1<-X[,1]
hat_beta_rm <- solve(t(X1) %*% X1) %*% t(X1) %*% y
haty_rm <- X1 %*% hat_beta_rm
SSE_rm <- sum((y-haty_rm)^2)
n<-30;p<-6;k<-1;
F_val <- (SSE_rm-SSE_fm)/SSE_fm*(n-p-1)/(p+1-k)

### automatically using anova
full_mod <- lm(Y~., data=supervisor_dat)
red_mod <- lm(Y~1, data=supervisor_dat)
anova(red_mod,full_mod)






###########################################  Example of Table 3.8 on lecture slides
##  H0: Y = beta_0
##  H1: Y ~ 1+X1+X3
########## obtain the anova table by direct computation
supervisor_dat<-read.table('data/P060.txt',header=TRUE)   ## read the data
y<-supervisor_dat$Y
X<-as.matrix(supervisor_dat[,c(2,4)])
X<-cbind(rep(1,30),X)
colnames(X)<-c("Const.","X1","X3")
hat_beta <- solve(t(X) %*% X) %*% t(X) %*% y 
haty <- X %*% hat_beta
SSE <- sum((y-haty)^2)
SST <- sum((y-mean(y))^2)
SSR <- SST-SSE
Fval<-SSR/SSE*27/2

### automaticlally obtain the anova table
red_mod<-lm(Y~X1+X3, data=supervisor_dat)
null_mod<-lm(Y~1,data=supervisor_dat)
anova(null_mod,red_mod)




#####################################  Testing (3.57) on lecture slides
## H_0: Y=beta0+beta1 X1 + beta1 X3 +eps
## H_1: Y=beta0+beta1 X1 + beta2 X2 + eps
supervisor_dat<-read.table('data/P060.txt',header=TRUE)   ## read the data
y<-supervisor_dat$Y
X<-as.matrix(supervisor_dat[,c(2,4)])
X<-cbind(rep(1,30),X)
colnames(X)<-c("Const.","X1","X3")
hat_beta_fm <- solve(t(X) %*% X) %*% t(X) %*% y 
haty_fm <- X %*% hat_beta_fm
SSE_fm <- sum((y-haty_fm)^2)

Xrm <- X[,2]+X[,3]
Xrm <- cbind(rep(1,30),Xrm)
colnames(Xrm) <- c("Const.","X1+X3")
hat_beta_rm <- solve(t(Xrm) %*% Xrm) %*% t(Xrm) %*% y 
haty_rm <- Xrm %*% hat_beta_rm
SSE_rm <- sum((y-haty_rm)^2)

n<-30
p<-2
k<-2
Fval <- (SSE_rm-SSE_fm)/SSE_fm*(n-p-1)/(p+1-k)








#####################################  Testing (3.61) on lecture slides
## H_0: Y=beta0+beta1 X1 + beta1 X3 +eps
## H_1: full model
supervisor_dat<-read.table('data/P060.txt',header=TRUE)   ## read the data
y<-supervisor_dat$Y
X<-as.matrix(supervisor_dat[,-1])
X<-cbind(rep(1,30),X)
colnames(X)<-c("Const.","X1","X2","X3","X4","X5","X6")
hat_beta_fm <- solve(t(X) %*% X) %*% t(X) %*% y 
haty_fm <- X %*% hat_beta_fm
SSE_fm <- sum((y-haty_fm)^2)

Xrm <- X[,2]+X[,4]
Xrm <- cbind(rep(1,30),Xrm)
colnames(Xrm) <- c("Const.","X1+X3")
hat_beta_rm <- solve(t(Xrm) %*% Xrm) %*% t(Xrm) %*% y 
haty_rm <- Xrm %*% hat_beta_rm
SSE_rm <- sum((y-haty_rm)^2)

n<-30
p<-6
k<-2
Fval <- (SSE_rm-SSE_fm)/SSE_fm*(n-p-1)/(p+1-k)


##### use Rsquared to obtain the results
fmodel<-lm(Y~.,data=supervisor_dat)
rmodel<-lm(y~.,data=as.data.frame(cbind(y,Xrm[,-1]),col.names = c("Y","X1+X3")))
fm_r2<-summary(fmodel)$r.squared
rm_r2<-summary(rmodel)$r.squared
Fval<-(fm_r2-rm_r2)/(1-fm_r2)*(n-p-1)/(p+1-k)




########  Do prediction for Supervisor dataset 
## at a new observation x0=(80,42,37,63,51,48)
## we use the full model
##  We begin with direct computation by using the formulas
supervisor_dat<-read.table('data/P060.txt',header=TRUE)   ## read the data
n<-30
p<-6
y<-supervisor_dat$Y
X<-as.matrix(supervisor_dat[,-1])
X<-cbind(rep(1,30),X)
colnames(X)<-c("Const.","X1","X2","X3","X4","X5","X6")
hat_beta_fm <- solve(t(X) %*% X) %*% t(X) %*% y 
haty_fm <- X %*% hat_beta_fm
SSE_fm <- sum((y-haty_fm)^2)
hat_sigma<- sqrt(SSE_fm/(n-p-1))

x0<-c(1,80,42,37,63,51,48)  ## at a new observation
haty0<-sum(x0*hat_beta_fm)    ## point prediction
se_y0 <- hat_sigma*sqrt(1+t(x0) %*% solve(t(X)%*% X) %*% x0)  ## standard error
crt_val <- qt(1-0.025,n-p-1)    ## critical value at alpha=0.05
pred_interval <-c(haty0-crt_val*se_y0, haty0+crt_val*se_y0)    ## prediction interval
hatmu0<-sum(x0*hat_beta_fm)    ## point prediction
se_mu0 <- hat_sigma*sqrt(t(x0) %*% solve(t(X)%*% X) %*% x0)  ## standard error
conf_interval <-c(hatmu0-crt_val*se_mu0, hatmu0+crt_val*se_mu0)  ## confidence interval


###### by using built-in function
fmodel<-lm(Y~.,data=supervisor_dat)     ## fit a full model
new_obs <- data.frame(X1=80, X2=42,X3=37,X4=63,X5=51,X6=48)   ## create the new observation x0
predict(fmodel,newdata=new_obs,interval="prediction",level=0.95)   ## output the prediction interval
predict(fmodel,newdata=new_obs,interval="confidence",level=0.95)   ## output the confidence interval

