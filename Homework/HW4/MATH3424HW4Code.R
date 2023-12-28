library(car)
#Q1a
q1data <- read.table("./Downloads/MATH3424HW4Data/Crude-Oil-Production.txt", header = TRUE, sep = ",")
q1data$logBarrels <- log(q1data$Barrels)
q1model <- lm(q1data$logBarrels ~ q1data$Year, data = q1data)
plot(1:29, rstudent(q1model), xlab = "Index" , ylab = "Standardized Residuals", main = "Standardized Residuals against Index")


#Q1b
library(lmtest)
dwtest(q1model)

#Q1c
count <- 1
for (i in c(2:29)) {
  if (q1model$residuals[i] * q1model$residuals[i-1] < 0) {
    count <- count + 1
  }
}
count
q1c1 <- length(which(q1model$residuals > 0))
q1c2 <- length(which(q1model$residuals < 0))
q1cmu <- 2 * q1c1 * q1c2 / (q1c1 + q1c2) + 1
q1cstd <- sqrt(2 * q1c1 * q1c2 * (2 * q1c1 * q1c2 - q1c1 - q1c2) / (q1c1 + q1c2)^2 / (q1c1 + q1c2 - 1))
q1cZscore <- (count - q1cmu) / q1cstd
q1cZscore
pnorm(q1cZscore)
#Q1d
hatrho <- sum(q1model$residuals[-1] * q1model$residuals[-29]) / sum(q1model$residuals^2)
hatrho
q1ddata <- data.frame(y = log(q1data$Barrels)[-1] - log(q1data$Barrels)[-29] * hatrho, x <- q1data$Year[-1] - q1data$Year[-29] * hatrho)
q1dmodel <- lm(y~x, data = q1ddata)
dwtest(q1dmodel)


#Q2
library(regclass)
q2data <- read.table("./Downloads/MATH3424HW4Data/Advertising.txt", header = TRUE)
q2ad <- q2data[-1,]
rownames(q2ad) <- 1:nrow(q2ad)
q2ad$S_.t.1 <- q2data$S_t[-22]
q2model1 <- lm(q2ad$S_t ~ q2ad$E_t + q2ad$A_t + q2ad$P_t + q2ad$A_.t.1., data = q2ad)
VIF(q2model1)
q2model2 <- lm(q2ad$S_t ~ q2ad$E_t + q2ad$A_t + q2ad$P_t + q2ad$S_.t.1, data = q2ad)
VIF(q2model2)
q2model3 <- lm(q2ad$S_t ~ q2ad$E_t + q2ad$A_t + q2ad$A_.t.1. + q2ad$S_.t.1, data = q2ad)
VIF(q2model3)
q2model4 <- lm(q2ad$S_t ~ q2ad$E_t + q2ad$P_t + q2ad$A_.t.1. + q2ad$S_.t.1, data = q2ad)
VIF(q2model4)

#### Q3a
normalize <- function(c) {
  return ((c - mean(c)) / sd(c))
}
q3data <- apply(q2data, 2, normalize)
q3y <- q3data[,1]
q3x <- q3data[,-1]

q3_vec1 <- c(0.000, 0.001, 0.003, 0.005, 0.007, 0.009)
q3_vec2<- seq(from=0.01, to=0.03, by=0.002)
q3_vec3 <- seq(from=0.04, to=0.09, by=0.01)
q3_vec4 <- seq(from=0.1, to=1, by=0.1)
q3_vec <- c(q3_vec1, q3_vec2, q3_vec3, q3_vec4)


q3_rec <- matrix(data=NA, nrow=length(q3_vec), ncol=5)
for (i in (1:length(q3_vec)))
{
  q3k <- q3_vec[i]
  q3_theta <- solve(t(q3x) %*% q3x + q3k*diag(5)) %*% t(q3x) %*% q3y
  q3_rec[i,] <- q3_theta
}
offset <- 0.05
colours = c("red", "green", "dark red", "blue", "purple")
legends = c("A_t", "P_t", "E_t", "A_.t.1.", "P_.t.1.")
plot(q3_vec, q3_rec[,1], col=colours[1], pch="o", xlab="k", ylab="theta_i(k)", ylim=c(-0.2,1))
text(q3_vec[1], q3_rec[1,1]+offset, legends[1])
for (i in (2:5))
{
  points(q3_vec, q3_rec[,i], col=colours[i], lty=1)
  lines(q3_vec, q3_rec[,i], col=colours[i], lty=1)
  text(q3_vec[1], q3_rec[1,i]+offset, legends[i])
}
#### Q3b
q3data <- sapply(q2data, normalize)
#q3bY <- q3data[,1]
#q3bX <- as.matrix(adv_data_new[,2:6])
q3bY <- as.matrix(q3data[,1])
q3bX <- as.matrix(q3data[,-1])
theta <- function(i) {
  return (solve(t(q3bX) %*% q3bX + i*diag(5)) %*% t(q3bX) %*% q3bY)
}
q3model <- lm(q3bY ~ ., data = as.data.frame(q3bX))
q3b_rhosq <- sum((q3model$residuals)^2)/ 16
q3b_k <- c()
q3b_k_1 <- 0
for (i in c(0:5)) {
  k <- 5 * q3b_rhosq / sum((theta(q3b_k_1)) ^ 2)
  q3b_k_1 <- k
  q3b_k <- c(q3b_k, k)
}
q3b_k
##OLS result
q3_sd_all <- apply(q2data[,-1], 2, sd)
q3_mean_all <- apply(q2data[,-1], 2, mean)
beta_1_to_j <- theta(q3b_k[6])*sd(q2data$S_t)/q3_sd_all 
beta_0 <- mean(q2data$S_t) - sum(q3_mean_all*beta_1_to_j) 
beta_original <- rbind(beta_0, beta_1_to_j)
beta_original
summary(lm(S_t ~ ., data=q2data))

###q4a
q4data <- read.table("./Downloads/MATH3424HW4Data/Gasoline-Consumption.txt", header= TRUE)
q4model <- lm(Y ~ . ,data = q4data)
summary(q4model)
###q4b
q4model1 <- lm(Y~q4data$X_1, data = q4data)
q4model2 <- lm(Y~q4data$X_.10., data = q4data)
q4model3 <- lm(Y ~ q4data$X_1 + q4data$X_.10., data = q4data)
q4model4 <- lm(Y ~ q4data$X_2 + q4data$X_.10., data = q4data)
q4model5 <- lm(Y ~ q4data$X_8 + q4data$X_.10., data = q4data)
q4model6 <- lm(Y ~ q4data$X_8 + q4data$X_5 + q4data$X_.10., data = q4data)
crit <- as.data.frame(matrix(data = NA, 4, 6), row.names=c("adj R^2", "Mallow's Cp", "AIC", "BIC"))
crit[1,] <- c(summary(q4model1)$adj.r.squared,
              summary(q4model2)$adj.r.squared,
              summary(q4model3)$adj.r.squared,
              summary(q4model4)$adj.r.squared,
              summary(q4model5)$adj.r.squared,
              summary(q4model6)$adj.r.squared)
crit[2,] <- c(ols_mallows_cp(q4model1, q4model),
              ols_mallows_cp(q4model2, q4model),
              ols_mallows_cp(q4model3, q4model),
              ols_mallows_cp(q4model4, q4model),
              ols_mallows_cp(q4model5, q4model),
              ols_mallows_cp(q4model6, q4model))
crit[3,] <- c(AIC(q4model1), 
              AIC(q4model2), 
              AIC(q4model3), 
              AIC(q4model4), 
              AIC(q4model5), 
              AIC(q4model6))
crit[4,] <- c(BIC(q4model1), 
              BIC(q4model2), 
              BIC(q4model3), 
              BIC(q4model4), 
              BIC(q4model5), 
              BIC(q4model6))
colnames(crit) <- c("modelA", "modelB", "modelC", "modelD", "modelE", "modelF")
crit
p <- c(2,2,3,3,3,4)
plot(p, crit[2, ], xlab="p", ylab="Cp", xlim=c(0,4), main="Cp versus p")
abline(0,1)
###q4c
pairs(q4data[,c(1,2,3,9,11)])
###q4d - X1 is selected
q4dX1 <-  q4data[,c(2,3,6,9,11)]
q4dSelected1 <- colnames(q4dX1)[1]
for( x in colnames(q4dX1)[-1]) {
  if(abs(cor(q4data$Y, q4data[x])) > abs(cor(q4data$Y, q4data[q4dSelected1]))) {
    q4dSelected1 <- x
  }
}
q4dmodel1 <- lm(as.formula(paste("Y~", q4dSelected1, sep="")), data = q4data)
summary(q4dmodel1)
###q4d - X5 is selected
q4dX2 <-  q4data[,c(3,6,9,11)]
q4dSelected2 <- colnames(q4dX2)[1]
for(x in colnames(q4dX2)[-1]) {
  if(abs(cor(q4dmodel1$residuals, q4data[x])) > abs(cor(q4dmodel1$residuals, q4data[q4dSelected2]))) {
    q4dSelected2 <- x
  }
}
q4dmodel2 <- lm(as.formula(paste("Y~X_1+", q4dSelected2, sep="")), data = q4data)
summary(q4dmodel2)

