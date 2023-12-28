## Problem 2
df <- read.table("Examination_Data.txt", header = TRUE)
m1 <- lm(F~P1,df)
m2 <- lm(F~P2,df)
m3 <- lm(F~P1+P2,df)
summary(m1)
summary(m2)
summary(m3)
predict(m3, newdata = data.frame(P1=78,P2=85), interval = "prediction", level = 0.95)

## Problem 4
df <- read.table("Supervisor.txt", header = T)
fm_a <- lm(Y~X1+X3,df)
fm_b <- lm(Y~X1+X2+X3,df)
df_r<- df
df_r["Y"]=df_r["Y"]-0.5*df_r["X1"]-0.5*df_r["X3"]
rm_a <- lm(Y~1,df_r)
rm_b <- lm(Y~X2,df_r)
anova(rm_a,fm_a)
anova(rm_b,fm_b)

## Problem 7
df <- read.table("Computer_Repair.txt", header = T)
m <- lm(Minutes~Units, df)
summary(m)
plot(m)
leverage <- function(X){
  pii <- diag(X%*%solve(t(X)%*%X)%*%t(X))
  return(pii)
}
X <- as.matrix(cbind(1,df[,1]))
p <- leverage(X)
std_residuals <- m$residuals/(summary(m)$sigma*sqrt(1-p))
plot(df[,1], m$residuals)
plot(fitted(m), m$residuals)
plot(df[,1], std_residuals)
plot(fitted(m), std_residuals)


## Problem 10
df <- read.table("Examination_Data.txt", header = TRUE)
m1 <- lm(F~P1,df)
m2 <- lm(F~P2,df)
m3 <- lm(F~P1+P2,df)
X1 <- cbind(rep(1,22),df[,2])
X2 <- cbind(rep(1,22),df[,3])
X3 <- as.matrix(cbind(rep(1,22),df[,c(2,3)]))
p1 <- leverage(X1)
d1 <- m1$residuals/(sqrt(sum(m1$residuals^2)))
res_func <- 2/(1-p1)*d1^2/(1-d1^2)
plot(res_func,p1/(1-p1))

p2 <- leverage(X2)
d2 <- m2$residuals/(sqrt(sum(m2$residuals^2)))
res_func <- 2/(1-p2)*d2^2/(1-d2^2)
plot(res_func,p2/(1-p2))

p3 <- leverage(X3)
d3 <- m3$residuals/(sqrt(sum(m3$residuals^2)))
res_func <- 2/(1-p3)*d3^2/(1-d3^2)
plot(res_func,p3/(1-p3))
