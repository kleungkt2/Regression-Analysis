## Problem 1
#(a)
df <- read.csv("Crude-Oil-Production.txt", header = TRUE)
m_simple <- lm(log(Barrels)~Year,df)
n <- nrow(df)
res <- m_simple$residuals
plot(c(1:n),res)
abline(h=0, col="red")
#(b)
d <- sum((res[-1]-res[-n])^2)/sum(res^2)
#(c)
n1 <- sum(res>0)
n2 <- sum(res<0)
mu <- 2*n1*n2/(n1+n2)+1
sigma2 <- 2*n1*n2*(2*n1*n2-n1-n2)/((n1+n2)^2*(n1+n2-1))
(5-mu)/sqrt(sigma2)
#(d)
rho <- sum(res[-1]*res[-n])/sum(res^2)
Xstar <- df$Year[-1]-rho*df$Year[-n]
Ystar <- log(df$Barrels[-1])-rho*log(df$Barrels[-n])
m_1 <- lm(Ystar~Xstar)
n_1 <- length(Xstar)
res_1 <- m_1$residuals
d_1 <- sum((res_1[-1]-res_1[-n_1])^2)/sum(res_1^2)


## Problem 3
#(a)
df <- read.table("Advertising.txt", header = TRUE)
df_scale <- as.data.frame(scale(df))
X <- as.matrix(df_scale[,-1])
Y <- as.matrix(df_scale[,1])
n <- length(Y);p <- 5
k_seq <- seq(0,1,length.out = 101)
theta_res <- matrix(0, length(k_seq), p)
for (i in 1:101){
  theta_res[i,]<-solve(t(X)%*%X/(n-1)+k_seq[i]*diag(p))%*%(t(X)%*%Y/(n-1))
}
theta_res <- cbind(k_seq,theta_res)
plot(theta_res[,1],theta_res[,2], type = "l", col="blue", 
     ylim = c(0,1),xlab = "bias parameter k", ylab = "theta(k)")
lines(theta_res[,1],theta_res[,3], type = "l", col="red")
lines(theta_res[,1],theta_res[,4], type = "l", col="green")
lines(theta_res[,1],theta_res[,5], type = "l", col="purple")
lines(theta_res[,1],theta_res[,6], type = "l", col="yellow")
legend("topright", inset=.05, title="predictor",
       c("theta_1","theta_2","theta_3","theta_4","theta_5"), 
       fill=c("blue","red","green","purple","yellow"), horiz=FALSE)
#(b)
m_scale <- lm(S_t~.,df_scale)
summary(m_scale)
k1 <- p*sum((m_scale$residuals)^2)/(n-p-1)/sum((m_scale$coefficients[-1])^2)
theta_new<-solve(t(X)%*%X/(n-1)+k1*diag(p))%*%(t(X)%*%Y/(n-1))
k2 <- p*sum((m_scale$residuals)^2)/(n-p-1)/sum(theta_new^2)
theta_new<-solve(t(X)%*%X/(n-1)+k2*diag(p))%*%(t(X)%*%Y/(n-1))
k3 <- p*sum((m_scale$residuals)^2)/(n-p-1)/sum(theta_new^2)
theta_new<-solve(t(X)%*%X/(n-1)+k3*diag(p))%*%(t(X)%*%Y/(n-1))
k4 <- p*sum((m_scale$residuals)^2)/(n-p-1)/sum(theta_new^2)




