library(olsrr)
library(car)
library(dplyr)
library(latex2exp)
pres_data <- read.table("~/Downloads/Presidential Election Data.txt", header = TRUE)
n <- dim(pres_data)[1]

#q2
pres_data %>% filter(D == -1)
pres_data %>% filter(D == 0)
pres_data %>% filter(D == 1)
GI <- pres_data$G * pres_data$I
pres_data$GI <- GI
GD <- pres_data$G * pres_data$D
pres_data$GD <- GD
PD <- pres_data$P * pres_data$D
pres_data$PD <- PD
ND <- pres_data$N * pres_data$D
pres_data$ND <- ND

model_q2_full <- lm(V~I+D+W+GI+P+N, data = pres_data)
summary(model_q2_full)

model_q2_1 <- lm(V~I+D+W+GI+P+N, data = pres_data %>% filter(D == -1))
summary(model_q2_1)
model_q2_2 <- lm(V~I+D+W+GI+P+N, data = pres_data %>% filter(D == 0))
summary(model_q2_2)
model_q2_3 <- lm(V~I+D+W+GI+P+N, data = pres_data %>% filter(D == 1))
summary(model_q2_3)
model_q2_d1 <- lm(V~D+G+GD+P+PD+N+ND, data = pres_data)
summary(model_q2_d1)
model_q2_d2 <- lm(V~G+GD+N+ND, data = pres_data)
summary(model_q2_d2)

#q3
pres_data$D1 <- rep(0,n)
pres_data$D2 <- rep(0,n)
pres_data$D1[which(pres_data$D==1)] <- 1
pres_data$D2[which(pres_data$D==-1)] <- 1

model_q3_full <- lm(V~I+D1+D2+W+GI+P+N, data = pres_data)
summary(model_q3_full)
model_q3_1 <- lm(V~I+D1+D2+W+GI+P+N, data = pres_data %>% filter(D == -1))
summary(model_q3_1)
model_q3_2 <- lm(V~I+D1+D2+W+GI+P+N, data = pres_data %>% filter(D == 0))
summary(model_q3_2)
model_q3_3 <- lm(V~I+D1+D2+W+GI+P+N, data = pres_data %>% filter(D == 1))
summary(model_q3_3)

pres_data$D1_minus_D2 <- pres_data$D1 - pres_data$D2
model_q3_reduced <- lm(V~I+D1_minus_D2+W+GI+P+N, data = pres_data)
anova(model_q3_reduced, model_q3_full)

#q5
pres_data$Y <- log(pres_data$V / (1-pres_data$V))
model_q5_1 <- lm(Y~I+D+W+GI+P+N, data = pres_data)
summary(model_q5_1)
plot(as.numeric(rownames(pres_data)), rstudent(model_q5_1),
     ylab = "Standardized residuals", xlab = "Index", main = "Using Y - Residual plot(Q5)")
plot(as.numeric(rownames(pres_data)), rstudent(model_q2_full),
     ylab = "Standardized residuals", xlab = "Index", main = "Using V - Residual Plot(Q5)")
qqPlot(rstudent(model_q5_1), main = "Using Y - QQ plot(Q5)")
qqPlot(rstudent(model_q2_full), main = "Using V - QQ plot(Q5)")

#q6
model_q6_1 <- lm(Y~I+D1+D2+W+GI+P+N, data = pres_data)
summary(model_q6_1)
plot(as.numeric(rownames(pres_data)), rstudent(model_q6_1),
     ylab = "Standardized residuals", xlab = "Index", main = "Using Y - Residual plot(Q6)")
plot(as.numeric(rownames(pres_data)), rstudent(model_q3_full),
     ylab = "Standardized residuals", xlab = "Index", main = "Using V - Residual plot(Q6)")
qqPlot(rstudent(model_q6_1), main = "Using Y - QQ plot(Q6)")
qqPlot(rstudent(model_q3_full), main = "Using V - QQ plot(Q6)")



