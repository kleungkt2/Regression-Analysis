#MATH3424HW5
#Q1a
q1data <- read.table("./Downloads/BreastCancer.txt", header = TRUE, sep = ",")
q1data$ClassIndex <- rep(0, nrow(q1data))
q1data$ClassIndex[which(q1data$Class == "benign")] <- 1
q1model1 <- glm(ClassIndex ~ . - Class, data = q1data, family = "binomial")
summary(q1model1)

#Q1c
q1model2 <- glm(ClassIndex ~ Cl.thickness + Cell.shape + Marg.adhesion + Bare.nuclei + Bl.cromatin, data = q1data, family = "binomial")
summary(q1model2)

#Q1d
q1d_data <- c(1,6,3,8,2,5)
e <- exp(sum(q1model2$coefficients * q1d_data))
e / (1+e)

#Q1e
library(leaps)
library(bestglm)
q1e_data <- cbind(q1data$Cl.thickness, q1data$Cell.shape, q1data$Marg.adhesion, q1data$Bare.nuclei, q1data$Bl.cromatin)
q1e_data <- cbind(as.data.frame(q1e_data), q1data$ClassIndex)
colnames(q1e_data) <- c("Cl.thickness", "Cell.shape", "Marg.adhesion", "Bare.nuclei", "Bl.cromatin", "ClassIndex")
bestglm(q1e_data, IC="AIC", family = binomial)$BestModel