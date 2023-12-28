library(glmnet)

p241data <- read.table("./Downloads/P241.txt", header = TRUE)
p241data_s <- p241data[1:11,]

p241data_snorm <- p241data_s
p241data_snorm$IMPORT <- (p241data_snorm$IMPORT - mean(p241data_snorm$IMPORT)) / sd(p241data_snorm$IMPORT)
p241data_snorm$DOPROD <- (p241data_snorm$DOPROD - mean(p241data_snorm$DOPROD)) / sd(p241data_snorm$DOPROD)
p241data_snorm$STOCK <- (p241data_snorm$STOCK - mean(p241data_snorm$STOCK)) / sd(p241data_snorm$STOCK)
p241data_snorm$CONSUM <- (p241data_snorm$CONSUM - mean(p241data_snorm$CONSUM)) / sd(p241data_snorm$CONSUM)

p241lm_s <- lm(IMPORT ~ DOPROD + STOCK + CONSUM , data = p241data_s)
summary(p241lm_s)
p241lm_snorm <- lm(IMPORT ~ DOPROD + STOCK + CONSUM, data = p241data_snorm)
summary(p241lm_snorm)

p241_xs <- data.matrix(p241data_s[, c("DOPROD", "STOCK", "CONSUM")])
p241_ys <- data.matrix(p241data_s[, c("IMPORT")])
p241_xsnorm <- data.matrix(p241data_snorm[, c("DOPROD", "STOCK", "CONSUM")])
p241_ysnorm <- data.matrix(p241data_snorm[, c("IMPORT")])
p241ridge_s <- glmnet(p241_xs, p241_ys, alpha = 0, lambda = 0.003)
summary(p241ridge_s)
coef(p241ridge_s)
#p241ridge_s[["beta"]]@x
p241ridge_snorm <- glmnet(p241_xsnorm, p241_ysnorm, alpha = 0, lambda = 0.003)
summary(p241ridge_snorm)
coef(p241ridge_snorm)

