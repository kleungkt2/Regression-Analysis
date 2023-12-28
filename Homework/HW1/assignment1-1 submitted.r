library(stats)

#q2
q2data <- read.table("~/Downloads/3424/q2.txt", head = TRUE)

t.test(q2data$observation, y = NULL,
       alternative = "two.sided",
       mu = 200,
       paired = FALSE,
       var.equal = FALSE,
       conf.level = 0.98
)


#q10
heights <- read.table("~/Downloads/3424/q10.txt", header=TRUE)
husband <- heights$Husband
wife <- heights$Wife
cov(husband, wife)
cov(husband * 0.393700787, wife * 0.393700787)
cor(husband, wife)
cor(husband, husband - 5)
regression <- lm(wife ~ husband, data = heights)
summary(regression)
summary(regression)$coefficients[1:2,]

