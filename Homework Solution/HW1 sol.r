## Problem 2
obs <- c(240,243,250,254,264,279,284,285,290,298,302,310,312,315,
         322,337,348,384,386,520)
t.test(obs, alternative="two.sided", mu=200, conf.level = 0.98)
sqrt(20)*(mean(obs)-200)/sqrt(var(obs))
 
## Problem 10
df <- read.table("Heights of husband and wife.txt", header = TRUE)
#(a)
cov(df)[1,2] 
#(b)
df_inches <- df*0.393700787
cov(df_inches)[1,2]
#(c)
cor(df)[1,2]
#(d)
cor(df[df[,1]-df[,2]==5,])[1,2] 
## or simply by definition, Cor(X,X-5)=1
#(e)
model <- lm(Wife~Husband, data=df)
summary(model)
#(f)&(g)
## Both reject the null hypothesis, based on the output in (e).
