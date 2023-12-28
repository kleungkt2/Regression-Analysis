## Problem 1
#(a)
df <- read.csv("BreastCancer.txt", header = TRUE)
df$Class <- as.factor(df$Class)
m_logit <- glm(Class~., family = "binomial", data = df)
m_summary <- summary(m_logit)
Rsquare <- 1-m_summary$deviance/m_summary$null.deviance
#(b)
m_summary
c(0.53501-0.14202*qnorm(0.975),0.53501+0.14202*qnorm(0.975))
1.399>qnorm(0.95)
#(c)
m2_logit <- glm(Class~Cl.thickness+Cell.shape+Marg.adhesion+Bare.nuclei+Bl.cromatin, family = "binomial", data = df)
m2_summary <- summary(m2_logit)
m2_summary
Rsquare_m2 <- 1-m2_summary$deviance/m2_summary$null.deviance
anova(m2_logit, m_logit, test="Chisq")
#(d)
1-predict(m2_logit, newdata = list("Cl.thickness"=6,"Cell.shape"=3,"Marg.adhesion"=8,"Bare.nuclei"=2,"Bl.cromatin"=5),
        type="response")
#(e)
aic.list <- data.frame(matrix(0, ncol = 2, nrow = 31))
colnames(aic.list) <- c("model","aic")
comb.list <- lapply(1:5, function(x) combn(5,x))
var.names <- c("Cl.thickness","Cell.shape","Marg.adhesion","Bare.nuclei","Bl.cromatin")
idx <- 1
for (i in 1:length(comb.list)){
  for (j in 1:ncol(comb.list[[i]])){
    temp.logit <- glm(paste0("Class~",paste(var.names[comb.list[[i]][,j]],collapse = "+")), family = "binomial", data = df)
    temp.summary <- summary(temp.logit)
    aic.list[idx,1] <- paste(var.names[comb.list[[i]][,j]],collapse = "+")
    aic.list[idx,2] <- temp.summary$aic
    idx <- idx+1
  }
}
aic.list
aic.list[which.min(aic.list$aic),]
