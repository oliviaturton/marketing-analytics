library("xtable")
library("knitr")
library("ggplot2")
library("mlogit")
library("caret")

cbc.df <- read.csv("Final Assessment Datasets/5_conjoint.csv")
str(cbc.df)
head(cbc.df)
summary(cbc.df)

xtabs(Choice~Price, data=cbc.df)
xtabs(Choice~Energy, data=cbc.df)
xtabs(Choice~Nuts, data=cbc.df)
xtabs(Choice~Tokens, data=cbc.df)
xtabs(Choice~Origin, data=cbc.df)
xtabs(Choice~Organic, data=cbc.df)
xtabs(Choice~Premium, data=cbc.df)
xtabs(Choice~Fairtrade, data=cbc.df)
xtabs(Choice~Sugar, data=cbc.df)


cbc.df$Energy <- as.factor(cbc.df$Energy)
cbc.df$Nuts <- as.factor(cbc.df$Nuts)
cbc.df$Tokens <- as.factor(cbc.df$Tokens)
cbc.df$Organic <- as.factor(cbc.df$Organic)
cbc.df$Premium <- as.factor(cbc.df$Premium)
cbc.df$Fairtrade <- as.factor(cbc.df$Fairtrade)
cbc.df$Sugar <- as.factor(cbc.df$Sugar)


cbc.df$Energy <- relevel(cbc.df$Energy, ref = "Low")
cbc.df$Nuts <- relevel(cbc.df$Nuts, ref = "No")
cbc.df$Tokens <- relevel(cbc.df$Tokens, ref = "No")
cbc.df$Organic<- relevel(cbc.df$Organic, ref = "No")
cbc.df$Premium <- relevel(cbc.df$Premium, ref = "No")
cbc.df$Faitrade <- relevel(cbc.df$Fairtrade, ref = "No")
cbc.df$Sugar <- relevel(cbc.df$Sugar, ref = "High")

library(dfidx) 
cbc.mlogit <- dfidx(cbc.df, choice="Choice",
                    idx=list(c("Choice_id", "Consumer_id"), "Alternative"))

model<-mlogit(Choice ~ 0+Energy+Nuts+Tokens+Organic+Premium+Fairtrade+Sugar+Price, data=cbc.mlogit)
kable(summary(model)$CoefTable)

model.constraint <-mlogit(Choice ~ 0+Premium, data = cbc.mlogit)
lrtest(model, model.constraint)

kable(head(predict(model,cbc.mlogit)))

predicted_alternative <- apply(predict(model,cbc.mlogit),1,which.max)
selected_alternative <- cbc.mlogit$Alternative[cbc.mlogit$Choice>0]
confusionMatrix(table(predicted_alternative,selected_alternative),positive = "1")

predict.share <- function(model, d) {
  temp <- model.matrix(update(model$formula, 0 ~ .), data = d)[,-1] 
  u <- temp%*%model$coef[colnames(temp)]
  probs <- t(exp(u)/sum(exp(u))) 
  colnames(probs) <- paste("alternative", colnames(probs))
  return(probs)
}

d.base <- cbc.df[c(44,34,33,40),c("Energy","Nuts","Tokens","Organic", "Premium","Fairtrade", "Sugar","Price")]
d.base <- cbind(d.base,as.vector(predict.share(model,d.base)))
colnames(d.base)[7] <- "Predicted.Share"
rownames(d.base) <- c()
kable(d.base)

coef(model)["PremiumYes"] / (-coef(model)["Price"])
coef(model)["EnergyHigh"] / (-coef(model)["Price"])
coef(model)["FairtradeYes"] / (-coef(model)["Price"])
coef(model)["SugarLow"] / (-coef(model)["Price"])
coef(model)["OrganicYes"] / (-coef(model)["Price"])

