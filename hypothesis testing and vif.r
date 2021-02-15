df = read.csv('E:/data/data science/data course/DATASET/seatdata.csv')

head(df)

dim(df)

str(df)

summary(df)

data.frame('Features'=c(colnames(df)), 'missing values counte'=sapply(df, function(x) sum(is.na(x))),row.names=NULL)

fullmodel<- lm(SeatX~Stature+Sitting.Height+SHS+BMI+Weight+L11+H17, data=df)

summary(fullmodel)

reducemodel = lm(SeatX~1, data=df)

summary(reducemodel)

 anova(reducemodel, fullmodel)

reducemodel1<- lm(SeatX~Stature+Sitting.Height+SHS+BMI+L11+H17, data=df)

summary(reducemodel1)

2*(1-pt(summary(fullmodel)$coef[2, 1]/summary(fullmodel)$coef[2, 2], 399-8))

reducemodel2<- lm(SeatX~Sitting.Height+SHS+BMI+Weight+L11+H17, data=df)

anova(reducemodel2, fullmodel)

1-pf(4.02, 1, 391)

reducemodel3 <- lm(SeatX~Stature+Sitting.Height+SHS+Weight+L11+H17, data=df)

summary(reducemodel3)

anova(reducemodel3, fullmodel)

reducemodel4<- lm(SeatX~Sitting.Height+BMI+Weight+L11+H17, data=df)

summary(reducemodel4)

anova(reducemodel4, fullmodel)

reducemodel5 <- lm(SeatX~I(Stature+Sitting.Height)+SHS+BMI+Weight+L11+H17, data=df)

summary(reducemodel5)

anova(reducemodel5, fullmodel)

 reducemodel6 <- lm(SeatX~offset(0.6*Stature)+Sitting.Height+SHS+BMI+Weight+L11+H17, data=df)

summary(reducemodel6)$coef

anova(reducemodel6, fullmodel)

t = (summary(fullmodel)$coef[2, 1]-0.6)/(summary(fullmodel)$coef[2, 2])

2*(1-pt(t, 399-8)) 

library(faraway)

fullmodel <- lm(SeatX~Stature+Sitting.Height+SHS+BMI+Weight+L11+H17, data=df)

summary(fullmodel)

vif(fullmodel)

mf <- lm(SeatX~Stature+SHS+BMI+Weight+L11+H17, data=df)

summary(mf)

vif(mf)

mf1 <- lm(SeatX~Stature+SHS+BMI+L11+H17, data=df)

summary(mf1)

vif(mf1)


