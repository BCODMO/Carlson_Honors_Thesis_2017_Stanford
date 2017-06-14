#Bring in data
rawdata <- read.csv("~/Documents/Thesis/Data/rawdata.csv")
View(rawdata)

#Subset experimental animals (exclude test animals)
newdata <- rawdata[c(1:256), 1:35]
View(newdata)

#Change column class
newdata[, c(18:31)] <- sapply(newdata[, c(18:31)], as.numeric)

#ANOVA Delta Length
length.aov <- aov(X_.Length ~ pH.Treat + Temp.Treat + Size.Class + pH.Treat:Temp.Treat + pH.Treat:Size.Class + Temp.Treat:Size.Class + pH.Treat:Temp.Treat:Size.Class, data=newdata)
summary(length.aov)

#ANOVA Delta Surface Area
sa.aov <- aov(X_.Surface.Area ~ pH.Treat + Temp.Treat + Size.Class + pH.Treat:Temp.Treat + pH.Treat:Size.Class + Temp.Treat:Size.Class + pH.Treat:Temp.Treat:Size.Class, data=newdata)
summary(sa.aov)

#ANOVA Final Shell Weight
weight.aov <- aov(Final.Dry.Shell.Weight ~ pH.Treat + Temp.Treat + Size.Class + pH.Treat:Temp.Treat + pH.Treat:Size.Class + Temp.Treat:Size.Class + pH.Treat:Temp.Treat:Size.Class, data=newdata)
summary(weight.aov)

#Exclude High Temp
nonheatdata <- subset(newdata, Temp.Treat=="Low", select=c(1:35))
View(nonheatdata)

#ANOVA_exclude Delta Length
length2.aov <- aov(X_.Length ~ pH.Treat + Size.Class + pH.Treat:Size.Class, data=nonheatdata)
summary(length2.aov)

#ANOVA_exclude Delta Surface Area
sa2.aov <- aov(X_.Surface.Area ~ pH.Treat + Size.Class + pH.Treat:Size.Class, data=nonheatdata)
summary(sa2.aov)

#ANOVA_exclude Final Shell Weight
weight2.aov <- aov(Final.Dry.Shell.Weight ~ pH.Treat + Size.Class + pH.Treat:Size.Class, data=nonheatdata)
summary(weight2.aov)

#GLM Mortality
mortality <- glm(Dead ~ pH.Treat + Temp.Treat + Size.Class + pH.Treat:Size.Class + Temp.Treat:Size.Class + pH.Treat:Temp.Treat + pH.Treat:Temp.Treat:Size.Class,family = binomial(link = "logit"),data=newdata)
summary(mortality)

#GLM_exclude Mortality
mortality2 <- glm(Dead ~ pH.Treat + Size.Class + pH.Treat:Size.Class,family = binomial(link = "logit"),data=nonheatdata)
summary(mortality2)

#GLM initial length and mortality
mortality3 <- glm(Dead ~ Initial.Length,family = binomial(link = "logit"),data=nonheatdata)
summary(mortality3)

#GLM initial SA and mortality
mortality4 <- glm(Dead ~ Initial.Surface.Area,family = binomial(link = "logit"),data=nonheatdata)
summary(mortality4)

#Post-Hoc Tests

#Change in Surface Area
with(nonheatdata, pairwise.t.test(X_.Surface.Area, Size.Class:pH.Treat, p.adjust.method = "BH"))
