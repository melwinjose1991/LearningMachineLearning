library(pls)

set.seed(2)
pcr.fit = pcr(Salary~., data=Hitters, scale=TRUE, validation="CV")
summary(pcr.fit)
validationplot(pcr.fit, val.type="MSEP")
