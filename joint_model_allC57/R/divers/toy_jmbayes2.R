library(JMbayes2)

length(unique(pbc2$id))
dim(pbc2.id)


pbc2.id$status2 <- as.numeric(pbc2.id$status != 'alive')

table(pbc2.id$status2)

CoxFit <- coxph(Surv(years, status2) ~ sex, data = pbc2.id)

fm1 <- lme(log(serBilir) ~ year * sex, data = pbc2, random = ~ year | id)

jointFit1 <- jm(CoxFit, fm1, time_var = "year")
summary(jointFit1)


model3 <- lme(travel ~ 1, data=Rail, random= ~1|Rail)
summary(model3)
var(Rail$travel)
