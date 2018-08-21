library(alr4)
attach(salary)
# 7.11.1
lm.main <- lm(salary ~ rank + degree + year + ysdeg)
lm.interaction <- lm(salary ~ rank * degree + rank * year + rank * ysdeg)
anova(lm.main, lm.interaction)

# 7.11.2
lm.all <- lm(salary ~ rank + degree + year + ysdeg + sex)
residualPlot(lm.all)
boxCox(lm(salary ~ degree + year + ysdeg + sex))

salary.log <- log(salary)
lm.log <- lm(salary.log ~ rank + degree + year + ysdeg + sex)
residualPlot(lm.log)

# 7.11.3 非常数方差
e <- lm.log$residuals
u <- e ^ 2 / mean(e ^ 2)
#作为salary的一个函数
lm.ncon.salary <- lm(u ~ salary.log + 1)
SSreg.salary <- sum((lm.ncon.salary$fitted.values - mean(u)) ^ 2)
S.salary <- SSreg.salary / 2
S.salary
pchisq(S.salary, df = 1, lower.tail = FALSE)
r2 <- (lm.log$residuals / sigma(lm.log)) ^ 2 / (1 - hatvalues(lm.log)) #学生化内残差
score.salary <- (1 - hatvalues(lm.log)) * salary.log
plot(score.salary, r2)
#作为sex的一个函数
'lm.ncon.sex <- lm(u ~ sex + 1)
SSreg.sex <- sum((lm.ncon.sex$fitted.values - mean(u)) ^ 2)
S.sex <- SSreg.sex / 2
S.sex
pchisq(S.sex, df = 1, lower.tail = FALSE)'

# 7.11.4 检验对变换后的薪水，在每种职位中，性别的差别是否一样
lm.log.ia <- lm(salary.log ~ rank * sex + degree + year + ysdeg)
Anova(lm.log.ia)

# 7.11.5 
summary(lm.log) #sex项不显著
confint(lm.log, c('sexFemale'))

# 7.11.6
summary(update(lm.log, ~ . - rank))$coef