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

# 7.11.3 �ǳ�������
e <- lm.log$residuals
u <- e ^ 2 / mean(e ^ 2)
#��Ϊsalary��һ������
lm.ncon.salary <- lm(u ~ salary.log + 1)
SSreg.salary <- sum((lm.ncon.salary$fitted.values - mean(u)) ^ 2)
S.salary <- SSreg.salary / 2
S.salary
pchisq(S.salary, df = 1, lower.tail = FALSE)
r2 <- (lm.log$residuals / sigma(lm.log)) ^ 2 / (1 - hatvalues(lm.log)) #ѧ�����ڲв�
score.salary <- (1 - hatvalues(lm.log)) * salary.log
plot(score.salary, r2)
#��Ϊsex��һ������
'lm.ncon.sex <- lm(u ~ sex + 1)
SSreg.sex <- sum((lm.ncon.sex$fitted.values - mean(u)) ^ 2)
S.sex <- SSreg.sex / 2
S.sex
pchisq(S.sex, df = 1, lower.tail = FALSE)'

# 7.11.4 ����Ա任���нˮ����ÿ��ְλ�У��Ա�Ĳ���Ƿ�һ��
lm.log.ia <- lm(salary.log ~ rank * sex + degree + year + ysdeg)
Anova(lm.log.ia)

# 7.11.5 
summary(lm.log) #sex�����
confint(lm.log, c('sexFemale'))

# 7.11.6
summary(update(lm.log, ~ . - rank))$coef