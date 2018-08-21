"
#习题1.2
#(1)
library(alr4)#Hooker的数据在package‘alr4’中
attach(Hooker)
plot(bp,pres)#在第四版中，temp名称改为bp (boiling point)
L1=lm(pres~bp)
summary(L1)
abline(L1)
residualPlot(L1)
#(2)
plot(bp,lpres)
#(3)
L2=lm(lpres~bp)
summary(L2)
abline(L2)
Anova(L2)
#1.2.4
confint(L2)
#1.2.5
new<-data.frame(bp=c(185,212))
predict.lm(L2,new,interval = 'prediction',level=0.9)
lm.Forbes=lm(Forbes$lpres~Forbes$bp)
abline(lm.Forbes,col='red',lty=2)
legend('topleft',col=c('black','black','red'),pch=c('o',NA,NA),lty=c(NA,1,2),legend=c('Hooker的数据点', 'Hooker的拟合直线','Forbes的拟合直线'))
#1.2.6
summary(lm.Forbes)
"
#习题1.4
#1.4.1
#1.4.3

library(alr4)#Hooker的数据在package‘alr4’中
attach(snake)
m0<-lm(Y~X-1)
summary(m0)
#验算
SXY=sum(X*Y)
SXX=sum(X^2)
SXY/SXX
SYY=sum(Y^2)
sigma=sqrt((SYY-SXY^2/SXX)/16)
confint(m0)
residualPlot(m0)
Total=sum((m0$residuals)^2)
ttt=sum((lm(Y~X)$residuals)^2)
result=(Total-ttt)/(ttt/15)
zzz=sum((m0$fitted.values-lm(Y~X)$fitted.values)^2)-(Total-ttt)
#习题1.10
Amazon<-read.csv("Amazon.csv",header = TRUE)
attach(Amazon)
plot(year,high,col='blue',ylim=c(16,28))#作high关于year，low关于year的散点图
points(year,low,col='red',pch=2)
legend('right',col=c('blue','red'),pch=c(1,2),legend=c('high','low'))
summary(lm(high~year))

