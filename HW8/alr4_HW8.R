data<-alr4::dwaste
n=20
car::scatterplotMatrix(~BOD+TKN+TS+TVS+COD+O2UP,data=data,
                       smoother=FALSE,reg.line=FALSE,
                       id.n=n,id.cex=0.7)
data['O2UP.log']<-log10(O2UP)

# Cp 图
#Cp_p<-data.frame('p'=c(rep(2,5),rep(3,10),rep(4,10),rep(5,5),6),
#                 'Cp'=c(6.29,6.57,13.50,20.33,56.84,
#                        1.74,5.27,6.87,6.88,7.16,7.33,7.70,9.09,11.33))


#删去第17个点
predictors<-c('BOD','TKN','TS','TVS','COD')
predictors.powerset<-ggm::powerset(predictors)
lm.tot<-lm(reformulate(predictors,response = "O2UP.log"),data=data,subset = -17)
RSS.tot<-sum(lm.tot$residuals^2)
sigma2<-RSS.tot/lm.tot$df.residual
n<-length(lm.tot$residuals)
SYY<-with(data,sum((O2UP.log-mean(O2UP.log))^2))
Cp_p<-data.frame('p'=integer(),
                 'Cp'=numeric(),
                 'R2'=numeric(),
                 'RSS'=numeric(),
                 'Model'=character(),stringsAsFactors = F)
for (i in 1:length(predictors.powerset)){
  terms<-predictors.powerset[[i]]
  lm.p<-lm(reformulate(terms,response = "O2UP.log"),data=data)
  Cp_p[i,'Model']<-paste(terms,collapse = " ")
  Cp_p[i,'p']<-lm.p$rank
  Cp_p[i,'RSS']<-sum(lm.p$residuals^2)
  Cp_p[i,"Cp"]<-Cp_p[i,"RSS"]/sigma2+2*Cp_p[i,'p']-n
  Cp_p[i,"R2"]<-1-Cp_p[i,"RSS"]/SYY
}
Cp_p<-dplyr::arrange(Cp_p,p,Cp)
with(Cp_p,plot(p,Cp-p))
with(Cp_p,lines(p[c(1,6,16,26,31)],(Cp-p)[c(1,6,16,26,31)],col="red"))
abline(h=0,lty=2)

Cp_p[Cp_p$Cp<=6,]

summary(lm.tot<-lm(reformulate(predictors,response =
                                 "O2UP.log"),data=data,subset = -17))
summary(lm.best<-lm(O2UP.log~TS+COD,data=data,subset=-17))
summary(b1 <- powerTransform(cbind(TS, COD) ~ 1,
                             data=data, subset=-17))
data['TS.log']<-log10(TS)
data['COD.log']<-log10(COD)
summary(lm.best2<-lm(O2UP.log~TS.log+COD.log,data=data,subset = -17))
residualPlot(lm.best2)
