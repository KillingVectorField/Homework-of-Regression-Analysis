DATA<-data.frame('X'=c(0,5,2,3,2,NA),
                 'Y'=c(0,5,3,2,NA,2))
plot(DATA)

lm.CC<-lm(Y~X,data = DATA,na.action = na.omit)
DATA.sameImputed<-data.frame('X'=c(0,5,2,3,2,3),
                             'Y'=c(0,5,3,2,3,2))
lm.sameImputed<-lm(Y~X,data = DATA.sameImputed)
lm.sameImputed$df.residual<-lm.sameImputed$df.residual-2
summary(lm.sameImputed)

# 10.3
Y_5<-predict(lm.CC,data.frame('X'=2))
X_Y.CC<-lm(X~Y,data = DATA,na.action = na.omit)
X_6<-predict(X_Y.CC,data.frame('Y'=2))
DATA.regImputed<-data.frame('X'=c(0,5,2,3,2,X_6),
                            'Y'=c(0,5,3,2,Y_5,2))
plot(DATA.regImputed,col=c(rep('black',4),rep('red',2)))
lm.regImputed<-lm(Y~X,data=DATA.regImputed)
lm.regImputed$df.residual<-lm.regImputed$df.residual-2
summary(lm.regImputed)

# 10.4

beta=beta_new=alpha=alpha_new=c(0,1)
epsilon<-1e-8
Delta<-Inf
X<-DATA$X
Y<-DATA$Y
while (Delta>=epsilon){
  X[6]<-alpha[1]+alpha[2]*Y[6]
  Y[5]<-beta[1]+beta[2]*X[5]
  beta_new<-lm(Y~X)$coef
  alpha_new<-lm(X~Y)$coef
  Delta<-max(abs(c(beta_new-beta,alpha_new-alpha)))
  beta<-beta_new; alpha<-alpha_new
}
cat('beta:', beta_new, 'alpha:',alpha_new ,
    'X[6]:', X[6], 'Y[5]:',Y[5])
plot(X,Y,col=c(rep('black',4),rep('red',2)))
