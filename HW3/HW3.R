if (FALSE) {
    mu = c(10.0, 17.5)
    sigma = matrix(c(9, 11.251, 11.251, 23.0625), ncol = 2)
    library(MASS)
    sim = mvrnorm(n = 250, mu = mu, Sigma = sigma)
    plot(sim[, 1], sim[, 2])
    beta1 <- sigma[1, 2] / sigma[1, 1]
    beta1_ <- sigma[1, 2] / sigma[2, 2]
    beta0_ <- mu[1] - beta1_ * mu[2]
    abline(a = mu[2] - beta * mu[1], b = beta, col = 'red', lty = 1) #y=beta1 x + beta0
    abline(a = -beta0_ / beta1_, b = 1 / beta1_, col = 'blue', lty = 2) #y=1/beta1_(x-beta0_)
    legend('topleft', col = c('red', 'blue'), lty = c(1, 2), legend = c('regression of y on x', 'regression of x on y'))
}

library(alr4)
attach(longley)
GNP <- GNP * 1e3
Unemployed <- Unemployed * 10
Armed.Forces <- Armed.Forces * 10
Population <- Population * 1e3
Employed <- Employed * 1e3
longley.lm <- lm(Employed ~ GNP.deflator + GNP + Unemployed + Armed.Forces + Population + Year)
summary(longley.lm)


longley.sim <- function() {
    GNP.deflator.sim <- GNP.deflator + runif(16, -0.05, 0.05)
    GNP.sim <- GNP + runif(16, -0.5, 0.5)
    Unemployed.sim <- Unemployed + runif(16, -0.5, 0.5)
    Armed.Forces.sim <- Armed.Forces + runif(16, -0.5, 0.5)
    Population.sim <- Population + runif(16, -0.5, 0.5)
    Year.sim <- Year + runif(16, 0, 1)
    longley.sim.lm <- lm(Employed ~ GNP.deflator.sim + GNP.sim + Unemployed.sim + Armed.Forces.sim + Population.sim + Year.sim)
    longley.sim.lm$coef
}

N <- 100
record=c()
for (i in 1:N) {
    record <- rbind(record, longley.sim())
}
record<-data.frame(record)
summary(record)
with(record, {
    opar <- par(mfrow = c(2, 3))
    hist(GNP.deflator.sim)
    abline(v = longley.lm$coefficients['GNP.deflator'],col='red')
    hist(GNP.sim)
    abline(v = longley.lm$coefficients['GNP'], col = 'red')
    hist(Unemployed.sim)
    abline(v = longley.lm$coefficients['Unemployed'], col = 'red')
    hist(Armed.Forces.sim)
    abline(v = longley.lm$coefficients['Armed.Forces'], col = 'red')
    hist(Population.sim)
    abline(v = longley.lm$coefficients['Population'], col = 'red')
    hist(Year.sim)
    abline(v = longley.lm$coefficients['Year'], col = 'red')
     })
