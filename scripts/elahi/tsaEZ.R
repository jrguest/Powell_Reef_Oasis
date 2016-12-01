## tsaEZ

library(astsa)
detach("package:dplyr", unload=TRUE)

# Fig 1.1
plot(jj, type = "o")
plot(log(jj))

structure(jj)
head(jj)
?jj

# Fig 1.3
plot(gtemp, type = "o")

# Fig 1.5
par(mfrow = c(2,1)) # set up the graphics
plot(soi, ylab="", xlab="", main="Southern Oscillation Index") 
plot(rec, ylab="", xlab="", main="Recruitment")

# Fig 1.7
w = rnorm(500,0,1) # 500 N(0,1) variates 
v = filter(w, sides = 2, rep(1/3,3)) # moving average 
v
par(mfrow=c(2,1))
plot.ts(w, main="white noise")
plot.ts(v, ylim=c(-3,3), main="moving average")

# Fig 1.8
w = rnorm(550,0,1) # 50 extra to avoid startup problems
x = filter(w, filter=c(1,-.9), method="recursive")[-(1:50)] 
plot.ts(x, main="autoregression")

# Fig 1.9
set.seed(154) # so you can reproduce the results
w = rnorm(200); x = cumsum(w) # two commands in one line
wd = w +.2; xd = cumsum(wd)
plot.ts(xd, ylim=c(-5,55), main="random walk", ylab='') 
abline(a=0, b=.2, lty=2) # drift
lines(x, col=4)
abline(h=0, col=4, lty=2)

arima(x); arima(xd)

# Fig 1.10
cs = 2*cos(2*pi*1:500/50 + .6*pi)
w = rnorm(500,0,1)
par(mfrow=c(3,1), mar=c(3,2,2,1), cex.main=1.5)
plot.ts(cs, main=expression(2*cos(2*pi*t/50+.6*pi)))
plot.ts(cs+w, main=expression(2*cos(2*pi*t/50+.6*pi) + N(0,1)))
plot.ts(cs+5*w, main=expression(2*cos(2*pi*t/50+.6*pi) + N(0,5^2)))

# Fig 1.13
(r = round(acf(soi, 6, plot=FALSE)$acf[-1], 3)) # sample acf values [1] 0.604 0.374 0.214 0.050 -0.107 -0.187
par(mfrow=c(1,2), mar=c(3,3,1,1), mgp=c(1.6,.6,0)) 
plot(lag(soi,-1), soi)
legend('topleft', legend=r[1])
plot(lag(soi,-6), soi)
legend('topleft', legend=r[6])

# Fig 2.4
fit = lm(gtemp~time(gtemp), na.action=NULL) # regress gtemp on time par(mfrow=c(2,1)) # plot detrended series
plot(resid(fit), main="detrended")
plot(diff(gtemp), main="first difference")
par(mfrow=c(3,1)) # plot ACFs
acf(gtemp, 48, main="gtemp")
acf(resid(fit), 48, main="detrended") 
acf(diff(gtemp), 48, main="first difference")

# Fig 2.10
set.seed(90210) # so you can reproduce these results
x = 2*cos(2*pi*1:500/50 + .6*pi) + rnorm(500,0,5)
z1 = cos(2*pi*1:500/50)
z2 = sin(2*pi*1:500/50)
summary(fit <- lm(x ~ 0 + z1 + z2)) # zero to exclude the intercept

par(mfrow=c(2,1))
plot.ts(x)
plot.ts(x, col=8, ylab=expression(hat(x)))
lines(fitted(fit), col=2)
