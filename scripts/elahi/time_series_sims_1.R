################################################################################
##' @title Time-series simulations
##'
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' @contributor 
##' 
##' @date 2016-12-01
##' 
##' @log Add a log here
################################################################################

rm(list=ls(all=TRUE))

##### LOAD PACKAGES, DATA #####
library(ggplot2)
library(dplyr)
library(tidyr)

##### EXAMPLES #####
#http://stats.stackexchange.com/questions/99924/simulate-regression-data-with-dependent-variable-being-non-normally-distributed

set.seed(5840)  # this makes the example exactly reproducible
N      <- 100
x      <- rnorm(N)
beta   <- 0.4
errors <- rlnorm(N, meanlog=0, sdlog=1)
errors <- -1*errors   # this makes them left skewed
errors <- errors - 1  # this centers the error distribution on 0
y      <- 1 + x*beta + errors
plot(y ~ x)

set.seed(5840)  # this makes the example exactly reproducible
N      <- 100
x      <- rnorm(N)
beta   <- 0.4
errors <- rweibull(N, shape=1.5, scale=1)
# errors <- -1*errors   # this makes them left skewed
errors <- errors - factorial(1/1.5)  # this centers the error distribution on 0
y      <- 1 + x*beta + errors
plot(y ~ x)

# http://stackoverflow.com/questions/14554558/simulate-a-linear-model-100-times
a <- 3
B <- 0.5
C <- -0.7

results <- matrix(nrow=100,ncol=3)
for (i in 1:100){
  x1 <- rnorm(200, mean=0, sd=1)
  x2 <- rnorm(200, mean=0, sd=1) 
  e <- rnorm(200, mean=0, sd=1)
  y1 <- a+B*x1+C*x2+e 
  
  y<- lm(y1~x1+x2)
  results[i,] <- coef(y)
}

results



x <- diffinv(rnorm(999))
x

##### ARIMA SIM #####

ts.sim <- arima.sim(n = 63, list(ar = c(0.8897, -0.4858), ma = c(-0.2279, 0.2488)),
          sd = sqrt(0.1796))
plot(ts.sim)
ts.plot(ts.sim)

# mildly long-tailed
ts.sim <- arima.sim(n = 63, list(ar = c(0.8897, -0.4858), ma = c(-0.2279, 0.2488)),
          rand.gen = function(n, ...) sqrt(0.1796) * rt(n, df = 5))
plot(ts.sim)

# An ARIMA simulation
ts.sim <- arima.sim(list(order = c(1,1,0), ar = 0.2), n = 200)
ts.plot(ts.sim)

arima(ts.sim)

# An ARIMA simulation - white noise
ts.sim <- arima.sim(list(order = c(0, 0, 0)), n = 200)
ts.plot(ts.sim)

# Other...
x <- arima.sim(list(order = c(1,0,2), ar = -0.5 , ma=c(0.5, -0.7)), 
               sd=sqrt(0.25), n = 1000)
ts.plot(x)
arima(x)

##' package stsm
# generate a quarterly series from a local level plus seasonal model
library(stsm)

pars <- c(var1 = 300, var2 = 10, var3 = 100)
m <- stsm.model(model = "llm+seas", y = ts(seq(120), frequency = 4), 
                pars = pars, nopars = NULL)
ss <- char2numeric(m)
set.seed(123)
y <- datagen.stsm(n = 120, model = list(Z = ss$Z, T = ss$T, H = ss$H, Q = ss$Q), 
                  n0 = 20, freq = 4, old.version = TRUE)$data
plot(y, main = "data generated from the local-level plus seasonal component")



pars <- c(var1 = 300, var2 = 10, var3 = 100)
m <- stsm.model(model = "local-trend", y = ts(seq(30), frequency = 1), 
                pars = pars, nopars = NULL)
ss <- char2numeric(m)
set.seed(123)
y <- datagen.stsm(n = 30, model = list(Z = ss$Z, T = ss$T, H = ss$H, Q = ss$Q), 
                  n0 = 20, freq = 1, old.version = TRUE)$data
plot(y, main = "")

plot(ts(seq(20), frequency = 4))


ts(1:10, frequency = 4, start = c(1959, 2)) # 2nd Quarter of 1959
print( ts(1:10, frequency = 7, start = c(12, 2)), calendar = TRUE)
# print.ts(.)
## Using July 1954 as start date:
gnp <- ts(cumsum(1 + round(rnorm(100), 2)),
          start = c(1954, 7), frequency = 12)
gnp
plot(gnp) # using 'plot.ts' for time-series plot

## Multivariate
z <- ts(matrix(rnorm(300), 100, 3), start = c(1961, 1), frequency = 12)
class(z)
head(z) # as "matrix"
plot(z)
plot(z, plot.type = "single", lty = 1:3)

## A phase plot:
plot(nhtemp, lag(nhtemp, 1), cex = .8, col = "blue",
     main = "Lag plot of New Haven temperatures")

##### MODIFY FOR CORAL COVER TIME SERIES #####

##' cc = coral cover
##' cc_sd = sd of cover
##' yrs = number of years
##' w = white noise (sampled from a normal distribution)
##' slope = slope of temporal trend
##' intercept = intercept of temporal trend


## White noise around a mean of coral cover
cc = 30
cc_sd = 3
yrs = 30
w = rnorm(yrs, mean = cc, sd = cc_sd) 
w
plot.ts(w)

stable_simF <- function(cc = 30, cc_sd = 3, yrs = 30){
  w = rnorm(yrs, mean = cc, sd = cc_sd)
}

stable_df <- as.data.frame(replicate(5, stable_simF())) %>%
  mutate(year = seq(1:yrs)) %>% 
  gather(key = "ID", value = "cover", V1:V5)
stable_df %>% 
  ggplot(aes(year, cover, color = ID)) + 
  geom_point() + geom_line()



## Linear trend
cc_sd = 3
w = rnorm(yrs, mean = 0, sd = cc_sd) 
slope = -1
intercept = 40

x = seq(1:N)
y = slope * x + intercept + w
plot.ts(y)
mean(y)
summary(lm(y ~ x))

## Cycles
period = 15
w = rnorm(yrs, mean = 0, sd = cc_sd) 
phase_shift = 0.5 * pi
intercept = 30
slope = 1
amp = 20

cs = amp*cos(2*pi*1:yrs/period + phase_shift)
plot.ts(cs)
y = slope * x + intercept + w + cs
plot.ts(y)
mean(y)

y_cycle = y + cs
plot(y_cycle)



## Random walk with drift
set.seed(154) # so you can reproduce the results
w = rnorm(yrs, mean = cc, sd = cc_sd) 
x = cumsum(w) 
wd = w +.2; xd = cumsum(wd)
plot.ts(xd, ylim=c(0, 100), main="random walk", ylab='') 
abline(a=0, b=.2, lty=2) # drift
lines(x, col=4)
abline(h=0, col=4, lty=2)

