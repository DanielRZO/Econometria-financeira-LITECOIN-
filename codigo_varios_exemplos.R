# Modelo ARMA ARCH

############ Simulation
set.seed(123456) 
library(fGarch)
spec <- garchSpec(model = list(ar=0.1, omega=1e-6, alpha=0.3, beta=0), cond.dist="norm") 
y <- garchSim(spec, n=1000, n.start = .1) 
tempo <- 1:length(y)
plot(tempo,y, type="l", ylab="Return", xlab="date") 

############ Diagnostics: Main Statistics
acf(y, lag=30) 
pacf(y, lag=30) 
hist(y,20) 
summary(y)
skewness(y)
kurtosis(y)
qqnorm(y); qqline(y, col=2)

############ Diagnostics: Stationarity and ARCH effect
library(tseries)
adf.test(y)
library(FinTS)
ArchTest(y, lags=10) 

############ Diagnostics: Selecting the ARMA model
library(forecast)
fit <- auto.arima(y, trace=TRUE, ic=c("aicc"))  #ver aicc para pequenas amostras
summary(fit)

############ Estimation
fit1 <- garchFit(~arma(1,0) + garch(1,1), data=y, trace=FALSE,
                 include.mean=FALSE, cond.dist="norm", algorithm="lbfgsb+nm")
summary(fit1)
str(fit1)
res1 <- fit1@residuals
acf(res1)
par(mfrow=c(2,1))
qqnorm(y); qqline(y, col="red")
qqnorm(res1); qqline(res1, col="red")

fit2 <- garchFit(~arma(1,0) + garch(1,0), data=y, trace=FALSE, include.mean=FALSE, cond.dist="norm", algorith="lbfgsb+nm")
summary(fit2)
res2 <- fit2@residuals
par(mfrow=c(1,1))
acf(res2)
par(mfrow=c(2,1))
qqnorm(y); qqline(y, col="red")
qqnorm(res2); qqline(res2, col="red")

########### previsão
ahead <- 200
pred2 <- predict(fit2, n.ahead=ahead)

par(mfrow=c(2,1))
plot(y, ylab="retorno", xlab="data")
plot(tempo, fit2@sigma.t, type="l", ylab="vol")
mean.vol<-mean(fit2@sigma.t)*(252^0,5)
mean.vol
par(mfrow=c(2,1))
plot(tempo, y, type="l", ylab="retorno", xlim=c(0,1300), xlab="data")
plot(tempo,fit2@sigma.t, type="l", xlim=c(0,1300), ylab="vol", xlab="data")
lines((1000:1199), pred2$standardDeviation, col="red")

#########################

# using rugarch package
library(rugarch)

#### exchnage rate
fit.spec <- ugarchspec(variance.model = list(model="sGARCH", garchOrder=c(1,0), submodel=NULL, variance.targeting=FALSE),
                       mean.model = list(armaOrder=c(1,0), include.mean=FALSE), distribution.model="norm")
my.ugarch.exp=ugarchfit(data=L.ret.NA,spec=fit.spec)
show(fit3)

#######################################################################################################################################################################
# Modelo ARMA GARCH

############ simulação
set.seed(123456) 
library(fGarch)
spec <- garchSpec(model = list(ar=0.7, omega=1e-6, alpha=0.1, beta=0.85, shape=5), cond.dist="std") # ARMA-GARCH t-dist(5) specification
y <- garchSim(spec, n=1500, n.start = .10)
n <- length(y)
tempo <- 1:n
plot(tempo,y, type="l", ylab="Return", xlab="date") 

############ estatísticas
acf(y, lag=30) 
pacf(y, lag=30) 
hist(y,50) 
summary(y)
skewness(y)
kurtosis(y)
qqnorm(y); qqline(y,col="red")

############ estacionariedade e efeito arch
library(tseries)
adf.test(y)
library(FinTS)
ArchTest(y, lags=10) 

############ modelo arma
library(forecast)
fit <- auto.arima(y, trace=TRUE, ic=c("aicc"))
summary(fit)

############ estimação com dist normal
fit1 <- garchFit(~arma(1,0) + garch(1,1), data=y, trace=FALSE, 
                 include.mean=FALSE, cond.dist="norm", algorithm="lbfgsb+nm")
summary(fit1)
res1 <- fit1@residuals
acf(res1)
par(mfrow=c(2,1))
qqnorm(y); qqline(y, col="red")
qqnorm(res1); qqline(res1, col="red")


########### estimação com dist t
fit2 <- garchFit(~arma(1,0) + garch(1,1), data=y, trace=FALSE,
                 include.mean=FALSE, cond.dist="std", algorithm="lbfgsb+nm")
summary(fit2)
res2 <- fit2@residuals
acf(res2)
par(mfrow=c(2,1))
qqnorm(y); qqline(y, col="red")
qqnorm(res2); qqline(res2, col="red")
qqplot(qt(ppoints(n), df=5), res2, xlab="QQ plot for t distribution")
qqline(res2,col=2)

########### previsão
ahead <- 200
pred2 <- predict(fit2, n.ahead=ahead)

par(mfrow=c(2,1))
plot(y, ylab="retorno", xlab="data")
plot(tempo, fit2@sigma.t, type="l", ylab="vol")
par(mfrow=c(2,1))
plot(tempo, y, type="l", ylab="retorno", xlim=c(0,1700), xlab="data")
lines((1500:1699), pred2$meanForecast, col="red")
plot(tempo,fit2@sigma.t, type="l", xlim=c(0,1700), ylab="vol", xlab="data")
lines((1500:1699), pred2$standardDeviation, col="red")

#############################################################################################################################################################
# Modelo ARMA GARCH com dist t e normal

############ simulação
set.seed(123456) 
library(fGarch)
spec_t <- garchSpec(model = list(ar=0.7, omega=1e-6, alpha=0.1, beta=0.85, shape=5), cond.dist="std") 
y1 <- garchSim(spec_t, n=3000, n.start = .10)
n <- length(y1)
tempo <- 1:n

spec_norm <- garchSpec(model = list(ar=0.7, omega=1e-6, alpha=0.1, beta=0.85), cond.dist="norm") 
y2 <- garchSim(spec_norm, n=3000, n.start = .10)

############ gráficos e curtose
par(mfrow=c(1,2))
hist(y1,50,prob=TRUE, xlim=c(-0.05,0.05), ylim=c(0,100), main="dist t Student")
lines(density(y1, adjust=1), lwd=2, ylim=c(0,100),
      xlim=c(-0.2,0.2), col="red")
hist(y2,50,prob=TRUE, xlim=c(-0.05,0.05), ylim=c(0,100), main="dist normal")
lines(density(y2, adjust=1), lwd=2, ylim=c(0,100),
      xlim=c(-0.2,0.2), col="blue")

kurtosis(y1)

kurtosis(y2)

##################################################################################################################################################################
# retornos e log-retornos

x <- seq(from=-0.2, to=0.2, by=0.001)
x
length(x)
y1 <- x
y2<- log(1+x)
plot(x,y1, type="l", col="blue", lwd=2, ylab="x e log(1+x)", main ="cálculo do retorno e log-retorno")
lines(x,y2, lty=2, lwd=2, col="red")
legend("topleft", col=c("blue","red"), lty=c(1,2), 
       legend=c("x", "log(1+x)"))

####################################################################################################################################################################
# preços e retornos

setwd("C:/Users/Daniel/Documents/MESTRADO/II SEMESTRE/FINANÇAS/SÉRIES TEMPORAIS/Códigos 2")
D <- read.csv(file="Series1.csv", row.names=1)
head(D)
library(timeSeries)
d <- timeSeries(D)
vale <- d[,1]
vale.ret <- returns(vale)
par(mfrow=c(2,1))
plot(vale, type="l", ylab="preços", xlab="data")
plot(vale.ret, type="l", ylab="retornos", xlab="data")

par(mfrow=c(1,1))
hist(vale.ret, n=50)
qqnorm(vale.ret); qqline(vale.ret, col="red")
x <- seq(-0.20,.20,by=0.001)
hist(vale.ret,50,prob=TRUE, xlim=c(-0.20,0.20), ylim=c(0,30))
lines(density(vale.ret, adjust=1), lwd=2, ylim=c(0,30),
      xlim=c(-0.2,0.2), col="red")
lines(x,dnorm(x,mean=mean(vale.ret),sd=sd(vale.ret)), lwd=2, col="blue")
legend("topleft", c("KDE","normal"), lwd=c(2,2), col=c("red","blue"))

hist(vale.ret,50,prob=TRUE, xlim=c(-0.20,-0.03), ylim=c(0,8))
lines(density(vale.ret, adjust=1), lwd=2, ylim=c(0,8),
      xlim=c(-0.2,-0.03), col="red")
lines(x,dnorm(x,mean=mean(vale.ret),sd=sd(vale.ret)), lwd=2, col="blue")
legend("topleft", c("KDE","normal"), lwd=c(2,2), col=c("red","blue"))

##################################################################################################################################################################
# testes nas séries de preços e retornos

setwd("C:/Users/Daniel/Documents/MESTRADO/II SEMESTRE/FINANÇAS/Códigos 2")
D <- read.csv(file="Series1.csv", row.names=1)
head(D)
d <- timeSeries(D)
vale <- d[,1]
petr <- d[,7]
library(fGarch)
vale.ret <- returns(vale)
petr.ret <- returns(petr)
par(mfrow=c(2,1))
plot(vale, type="l", ylab="preços", xlab="data")
plot(petr, type="l", ylab="retornos", xlab="data")

# estacionariedade
library(tseries)
adf.test(vale)
adf.test(petr)

adf.test(vale.ret)
kpss.test(vale.ret)

adf.test(petr.ret)
kpss.test(petr.ret)

# teste de auto-correlação
par(mfrow=c(1,1))
Box.test(vale.ret, lag=1)
Box.test(vale.ret, lag=5)
Box.test(vale.ret, lag=10)
acf(vale.ret, lag=30)

Box.test(petr.ret, lag=1)
Box.test(petr.ret, lag=5)
Box.test(petr.ret, lag=10)
acf(petr.ret, lag=30)


# teste Jarque-Bera
jarque.bera.test(vale.ret)
jarque.bera.test(petr.ret)


# ARCH-LM teste
library(FinTS)
ArchTest(petr.ret)
ArchTest(vale.ret)

##################################################################################################################################################################
# Distribuições normal, t e ged

library(fGarch)
x <- seq(from=-4, to=4, by=0.001)
y1 <- dnorm(x, mean=0, sd=1)
xi <- 1.5
nu2 <- 3
y2 <- dsstd(x,mean=0,sd=1, nu=nu2, xi=xi)
y3 <- dt(x,nu)
y4 <- dsstd(x,mean=0,sd=1, nu=nu2, xi=0.5)


plot(x, y1, type="l", col="red", ylab="", ylim=c(0,0.8))
lines(x, y2, type="l", col="blue")
lines(x, y3, type="l", col="green")  
lines(x, y4, type="l", col="black")      
legend("topleft", col=c("red","blue","green","black"),
       lty=c(1,1,1,1), legend=c("Normal","Skew t (3 gl, 1.5)", "t (3gl)","Skew t (3 gl, 0.5)"))

################################################################################################################################################################
# volatilidade das series de retornos

D <- read.csv(file="Series1.csv", row.names=1)
head(D)
d <- timeSeries(D)
vale <- d[,1]
library(fGarch)
vale.ret <- returns(vale)
vol.vale <- sd(vale.ret)*(252^.5)
vol.vale
d.ret <- returns(d)
head(d.ret)
vol <- apply(d.ret,2,sd)*(252^.5)
vol


