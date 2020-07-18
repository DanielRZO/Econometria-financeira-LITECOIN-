######  LITECOIN  ######


# serie dos logretornos da serie, apresente os gráficos contra o tempo dos preços e retornos bem com os respectivos histogramas. Faca o qq-plot para a serie de retornos.

# inserindo os dados:
library(readxl)
litecoin <- read_excel("C:\\Users\\Daniel\\Documents\\MESTRADO\\II SEMESTRE\\FINANÇAS\\EXERCÍCIOS\\TRABALHO 2\\litecoin.xlsx")


# transformando em uma série de tempo:
library(timeSeries)
L <- timeSeries(litecoin)
head (L)
L <- rev(L)
head (L)

# para avaliar o retorno e o logretorno, trabalharemos com o preço de fechamento:
preço <- as.numeric(L[,5])
head (preço)

# calculando o log retorno:
L.ret <- returns(preço)

# fazendo os gráficos:
plot(preço, main="Litecoin", type="l", ylab="preços", xlab="data")
plot(L.ret, main="Litecoin#LogRetorno", type="l", ylab="logretorno", xlab="data")

# histogramas:
par(mfrow=c(1,1))
hist(preço, breaks = "FD", col = "red", n=100, 
     main = "Histograma Litecoin", xlab = "Preços")

hist(L.ret, breaks = "FD", col = "blue", n=100, 
     main = "Histograma Litecoin", xlab = "Returns")

#qqplot
qqnorm(L.ret); qqline(L.ret, col="red")

# tabela com as principais estaísticas das séries de preços e retornos: média, min, max, desvio, variância, assimetria e curtose.

library(tseries)
library(fBasics)

options(digits=4)

#série de preços:
StatsPreço=as.matrix(basicStats(preço), 2,1)

#série de log retornos:
StatsRet=as.matrix(basicStats(removeNA(L.ret)), 2,1)

STATS <- cbind(StatsRet, StatsPreço)
STATS

#(a) estacionarieade: série de preços e logretornos 
#(b) teste de normalidade: ambas as séries 
#(c) testes Box Pierce Ljung Box na série de retornos 
#(d) testes Box Pierce Ljung Box na série dos quadrados dos retornos r2 
#(e) teste ARCH-LM na série de retorno


#(a)
L.ret.NA <- removeNA(L.ret)

# teste em preços (fechamento)
adf.test(preço)
kpss.test(preço)

# teste em retornos
adf.test(L.ret.NA)                               
kpss.test(L.ret.NA, null="Trend")

#(b)
jarque.bera.test(preço)
jarque.bera.test(L.ret.NA)

#(c)
Box.test(L.ret.NA, lag = 5)
Box.test(L.ret.NA, lag = 10)
Box.test(L.ret.NA, lag = 30)

#(d)
Box.test(L.ret.NA^2, lag = 5)
Box.test(L.ret.NA^2, lag = 10)
Box.test(L.ret.NA^2, lag = 30)
                                                     
#(e)
library(FinTS)
ArchTest(L.ret.NA)

summary(L.ret.NA)
par(mfrow=c(1,2))
acf(L.ret.NA)
acf(L.ret.NA^2)


## Ajuste de modelos ARMA-GARCH ou AR-GARCH para série observada
#(a) tente o ajuste com modelos lineare e não lineares
#(b) em ambos os casos do item anterior teste as distribuições: normal, student, student skew e ged
#(c) faça a seleção do modelo escolhido pelos critéricos AIC e BIC 
#(d) faça comentários gerais sobre o ajuste


library(forecast)
library(rugarch)
library(fGarch)
library(rmgarch)
library(quantmod)

fit <- auto.arima(L.ret.NA, seasonal=FALSE, stepwise=FALSE, approximation=FALSE)   
summary(fit)

#Best model ARIMA(2,0,3)

######################################### TESTES #######################################################

# Garch ~ Normal ~ AIC

best_r_N_A = 1
best_s_N_A = 1


formula = as.formula( paste( sep="", "~arma(2,3) + garch(", best_r_N_A, ",", best_s_N_A,")" ) )

best_model_N_A = garchFit(formula = formula, data = L.ret.NA, trace = FALSE, cond.dist = "norm")

best_aic_N_A = best_model_N_A@fit$ics[["AIC"]]

cat(sprintf("GARCH(%d, %d)\t: %f\n", best_r_N_A, best_s_N_A, best_aic_N_A))

for(r in 1 : 5){
  
  for(s in 1 : 5){
    
    formula = as.formula( paste( sep="", "~arma(2,3) + garch(", r, ",", s,")" ) )
    
    garch_model_N_A = garchFit(formula = formula, data = L.ret.NA, trace = FALSE, cond.dist = "norm")
    
    aic_N_A = garch_model_N_A@fit$ics[["AIC"]]
    
    cat(sprintf("ARMA(%d, %d)-GARCH(%d, %d)\t: %f\n", 2, 3, r, s, aic_N_A))
    
    if(aic_N_A < best_aic_N_A){
      
      best_model_N_A = garch_model_N_A
      
      best_r_N_A = r
      
      best_s_N_A = s
      
      best_aic_N_A = aic_N_A
      
    }
  }
}

cat(sprintf("Best GARCH Model : ARMA(%d, %d)-GARCH(%d, %d)\t: %f\n", 2, 3, best_r_N_A, best_s_N_A, best_aic_N_A))

## Best GARCH Model : ARMA(2, 3)-GARCH(2, 5)	: -2.914779


# Garch ~ Normal ~ AIC

best_r_N_A1 = 1
best_s_N_A1 = 1


formula = as.formula( paste( sep="", "~arma(1,1) + garch(", best_r_N_A1, ",", best_s_N_A1,")" ) )

best_model_N_A1 = garchFit(formula = formula, data = L.ret.NA, trace = FALSE, cond.dist = "norm")

best_aic_N_A1 = best_model_N_A1@fit$ics[["AIC"]]

cat(sprintf("GARCH(%d, %d)\t: %f\n", best_r_N_A1, best_s_N_A1, best_aic_N_A1))

for(r in 1 : 5){
  
  for(s in 1 : 5){
    
    formula = as.formula( paste( sep="", "~arma(1,1) + garch(", r, ",", s,")" ) )
    
    garch_model_N_A1 = garchFit(formula = formula, data = L.ret.NA, trace = FALSE, cond.dist = "norm")
    
    aic_N_A1 = garch_model_N_A1@fit$ics[["AIC"]]
    
    cat(sprintf("ARMA(%d, %d)-GARCH(%d, %d)\t: %f\n", 2, 3, r, s, aic_N_A1))
    
    if(aic_N_A1 < best_aic_N_A1){
      
      best_model_N_A1 = garch_model_N_A1
      
      best_r_N_A1 = r
      
      best_s_N_A1 = s
      
      best_aic_N_A1 = aic_N_A1
      
    }
  }
}


cat(sprintf("Best GARCH Model : ARMA(%d, %d)-GARCH(%d, %d)\t: %f\n", 1, 1, best_r_N_A1, best_s_N_A1, best_aic_N_A1))


# Best model: Garch ~ Normal ~ AIC (1,1) = -2.91

# Garch ~ Normal ~ BIC

best_r_N_B = 1
best_s_N_B = 1


formula = as.formula( paste( sep="", "~arma(2,3) + garch(", best_r_N_B, ",", best_s_N_B,")" ) )

best_model_N_B = garchFit(formula = formula, data = L.ret.NA, trace = FALSE, cond.dist = "norm")

best_bic_N_B = best_model_N_B@fit$ics[["BIC"]]

cat(sprintf("GARCH(%d, %d)\t: %f\n", best_r_N_B, best_s_N_B, best_bic_N_B))

for(r in 1 : 5){
  
  for(s in 1 : 5){
    
    formula = as.formula( paste( sep="", "~arma(2,3) + garch(", r, ",", s,")" ) )
    
    garch_model_N_B = garchFit(formula = formula, data = L.ret.NA, trace = FALSE, cond.dist = "norm")
    
    bic_N_B = garch_model_N_B@fit$ics[["BIC"]]
    
    cat(sprintf("ARMA(%d, %d)-GARCH(%d, %d)\t: %f\n", 2, 3, r, s, bic_N_B))
    
    if(bic_N_B < best_bic_N_B){
      
      best_model_N_B = garch_model_N_B
      
      best_r_N_B = r
      
      best_s_N_B = s
      
      best_bic_N_B = bic_N_B
      
    }
  }
}

cat(sprintf("Best GARCH Model : ARMA(%d, %d)-GARCH(%d, %d)\t: %f\n", 2, 3, best_r_N_B, best_s_N_B, best_bic_N_B))

## Garch ~ Normal ~ BIC
## Best GARCH Model : ARMA(2, 3)-GARCH(1, 5)	: -2.873767

# Garch ~ Student ~ AIC

best_r_S_A = 1
best_s_S_A = 1


formula = as.formula( paste( sep="", "~arma(2,3) + garch(", best_r_S_A, ",", best_s_S_A,")" ) )

best_model_S_A = garchFit(formula = formula, data = L.ret.NA, trace = FALSE, cond.dist = "std")

best_aic_S_A = best_model_S_A@fit$ics[["AIC"]]

cat(sprintf("GARCH(%d, %d)\t: %f\n", best_r_S_A, best_s_S_A, best_aic_S_A))

for(r in 1 : 5){
  
  for(s in 1 : 5){
    
    formula = as.formula( paste( sep="", "~arma(2,3) + garch(", r, ",", s,")" ) )
    
    garch_model_S_A = garchFit(formula = formula, data = L.ret.NA, trace = FALSE, cond.dist = "std")
    
    aic_S_A = garch_model_S_A@fit$ics[["AIC"]]
    
    cat(sprintf("ARMA(%d, %d)-GARCH(%d, %d)\t: %f\n", 2, 3, r, s, aic_S_A))
    
    if(aic_S_A < best_aic_S_A){
      
      best_model_S_A = garch_model_S_A
      
      best_r_S_A = r
      
      best_s_S_A = s
      
      best_aic_S_A = aic_S_A
      
    }
  }
}
cat(sprintf("Best GARCH Model : ARMA(%d, %d)-GARCH(%d, %d)\t: %f\n", 2, 3, best_r_S_A, best_s_S_A, best_aic_S_A))


## Best model: Garch ~ Student ~ AIC 
## Best GARCH Model : ARMA(2, 3)-GARCH(1, 5)	: -3.572105

# Garch ~ Student ~ BIC

best_r_S_B = 1
best_s_S_B = 1


formula = as.formula( paste( sep="", "~arma(2,3) + garch(", best_r_S_B, ",", best_s_S_B,")" ) )

best_model_S_B = garchFit(formula = formula, data = L.ret.NA, trace = FALSE, cond.dist = "std")

best_bic_S_B = best_model_S_B@fit$ics[["BIC"]]

cat(sprintf("GARCH(%d, %d)\t: %f\n", best_r_S_B, best_s_S_B, best_bic_S_B))

for(r in 1 : 5){
  
  for(s in 1 : 5){
    
    formula = as.formula( paste( sep="", "~arma(2,3) + garch(", r, ",", s,")" ) )
    
    garch_model_S_B = garchFit(formula = formula, data = L.ret.NA, trace = FALSE, cond.dist = "std")
    
    bic_S_B = garch_model_S_B@fit$ics[["BIC"]]
    
    cat(sprintf("ARMA(%d, %d)-GARCH(%d, %d)\t: %f\n", 2, 3, r, s, bic_S_B))
    
    if(bic_S_B < best_bic_S_B){
      
      best_model_S_B = garch_model_S_B
      
      best_r_S_B = r
      
      best_s_S_B = s
      
      best_bic_S_B = bic_S_B
      
    }
  }
}

cat(sprintf("Best GARCH Model : ARMA(%d, %d)-GARCH(%d, %d)\t: %f\n", 2, 3, best_r_S_B, best_s_S_B, best_bic_S_B))

## Garch ~ Student ~ BIC
## Best GARCH Model : ARMA(2, 3)-GARCH(1, 1)	: -3.538529


# Garch ~ StudentSKEW ~ AIC

best_r_SS_A = 1
best_s_SS_A = 1


formula = as.formula( paste( sep="", "~arma(2,3) + garch(", best_r_SS_A, ",", best_s_SS_A,")" ) )

best_model_SS_A = garchFit(formula = formula, data = L.ret.NA, trace = FALSE, cond.dist = "sstd", skew = 2)

best_aic_SS_A = best_model_SS_A@fit$ics[["AIC"]]

cat(sprintf("GARCH(%d, %d)\t: %f\n", best_r_SS_A, best_s_SS_A, best_aic_SS_A))

for(r in 1 : 5){
  
  for(s in 1 : 5){
    
    formula = as.formula( paste( sep="", "~arma(2,3) + garch(", r, ",", s,")" ) )
    
    garch_model_SS_A = garchFit(formula = formula, data = L.ret.NA, trace = FALSE, cond.dist = "sstd", skew = 2)
    
    aic_SS_A = garch_model_SS_A@fit$ics[["AIC"]]
    
    cat(sprintf("ARMA(%d, %d)-GARCH(%d, %d)\t: %f\n", 2, 3, r, s, aic_SS_A))
    
    if(aic_SS_A < best_aic_SS_A){
      
      best_model_SS_A = garch_model_SS_A
      
      best_r_SS_A = r
      
      best_s_SS_A = s
      
      best_aic_SS_A = aic_SS_A
      
    }
  }
}

cat(sprintf("Best GARCH Model : ARMA(%d, %d)-GARCH(%d, %d)\t: %f\n", 2, 3, best_r_SS_A, best_s_SS_A, best_aic_SS_A))


## Best model: Garch ~ Student Skew ~ AIC 
## Best GARCH Model : ARMA(2, 3)-GARCH(1, 5)	: -3.572099


# Garch ~ StudentSKEW ~ AIC arma(1;1)

best_r_SS_A1 = 1
best_s_SS_A1 = 1


formula = as.formula( paste( sep="", "~arma(1,1) + garch(", best_r_SS_A1, ",", best_s_SS_A1,")" ) )

best_model_SS_A1 = garchFit(formula = formula, data = L.ret.NA, trace = FALSE, cond.dist = "sstd", skew = 2)

best_aic_SS_A1 = best_model_SS_A1@fit$ics[["AIC"]]

cat(sprintf("GARCH(%d, %d)\t: %f\n", best_r_SS_A1, best_s_SS_A1, best_aic_SS_A1))

for(r in 1 : 5){
  
  for(s in 1 : 5){
    
    formula = as.formula( paste( sep="", "~arma(1,1) + garch(", r, ",", s,")" ) )
    
    garch_model_SS_A1 = garchFit(formula = formula, data = L.ret.NA, trace = FALSE, cond.dist = "sstd", skew = 2)
    
    aic_SS_A1 = garch_model_SS_A1@fit$ics[["AIC"]]
    
    cat(sprintf("ARMA(%d, %d)-GARCH(%d, %d)\t: %f\n", 1, 1, r, s, aic_SS_A1))
    
    if(aic_SS_A1 < best_aic_SS_A1){
      
      best_model_SS_A1 = garch_model_SS_A1
      
      best_r_SS_A1 = r
      
      best_s_SS_A1 = s
      
      best_aic_SS_A1 = aic_SS_A1
      
    }
  }
}

cat(sprintf("Best GARCH Model : ARMA(%d, %d)-GARCH(%d, %d)\t: %f\n", 1, 1, best_r_SS_A1, best_s_SS_A1, best_aic_SS_A1))


##Best GARCH Model : ARMA(1, 1)-GARCH(1, 3)	: -3.564176


# Garch ~ Student Skew ~ BIC

best_r_SS_B = 1
best_s_SS_B = 1


formula = as.formula( paste( sep="", "~arma(2,3) + garch(", best_r_SS_B, ",", best_s_SS_B,")" ) )

best_model_SS_B = garchFit(formula = formula, data = L.ret.NA, trace = FALSE, cond.dist = "sstd", skew = 2)

best_bic_SS_B = best_model_SS_B@fit$ics[["BIC"]]

cat(sprintf("GARCH(%d, %d)\t: %f\n", best_r_SS_B, best_s_SS_B, best_bic_SS_B))

for(r in 1 : 5){
  
  for(s in 1 : 5){
    
    formula = as.formula( paste( sep="", "~arma(2,3) + garch(", r, ",", s,")" ) )
    
    garch_model_SS_B = garchFit(formula = formula, data = L.ret.NA, trace = FALSE, cond.dist = "sstd", skew = 2)
    
    bic_SS_B = garch_model_SS_B@fit$ics[["BIC"]]
    
    cat(sprintf("ARMA(%d, %d)-GARCH(%d, %d)\t: %f\n", 2, 3, r, s, bic_SS_B))
    
    if(bic_SS_B < best_bic_SS_B){
      
      best_model_SS_B = garch_model_SS_B
      
      best_r_SS_B = r
      
      best_s_SS_B = s
      
      best_bic_SS_B = bic_SS_B
      
    }
  }
}

cat(sprintf("Best GARCH Model : ARMA(%d, %d)-GARCH(%d, %d)\t: %f\n", 2, 3, best_r_SS_B, best_s_SS_B, best_bic_SS_B))


########## Exponencial Garch ###################################################

ExpGarch=ugarchspec(variance.model=list(model="eGARCH", garchOrder=c(1,1), submodel=NULL,                                         variance.targeting=FALSE), 
                 mean.model=list(armaOrder=c(2,3), include.mean=FALSE), distribution.model="norm")
my.ugarch.exp=ugarchfit(data=L.ret.NA,spec=ExpGarch)
print(my.ugarch.exp)


ExpGarch1=ugarchspec(variance.model=list(model="eGARCH", garchOrder=c(2,1), submodel=NULL,                                         variance.targeting=FALSE), 
                    mean.model=list(armaOrder=c(2,3), include.mean=FALSE), distribution.model="norm")
my.ugarch.exp1=ugarchfit(data=L.ret.NA,spec=ExpGarch1)
print(my.ugarch.exp1)

ExpGarch2=ugarchspec(variance.model=list(model="eGARCH", garchOrder=c(1,2), submodel=NULL,                                         variance.targeting=FALSE), 
                    mean.model=list(armaOrder=c(2,3), include.mean=FALSE), distribution.model="norm")
my.ugarch.exp2=ugarchfit(data=L.ret.NA,spec=ExpGarch2)
print(my.ugarch.exp2)

ExpGarch3=ugarchspec(variance.model=list(model="eGARCH", garchOrder=c(1,1), submodel=NULL,                                         variance.targeting=FALSE), 
                    mean.model=list(armaOrder=c(1,1), include.mean=FALSE), distribution.model="norm")
my.ugarch.exp3=ugarchfit(data=L.ret.NA,spec=ExpGarch)
print(my.ugarch.exp3)

ExpGarch4=ugarchspec(variance.model=list(model="eGARCH", garchOrder=c(1,1), submodel=NULL,                                         variance.targeting=FALSE), 
                     mean.model=list(armaOrder=c(1,1), include.mean=FALSE), distribution.model="norm")
my.ugarch.exp4=ugarchfit(data=L.ret.NA,spec=ExpGarch4)
print(my.ugarch.exp4)

ExpGarch5=ugarchspec(variance.model=list(model="eGARCH", garchOrder=c(2,2), submodel=NULL,                                         variance.targeting=FALSE), 
                     mean.model=list(armaOrder=c(3,1), include.mean=FALSE), distribution.model="std")
my.ugarch.exp5=ugarchfit(data=L.ret.NA,spec=ExpGarch5)
print(my.ugarch.exp5)

FAPARCH<-garchFit(formula = ~arma(2,3) + aparch(1,1), data=L.ret.NA, trace=FALSE, cond.dist="std", algorithm="lbfgsb+nm")  
summary (FAPARCH)

#(a) testes Box Pierce e Ljung e Box 
#(b) testes Box Pierce e Ljung e Box na série dos quadrados dos resíduos 
#(c) teste ARCH-LM
#(d) apresente o qqplot (cuidado: se foi ajustado uma dist diferente da normal o qqplot deve ser com os quantis da respectiva distribui¸c~ao) 
#(e) faça os comentários gerais sobre os testes realizados


#(a) Após Avaliar os melhores dados, selecionamos o modelo APARCH

summary(FAPARCH)

FR=residuals(FAPARCH, standardize=TRUE)
par(mfrow=c(1,2))
acf(FR)
acf(FR^2)


#(b)
adf.test(FR)                           
kpss.test(FR, null="Trend")
jarque.bera.test(FR)
Box.test(FR, lag = 5)
Box.test(FR, lag = 10)
Box.test(FR, lag = 50)
Box.test(FR^2, lag=10)


#(c)
ArchTest(FR)

#(d)
par(mfrow=c(1,1))
n=length(FR)
qqplot(qt(ppoints(n), df=5), FR, ylim=c(-5,5), main="QQ plot for t distribution")
qqline(FR,col=2)

