# Dados
setwd("C:/Users/Daniel/Documents/MESTRADO/II SEMESTRE/ECONOMETRIA/EXERCÍCIOS EM R/Frama & French (paper)")

data = read.csv("DADOS DIARIOS.csv")

# Carregamento de pacotes:
library (moments)
library(FinTS)
library(ggplot2)
library(anytime)
options(digits = 03)

# Manipulação dos dados:
portfolio = read.csv("Global_6_Portfolios_ME_BE-ME_Daily.csv")

junto = cbind(data,portfolio[-1])

junto$date <- anydate(junto$date)

# Escolhendo um  portifólio:
Small <- cbind(data, portfolio[2])

# Manipulação de dados:
RMRF <- junto$Mkt.RF/100
SMB <- junto$SMB/100
HML <- junto$HML/100
RF <- junto$RF/(252)^0.5  

# Retorno do SMALL.LOBM
SMALL.LOBM <- (junto$SMALL.LoBM/100 - junto$RF)

# Resolução do modelo linear com mínimos quadrados:
Ysmall = SMALL.LOBM
Xsmall = cbind(1,RMRF,SMB,HML)

n <- nrow(Xsmall)
k <- ncol(Xsmall)

data2 <- cbind(Ysmall, Xsmall [,2:4])

#b = (X'X)*X'y (Regressão com minimo quadrado)
bsmall <- solve(t(Xsmall)%*%Xsmall)%*%t(Xsmall)%*%Ysmall
bsmall

# Resíduos:
e_small <- Ysmall - Xsmall%*%bsmall
e_small

# RSS 
RSS <- as.numeric(t(e_small)%*%e_small)
RSS

# R2
R2 <- 1 - as.numeric(RSS)/as.numeric(t(Ysmall)%*%Ysmall)
R2

# R2 ajustado
R2A <- 1 - (n-1)/(n-k)*(1-R2) 
R2A

# Estimação
Sigma_2 <- as.numeric(RSS/(n-k))
Sigma_2

Var_b <- Sigma_2*solve(t(Xsmall)%*% Xsmall)
Var_b

## Regressão
ffregression <- lm(SMALL.LOBM ~ RMRF + SMB + HML,data=as.data.frame(data2))
summary (ffregression)


# Fazendo as estatísticas dos dados 
estatisticas <- function(x)
{
  y = as.matrix(x[,-1])  
  nobs=NROW(y)
  N=NCOL(y)
  out <- matrix(0, ncol=N, nrow=6)
  for(i in 1:N){
    mean = mean(y[,i])
    std.dev = sd(y[,i])
    max = max(y[,i])
    min = min(y[,i])
    skewness = skewness(y[,i])
    kurtosis = kurtosis(y[,i])
    out[,i] = t(cbind(mean, std.dev, max, min, skewness, kurtosis))
    out = cbind(out)
  }
  colnames(out) <- colnames(y)
  rownames(out) <- c("mean","std.dev","max","min","skewness","kurtosis")
  out
}

DS = junto$SMALL.LoBM/100
DD = cbind(1,RMRF,SMB,HML,RF, DS ,Ysmall)
options(digits = 03)
estatisticas (DD)


# gráficos

mtplot <- matrix(1:6, ncol=3, nrow=2, byrow=TRUE)
layout(mtplot)
layout.show(6)
plot (Small$Mkt.R, type="l",xlab = "observações", ylab = "Mkt.RF")
plot (Small$SMB, type="l",xlab = "observações", ylab = "SMB")
plot (Small$HML, type="l",xlab = "observações", ylab = "HML")
plot (Small$SMALL.LoBM, type="l",xlab = "observações", ylab = "SMALL.LoBM - RF")
plot (Small$RF, type="l",xlab = "observações", ylab = "RF")

# gráfico da regressão 

mtplot <- matrix(1:6, ncol=3, nrow=2, byrow=TRUE)
layout(mtplot)
layout.show(6)
plot (ffregression)




