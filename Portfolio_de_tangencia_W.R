library(pracma)
library(reshape2)
library(ggplot2)
library(ggthemes)
library(cowplot)

Sigma <- matrix(c(16,0,0,0,9,0,0,0,4),ncol=3)
m <- c(8,4,2)
invSigma <- inv(Sigma)
rf = 2.5

um <- ones(length(m),1)

a <- as.vector(t(um)%*%invSigma%*%um)
b <- as.vector(t(um)%*%invSigma%*%m)
c <- as.vector(t(m)%*%invSigma%*%m)
d <- as.vector(a*c-b^2)

# Portfolio de tangencia
wt <- invSigma%*%((m-rf*um)/(b-a*rf)) 
mu_t <- (b*rf-c)/(a*rf-b)
sigma_t <- sqrt((a*rf^2-2*b*rf + c)/(b-a*rf)^2)

h <- (mu_t-rf)/sigma_t
sdcml <- seq(0,7,0.01)
cml <- rf + h*sdcml

mucml <- seq(-8,10,0.01)
cmlinef <- rf - h*sdcml

wg <- invSigma%*%um/a #Pesos do portfolio a variancia maxima global
v_g <- t(wg)%*%Sigma%*%wg # Minima variania global
m_g <- t(wg)%*%m # Minimo retorno medio global

###################################
# Costruir a Fronteira Eficiente #
###################################
v_eff <- (((a*mucml^2)-2*b*mucml+c)/d)^0.5

plot_1 <- cbind.data.frame(mucml,v_eff)
plot_2 <- cbind.data.frame(sdcml,cml)
plot_3 <- cbind.data.frame(sdcml,cmlinef)
plot_1_1 <- subset(plot_1,mucml < as.numeric(m_g))
plot_1_2 <- subset(plot_1,mucml >= as.numeric(m_g))


ggplot(plot_1_1,aes(x = v_eff, y = mucml)) + 
  geom_path(col="blue") +
  geom_line(data  = plot_1_2, aes(x = v_eff, y = mucml), col="red")+
  geom_line(data  = plot_2, aes(x = sdcml, y = cml), col="green") +
  geom_line(data  = plot_2, aes(x = sdcml, y = cmlinef), col="blue") +
  geom_point(data = cbind.data.frame(mu_t,sigma_t), aes(sigma_t,mu_t), col = "red") +
  geom_point(data = cbind.data.frame(m,sd=diag(Sigma)^0.5),aes(sd,m)) +
  xlab("Desvio padrao") + ylab("Retorno medio") +
  geom_text(x=1, y=5, label="0<w<1",family = "Times New Roman") +
  geom_text(x=1, y=0, label="w<0",family = "Times New Roman") +
  geom_text(x=4.5, y=10, label="1<w",family = "Times New Roman") +
  ggtitle("CML (Verde), FE (Vermelho) e Portfolio de Tangencia (Ponto Vermelho)") + theme_grey() +
  scale_y_continuous(breaks=seq(-8,12,1),limits = c(-8,12)) +
  scale_x_continuous(breaks=seq(0,7,0.5),limits = c(0,7)) +
  theme(plot.title = element_text(size = 10))


