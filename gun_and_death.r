# read data
library(astsa)

GD_data = read.table("/home/huachao/Documents/course/STA137/Final Project/GD.dat.txt",header = FALSE)
Gun = GD_data[,1]
Death = GD_data[,2]
## plot the data
layout(matrix(1:4,2), widths=c(2.5,1))
par(mgp=c(1.6,.6,0), mar=c(2,2,.5,0)+.5)
tsplot(Gun, main="", ylab="", col=4, margin=0)
mtext("Gun_sale", side=3, line=.5, cex=1.2, font=2, adj=0)
tsplot(Death, main="", ylab="", col=4, margin=0)
mtext("Death", side=3, line=.5, cex=1.2, font=2, adj=0)
qqnorm(Gun, main="", col=4); qqline(Gun, col=2, lwd=2)
qqnorm(Death, main="", col=4); qqline(Death, col=2, lwd=2)

par(mfrow = c(2,2))  
acf(Gun)
acf(Death)
pacf(Gun)
pacf(Death)
# correaltion part plot
pairs(~Death+Gun_sale,data=GD_data,main="Scatterplot Matrix")
ccf(Death,Gun)
## -------------------------------------------------------------------------  ##
## tranformation         # do Gun first
## plot the data
layout(matrix(1:4,2), widths=c(2.5,1))
par(mgp=c(1.6,.6,0), mar=c(2,2,.5,0)+.5)
tsplot(log(Gun), main="", ylab="log(Gun_sale)", col=4, margin=0)
tsplot(diff(Gun), ylab=expression(nabla~Gun_sale), main="", col=4, margin=0)
qqnorm(log(Gun), main="", col=4); qqline(log(Gun), col=2, lwd=2)
qqnorm(diff(Gun), main="", col=4); qqline(diff(Gun), col=2, lwd=2)

# do Death second
tsplot(log(Death), main="", ylab="log(Death)", col=4, margin=0)
tsplot(diff(Death), main="", ylab=expression(nabla~Death), col=4, margin=0)
qqnorm(log(Death), main="", col=4); qqline(log(Death), col=2, lwd=2)
qqnorm(diff(Death), main="", col=4); qqline(diff(Death), col=2, lwd=2)

# draw acf and pacf of gun
par(mfrow=c(2,1))
acf2(diff(Gun),50)
tsplot(diff(diff(Gun),12), ylab=expression(nabla~nabla[12]~Gun_sale), main="", col=4, margin=0)
acf2(diff(diff(Gun),12),50)
# model selection and diagonose
m1=sarima(Gun, p=0, d=1, q=2,D=1,P=1,Q=1,S=12)
m2=sarima(Gun, p=0, d=1, q=2,D=1,P=1,S=12)
m3=sarima(Gun, p=0, d=1, q=2,D=1,P=3,S=12)
c(m1$AIC,m1$AICc,m1$BIC)
c(m2$AIC,m2$AICc,m2$BIC)
c(m3$AIC,m3$AICc,m3$BIC)
# draw acf and pacf of death
acf2(diff(Death),50)
tsplot(diff(diff(Death),12), ylab=expression(nabla~nabla[12]~Death), main="", col=4, margin=0)
acf2(diff(diff(Death),12),50)
m4=sarima(Death, p=0, d=1, q=1,D=1,Q=1,S=12)
m5=sarima(Death, p=1, d=1, q=1,D=1,Q=1,S=12)
c(m4$AIC,m4$AICc,m4$BIC)
c(m4$AIC,m4$AICc,m4$BIC)

## do autocorrelated part 
fit1 = lm(Death[6:227]~Gun[1:222])
x = resid(fit1)
layout(matrix(1:4,2), widths=c(2.5,1))
par(mgp=c(1.6,.6,0), mar=c(2,2,.5,0)+.5)
tsplot(x, main="", ylab="", col=4, margin=0)
mtext("residual", side=3, line=.5, cex=1.2, font=2, adj=0)
tsplot(log(x), main="", ylab="", col=4, margin=0)
mtext("log(residual)", side=3, line=.5, cex=1.2, font=2, adj=0)
qqnorm(x, main="", col=4); qqline(x, col=2, lwd=2)
qqnorm(log(x), main="", col=4); qqline(log(x1), col=2, lwd=2)
acf2(x,50)
par(mfrow=c(1,2))
tsplot(diff(x,12), ylab=expression(nabla[12]~residual), main="", col=4, margin=0)
acf2(diff(x,12),50)
tsplot(diff(diff(x,12)), ylab=expression(nabla[12]~residual), main="", col=4, margin=0)
qqnorm(diff(diff(x,12)), main="", col=4); qqline(diff(diff(x,12)), col=2, lwd=2)
acf2(diff(diff(x,12)),50)

# fit 
y=Death[6:227]
z=Gun[1:222]
m6 = sarima(y,0,1,1,0,1,1,S=12,xreg=z)
m7 = sarima(y,0,1,1,1,1,1,S=12,xreg=z)

## model selection
c(m6$AIC,m6$AICc,m6$BIC)
c(m7$AIC,m7$AICc,m7$BIC)

  par(mfrow=c(1,2))
  sarima.for(Gun,0,1,2,1,1,0,S=12,n.ahead = 50)
  sarima.for(Death,0,1,1,0,1,1,S=12,n.ahead = 50)
