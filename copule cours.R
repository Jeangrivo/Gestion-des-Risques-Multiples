install.packages("copula")
install.packages("psych")
install.packages("rgl")
library("copula")
install.packages("mvtnorm")
library(plot3D)
library("mvtnorm")
library(MASS)
set.seed(100)

cop1 <- normalCopula(param=c(-0.5), dim=2, dispstr="un") # 2D copula to plot
cop2 <- normalCopula(param=c(0), dim=2, dispstr="un") # 2D copula to plot
cop3 <- normalCopula(param=c(0.9), dim=2, dispstr="un") # 2D copula to plot


grid <- expand.grid(x=x, y=y)
p1=wireframe2(cop1, dCopula, main=expression(rho==-0.5),scales = list(arrows =T, col = "black"),col = "gray60", theta=35,phi=35, shade = 1,  xlab = expression(u[1]), ylab = expression(u[2]),zlab=expression(paste(c(list(u[1],u[2])),"        ")))
p2=wireframe2(cop2, dCopula, main=expression(rho==0),scales = list(arrows =T, col = "black"),col = "gray60", theta=35,phi=35, shade = 1,   xlab = expression(u[1]), ylab = expression(u[2]),zlab=expression(paste(c(list(u[1],u[2])),"        ")))
p3=wireframe2(cop3, dCopula, main=expression(rho==0.9),scales = list(arrows =T, col = "black"),col = "gray60", theta=35,phi=35, shade = 1,   xlab = expression(u[1]), ylab = expression(u[2]),zlab=expression(paste(c(list(u[1],u[2])),"        ")))
grid.arrange(p1,p2,p3, ncol=3)


myMvd1 <- mvdc(cop1,margins = c("norm", "norm"), paramMargins = list(list(mean = 0,  sd = 1), list(mean = 0, sd = 1)))
myMvd2 <- mvdc(cop2,margins = c("norm", "norm"), paramMargins = list(list(mean = 0,  sd = 1), list(mean = 0, sd = 1)))
myMvd3 <- mvdc(cop3,margins = c("norm", "norm"), paramMargins = list(list(mean = 0,  sd = 1), list(mean = 0, sd = 1)))

grid <- expand.grid(x=x, y=y)
c1=wireframe2(myMvd1,  dMvdc, xlim = c(-4, 4), ylim = c(-4, 4),scales = list(arrows =T, col = "black"),col = "gray60",main=expression(rho==-0.5), theta=35,phi=35, shade = 1,   xlab = expression(x[1]), ylab = expression(x[2]),zlab=expression(paste(f(list(x[1],x[2])),"        ")))
c2=wireframe2(myMvd2,  dMvdc, xlim = c(-4, 4), ylim = c(-4, 4),scales = list(arrows =T, col = "black"),col = "gray60",main=expression(rho==0), theta=35,phi=35, shade = 1,   xlab = expression(x[1]), ylab = expression(x[2]),zlab=expression(paste(f(list(x[1],x[2])),"        ")))
c3=wireframe2(myMvd3,  dMvdc, xlim = c(-4, 4), ylim = c(-4, 4),scales = list(arrows =T, col = "black"),col = "gray60",main=expression(rho==0.9), theta=35,phi=35, shade = 1,   xlab = expression(x[1]), ylab = expression(x[2]),zlab=expression(paste(f(list(x[1],x[2])),"        ")))
grid.arrange(c1,c2,c3, ncol=3)



par(mfrow=c(1,3))
contour(myMvd1, dMvdc, xlim = c(-3, 3), ylim = c(-3, 3),main=expression(rho==-0.5),   xlab = expression(x[1]), ylab = expression(x[2]))
contour(myMvd2, dMvdc, xlim = c(-3, 3), ylim = c(-3, 3),main=expression(rho==0),   xlab = expression(x[1]), ylab = expression(x[2]))
contour(myMvd3, dMvdc, xlim = c(-3, 3), ylim = c(-3, 3),main=expression(rho==0.9),   xlab = expression(x[1]), ylab = expression(x[2]))

################ Clayton etc ##########################
cop1 <-claytonCopula(param=c(3), dim=2) # 2D copula to plot
cop2 <- gumbelCopula(param=c(3), dim=2) # 2D copula to plot
cop3 <- frankCopula(param=c(3), dim=2) # 2D copula to plot


grid <- expand.grid(x=x, y=y)
p1=wireframe2(cop1, dCopula, main=expression(paste("Clayton ", theta==3)),scales = list(arrows =T, col = "black"),col = "gray60", theta=35,phi=35, shade = 1,  xlab = expression(u[1]), ylab = expression(u[2]),zlab=expression(paste(c(list(u[1],u[2])),"        ")))
p2=wireframe2(cop2, dCopula, main=expression(paste("Gumbel ", theta==3)),scales = list(arrows =T, col = "black"),col = "gray60", theta=35,phi=35, shade = 1,   xlab = expression(u[1]), ylab = expression(u[2]),zlab=expression(paste(c(list(u[1],u[2])),"        ")))
p3=wireframe2(cop3, dCopula, main=expression(paste("Frank ", theta==3)),scales = list(arrows =T, col = "black"),col = "gray60", theta=35,phi=35, shade = 1,   xlab = expression(u[1]), ylab = expression(u[2]),zlab=expression(paste(c(list(u[1],u[2])),"        ")))
grid.arrange(p1,p2,p3, ncol=3)



myMvd1 <- mvdc(cop1,margins = c("norm", "norm"), paramMargins = list(list(mean = 0,  sd = 1), list(mean = 0, sd = 1)))
myMvd2 <- mvdc(cop2,margins = c("norm", "norm"), paramMargins = list(list(mean = 0,  sd = 1), list(mean = 0, sd = 1)))
myMvd3 <- mvdc(cop3,margins = c("norm", "norm"), paramMargins = list(list(mean = 0,  sd = 1), list(mean = 0, sd = 1)))

grid <- expand.grid(x=x, y=y)
c1=wireframe2(myMvd1,  dMvdc, xlim = c(-4, 4),scales = list(arrows =T, col = "black"), ylim = c(-4, 4),col = "gray60",main=expression(paste("Clayton ", theta==3)), theta=35,phi=35, shade = 1,   xlab = expression(x[1]), ylab = expression(x[2]),zlab=expression(paste(f(list(x[1],x[2])),"        ")))
c2=wireframe2(myMvd2,  dMvdc, xlim = c(-4, 4),scales = list(arrows =T, col = "black"), ylim = c(-4, 4),col = "gray60",main=expression(paste("Gumbel ", theta==3)), theta=35,phi=35, shade = 1,   xlab = expression(x[1]), ylab = expression(x[2]),zlab=expression(paste(f(list(x[1],x[2])),"        ")))
c3=wireframe2(myMvd3,  dMvdc, xlim = c(-4, 4),scales = list(arrows =T, col = "black"), ylim = c(-4, 4),col = "gray60",main=expression(paste("Frank ", theta==3)), theta=35,phi=35, shade = 1,   xlab = expression(x[1]), ylab = expression(x[2]),zlab=expression(paste(f(list(x[1],x[2])),"        ")))
grid.arrange(c1,c2,c3, ncol=3)



par(mfrow=c(1,3))
contour(myMvd1, dMvdc, xlim = c(-3, 3), ylim = c(-3, 3),main=expression(paste("Clayton ", theta==3)),   xlab = expression(x[1]), ylab = expression(x[2]))
contour(myMvd2, dMvdc, xlim = c(-3, 3), ylim = c(-3, 3),main=expression(paste("Gumbel ", theta==3)),   xlab = expression(x[1]), ylab = expression(x[2]))
contour(myMvd3, dMvdc, xlim = c(-3, 3), ylim = c(-3, 3),main=expression(paste("Frank ", theta==3)),  xlab = expression(x[1]), ylab = expression(x[2]))





#################### Dependogramme ##############



cop1 <-normalCopula(param=c(0.9), dim=2) # 2D copula to plot
cop2 <- tCopula(param=c(0.9),df=3, dim=2) # 2D copula to plot
cop3 <- claytonCopula(param=c(3), dim=2) # 2D copula to plot

myMvd1 <- mvdc(cop1,margins = c("norm", "norm"), paramMargins = list(list(mean = 0,  sd = 1), list(mean = 0, sd = 1)))
myMvd2 <- mvdc(cop2,margins = c("norm", "norm"), paramMargins = list(list(mean = 0,  sd = 1), list(mean = 0, sd = 1)))
myMvd3 <- mvdc(cop3,margins = c("norm", "norm"), paramMargins = list(list(mean = 0,  sd = 1), list(mean = 0, sd = 1)))

n=2000
X=rMvdc(n, myMvd1)
Y=rMvdc(n, myMvd2)
Z=rMvdc(n, myMvd3)

par(mfrow=c(1,3))
plot(rank(X[,1])/length(X[,1]),rank(X[,2])/length(X[,2]),main=expression(paste("Copule Gaussienne, ", rho==0.9)),  xlab = expression(u[1]), ylab = expression(u[2]))
plot(rank(Y[,1])/length(Y[,1]),rank(Y[,2])/length(Y[,2]),main=expression(paste("Copule Student, ",list( rho==0.9, nu==2))),  xlab = expression(u[1]), ylab = expression(u[2]))
plot(rank(Z[,1])/length(Z[,1]),rank(Z[,2])/length(Z[,2]),main=expression(paste("Copule Clayton, ", theta==3)),  xlab = expression(u[1]), ylab = expression(u[2]))






#################### Kendall plot ##############
cop2 <- tCopula(param=c(0.9),df=2, dim=2) # 2D copula to plot
myMvd2 <- mvdc(cop2,margins = c("norm", "norm"), paramMargins = list(list(mean = 0,  sd = 1), list(mean = 0, sd = 1)))
Y=rMvdc(n, myMvd2)

n=nrow(X)
i=rep(1:n,each=n)
j=rep(1:n,n)
S=((Y[i,1]>Y[j,1])&(Y[i,2]>Y[j,2]))
H=tapply(S,i,sum)/(n-1)



cop1 <-indepCopula( dim=2) # 2D copula to plot
cop2 <- normalCopula(param=c(0.9), dim=2) # 2D copula to plot
cop3 <- claytonCopula(param=c(3), dim=2) # 2D copula to plot
cop4 <- tCopula(param=c(0.9),df=2, dim=2) # 2D copula to plot

myMvd1 <- mvdc(cop1,margins = c("norm", "norm"), paramMargins = list(list(mean = 0,  sd = 1), list(mean = 0, sd = 1)))
myMvd2 <- mvdc(cop2,margins = c("norm", "norm"), paramMargins = list(list(mean = 0,  sd = 1), list(mean = 0, sd = 1)))
myMvd3 <- mvdc(cop3,margins = c("norm", "norm"), paramMargins = list(list(mean = 0,  sd = 1), list(mean = 0, sd = 1)))
myMvd4 <- mvdc(cop4,margins = c("norm", "norm"), paramMargins = list(list(mean = 0,  sd = 1), list(mean = 0, sd = 1)))

s=10
H1=H2=H3=H4<- matrix(nrow = n, ncol = s)

for (k in 1:s)
{
n=2000
X1=rMvdc(n, myMvd1)
X2=rMvdc(n, myMvd2)
X3=rMvdc(n, myMvd3)
X4=rMvdc(n, myMvd4)
S=((X1[i,1]>X1[j,1])&(X1[i,2]>X1[j,2]))
H1[,k]=sort(tapply(S,i,sum)/(n-1))
S=((X2[i,1]>X2[j,1])&(X2[i,2]>X2[j,2]))
H2[,k]=sort(tapply(S,i,sum)/(n-1))
S=((X3[i,1]>X3[j,1])&(X3[i,2]>X3[j,2]))
H3[,k]=sort(tapply(S,i,sum)/(n-1))
S=((X4[i,1]>X4[j,1])&(X4[i,2]>X4[j,2]))
H4[,k]=sort(tapply(S,i,sum)/(n-1))
}
par(mfrow=c(2,2))
plot(sort(H),(rowMeans(H1)),type="l",xlim=c(0,1),xlab="Theorique", ylab="Empirique",main="Ajustment Copule Produit",lwd=2)
lines(seq(0,1,0.1),seq(0,1,0.1))
plot(sort(H),(rowMeans(H2)),type="l",xlim=c(0,1),xlab="Theorique", ylab="Empirique",main="Ajustment Copule Gaussienne",lwd=2)
lines(seq(0,1,0.1),seq(0,1,0.1))
plot(sort(H),(rowMeans(H3)),type="l",xlim=c(0,1),xlab="Theorique", ylab="Empirique",main="Ajustment Copule Clayton",lwd=2)
lines(seq(0,1,0.1),seq(0,1,0.1))
plot(sort(H),(rowMeans(H4)),type="l",xlim=c(0,1),xlab="Theorique", ylab="Empirique",main="Ajustment Copule Student",lwd=2)
lines(seq(0,1,0.1),seq(0,1,0.1))




