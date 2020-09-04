## ----eval=FALSE,include=FALSE-------------------------------------------------
## source("../../rnw2pdf.R")
## rnw2pdf("lecture-harvest")
## rnw2pdf("lecture-harvest",tangle=TRUE)


## ----msy1,include=FALSE,echo=FALSE--------------------------------------------
N <- seq(0, 1000, by=10)
rmax <- 0.1
K <- 1000
H.1 <- N*rmax*(1 - N/K)
#par(mfrow=c(1,2), mai=c(0.7, 0.7, 0.2, 0.2))
#plot(0:T, Nl, xlab="Time", ylab="Population size (N)")
plot(N, H.1, type="l", #asp=1,
     col="black", lwd=2,
     cex.lab=1.4,
     xlab="Population size (N)",
     ylab="Sustainable harvest (H)")
abline(h=0, col=gray(0.7))
segments(K/2, 0, K/2, max(H.1), lty=2, lwd=2, col="purple")


## ----msy2,include=FALSE,echo=FALSE--------------------------------------------
N <- seq(0, 1000, by=10)
rmax <- 0.5
K <- 1000
H <- N*rmax*(1 - N/K)
plot(N, H, type="l", #asp=1,
     col="black", lwd=2,
     cex.lab=1.4,
     xlab="Population size (N)",
     ylab="Sustainable harvest (H)")
abline(h=0, col=gray(0.7))
segments(K/2, 0, K/2, max(H), lty=2, lwd=2, col="purple")


## ----phi1,echo=FALSE,include=FALSE,fig.width=8,fig.height=6-------------------
plot(function(x) 0.8 - 0.005*x, 0, 100, col="blue",
     xlab="Population size (N)",
     ylab="Survival (S)",
     ylim=c(0, 1), lwd=2)


## ----phibar,include=FALSE,echo=FALSE,fig.height=6,fig.width=8-----------------
plot(function(h) (1-h)*(0.8 - 0.005*(100 - 100*h)), 0, 1,
     ylim=c(0,0.6), xlab="Harvest rate (h)",
     ylab="Overall survival rate",
     main=expression(paste(beta[0], "= 0.8, ", beta[1], "= 0.005, N=100")),
     lwd=3, col="purple")

