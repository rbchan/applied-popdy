## ----eval=FALSE,include=FALSE-------------------------------------------------
## source("../../rnw2pdf.R")
## rnw2pdf("lecture-logistic-growth")
## rnw2pdf("lecture-logistic-growth",tangle=TRUE)


## ----geomVlogistic,echo=FALSE-------------------------------------------------
#Year <- 2000:3000
Time <- 0:100
T <- length(Time)
rmax <- .1
K <- 50
Ng <- Nl <- rep(0, T)
Ng[1] <- Nl[1] <- 2
for(t in 2:T) {
    Ng[t] <- Ng[t-1] + Ng[t-1]*rmax
    Nl[t] <- Nl[t-1] + Nl[t-1]*rmax*(1 - Nl[t-1]/K)
}

## ----geom,include=FALSE,echo=FALSE--------------------------------------------
plot(Time, Ng, lwd=5, type="l", ylim=c(0, 100), cex.lab=1.5,
     xlab="Time (t)", ylab="Population size (N)")
legend(50, 100, c("Geometric growth", ""), lty=1, col=c("black", NA), lwd=4)


## ----geom-logistic,include=FALSE,echo=FALSE-----------------------------------
plot(Time, Ng, lwd=5, type="l", ylim=c(0, 100), cex.lab=1.5,
     xlab="Time (t)", ylab="Population size (N)")
lines(Time, Nl, lwd=4, col="purple")
legend(50, 100, c("Geometric growth", "Logistic growth"), lty=1,
       col=c("black","purple"), lwd=4)


## ----lr1,include=FALSE,echo=FALSE,fig.width=12,fig.height=4-------------------
par(mai=c(0.9,1,0.3,0.3))
plot(Time, Nl, lwd=4, type="l", ylim=c(0, 60), cex.lab=1.9,
     xlab="Time (t)", ylab="Population size (N)", col="purple")
points(Time[which.min(abs(Nl-K/2))], Nl[which.min(abs(Nl-K/2))],
       pch=16, cex=5, col="orange")
text(Time[which.min(abs(Nl-K/2))]+5, Nl[which.min(abs(Nl-K/2))],
     labels="Inflection point (K/2)", pos=4, cex=1.5)

## ----lambda,include=FALSE,echo=FALSE,fig.width=12,fig.height=4----------------
lambda <- Nl[-1]/Nl[-T]
par(mai=c(0.9,1,0.3,0.3))
plot(Time[-1], lambda, lwd=4, type="l", cex.lab=1.9,
     xlab="Time (t)", col="blue",
     ylab=expression(paste("Growth rate: ", "(", lambda, "=", N[t+1]/N[t], ")")))


## ----lr2,include=FALSE,echo=FALSE,fig.width=12,fig.height=4-------------------
par(mai=c(0.9,1,0.3,0.3))
plot(Time, Nl, lwd=4, type="l", ylim=c(0, 60), cex.lab=1.9,
     xlab="Time (t)", ylab="Population size (N)", col="purple")
points(Time[which.min(abs(Nl-K/2))], Nl[which.min(abs(Nl-K/2))],
       pch=16, cex=5, col="orange")
text(Time[which.min(abs(Nl-K/2))]+5, Nl[which.min(abs(Nl-K/2))],
     labels="Inflection point (K/2)", pos=4, cex=1.5)

## ----delta,include=FALSE,echo=FALSE,fig.width=12,fig.height=4-----------------
delta <- Nl[-1] - Nl[-T]
par(mai=c(0.9,1,0.3,0.3))
plot(Time[-1], delta, lwd=4, type="l", cex.lab=1.9,
     xlab="Time (t)", col="turquoise4",
     ylab=expression(paste("Growth: ", "(", Delta, "N =", N[t+1]-N[t], ")")))


## ----delta-N,include=FALSE,echo=FALSE,fig.width=8,fig.height=6----------------
plot(Nl[-T], delta, lwd=4, type="l", cex.lab=1.2,
     xlab="Population size (N)",
     ylab=expression(paste("Growth: ", "(", Delta, "N =", N[t+1]-N[t], ")")))
abline(h=0, col="gray")
abline(v=K/2, lty=3, lwd=2)
text(K/2, 0.05, "Inflection point (K/2)", pos=4)


## ----echo=FALSE---------------------------------------------------------------
Nl4 <- Nl3 <- Nl2 <- Nl
Nl4[1] <- Nl3[1] <- Nl2[1] <- 2
for(t in 2:T) {
    Nl2[t] <- Nl2[t-1] + Nl2[t-1]*0.5*(1-Nl2[t-1]/K)
    Nl3[t] <- Nl3[t-1] + Nl3[t-1]*2.0*(1-Nl3[t-1]/K)
    Nl4[t] <- Nl4[t-1] + Nl4[t-1]*3.0*(1-Nl4[t-1]/K)
}

## ----Nl,include=FALSE,echo=FALSE,fig.width=8,fig.height=6---------------------
plot(Time, Nl, lwd=4, type="l", ylim=c(0, 100), cex.lab=1.3,
     xlab="Time (t)", ylab="Population size (N)", col="purple")
legend(0, 100, c("rmax=0.1", "", "", ""),
       lwd=4, col=c("purple", NA, NA, NA))

## ----Nl2,include=FALSE,echo=FALSE,fig.width=8,fig.height=6--------------------
plot(Time, Nl, lwd=4, type="l", ylim=c(0, 100), cex.lab=1.3,
     xlab="Time (t)", ylab="Population size (N)", col="purple")
lines(Time, Nl2, lwd=4, col="blue")
legend(0, 100, c("rmax=0.1", "rmax=0.5", "", ""),
       lwd=4, col=c("purple", "blue", NA, NA))

## ----Nl3,include=FALSE,echo=FALSE,fig.width=8,fig.height=6--------------------
plot(Time, Nl, lwd=4, type="l", ylim=c(0, 100), cex.lab=1.3,
     xlab="Time (t)", ylab="Population size (N)", col="purple",
     main="Damped oscillation")
lines(Time, Nl2, lwd=4, col="blue")
lines(Time, Nl3, lwd=4, col="orange")
legend(0, 100, c("rmax=0.1", "rmax=0.5", "rmax=2.0", ""),
       lwd=4, col=c("purple", "blue", "orange", NA))

## ----Nl4,include=FALSE,echo=FALSE,fig.width=8,fig.height=6--------------------
plot(Time, Nl, lwd=4, type="l", ylim=c(0, 100), cex.lab=1.3,
     xlab="Time (t)", ylab="Population size (N)", col="purple",
     main="Chaos")
lines(Time, Nl2, lwd=4, col="blue")
lines(Time, Nl3, lwd=4, col="orange")
lines(Time, Nl4, lwd=4, col="gray")
legend(0, 100, c("rmax=0.1", "rmax=0.5", "rmax=2.0", "rmax=3.0"),
       lwd=4, col=c("purple", "blue", "orange", "gray"))


## ----Nl5,include=FALSE,echo=FALSE,fig.width=8,fig.height=6--------------------
Nl5 <- Nl
Nl5[1] <- 2
Nl5[2] <- 3
Nl5[3] <- 4
for(t in 4:T) {
    Nl5[t] <- Nl5[t-1] + Nl5[t-1]*1.1*(1-Nl5[t-2]/K)
}
plot(Nl5, type="l", xlab="Time (t)", ylab="Population size (N)",
     cex.lab=1.3, panel.first=abline(h=K, col="blue", lty=2),
     col="darkslategray3",
     lwd=4, main="r=1.1, K=50, lag=2")


## ----Nl6,include=FALSE,echo=FALSE,fig.width=8,fig.height=6--------------------
Nl6 <- Nl
Nl6[1] <- 50
k0 <- K
k1 <- 10
c <- 20
Kt <- numeric(T)
for(t in 2:T) {
    Kt[t] <- k0 + k1*cos(2*pi*t/c)
    Nl6[t] <- Nl6[t-1] + Nl6[t-1]*1.1*(1-Nl6[t-1]/Kt[t])
}
plot(Nl6, type="l", xlab="Time (t)", ylab="Population size (N)",
     cex.lab=1.3, panel.first=abline(h=K, col="blue", lty=2),
     lwd=4, main="r=1.1, k0=50, k1=10, c=20")

## ----Nl6-2,include=FALSE,echo=FALSE,fig.width=8,fig.height=6------------------
plot(Nl6, type="l", xlab="Time (t)", ylab="Population size (N)",
     cex.lab=1.3, panel.first=abline(h=K, col="blue", lty=2),
     lwd=4, col=1, main="k0=50, k1=10, c=20", xlim=c(25, 55),
     ylim=c(35,65))
arrows(30, 38, 50, 38, angle=90, code=3, length=0.05, lwd=2)
text(40, 36, "period (c)", cex=1)
arrows(53, 50, 53, 60, angle=90, code=3, length=0.05, lwd=2)
text(53, 55, "amplitude (k1)", pos=2, cex=1)

