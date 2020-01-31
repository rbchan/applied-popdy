## ----eval=FALSE,include=FALSE-------------------------------------------------
## source("../../rnw2pdf.R")
## rnw2pdf("lecture-extinction")
## rnw2pdf("lecture-extinction",tangle=TRUE)


## ----exp1,include=FALSE,echo=FALSE,fig.width=8,fig.height=6-------------------
T <- 50
N <- integer(T+1)
N[1] <- 50
for(t in 1:T) {
    N[t+1] <- N[t] + N[t]*-0.1
}
plot(0:T, N, type="b", col="orange", pch=16,
##     xlab="Time", ylab="Superb owl population size", cex.lab=1.3)
     xlab="Time", ylab="Population size", cex.lab=1.3)
abline(h=2, col="red", lwd=2)
text(5, 2, "N=2", pos=3)
Te <- (0:T)[which.max(N<2)]
text(Te-1, 20, paste("Time to quasi-extinction Te =", Te-1))
arrows(Te-1, 19, Te-1, 3, length=0.1)


## ----fig.show='hide',include=FALSE,echo=FALSE,fig.width=8,fig.height=6--------
r.max <- 0.3
K.bar <- 100
sigma.d <- 20
T <- 20
set.seed(4450)
##plot(0:T, N, type="b", xlab="Time", ylab="Population size (N)",
##     ylim=c(0, 300))
nSim <- 100
N <- Nlr <- matrix(NA, T+1, nSim)
N[1,] <- Nlr[1,] <- 25
Te <- integer(nSim)
if(!dir.exists("figs/lg-d"))
    dir.create("figs/lg-d")
for(i in 1:nSim) {
    fl <- paste("figs/lg-d/lg-d", i, ".pdf", sep="")
    for(t in 1:T) {
        K.t <- rnorm(1, K.bar, sigma.d)
        N[t+1,i] <- N[t,i] + N[t,i]*r.max*(1 - N[t,i]/K.t)
        N[t+1,i] <- N[t+1,i]*(N[t+1,i]>0)
        Nlr[t+1,i] <- Nlr[t,i] + Nlr[t,i]*r.max*(1 - Nlr[t,i]/K.bar)
    }
    if(i < 11 || i==nSim) {
        pdf(fl, width=8, heigh=6)
        par(mai=c(0.9,0.9,0.3,0.3))
        mn <- ifelse(i==nSim, paste(nSim, "Simulations"), "")
        matplot(0:T, N, type="l", xlab="Time", ylab="Population size (N)", main=mn,
             ylim=c(0, 150), col=gray(0.7), lty=1, cex.lab=1.5)
        lines(0:T, Nlr[,1], col="blue", lwd=3)
        ext <- N[,i]==0
        if(any(ext)) {
            Te[i] <- min(which(ext))
            points(Te[i], 0, pch=16, col="orange")
        }
        abline(h=0, col="red", lwd=2)
        dev.off()
    }
}


## ----fig.show='hide',echo=FALSE-----------------------------------------------
r.max <- 0.3
K.bar <- 100
sigma.d <- 40
T <- 20
set.seed(4450)
##plot(0:T, N, type="b", xlab="Time", ylab="Population size (N)",
##     ylim=c(0, 300))
nSim <- 100
N <- Nlr <- matrix(NA, T+1, nSim)
N[1,] <- Nlr[1,] <- 25
Te <- integer(nSim)
Tes <- NULL
for(i in 1:nSim) {
    fl <- paste("figs/lg-d/lg2-d", i, ".pdf", sep="")
    for(t in 1:T) {
        K.t <- rnorm(1, K.bar, sigma.d)
        N[t+1,i] <- N[t,i] + N[t,i]*r.max*(1 - N[t,i]/K.t)
        N[t+1,i] <- N[t+1,i]*(N[t+1,i]>0)
        Nlr[t+1,i] <- Nlr[t,i] + Nlr[t,i]*r.max*(1 - Nlr[t,i]/K.bar)
    }
    ext <- N[,i]<1
    if(any(ext)) {
        Te[i] <- min(which(ext))-1
        Tes <- which(Te>0)
    }
    if(i < 11 || i==nSim) {
        pdf(fl, width=8, heigh=6)
        par(mai=c(0.9,0.9,0.3,0.3))
        mn <- ifelse(i==nSim, "17 extinctions in 100 simulations", "")
        matplot(0:T, N, type="l", xlab="Time", ylab="Population size (N)", main=mn,
             ylim=c(0, 150), col=gray(0.7), lty=1, cex.lab=1.5)
        lines(0:T, Nlr[,1], col="blue", lwd=3)
        abline(h=0, col="red", lwd=2)
        points(Te[Tes], rep(0, length(Tes)), pch=16, col="orange", cex=1.5)
        dev.off()
    }
}


## ----Allee,include=FALSE,echo=FALSE,fig.width=18,fig.height=6-----------------
N <- 0:100
beta0 <- 0.1
beta1.1 <- -0.001
par(mfrow=c(1, 3), mai=c(.7, 0.7, 0.4, 0.4))
plot(N, beta0 + 0*N, xlab="Population size (N)", ylab="Population growth rate (r)",
     type="l", main="No density-dependence", cex.lab=3, cex.main=3, col="blue", lwd=2)
plot(N, beta0 + beta1.1*N, xlab="Population size (N)", ylab="Population growth rate (r)",
     type="l", main="Standard density-dependence", cex.lab=3, cex.main=3, col="blue", lwd=2)
plot(N, #ifelse(N>20, beta0+beta1.1*N, 0 + 0.004*N),
     log(0.89+beta0 + 0.0009*N - 0.00001*N^2),
     xlab="Population size (N)", ylab="Population growth rate (r)",
     type="l", main="Allee effect", cex.lab=3, cex.main=3, col="blue", lwd=2)

