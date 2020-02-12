## ----eval=FALSE,include=FALSE-------------------------------------------------
## source("../../rnw2pdf.R")
## rnw2pdf("lecture-age-structure")
## rnw2pdf("lecture-age-structure",tangle=TRUE)


## ----struc1,include=TRUE,echo=FALSE,fig.width=6,fig.height=6,out.width='0.85\\textwidth',fig.align='center'----
barplot(c(0.5, 0.4, 0.1), cex.lab=1.3, cex.names=1.3, cex.main=1.3,
        names=c("Juveniles", "Subadults", "Adults"),
        ylab="Proportion in each age class",
        main="Initial age distribution")


## ----proj1,include=FALSE,echo=FALSE,results='hide'----------------------------
T <- 30
N <- matrix(NA, 3, T+1)
N[,1] <- c(50, 40, 10)
phi <- c(0.5, 0.6, 0.0)
gamma <- c(0, 0.8, 1.7)
A <- matrix(c(gamma, phi[1], 0, 0, 0, phi[2], 0), 3, 3, byrow=TRUE)
for(t in 1:T) {
    N[,t+1] <- A %*% N[,t]
}
TT <- matrix(0:T, 1, T+1)
matplot(t(TT), t(N), type="o", pch=16, xlab="Time", ylab="Population size",
        cex.lab=1.3, ylim=c(0, 60), col=c("black", "orange", "purple"))
legend(20, 60, c("Age class 1", "Age class 2", "Age class 3"),
       col=c("black", "orange", "purple"), pch=16, lty=1:3)


## ----lambda1,include=FALSE,echo=FALSE-----------------------------------------
lambda <- N[,2:(T+1)] / N[,1:T]
matplot(t(lambda), type="o", pch=16,
        xlab="Time", ylab="Population growth rate (lambda)",
        cex.lab=1.3,
        col=c("black", "orange", "purple"))
legend(20, 2.4, c("Age class 1", "Age class 2", "Age class 3"),
       col=c("black", "orange", "purple"), pch=16, lty=1:3)


## ----prop1,include=FALSE,echo=FALSE-------------------------------------------
Nt <- colSums(N)
C <- sweep(N, 2, Nt, "/")
matplot(t(TT), t(C), type="o", pch=16, xlab="Time",
        cex.lab=1.3,
        ylab="Proportion in age class",
        col=c("black", "orange", "purple"), ylim=c(0, 1))
legend(20, 1, c("Age class 1", "Age class 2", "Age class 3"),
       col=c("black", "orange", "purple"), pch=16, lty=1:3)


## ----rv,include=FALSE,echo=FALSE----------------------------------------------
V <- eigen(t(A))$vectors[,1]
rv <- as.numeric(V/V[1])
barplot(rv, names=c("Age class 1", "Age class 2", "Age class 3"),
        ylab="Fisher's reproductive value", ylim=c(0, 2), cex.names=1.5,
        cex.lab=1.5)
box()

