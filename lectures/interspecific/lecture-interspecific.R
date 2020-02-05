## ----eval=FALSE,include=FALSE-------------------------------------------------
## source("../../rnw2pdf.R")
## rnw2pdf("lecture-interspecific")
## rnw2pdf("lecture-interspecific",tangle=TRUE)


## ----predprey1,include=FALSE,echo=FALSE,fig.width=8,fig.height=6--------------
T <- 75
Nprey <- integer(T+1)
Npred <- integer(T+1)
Nprey[1] <- 101
Npred[1] <- 22
rprey <- 0.2
dprey <- 0.01
bpred <- 0.005
dpred <- 0.5
for(t in 1:T) {
    Nprey[t+1] <- Nprey[t] + Nprey[t]*(rprey - dprey*Npred[t])
    Npred[t+1] <- Npred[t] + Npred[t]*(bpred*Nprey[t] - dpred)
}
plot(0:T, Nprey, xlab="Time", ylab="Population size", type="b", col="cyan4",
     pch=16, ylim=c(0, 250))
lines(0:T, Npred, type="b", col="red", pch=16)
legend(0, 250, c("Prey", "Predator"), lty=1, pch=16, col=c("cyan4", "red"))


## ----compex,include=FALSE,echo=FALSE------------------------------------------
T <- 75
N.A <- integer(T+1)
N.B <- integer(T+1)
N.A[1] <- 100
N.B[1] <- 50
r.A <- 0.2
r.B <- 0.1
K.A <- 150
K.B <- 90
alpha.A <- 0.4
alpha.B <- 0.2
for(t in 1:T) {
    N.A[t+1] <- N.A[t] + r.A*N.A[t]*(K.A - N.A[t] - alpha.B*N.B[t])/K.A
    N.B[t+1] <- N.B[t] + r.B*N.B[t]*(K.B - N.B[t] - alpha.A*N.A[t])/K.B
}
plot(0:T, N.A, xlab="Time", ylab="Population size", type="b", col="cyan4",
     main="Competitive exclusion", cex.lab=1.5, cex.main=2,
     pch=16, ylim=c(0, 200))
lines(0:T, N.B, type="b", col="red", pch=16)
legend(0, 200, c("Species A", "Species B"), lty=1, pch=16, col=c("cyan4", "red"))

## ----stable,include=FALSE,echo=FALSE------------------------------------------
T <- 75
N.A <- integer(T+1)
N.B <- integer(T+1)
N.A[1] <- 100
N.B[1] <- 50
r.A <- 0.2
r.B <- 0.1
K.A <- 150
K.B <- 90
alpha.A <- 0.05
alpha.B <- 0.1
for(t in 1:T) {
    N.A[t+1] <- N.A[t] + r.A*N.A[t]*(K.A - N.A[t] - alpha.B*N.B[t])/K.A
    N.B[t+1] <- N.B[t] + r.B*N.B[t]*(K.B - N.B[t] - alpha.A*N.A[t])/K.B
}
plot(0:T, N.A, xlab="Time", ylab="Population size", type="b", col="cyan4",
     main="Stable coexistence", cex.lab=1.5, cex.main=2,
     pch=16, ylim=c(0, 200))
lines(0:T, N.B, type="b", col="red", pch=16)
legend(0, 200, c("Species A", "Species B"), lty=1, pch=16, col=c("cyan4", "red"))

