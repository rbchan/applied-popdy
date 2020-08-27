## ----eval=FALSE,include=FALSE-------------------------------------------------
## source("../../rnw2pdf.R")
## rnw2pdf("lecture-BIDE")
## rnw2pdf("lecture-BIDE",tangle=TRUE)


## ----bide0,echo=FALSE,include=FALSE,fig.width=8,fig.height=6,cache=TRUE-------
T <- 20
N <- integer(T)
B <- D <- I <- E <- integer(T-1)
N[1] <- 100
set.seed(340)
for(t in 1:(T-1)) {
    B[t] <- rpois(1, N[t]*0.2)
    D[t] <- rbinom(1, N[t], 0.1)
    I[t] <- rpois(1, 0.2)
    E[t] <- rpois(1, N[t]*0.05)
    N[t+1] <- N[t] + B[t] + I[t] - D[t] - E[t]
}
par(mai=c(0.9, 0.5, 0.2, 0.2))
plot(1:T, N, ylim=c(0, max(N)), type="o",
     xlab="Year", ylab="", pch=16, cex.lab=1.7)
lines(1:(T-1), B, col="blue", pch=17, type="o")
lines(1:(T-1), I, col="green", pch=18, type="o")
lines(1:(T-1), D, col="red", pch=19, type="o")
lines(1:(T-1), E, col="orange", pch=20, type="o")
legend(1, 230, c("Population size (N)",
                 "Births (B)",
                 "Immigrants (I)",
                 "Deaths (D)",
                 "Emigrants (E)"),
       lty=1, pch=c(16:20), cex=1.2,
       col=c("black", "blue", "green", "red", "orange"))

