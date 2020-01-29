## ----eval=FALSE,include=FALSE--------------------------------------------
## source("../../rnw2pdf.R")
## rnw2pdf("lecture-stochasticity")
## rnw2pdf("lecture-stochasticity",tangle=TRUE)


## ----echo=FALSE,results='hide',eval=TRUE---------------------------------
if(!dir.exists("figs/norm"))
    dir.create("figs/norm")
pdf("figs/norm/norm0.pdf", width=8, height=6)
curve(dnorm(x, mean=0, sd=1), -3, 3,
      xlab="X", yaxt="n", ylab="", cex.lab=1.4)
dev.off()
x <- numeric(10)
set.seed(4549)
for(i in 1:10) {
    fl <- paste("figs/norm/norm", i, ".pdf", sep="")
    pdf(fl, width=8, height=6)
    curve(dnorm(x, mean=0, sd=1), -3, 3,
      xlab="X", yaxt="n", ylab="", cex.lab=1.4)
    x[i] <- rnorm(1, mean=0, sd=1)
    points(x[1:i], rep(0, i), col="orange", cex=1.5, lwd=2, pch=16)
    dev.off()
}


## ----norm1,echo=FALSE,include=FALSE,fig.width=8,fig.height=6-------------
curve(dnorm(x, 0, 0.6), -2, 2, xlab="X", cex.lab=1.4,
      ylab="Relative probability", ylim=c(0, 3), lwd=3)
curve(dnorm(x, 0, 0.4), -2, 2, col="blue", lwd=3, add=TRUE)
curve(dnorm(x, 0, 0.2), -2, 2, col="red", lwd=3, add=TRUE)
legend(-2.1, 3, c(expression(paste(mu, "=0, ", sigma^2, "=0.6")),
                  expression(paste(mu, "=0, ", sigma^2, "=0.4")),
                  expression(paste(mu, "=0, ", sigma^2, "=0.2"))),
       col=c("black", "blue", "red"), lwd=2, cex=0.9)


## ----normpop,include=FALSE,echo=FALSE,fig.width=8,fig.height=6-----------
x <- rnorm(100, 50)
plot(x, type="o", xlab="Time", ylab="Population size (N)", cex.lab=1.4)


## ----geo-env,eval=FALSE,size='small'-------------------------------------
## r <- 0.1
## sigma.e <- 10
## for(t in 2:nYears) {
##     X[t-1] <- rnorm(n=1, mean=0, sd=sigma.e)
##     N[t] <- N[t-1] + N[t-1]*r + X[t-1]
## }


## ----geo-e,echo=FALSE,include=FALSE,cache=TRUE---------------------------
r <- 0.1
sigma.e <- 10
T <- 20
if(!dir.exists("figs/exp-e"))
    dir.create("figs/exp-e")
set.seed(350)
for(i in 1:10) {
    N <- integer(T+1)
    N[1] <- 100
    fl <- paste("figs/exp-e/exp-e", i, ".pdf", sep="")
    for(t in 1:T) {
        X.t <- rnorm(1, 0, sigma.e)
        N[t+1] <- N[t] + N[t]*r + X.t
    }
    pdf(fl, width=8, heigh=6)
    par(mai=c(0.9,0.9,0.1,0.1))
    plot(0:T, N, type="b", xlab="Time", ylab="Population size (N)",
         ylim=c(0, 1000), cex.lab=1.4)
    plot(function(x) N[1]*(1+r)^x, 0, T+1, add=TRUE)
    abline(h=0, col=gray(0.8))
    dev.off()
}


## ----geo-e2,fig.show='hide',echo=FALSE,cache=TRUE------------------------
r <- 0.1
sigma.e <- 100
T <- 20
if(!dir.exists("figs/exp-e2"))
    dir.create("figs/exp-e2")
set.seed(540)
for(i in 1:10) {
    N <- integer(T+1)
    N[1] <- 100
    fl <- paste("figs/exp-e2/exp-e", i, ".pdf", sep="")
    for(t in 1:T) {
        X.t <- rnorm(1, 0, sigma.e)
        N[t+1] <- max(N[t] + N[t]*r + X.t, 0)
    }
    pdf(fl, width=8, height=6)
    par(mai=c(0.9,0.9,0.1,0.1))
    plot(0:T, N, type="b", xlab="Time", ylab="Population size (N)",
         ylim=c(0, 1000), cex.lab=1.4)
    plot(function(x) N[1]*(1+r)^x, 0, T+1, add=TRUE)
    abline(h=0, col=gray(0.8))
    dev.off()
}


## ----fig.show='hide',echo=FALSE------------------------------------------
r.bar <- 0.1
sigma.d <- 0.1
T <- 20
if(!dir.exists("figs/exp-d"))
    dir.create("figs/exp-d")
for(i in 1:10) {
    N <- integer(T+1)
    N[1] <- 100
    fl <- paste("figs/exp-d/exp-d", i, ".pdf", sep="")
    for(t in 1:T) {
        r.t <- rnorm(1, r.bar, sigma.d)
        N[t+1] <- N[t] + N[t]*r.t
    }
    pdf(fl, width=8, height=6)
    par(mai=c(0.9,0.9,0.1,0.1))
    plot(0:T, N, type="b", xlab="Time", ylab="Population size (N)",
         ylim=c(0, 1000), cex.lab=1.4)
    plot(function(x) N[1]*(1+r.bar)^x, 0, T+1, add=TRUE)
    abline(h=0, col=gray(0.8))
    dev.off()
}


## ----fig.show='hide',echo=FALSE------------------------------------------
r.bar <- 0.1
sigma.d <- 0.5
T <- 20
if(!dir.exists("figs/exp-d2"))
    dir.create("figs/exp-d2")
for(i in 1:10) {
    N <- integer(T+1)
    N[1] <- 100
    fl <- paste("figs/exp-d2/exp-d", i, ".pdf", sep="")
    for(t in 1:T) {
        r.t <- rnorm(1, r.bar, sigma.d)
        N[t+1] <- N[t] + N[t]*r.t
    }
    pdf(fl, width=8, height=6)
    par(mai=c(0.9,0.9,0.1,0.1))
    plot(0:T, N, type="b", xlab="Time", ylab="Population size (N)",
         ylim=c(0, 1000), cex.lab=1.4)
    plot(function(x) N[1]*(1+r.bar)^x, 0, T+1, add=TRUE)
    abline(h=0, col=gray(0.8))
    dev.off()
}


## ----fig.show='hide',echo=FALSE------------------------------------------
r.max <- 0.3
K.bar <- 100
sigma.d <- 20
T <- 20
if(!dir.exists("figs/lg-d"))
    dir.create("figs/lg-d")
set.seed(4450)
for(i in 1:10) {
    N <- Nlr <- integer(T+1)
    N[1] <- Nlr[1] <- 200
    fl <- paste("figs/lg-d/lg-d", i, ".pdf", sep="")
    for(t in 1:T) {
        K.t <- rnorm(1, K.bar, sigma.d)
        N[t+1] <- N[t] + N[t]*r.max*(1 - N[t]/K.t)
        Nlr[t+1] <- Nlr[t] + Nlr[t]*r.max*(1 - Nlr[t]/K.bar)
    }
    pdf(fl, width=8, height=6)
    par(mai=c(0.9,0.9,0.1,0.1))
    plot(0:T, N, type="b", xlab="Time", ylab="Population size (N)",
         ylim=c(0, 300), cex.lab=1.4)
    lines(0:T, Nlr)
    abline(h=0, col=gray(0.8))
    dev.off()
}

