## ----eval=FALSE,include=FALSE--------------------------------------------
## source("../../rnw2pdf.R")
## rnw2pdf("lecture-mark-recap-closedpop")
## rnw2pdf("lecture-mark-recap-closedpop",tangle=TRUE)


## ----rem,include=FALSE,echo=FALSE,fig.width=12,fig.height=6--------------
N <- 100
p1 <- 0.8
p2 <- 0.3
pi1 <- c(p1, p1*(1-p1), p1*(1-p1)^2)
pi2 <- c(p2, p2*(1-p2), p2*(1-p2)^2)
pi1[4] <- 1 - sum(pi1)
pi2[4] <- 1 - sum(pi2)
set.seed(0324)
x1 <- drop(rmultinom(1, N, pi1))
x2 <- drop(rmultinom(1, N, pi2))
par(mfrow=c(1, 2), mai=c(0.8,0.9,0.5,0.2))
barplot(x1[1:3], main="p=0.8, N=100", cex.lab=2, cex.main=2, cex.axis=1.5,
        xlab="Occasion", names=1:3, ylab="New captures")
barplot(x2[1:3], main="p=0.3, N=100", cex.lab=2, cex.main=2, cex.axis=1.5,
        xlab="Occasion", names=1:3, ylab="New captures")

## ----rem2,include=FALSE,echo=FALSE,fig.width=12,fig.height=6-------------
N <- 100
p1 <- 0.2
p2 <- 0.7
pi1 <- c(p1, p1*(1-p1), p1*(1-p1)^2)
pi2 <- c(p2, p2*(1-p2), p2*(1-p2)^2)
pi1[4] <- 1 - sum(pi1)
pi2[4] <- 1 - sum(pi2)
set.seed(0324)
x1 <- drop(rmultinom(1, N, pi1))
x2 <- drop(rmultinom(1, N, pi2))
par(mfrow=c(1, 2), mai=c(0.8,0.9,0.5,0.2))
barplot(x1[1:3], main="", cex.lab=2, cex.main=2, cex.axis=1.5,
        xlab="Occasion", names=1:3, ylab="New captures")
barplot(x2[1:3], main="", cex.lab=2, cex.main=2, cex.axis=1.5,
        xlab="Occasion", names=1:3, ylab="New captures")

