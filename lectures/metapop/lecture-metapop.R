## ----eval=FALSE,include=FALSE-------------------------------------------------
## source("../../rnw2pdf.R")
## rnw2pdf("lecture-metapop")
## rnw2pdf("lecture-metapop",tangle=TRUE)


## ----metapop1,include=FALSE,echo=FALSE,results='hide'-------------------------
set.seed(1)#(388730)
N <- 10
T <- 5
X <- cbind(runif(N, 1, 9), runif(N, 1, 9))
psi <- 0.4
z1 <- z2 <- z3 <- matrix(0L, N, T)
z1[,1] <- z2[,1] <- z3[,1] <- rbinom(N, 1, psi)
for(t in 2:T) {
    z1[,t] <- z1[,t-1]
    z2[,t] <- rbinom(N, 1, z2[,t-1]*1 + (1-z2[,t-1])*0.8)
    z3[,t] <- rbinom(N, 1, z3[,t-1]*.2 + (1-z3[,t-1])*0.8)
}
if(!dir.exists("figs")) dir.create("figs")
if(!dir.exists("figs/ex1")) dir.create("figs/ex1")
pdf("figs/ex1/ex1-1.pdf", width=8, height=6)
par(mai=c(0.02,0.02,0.02,0.02), mfrow=c(4, 6))
plot(0, 0, type="n", ann=FALSE, axes=FALSE)
plot(0, 0, type="n", ann=FALSE, axes=FALSE)
text(0, -0.8, "Year 1", cex=2)
plot(0, 0, type="n", ann=FALSE, axes=FALSE)
text(0, -0.8, "Year 2", cex=2)
plot(0, 0, type="n", ann=FALSE, axes=FALSE)
text(0, -0.8, "Year 3", cex=2)
plot(0, 0, type="n", ann=FALSE, axes=FALSE)
text(0, -0.8, "Year 4", cex=2)
plot(0, 0, type="n", ann=FALSE, axes=FALSE)
text(0, -0.8, "Year 5", cex=2)
plot(0, 0, type="n", ann=FALSE, axes=FALSE)
text(0, 0, "Low extinction\nLow colonization", cex=1.5)
symbols(X, circles=rep(0.5,nrow(X)), bg=ifelse(z1[,1]==TRUE, "cyan", "white"),
        xlim=c(0, 10), ylim=c(0, 10), inches=FALSE, xaxt="n", yaxt="n", ann=FALSE)
symbols(X, circles=rep(0.5,nrow(X)), bg=ifelse(z1[,1]==TRUE, "cyan", "white"),
        xlim=c(0, 10), ylim=c(0, 10), inches=FALSE, xaxt="n", yaxt="n", ann=FALSE)
symbols(X, circles=rep(0.5,nrow(X)), bg=ifelse(z1[,1]==TRUE, "cyan", "white"),
        xlim=c(0, 10), ylim=c(0, 10), inches=FALSE, xaxt="n", yaxt="n", ann=FALSE)
symbols(X, circles=rep(0.5,nrow(X)), bg=ifelse(z1[,1]==TRUE, "cyan", "white"),
        xlim=c(0, 10), ylim=c(0, 10), inches=FALSE, xaxt="n", yaxt="n", ann=FALSE)
symbols(X, circles=rep(0.5,nrow(X)), bg=ifelse(z1[,1]==TRUE, "cyan", "white"),
        xlim=c(0, 10), ylim=c(0, 10), inches=FALSE, xaxt="n", yaxt="n", ann=FALSE)
plot(0, 0, type="n", ann=FALSE, axes=FALSE)
text(0, 0, "Low extinction\nHigh colonization", cex=1.5)
symbols(X, circles=rep(0.5,nrow(X)), bg=ifelse(z2[,1]==TRUE, "cyan", "white"),
        xlim=c(0, 10), ylim=c(0, 10), inches=FALSE, xaxt="n", yaxt="n", ann=FALSE)
symbols(X, circles=rep(0.5,nrow(X)), bg=ifelse(z2[,2]==TRUE, "cyan", "white"),
        xlim=c(0, 10), ylim=c(0, 10), inches=FALSE, xaxt="n", yaxt="n", ann=FALSE)
symbols(X, circles=rep(0.5,nrow(X)), bg=ifelse(z2[,3]==TRUE, "cyan", "white"),
        xlim=c(0, 10), ylim=c(0, 10), inches=FALSE, xaxt="n", yaxt="n", ann=FALSE)
symbols(X, circles=rep(0.5,nrow(X)), bg=ifelse(z2[,4]==TRUE, "cyan", "white"),
        xlim=c(0, 10), ylim=c(0, 10), inches=FALSE, xaxt="n", yaxt="n", ann=FALSE)
symbols(X, circles=rep(0.5,nrow(X)), bg=ifelse(z2[,5]==TRUE, "cyan", "white"),
        xlim=c(0, 10), ylim=c(0, 10), inches=FALSE, xaxt="n", yaxt="n", ann=FALSE)
plot(0, 0, type="n", ann=FALSE, axes=FALSE)
text(0, 0, "High extinction\nHigh colonization", cex=1.5)
symbols(X, circles=rep(0.5,nrow(X)), bg=ifelse(z3[,1]==TRUE, "cyan", "white"),
        xlim=c(0, 10), ylim=c(0, 10), inches=FALSE, xaxt="n", yaxt="n", ann=FALSE)
symbols(X, circles=rep(0.5,nrow(X)), bg=ifelse(z3[,2]==TRUE, "cyan", "white"),
        xlim=c(0, 10), ylim=c(0, 10), inches=FALSE, xaxt="n", yaxt="n", ann=FALSE)
symbols(X, circles=rep(0.5,nrow(X)), bg=ifelse(z3[,3]==TRUE, "cyan", "white"),
        xlim=c(0, 10), ylim=c(0, 10), inches=FALSE, xaxt="n", yaxt="n", ann=FALSE)
symbols(X, circles=rep(0.5,nrow(X)), bg=ifelse(z3[,4]==TRUE, "cyan", "white"),
        xlim=c(0, 10), ylim=c(0, 10), inches=FALSE, xaxt="n", yaxt="n", ann=FALSE)
symbols(X, circles=rep(0.5,nrow(X)), bg=ifelse(z3[,5]==TRUE, "cyan", "white"),
        xlim=c(0, 10), ylim=c(0, 10), inches=FALSE, xaxt="n", yaxt="n", ann=FALSE)
dev.off()


## ----ex2,include=TRUE,echo=FALSE,results='hide',fig.height=5.5,fig.align='left'----
par(mfrow=c(3,2), mai=c(0.5,0.5,0.1,0.1))
plot(0, 0, type="n", ann=FALSE, axes=FALSE,)
text(0, 0, "Low extinction\nLow colonization", cex=1.5)
plot(1:5, colSums(z1)/N, type="b", xlab="Year",
     ylab="Proportion of sites occupied", ylim=0:1)
plot(0, 0, type="n", ann=FALSE, axes=FALSE)
text(0, 0, "Low extinction\nHigh colonization", cex=1.5)
plot(1:5, colSums(z2)/N, type="b", xlab="Year",
     ylab="Proportion of sites occupied", ylim=0:1)
plot(0, 0, type="n", ann=FALSE, axes=FALSE)
text(0, 0, "High extinction\nHigh colonization", cex=1.5)
plot(1:5, colSums(z3)/N, type="b", xlab="Year",
     ylab="Proportion of sites occupied", ylim=0:1)

