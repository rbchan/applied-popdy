## ----eval=FALSE,include=FALSE-------------------------------------------------
## source("../../rnw2pdf.R")
## rnw2pdf("lecture-sampling")
## rnw2pdf("lecture-sampling",tangle=TRUE)


## ----mkdir,include=FALSE------------------------------------------------------
## Should use knitr for this business 
if(!dir.exists("figs"))
    dir.create("figs")
if(!dir.exists("figs/sample-size"))
    dir.create("figs/sample-size")


## ----N,include=FALSE,echo=FALSE,eval=TRUE,cache=TRUE--------------------------
height <- 5.5
heightSD <- 0.5
nSim <- 100
x1 <- x2 <- numeric(nSim)
set.seed(343)
for(i in 1:nSim) {
    x1[i] <- mean(rnorm(10, height, heightSD))
    x2[i] <- mean(rnorm(100, height, heightSD))
}
for(i in 1:nSim) {
    fn <- paste("figs/sample-size/f", i, ".pdf", sep="")
    pdf(fn, width=12, height=6)
    par(mfrow=c(1,2), mai=c(0.9, 0.2, 0.4, 0.2))
    plot(0, type="n", frame=FALSE, yaxt="n",
         xlim=c(5, 6), ylim=c(0, 7),
         xlab="Height", ylab="",
         cex.lab=2.5, cex.main=2.5, cex.axis=2,
         main="Sample size of 10")
    abline(v=height, lty=3)
    points(x1[1:i], rep(0, i), cex=2.5, col="blue")
    if(i==nSim)
        hist(x1, freq=FALSE, add=TRUE, border="blue", lwd=2)
    plot(0, type="n", frame=FALSE, yaxt="n",
         xlim=c(5, 6), ylim=c(0, 7),
         xlab="Height", ylab="",
         cex.lab=2.5, cex.main=2.5, cex.axis=2,
         main="Sample size of 100")
    abline(v=height, lty=3)
    points(x2[1:i], rep(0, i), cex=2.5)
    if(i==nSim)
        hist(x2, freq=FALSE, add=TRUE)
    dev.off()
}

