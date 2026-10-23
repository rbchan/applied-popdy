## ----eval=FALSE,include=FALSE-------------------------------------------------
## source("../../rnw2pdf.R")
## rnw2pdf("lecture-occupancy")
## rnw2pdf("lecture-occupancy",tangle=TRUE)


## ----cov1,show.fig='hide',echo=FALSE,results='hide'---------------------------
pdf("figs/cov1.pdf", width=12, height=6)
par(mfrow=c(1,2), mai=c(0.9,0.9,0.2,0.2))
plot(function(x,beta0=-2,beta1=0.01) plogis(beta0 + beta1*x), 0, 1000,
     xlab="Elevation (m)", ylab="Occurrence probability", ylim=c(0,1),
     cex.lab=1.7, lwd=2)
plot(function(x,beta0=4,beta1=-0.1) plogis(beta0 + beta1*x), 30, 90,
     xlab="Temperature", ylab="Detection probability", ylim=c(0,1),
     cex.lab=1.7, lwd=2)
dev.off()

