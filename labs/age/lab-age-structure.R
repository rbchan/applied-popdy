## ----eval=FALSE,include=FALSE-------------------------------------------------
## source("../../rnw2pdf.R")
## rnw2pdf("lab-age-structure")
## rnw2pdf("lab-age-structure",tangle=TRUE)


## ----ex1-A--------------------------------------------------------------------
A <- matrix(c(  0, 1.5, 0.5,  ## The fecundities
              0.5,   0,   0,  ## The first value is s1
                0, 0.9,   0), ## The second value is s2
            nrow=3, ncol=3, byrow=TRUE)
A


## ----ex1-n0-------------------------------------------------------------------
years <- 0:50
nYears <- length(years)
n <- matrix(NA, nYears, 3)
n[1,] <- c(100, 50, 25)    ## Initial abundance


## ----ex1-proj-----------------------------------------------------------------
for(t in 2:nYears) {
  n[t,] <- A %*% n[t-1,]   ## Matrix multiplication
}


## ----ex1-plot,out.width='0.85\\textwidth',fig.align='center'------------------
matplot(years, n, type="o", pch=16, xlab="Time", ylab="Abundance")
legend(35, 100, c("Age class 1", "Age class 2", "Age class 3"),
       col=1:3, pch=16, lty=1)


## ----ex1-AD,out.width='0.85\\textwidth',fig.align='center'--------------------
N <- rowSums(n) ## Total abundance each year
c <- n/N        ## This works because of R's recycling rules
matplot(years, c, type="o", pch=16, ylim=c(0, 1), xlab="Time",
        ylab="Proportion in each age class")
legend(35, 1, c("Age class 1", "Age class 2", "Age class 3"),
       col=1:3, pch=16, lty=1)


## ----ex-AD-proj---------------------------------------------------------------
SAD.proj <- c[nYears,]
SAD.proj


## ----ex1-lam_it,out.width='0.85\\textwidth',fig.align='center'----------------
lambda.it <- n[-1,]/n[-nYears,] ## Divide each row by the row before it
matplot(years[-1], ## There are nYears-1 *intervals* between years
        lambda.it, type="o", pch=16, xlab="Time", ylab="Growth rates")
abline(h=1, lty=2, col="grey") ## Horizontal line
legend(35, 1.8, c("Age class 1", "Age class 2", "Age class 3"),
       col=1:3, pch=16, lty=1)


## ----ex-lam-proj--------------------------------------------------------------
lambda.it[nYears-1,] ## These should all be the same
lambda.proj <- lambda.it[nYears-1,1]
lambda.proj ## Asymptotic growth rate


## ----ex1-lam------------------------------------------------------------------
eA <- eigen(A)
lambda <- Re(eA$values[1])
lambda  ## Asymptotic growth rate


## ----ex1-lam-check------------------------------------------------------------
lambda.proj
lambda


## ----ex1-sad------------------------------------------------------------------
SADu <- Re(eA$vectors[,1])
SAD <- SADu/sum(SADu)
SAD


## ----ex1-sad-check------------------------------------------------------------
SAD
SAD.proj


## ----ex1-rv-------------------------------------------------------------------
eAT <- eigen(t(A))
RVu <- Re(eAT$vectors[,1])
RV <- RVu/RVu[1]
RV

