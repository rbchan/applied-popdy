## ----eval=FALSE,include=FALSE-------------------------------------------------
## source("../../rnw2pdf.R")
## rnw2pdf("lecture-pva")
## rnw2pdf("lecture-pva",tangle=TRUE)


## ----A,size='scriptsize'------------------------------------------------------
A <- matrix(c(
    0.2, 0.8, 1.0, 0.9,
    0.4, 0.0, 0.0, 0.0,
    0.0, 0.6, 0.0, 0.0,
    0.0, 0.0, 0.8, 0.5), nrow=4, byrow=TRUE)


## ----eigen,size='scriptsize'--------------------------------------------------
eA <- eigen(A)
lam <- Re(eA$values[1])
lam                              ## Asymtotic growth rate

w <- Re(eA$vectors[,1])
w <- w/sum(w)                    ## Stable age distribution

v <- Re(eigen(t(A))$vectors[,1]) ## Reproductive value


## ----sen,size='scriptsize'----------------------------------------------------
z <- outer(v,w)/c(v%*%w)
z ## Sensitivites


## ----el,size='scriptsize'-----------------------------------------------------
e <- A*z/lam
e  ## Elasticities


## -----------------------------------------------------------------------------
A <- matrix(c(
    0.2, 0.8, 1.0, 0.9,
    0.4, 0.0, 0.0, 0.0,
    0.0, 0.6, 0.0, 0.0,
    0.0, 0.0, 0.8, 0.5), nrow=4, byrow=TRUE)
eA <- eigen(A)
eA

lam <- Re(eA$values[1])
lam

w <- Re(eA$vectors[,1])
w

v <- Re(eigen(t(A))$vectors[,1])
v

sen <- outer(v,w)/c(v%*%w)
sen

el <- A*sen/lam
el


A2 <- A
A2[1,1] <- A[1,1]+0.1
lam2 <- Re(eigen(A2)$val[1])

A3 <- A
A3[1,2] <- A[1,2]+0.1
lam3 <- Re(eigen(A3)$val[1])

(lam2-lam)/0.1
(lam3-lam)/0.1



