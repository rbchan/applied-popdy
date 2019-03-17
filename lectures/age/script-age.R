## Age-structured model

A <- matrix(c(
    0.2, 0.6, 0.1,
    0.5, 0.0, 0.0,
    0.0, 0.7, 0.0), nrow=3, byrow=TRUE)

eA <- eigen(A)
eA

(lam <- eA$val[1])

v0 <- eA$vec[,1]
(v <- v0/sum(v0))

w0 <- eigen(t(A))$vec[,1]
(w <- w0/w0[1])


f <- A[1,]
s <- A[which(row(A)==col(A)+1)]
cs <- c(1, cumprod(s))


## Note that the probability of reaching
## age i from age i is 1
w20 <- matrix(0, 3, 3)
w20[1,1] <- 1*f[1]*lam^(1-1-1)
w20[1,2] <- s[1]*f[2]*lam^(1-2-1)
w20[1,3] <- prod(s[1:2])*f[3]*lam^(1-3-1)
w20[2,2] <- 1*f[2]*lam^(2-2-1)
w20[2,3] <- s[2]*f[3]*lam^(2-3-1)
w20[3,3] <- 1*f[3]*lam^(3-3-1)


rowSums(w20)
w

s1 <- c(1, s)
for(i in 1:3) {
    for(j in i:3) {
        w20[i,j] <- prod(s1[i:j])*f[j]*lam^(i-j-1)
    }
}




library(popbio)

lambda(A)
lam

reproductive.value(A)
w

stable.stage(A)
v

F <- T <- A
F[-1,] <- 0
T[1,] <- 0
N <- solve(diag(3)-T)
R <- F %*% N
eigen(R)$val[1]
(R0 <- cs%*%f)

net.reproductive.rate(A) ## Wrong for age-structured
net.reproductive.rate(A, r=1, c=1:3)  ## Better

log(R0)/log(lam)
generation.time(A, r=1, c=1:3)


sensitivity(A)
(z <- outer(w,v)/drop(w%*%v))

elasticity(A)
z*A/lam



