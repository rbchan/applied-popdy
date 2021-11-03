

# -------------------------- Lecture notes ----------------------------



# Detection functions

pdf("../figs/detfuns.pdf", width=12, height=4)
par(mfrow=c(1,3), mai=c(0.7, 0.7, 0.3, 0.1))
plot(function(x, sigma=20) exp(-x^2/(2*sigma^2)), 0, 100, cex.lab=2,
     cex.axis=2, main="Half-normal", cex.main=2,
     xlab="distance (x)", ylab="Detection probability")
plot(function(x, sigma=20) exp(-x/sigma), 0, 100, cex.lab=2,
     cex.axis=2, main="Negative exponential", cex.main=2,
     xlab="distance (x)", ylab="Detection probability")
plot(function(x, sigma=20, scale=10) 1 - exp(-(x/sigma)^-scale), 0, 100,
     cex.axis=2, main="Hazard-rate", cex.main=2,
     cex.lab=2, xlab="distance (x)", ylab="Detection probability")
dev.off()
system("open ../figs/detfuns.pdf")


ls()




# ---------------------------- State process -----------------------------

# Closed population, instantaneous time

set.seed(34143)
# Dimensions of rectangular study area
xlim <- c(0,1)
ylim <- c(0,1)
A <- (xlim[2]-xlim[1]) * (ylim[2]-ylim[1])

# Poisson intensity and realized population size (Ntot)
lambda <- 70
Ntot <- rpois(1, lambda)
Ntot # 33 animals in the study area

# Coordinates of each individual
S <- cbind(runif(Ntot), runif(Ntot))

# Coordinates of the survey plot centroids
nPlots <- 9
co <- seq(0.2, 0.8, length=sqrt(nPlots))
X <- cbind(rep(co, each=length(co)), rep(co, seq=length(co)))
rad <- 0.1 # plot radii


# Distance between animals and plot centroids
dist <- matrix(NA, Ntot, nPlots)
for(i in 1:Ntot) {
    for(j in 1:nPlots) {
        dist[i,j] <- sqrt((S[i,1]-X[j,1])^2 + (S[i,2]-X[j,2])^2)
    }
}

# Is the individual in any plot?
inplot <- apply(dist <= rad, 1, any)


# Number of individuals in each plot
N <- apply(dist, 2, function(x) sum(x <= rad))



# Detections
sigma <- 0.1
z <- matrix(NA, Ntot, nPlots)
for(i in 1:nPlots) {
    p <- exp(-dist[,i]^2/(2*sigma^2))
    p[dist[,i] > rad] <- 0
    z[,i] <- rbinom(Ntot, 1, p)
}



# Survey plots and animals
png("../figs/design1.png", width=7, height=7, res=400, units="in")
op <- par(mai=c(0.1,0.1,0.4,0.1))
plot(0, type="n", xlim=c(0,1),ylim=c(0,1), axes=FALSE, frame=TRUE,
     main=expression(italic(N)==75), cex.main=1.8)
symbols(X, circles=rep(rad, nPlots), inches=FALSE, add=TRUE,
        fg=gray(0.8))
points(X, pch="+")
points(S, pch=16, col="blue", cex=0.9)
par(op)
dev.off()
system("open ../figs/design1.png")


# Guys in the plot
png("../figs/inplot1.png", width=7, height=7, res=400, units="in")
op <- par(mai=c(0.1,0.1,0.4,0.1))
plot(0, type="n", xlim=c(0,1),ylim=c(0,1), axes=FALSE, frame=TRUE,
     main=expression(paste(italic(N)==75, ", ", italic(N[s])==23)),
     cex.main=1.8)
symbols(X, circles=rep(rad, nPlots), inches=FALSE, add=TRUE,
        fg=gray(0.8))
points(X, pch="+")
points(S, pch=16, col=gray(0.7), cex=0.9)
points(S[inplot,], pch=16, col="blue", cex=0.9)
par(op)
dev.off()
system("open ../figs/inplot1.png")


# Guys in plot and detected
png("../figs/detect1.png", width=7, height=7, res=400, units="in")
op <- par(mai=c(0.1,0.1,0.4,0.1))
plot(0, type="n", xlim=c(0,1),ylim=c(0,1), axes=FALSE, frame=TRUE,
     main=expression(paste(italic(N)==75, ", ", italic(N[s])==23, ", ",
         italic(C)==14)),
     cex.main=1.8)
symbols(X, circles=rep(rad, nPlots), inches=FALSE, add=TRUE,
        fg=gray(0.8))
points(X, pch="+")
points(S, pch=16, col=gray(0.7), cex=0.9)
for(i in 1:nPlots) {
    det.i <- z[,i]==1
    if(any(det.i)) {
        points(S[det.i,,drop=FALSE], pch=16, col="blue", cex=0.9)
        segments(X[i,1], X[i,2], S[det.i,1], S[det.i,2], col="green",
                 lwd=2)
    }
}
par(op)
dev.off()
system("open ../figs/detect1.png")









# ----------------------------- Covariates -------------------------------

# Create a spatially-smooted covariate
set.seed(33)
grid.co <- seq(-0.05, 1.05, by=0.01)
grid <- data.frame(x=rep(grid.co, each=length(grid.co)),
                   y=rep(grid.co, times=length(grid.co)))
grid$g <- rnorm(nrow(grid)) # white noise
grid$g1 <- NA
# spatial smoothing
for(i in 1:nrow(grid)) {
    if(i %% 1000 == 0) cat("doing", i, "\n")
    dist <- (grid$x[i]-grid$x)^2 + (grid$y[i]-grid$y)^2
    p <- exp(-dist/(2*.002))
    grid$g1[i] <- mean(grid$g * p)
}
grid$g1 <- scale(grid$g1)
summary(grid)

# remove edges
grid <- grid[grid$x>0 & grid$x<1 & grid$y>0 & grid$y<1,]

x.mat <- matrix(grid$x, sqrt(nrow(grid)))
y.mat <- matrix(grid$y, sqrt(nrow(grid)))

# A continuous covariate
cov1 <- matrix(grid$g1, sqrt(nrow(grid)))
image(cov1, col=terrain.colors(100))


# A discrete covariate
cov2 <- cov1
cov2[] <- 1
cov2[cov1 > -1] <- 2
cov2[cov1 > 1] <- 3
image(cov2, col=terrain.colors(3))


# Inhomogeneous point process with continuous covariate
set.seed(1989)
N.mat <- E.N <- cov1
beta <- 1
E.N[] <- exp(beta * cov1) # Intensity function
S.probs <- E.N/sum(E.N)
N.mat[] <- rmultinom(1, Ntot, c(S.probs))
S.ipp <- cbind(x.mat[N.mat>0], y.mat[N.mat>0])

image(t(S.probs))
points(S.ipp)



# Continuous covariate with IPP
png("../figs/nocov1.png", width=7, height=7, res=400, units="in")
op <- par(mai=c(0.1,0.1,0.1,0.1))
plot(0, type="n", xlim=c(0,1),ylim=c(0,1), axes=FALSE, frame=TRUE)
points(S, pch=16, col="blue")
par(op)
dev.off()
system("open ../figs/nocov1.png")



# Continuous covariate with IPP
png("../figs/ccov1.png", width=7, height=7, res=400, units="in")
op <- par(mai=c(0.1,0.1,0.1,0.1))
plot(0, type="n", xlim=c(0,1),ylim=c(0,1), axes=FALSE, frame=TRUE)
image(seq(0,1,l=100), seq(0,1,l=100), t(cov1), col=terrain.colors(100),
      add=TRUE)
#points(X, pch="+")
#symbols(X, circles=rep(rad, nPlots), inches=FALSE, add=TRUE,
#        fg=gray(0.4))
points(S.ipp, pch=16, col="blue")
par(op)
dev.off()
system("open ../figs/ccov1.png")



# Inhomogeneous point process with discrete covariate
set.seed(1989)
N.mat2 <- E.N <- cov2
beta <- 1
E.N[cov2==1] <- 2
E.N[cov2==2] <- 0
E.N[cov2==3] <- 1
S.probs <- E.N/sum(E.N)
N.mat2[] <- rmultinom(1, Ntot, c(S.probs))
S.ipp2 <- cbind(x.mat[N.mat2>0], y.mat[N.mat2>0])


png("../figs/dcov1.png", width=5, height=5, res=400, units="in")
op <- par(mai=c(0.1,0.1,0.1,0.1))
plot(0, type="n", xlim=c(0,1),ylim=c(0,1), axes=FALSE, frame=TRUE)
image(seq(0,1,l=100), seq(0,1,l=100), t(cov2), col=terrain.colors(100),
      add=TRUE)
points(S.ipp2, pch=16, col="blue")
par(op)
dev.off()






# ------------------------- Detection process ---------------------------

set.seed(3458)
p <- 0.7
detected <- inplot & (rbinom(Ntot, 1, p)==1)

# Survey plots with detection/non-detection
png("../figs/detect1.png", width=5, height=5, res=400, units="in")
op <- par(mai=c(0.1,0.1,0.1,0.1))
plot(0, type="n", xlim=c(0,1),ylim=c(0,1), axes=FALSE, frame=TRUE)
symbols(X, circles=rep(rad, nPlots), inches=FALSE, add=TRUE)
points(S, pch=16, col=gray(0.9))
points(S[detected,], pch=16, col="blue")
#text(0.96, 0.98, "plot", cex=1.5)
#arrows(0.95,0.95,0.88,0.88, length=0.1, lwd=1.5)
par(op)
dev.off()




# Distance-related heterogeneity in detction (line transects)

set.seed(354)
dis <- runif(100, 0, 100)
sigma <- 30
p <- exp(-dis^2/(2*sigma^2))
detected <- rbinom(100, 1, p)==1
y <- dis[detected]

hist(y, breaks=seq(0,100,10), freq=FALSE, xlim=c(0, 100), xlab="distance",
     col=gray(0.8))
curve(unmarked:::dxhn(x, sigma=sigma), 0, 100, add=TRUE)
points(dis, rep(-.0010, 100))
points(dis[detected], rep(-.0010, sum(detected)), pch=16)













# ----------------------- Detection functions ----------------------------






g <- function(x, sigma=25) exp(-x^2/(2*sigma^2))

# 0-10m distance interval
x1 <- seq(0, 10, by=1)
poly1 <- c(g(x1), rep(0, length(x1)))   # polygon
p1 <- integrate(g, 0, 10)$value / 10    # detection prob

x2 <- seq(10, 20, by=1)
poly2 <- c(g(x2), rep(0, length(x2)))
p2 <- integrate(g, 10, 20)$value / 10   # detection prob

x3 <- seq(20, 30, by=1)
poly3 <- c(g(x3), rep(0, length(x3)))
p3 <- integrate(g, 20, 30)$value / 10   # detection prob

x4 <- seq(30, 40, by=1)
poly4 <- c(g(x4), rep(0, length(x4)))
p4 <- integrate(g, 30, 40)$value / 10   # detection prob

x5 <- seq(40, 50, by=1)
poly5 <- c(g(x5), rep(0, length(x5)))
p5 <- integrate(g, 40, 50)$value / 10   # detection prob

x <- seq(0, 50, by=1)
poly <- c(g(x), rep(0, length(x)))
p <- integrate(g, 0, 50)$value / 50   # detection prob

x0 <- seq(0, 50, by=1)
poly0 <- c(g(x0), rep(1, length(x0)))
p0 <- 1 - integrate(g, 0, 50)$value / 50   # detection prob


xT <- seq(0, 50, by=1)
polyT <- c(1-1/50*xT, rep(0, length(xT)))
pT <- 0.5



# Multinomial cell probs
pi <- c(p1,p2,p3,p4,p5)/5
pi[6] <- 1-sum(pi)
round(pi,2)




# p=1
set.seed(3434)
xx <- runif(500, 0, 50)
hx1 <- hist(xx)
hx1$counts <- hx1$counts/hx1$counts[1]
plot(hx1)



set.seed(3434)
xx <- runif(500, 0, 50)
p <- 1 - 1/50*xx
xxx <- xx[rbinom(length(xx), 1, p)==1]
hx <- hist(xxx)
hx$counts <- hx$counts/hx$counts[1]
plot(hx)



png("../figs/detfun0-0.png", width=7, height=7, units="in", res=400)
par(mai=c(0.9, 0.9, 0.8, 0.2))
plot(hx1, #ylim=c(0, 1),
     yaxt="n",
      xlab="Distance (x)", #type="n",
      ylab="Frequency", #"Detection probability",
      main="", #expression(g(x) == exp(-x^2/sigma^2)),
      cex.lab=1.8, cex.main=1.8, lwd=2)
#polygon(c(x, rev(x)), polyT, col=gray(0.8), border=FALSE)
box()
abline(h=0, col=gray(0.9))
abline(h=1, col=gray(0.9))
lines(hx1)
dev.off()
system("open ../figs/detfun0-0.png")


png("../figs/detfun0-1.png", width=7, height=7, units="in", res=400)
par(mai=c(0.9, 0.9, 0.8, 0.2))
plot(function(x) 1 - 1/50*x, 0, 50, ylim=c(0, 1),
     yaxt="n",
      xlab="Distance (x)", type="n",
      ylab="Frequency", #"Detection probability",
      main="", #expression(g(x) == exp(-x^2/sigma^2)),
      cex.lab=1.8, cex.main=1.8, lwd=2)
#polygon(c(x, rev(x)), polyT, col=gray(0.8), border=FALSE)
abline(h=0, col=gray(0.9))
abline(h=1, col=gray(0.9))
lines(hx)
dev.off()
system("open ../figs/detfun0-1.png")


png("../figs/detfun0-2.png", width=7, height=7, units="in", res=400)
par(mai=c(0.9, 0.9, 0.8, 0.2))
plot(function(x) 1 - 1/50*x, 0, 50, ylim=c(0, 1),
      xlab="Distance (x)",
      ylab="Detection probability",
#      main=expression(g(x) == exp(-x^2/sigma^2)),
      cex.lab=1.8, cex.main=1.8, lwd=2)
#polygon(c(x, rev(x)), polyT, col=gray(0.8), border=FALSE)
abline(h=0, col=gray(0.9))
abline(h=1, col=gray(0.9))
lines(hx)
dev.off()
system("open ../figs/detfun0-2.png")



png("../figs/detfun0-3.png", width=7, height=7, units="in", res=400)
par(mai=c(0.9, 0.9, 0.8, 0.2))
plot(function(x) 1 - 1/50*x, 0, 50, ylim=c(0, 1),
      xlab="Distance (x)",
      ylab="Detection probability",
#      main=expression(g(x) == exp(-x^2/sigma^2)),
      cex.lab=1.8, cex.main=1.8, lwd=2)
polygon(c(x, rev(x)), polyT, col=gray(0.8), border=FALSE)
abline(h=0, col=gray(0.9))
abline(h=1, col=gray(0.9))
text(15, 0.3, expression(bar(p) == 0.5), cex=1.8)
lines(hx)
dev.off()
system("open ../figs/detfun0-3.png")





png("figs/detfun1.png", width=7, height=7, units="in", res=400)
par(mai=c(0.9, 0.9, 0.8, 0.2))
curve(g, 0, 50, ylim=c(0, 1),
      xlab="Distance (x)",
      ylab="Detection probability",
      main=expression(g(x) == exp(-x^2/(2*sigma^2))),
      cex.lab=1.8, cex.main=1.8, lwd=2)
abline(h=0, col=gray(0.9))
text(30, 0.6, expression(sigma == 25), cex=1.8)
dev.off()
system("open figs/detfun1.png")





png("figs/detfun2.png", width=7, height=7, units="in", res=400)
par(mai=c(0.9, 0.9, 0.8, 0.2))
curve(g, 0, 50, ylim=c(0, 1),
      xlab="Distance (x)",
      ylab="Detection probability",
      main=expression(g(x) == exp(-x^2/(2*sigma^2))),
      cex.lab=1.8, cex.main=1.8, lwd=2)
text(30, 0.6, expression(sigma == 25), cex=1.8)
curve(g(x, 10), lwd=2, add=TRUE, col="blue")
text(10, 0.2, expression(sigma == 10), cex=1.8, col="blue")
curve(g(x, 50), lwd=2, add=TRUE, col="purple")
text(40, 0.8, expression(sigma == 50), cex=1.8, col="purple")
abline(h=0, col=gray(0.9))
dev.off()
system("open figs/detfun2.png")





png("../figs/detfun3.png", width=7, height=7, units="in", res=400)
par(mai=c(0.9, 0.9, 0.8, 0.2))
curve(g, 0, 50, ylim=c(0, 1),
      xlab="Distance (x)",
      ylab="Detection probability",
#      main=expression(
#       paste(p, "  =  ", integral(exp(-x^2/2*sigma^2)/50 * dx, 0, 50))),
      cex.lab=1.8, cex.main=1.8, lwd=2)
abline(h=0, col=gray(0.9))
text(30, 0.6, expression(sigma == 25), cex=1.8)
polygon(c(x, rev(x)), poly, col=gray(0.8), border=FALSE)
text(20, 0.4, expression(bar(p) == 0.60), cex=1.8)
dev.off()
system("open ../figs/detfun3.png")





png("../figs/detfun4.png", width=7, height=7, units="in", res=400)
par(mai=c(0.9, 0.9, 0.8, 0.2))
curve(g(x, sigma=10), 0, 50, ylim=c(0, 1),
      xlab="Distance (x)",
      ylab="Detection probability",
#      main=expression(
#       paste(p[d], "  =  ", integral(exp(-x^2/2*sigma^2)/50 * dx, 0, 50))),
      cex.lab=1.8, cex.main=1.8, lwd=2)
abline(h=0, col=gray(0.9))
text(15, 0.6, expression(sigma == 10), cex=1.8)
polyS10 <- c(g(x, sigma=10), rep(0, length(x)))   # polygon
polygon(c(x, rev(x)), polyS10, col=gray(0.8), border=FALSE)
text(10, 0.1, expression(bar(p) == 0.25), cex=1.8)
dev.off()
system("open ../figs/detfun4.png")




plot(function(x, sigma=25) exp(-x^2/sigma^2), 0, 50)
plot(function(x) 2*x/50^2, 0, 50, add=TRUE)




set.seed(3594)
N <- 1000
u <- cbind(runif(N, -50, 50), runif(N, -50, 50))
du <- sqrt(u[,1]^2 + u[,2]^2)
du <- du[du <= 50]
pu <- exp(-du^2/(2*25^2))
du1 <- du[rbinom(N, 1, pu)==1]
hu <- hist(du, plot=FALSE)
hu1 <- hist(du1, plot=FALSE)
#hu$counts <- hu$counts/(50^2) #sum(hu$counts)

plot(hu, freq=FALSE)
plot(function(x) 2*x/50^2, 0, 50, add=TRUE)

plot(hu1, freq=FALSE)
plot(function(x) 2*x/50^2, 0, 50, add=TRUE)



png("../figs/detfun5.png", width=7, height=7, units="in", res=400)
par(mai=c(0.9,0.9,0.8,0.2))
plot(hu, xlab="Distance (x)", cex.lab=1.8, main="")
box()
dev.off()
system("open ../figs/detfun5.png")


png("../figs/detfun6.png", width=7, height=7, units="in", res=400)
par(mai=c(0.9,0.9,0.8,0.2))
plot(hu1, xlab="Distance (x)", cex.lab=1.8, main="")
box()
dev.off()
system("open ../figs/detfun6.png")





png("../figs/detfun7.png", width=7, height=7, units="in", res=400)
par(mai=c(0.9, 0.9, 0.8, 0.2))
#plot(function(x, sigma=25) exp(-x^2/(2*sigma^2))*2*x/50^2, 0, 50,
hu2 <- hu1
#hu2$counts <- hu1$counts/3
curve(unmarked:::drhn(x, sigma=25), 0, 50,
      ylim=c(0, 0.04),
      xlab="Distance (x)",
      ylab="", #"Probability density",
#      main=expression(
#       paste(p[d], "  =  ", integral(exp(-x^2/2*sigma^2)*2*x/50^2 * dx, 0, 50))),
      cex.lab=1.8, cex.main=1.8, lwd=2)
lines(hu2, freq=FALSE)
#plot(function(x) 2*x/50^2, 0, 50, add=TRUE)
abline(h=0, col=gray(0.9))
text(30, 0.6, expression(sigma == 25), cex=1.8)
#polygon(c(x, rev(x)), poly, col=gray(0.8), border=FALSE)
text(20, 0.4, expression(p[d] == 0.60), cex=1.8)
dev.off()
system("open ../figs/detfun7.png")





png("../figs/detfun8.png", width=7, height=7, units="in", res=400)
par(mai=c(0.9, 0.9, 0.8, 0.2))
curve(g, 0, 50, ylim=c(0, 1),
      xlab="Distance (x)", ylab="Detection probability",
#      main="g <- function(x, sigma=25) exp(-x^2/(2*sigma^2))
#             integrate(g, 0, 10)$value / 10",
#      main=expression(
#       paste(p[d], "  =  ", integral(exp(-x^2/2*sigma^2)/10 * dx, 0, 10))),
      cex.lab=1.8, cex.main=1.8, lwd=2)
abline(h=0, col=gray(0.9))
polygon(c(x1, rev(x1)), poly1, col=gray(0.8), border=FALSE)
seg.x <- c(0,0,10,10,20,20,30,30,40,40,50,50)
segments(seg.x, rep(0, length(seg.x)), seg.x, g(seg.x))
text(5, .12, bquote(paste(p[d1], "=", .(round(p1, 2)))), cex=1.5)
dev.off()
system("open ../figs/detfun8.png")






png("../figs/g0-10R.png", width=7, height=7, units="in", res=400)
curve(g, 0, 50, ylim=c(0, 1),
      xlab="Distance", ylab="Detection probability",
#      main="g <- function(x, sigma=25) exp(-x^2/(2*sigma^2))
             integrate(g, 0, 10)$value / 10",
      cex.lab=1.3, cex.main=1.5, lwd=2)
abline(h=0, col=gray(0.9))
polygon(c(x1, rev(x1)), poly1, col=gray(0.8), border=FALSE)
seg.x <- c(0,0,10,10,20,20,30,30,40,40,50,50)
segments(seg.x, rep(0, length(seg.x)), seg.x, g(seg.x))
text(5, .12, bquote(paste(p[1], "=", .(round(p1, 2)))), cex=1.5)
dev.off()


png("../figs/g0-10R2.png", width=7, height=7, units="in", res=400)
curve(g, 0, 50, ylim=c(0, 1),
      xlab="Distance", ylab="Detection probability",
      main=expression(paste(
      "y ~ Multinomial(N,", pi, ") where  ", pi, "  =  ", p %*% phi)),
      cex.lab=1.3, cex.main=1.5, lwd=2)
abline(h=0, col=gray(0.9))
polygon(c(x1, rev(x1)), poly1, col=gray(0.8), border=FALSE)
seg.x <- c(0,0,10,10,20,20,30,30,40,40,50,50)
segments(seg.x, rep(0, length(seg.x)), seg.x, g(seg.x))
text(5, .12, bquote(paste(p[1], "=", .(round(p1, 2)))), cex=1.5)
text(5, .05, bquote(paste(pi[1], "=", p[1]/5)), cex=1.5)
dev.off()



png("../figs/g-pi.png", width=7, height=7, units="in", res=400)
curve(g, 0, 50, ylim=c(0, 1),
      xlab="Distance", ylab="Detection probability",
      main=bquote(
        paste("y ~ Multinomial(N, (0.19,0.17,0.12,0.08,0.04,0.4))")),
      cex.lab=1.3, cex.main=1.5, lwd=2)
abline(h=0, col=gray(0.9))
seg.x <- c(0,0,10,10,20,20,30,30,40,40,50,50)
polygon(c(x, rev(x)), poly, col=gray(0.8), border=FALSE)
segments(seg.x, rep(0, length(seg.x)), seg.x, g(seg.x))
text(5, .12, bquote(paste(p[1], "=", .(round(p1, 2)))), cex=1.5)
text(5, .05, bquote(paste(pi[1], "=", p[1]/5)), cex=1.5)
text(15, .12, bquote(paste(p[2], "=", .(round(p2, 2)))), cex=1.5)
text(15, .05, bquote(paste(pi[2], "=", p[2]/5)), cex=1.5)
text(25, .12, bquote(paste(p[3], "=", .(round(p3, 2)))), cex=1.5)
text(25, .05, bquote(paste(pi[3], "=", p[3]/5)), cex=1.5)
text(35, .12, bquote(paste(p[4], "=", .(round(p4, 2)))), cex=1.5)
text(35, .05, bquote(paste(pi[4], "=", p[4]/5)), cex=1.5)
text(45, .12, bquote(paste(p[5], "=", .(round(p5, 2)))), cex=1.5)
text(45, .05, bquote(paste(pi[5], "=", p[5]/5)), cex=1.5)
text(35, .7, bquote(paste(pi[0], " = ", 1-sum(pi[j], j==1, j==5))),
     cex=1.5)
dev.off()


N <- 10
y <- as.vector(rmultinom(1, N, c(0.19,0.17,0.12,0.08,0.04,0.4)))
names(y) <-
    c("[0-10]","(10-20]","(20-30]","(30-40]","(40-50]","missed")
y


png("../figs/g10-20.png", width=7, height=7, units="in", res=400)
curve(g, 0, 50, ylim=c(0, 1),
      xlab="Distance", ylab="Detection probability", cex.lab=1.3, lwd=2)
abline(h=0, col=gray(0.9))
polygon(c(x2, rev(x2)), poly2, col=gray(0.8), border=FALSE)
text(5, .5, bquote(paste(pi[1], "=", .(round(p1, 2)))), cex=1.5)
text(15, .35, paste("p =", round(p2, 2)), cex=1.5)
dev.off()


png("../figs/g20-30.png", width=7, height=7, units="in", res=400)
curve(g, 0, 50, ylim=c(0, 1),
      xlab="Distance", ylab="Detection probability", cex.lab=1.3, lwd=2)
abline(h=0, col=gray(0.9))
polygon(c(x3, rev(x3)), poly3, col=gray(0.8), border=FALSE)
text(5, .5, bquote(paste(pi[1], "=", .(round(p1, 2)))), cex=1.5)
text(25, .2, paste("p =", round(p3, 2)), cex=1.5)
dev.off()


png("../figs/g30-40.png", width=7, height=7, units="in", res=400)
curve(g, 0, 50, ylim=c(0, 1),
      xlab="Distance", ylab="Detection probability", cex.lab=1.3, lwd=2)
abline(h=0, col=gray(0.9))
polygon(c(x4, rev(x4)), poly4, col=gray(0.8), border=FALSE)
text(5, .5, bquote(paste(pi[1], "=", .(round(p1, 2)))), cex=1.5)
text(35, .05, paste("p =", round(p4, 2)), cex=1.5)
dev.off()


png("../figs/g40-50.png", width=7, height=7, units="in", res=400)
curve(g, 0, 50, ylim=c(0, 1),
      xlab="Distance", ylab="Detection probability", cex.lab=1.3, lwd=2)
abline(h=0, col=gray(0.9))
polygon(c(x5, rev(x5)), poly5, col=gray(0.8), border=FALSE)
text(5, .5, bquote(paste(pi[1], "=", .(round(p1, 2)))), cex=1.5)
text(45, .05, paste("p =", round(p5, 2)), cex=1.5)
dev.off()



png("../figs/g0-50p0.png", width=7, height=7, units="in", res=400)
curve(g, 0, 50, ylim=c(0, 1),
      xlab="Distance", ylab="Detection probability", cex.lab=1.3, lwd=2)
abline(h=0, col=gray(0.9))
polygon(c(x0, rev(x0)), poly0, col=gray(0.8), border=FALSE)
text(35, .7, bquote(paste(pi[0], "=", .(round(p0, 2)))), cex=1.5)
dev.off()










# ------------------------------ Estimation ------------------------------

# Simulate line transect data
set.seed(38016)
w <- 100                # Strip width (100 meters)
Ns <- 500               # Number of individuals in strip of width w
g <- function(x, theta) exp(-x^2/(2*theta^2)) # Half-normal det function
sigma <- 40             # Parameter of detection function
X <- runif(Ns, 0, w)    # Distances for each individual
p <- g(X, sigma)        # Detection probs for each individual
det <- rbinom(Ns, 1, p) # Was the individual detected?
x <- X[det==1]          # *Observed* distances (the data)
(C <- length(x))        # The count statistic

# Conditional log-likelihood
likeC <- function(par, x) {
    sigma <- par[1]
    denom <- integrate(g, 0, w, theta=sigma)$value
    -sum(log(g(x, theta=sigma)/denom))
}

# Maximum likelihood estimate of sigma
sigma.hat <- optim(20, likeC, method="Brent", lower=0, upper=w, x=x)$par

# Convert to estimate of p
pa.hat <- integrate(function(x, sigma=sigma.hat) exp(-x^2/(2*sigma^2))/w,
                    lower=0, upper=w)$value

# Estimate of Ns.hat
(Ns.hat <- C/pa.hat)



# Export
write.csv(matrix(x, dimnames=list(NULL, "Distance")), "lt.csv",
          row.names=FALSE)




# Unconditional likelihood, assuming C~Bin(Ns,p)
likeUC <- function(par, x) {
    sigma <- exp(par[1])
    Ns <- exp(par[2])
    n0 <- Ns-C
    if(n0<0) return(NA)
    denom <- integrate(g, 0, w, theta=sigma)$value
    p <- denom/w
    part1 <- lchoose(Ns,C) + log(p)*C + log(1-p)*(Ns-C) # Bin(C,n)
    part2 <- sum(log(g(x, theta=sigma)/denom))        # log-like for sigma
    -(part1 + part2)
}

# Need decent starting values
fm2 <- optim(c(sigma=3, Ns=log(Ns)), likeUC, x=x, hessian=TRUE)

# Estimates and confidence intervals
mle <- fm2$par
SE <- sqrt(diag(solve(fm2$hessian)))
cbind(Est=exp(mle), lower=exp(mle-1.96*SE), upper=exp(mle+1.96*SE))



exp(optim(c(3, 10), likeUC, x=x)$par)
exp((fm2 <- optim(c(3, log(n)), likeUC, x=x, hessian=TRUE))$par)
exp(optim(c(5, 5), likeUC, x=x)$par)
exp(optim(c(1, log(n+1)), likeUC, x=x)$par)


sqrt(diag(solve(fm2$hessian)))










# 2nd simulated dataset

# Simulate line transect data
set.seed(38016)
w <- 100                # Strip width (100 meters)
Ns <- 500               # Number of individuals in strip of width w
g <- function(x, theta) exp(-x^2/(2*theta^2)) # Half-normal det function
sigma <- 40             # Parameter of detection function
X <- runif(Ns, 0, w)    # Distances for each individual
p <- g(X, sigma)        # Detection probs for each individual
det <- rbinom(Ns, 1, p) # Was the individual detected?
x <- X[det==1]          # *Observed* distances (the data)
(C <- length(x))        # The count statistic


nTr <- 4
dataout <- data.frame(#transect=rep(1:nTr, each=length(x)/nTr),
                      #length=100,
                      #area=100*100*nTr/(100*100),
                      distance=x)#,
                      #sex=sample(c("M","F"), size=length(x), replace=TRUE))
head(dataout, 30)
summary(dataout)

write.table(dataout, "C:/Work/Workshops/linetransect.txt",
            sep="\t",
            row.names=FALSE)

write.csv(dataout, "C:/Work/Workshops/linetransect.csv",
          row.names=FALSE)







## Figures

png("../figs/example1.png", width=7, height=7, units="in", res=200)
par(mai=c(0.9, 0.9, 0.2, 0.2))
hist(z1, xlab="Distance (x)", main="", cex.lab=1.8)
box()
dev.off()
system("open ../figs/example1.png")



png("../figs/example2.png", width=7, height=7, units="in", res=200)
par(mai=c(0.9, 0.9, 0.2, 0.2))
hist(x, xlab="Distance (x)", main="", cex.lab=1.8, freq=FALSE)
curve(unmarked:::dxhn(x, sigma=sigma.hat), 0, w, add=TRUE)
box()
dev.off()
system("open ../figs/example2.png")

