function(input, output, session) {

    set.seed(40)
    ## nsites <- input$nsites
    ## nvisits <- input$nvisits
    ## psi <- input$psi_actual
    ## p <- input$p_actual
    nsites <- 100
    nvisits <- 4
    psi <- 0.6
    p <- 0.4
    y <- matrix(NA_integer_, nsites, nvisits)
    z <- rbinom(nsites, 1, psi)
    for(i in 1:nsites)
      y[i,] <- rbinom(nvisits, 1, z[i]*p)
  
    ll <- function(pars) {
      psi <- pars[1]
      p <- pars[2]
      cll_z0 <- rowSums(dbinom(y, 1, 0, log=TRUE))
      cll_z1 <- rowSums(dbinom(y, 1, p, log=TRUE))
      cl <- exp(cbind(cll_z0, cll_z1))
      marginal_like <- cl %*% c(1-psi, psi)
      log_like <- sum(log(marginal_like))
      return(log_like)
    }
    
    delta <- 0.01
    xpsi <- seq(delta/2, 1-delta/2, by=delta)
    xp <- seq(delta/2, 1-delta/2, by=delta)
    x <- cbind(rep(xpsi, each=length(xp)),
               rep(xp, times=length(xpsi)))
    ll_surf <- apply(x, 1, ll)
    ll_surf_mat <- matrix(ll_surf, length(xp), length(xpsi))

  ydata <- as.data.frame(cbind(Site=1:nsites, y))
  colnames(ydata) <- c("Site", paste("Occasion", 1:nvisits))

  ## ydt <- datatable(ydata)
  ydt <- ydata

  output$data <- DT::renderDT(ydt,
    options = list(pageLength=5))

  output$likelihood <- renderPlot({
    
      ## image(xpsi, xp, ll_surf_mat, xlab="Occupancy probability (psi)",
      ##       ylab="Detection probability (p)")
      ## contour(xpsi, xp, ll_surf_mat, add=TRUE)
      
    library(lattice)

    ## xll <- log(seq(exp(min(ll_surf)-0.1), exp(max(ll_surf)+0.1), length=20))
    ## xll <- log(seq(exp(min(ll_surf)), exp(max(ll_surf)), length=10))
    xll <- seq(min(ll_surf)-0.01, max(ll_surf)+0.01, length=100)
    xll <- xll[seq(1, length(xll), by=5)]
    cuts <- round(xll, 2)
    
    levelplot(ll_surf ~ x[,1]+x[,2], aspect="iso", contour=TRUE,
              #at=cuts,
              labels=FALSE, 
              panel=function(...) {
                panel.levelplot(...)
                #panel.contourplot(..., at=cuts)
                lpoints(input$psi_guess, input$p_guess, pch=4, cex=5, col="red")
                ltext(input$psi_guess, input$p_guess,
                      round(ll(c(input$psi_guess, input$p_guess)), 2),
                      cex=2, col="red", pos=1)
              }, 
              xlab="Occupancy probability (psi)",
              ylab="Detection probability (p)",
              main="Log-likelihood surface")
    
  })


  elevation <- seq(0, 10, by=0.1)

  output$logistic_link <- renderPlot({
    beta0 <- input$beta0
    beta1 <- input$beta1
    plot(elevation, beta0 + beta1*elevation, xlab="Elevation",
         ylab="Occupancy on the logit scale", ylim=c(-10, 10), type="l")
  })

  output$logistic_prob <- renderPlot({
    beta0 <- input$beta0
    beta1 <- input$beta1
    plot(elevation, plogis(beta0 + beta1*elevation), xlab="Elevation",
         ylab="Occupancy", ylim=c(0,1), type="l")
  })

}
