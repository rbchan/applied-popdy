function(input, output, session) {

  output$likelihood <- renderPlot({

    set.seed(40)
    nsites <- input$nsites
    nvisits <- input$nvisits
    psi <- input$psi_actual
    p <- input$p_actual
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
    
      ## image(xpsi, xp, ll_surf_mat, xlab="Occupancy probability (psi)",
      ##       ylab="Detection probability (p)")
      ## contour(xpsi, xp, ll_surf_mat, add=TRUE)
      
    library(lattice)
    
    levelplot(ll_surf ~ x[,1]+x[,2], aspect="iso", contour=TRUE,
              at=round(seq(min(ll_surf)-0.1, max(ll_surf)+0.1, length=25), 1),
              labels=TRUE, ##pretty=FALSE,
              panel=function(...) {
                panel.levelplot(...)
                lpoints(input$psi_guess, input$p_guess, pch=4, cex=5, col="red")
                ltext(input$psi_guess, input$p_guess,
                      round(ll(c(input$psi_guess, input$p_guess)), 2),
                      cex=2, col="red", pos=1)
              }, 
              xlab="Occupancy probability (psi)",
              ylab="Detection probability (p)")
    
  })

}
