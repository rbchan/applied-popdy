function(input, output, session) {

    projMat <- reactive({
        A <- matrix(c(input$f1, input$f2, input$f3,
                      input$s1, 0, 0,
                      0, input$s2, input$s3),
                    nrow=3, byrow=TRUE)
        eA <- eigen(A)
        lambda <- Re(eA$values[1])
        ageDist <- Re(eA$vectors[,1])
        ageDist <- ageDist/sum(ageDist)
        reproValue <- Re(eigen(t(A))$vectors[,1])
        reproValue <- reproValue/reproValue[1]
        return(list(A=A, lambda=lambda, ageDist=ageDist, reproValue=reproValue))
    })
    
    ## output$plot1 <- renderPlot({
    ##     plot(rnorm(10), type="b")
    ## })

    output$tableProjMat <- renderTable({
        A <- projMat()$A
        return(A)
    }, colnames=FALSE)
    
    output$lambda <- renderTable({
        lambda <- rbind("Growth rate (lambda)" = projMat()$lambda)
        colnames(lambda) <- "Growth rate (lambda)"
        return(lambda)
    }, rownames=TRUE, colnames=FALSE)

    output$ageRepro <- renderTable({
        eA <- projMat()
        ageRepro <- rbind("Age distribution" = eA$ageDist,
                          "Reproductive value" = eA$reproValue)
        colnames(ageRepro) <- c("Fawns", "Yearlings", "Adults")
        return(ageRepro)
    }, rownames=TRUE, colnames=TRUE)
    
    
}
