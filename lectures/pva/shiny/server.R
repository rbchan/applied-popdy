function(input, output, session) {

    projMat <- reactive({
        A <- matrix(c(input$f1, input$f2, input$f3,
                      input$s1, 0, 0,
                      0, input$s2, input$s3),
                    nrow=3, byrow=TRUE)
        eA <- eigen(A)
        lambda <- eA$values[1]
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
        lambda <- projMat()$lambda
        names(lambda) <- lambda
        return(lambda)
    }, colnames=FALSE)

    output$ageDist <- renderTable({
        ageDist <- t(projMat()$ageDist)
        colnames(ageDist) <- c("Fawns", "Yearlings", "Adults")
        return(ageDist)
    }, colnames=TRUE)
    
    
}
