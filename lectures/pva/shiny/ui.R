pageWithSidebar(
    titlePanel('Sensitivity analysis'),
    sidebarPanel(
        numericInput('s1', 'Fawn survival', 0.3, min=0, max=1, step=0.05),
        numericInput('s2', 'Yearling survival', 0.9, min=0, max=1, step=0.05),
        numericInput('s3', 'Adult survival', 0.7, min=0, max=1, step=0.05),
        numericInput('f1', 'Fawn fecundity', 0.3, min=0, max=3, step=0.05),
        numericInput('f2', 'Yearling fecundity', 0.9, min=0, max=3, step=0.05),
        numericInput('f3', 'Adult fecundity', 0.7, min=0, max=3, step=0.05),
        h2('Projection matrix'),
        fluidRow({
            column(4, tableOutput('tableProjMat'))
        })
    ),
    mainPanel(
        ## fluidRow({
        ##     plotOutput('plot1')
        ## }),
        ## h2('Growth rate (lambda)'),
        fluidRow({
            tableOutput('lambda')
        }),
        ## h2('Age distribution and reproductive value'),
        fluidRow({
            tableOutput('ageRepro')
        })
    )
)
