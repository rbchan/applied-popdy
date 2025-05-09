## pageWithSidebar(
navbarPage('Occupancy model',

  tabPanel("Occupancy dataset",
    titlePanel("Occupancy dataset"),
#    sidebarLayout(
      mainPanel(
        DT::DTOutput('data')
      )
#    )
    ),
  
  tabPanel("Likelihood", ##verbatimTextOutput("State")),
    titlePanel("Likelihood"),
    sidebarLayout(
      sidebarPanel(
        ## numericInput('nsites', 'Number of sites', 50, min=0, max=1000, step=5),
        ## numericInput('nvisits', 'Number of survey occasions', 4, min=0, max=10, step=1), 
        ## numericInput('psi_actual', 'Occupancy probability (psi)', 0.5,
        ##              min=0.01, max=0.99, step=0.05),  
        ## numericInput('p_actual', 'Detection probability (p)', 0.4,
        ##              min=0.01, max=0.99, step=0.05),  
        numericInput('psi_guess', 'Occupancy probability (p) guess', 0.2,
                     min=0.01, max=0.99, step=0.05),  
        numericInput('p_guess', 'Detection probability (psi) guess', 0.2,
                     min=0.01, max=0.99, step=0.05),  
        ## actionButton('simButton', 'Simulate', class = "btn-simulate"),
        ),
      mainPanel(
#        fluidRow(
#          column(7, plotOutput('likelihood'))
#        )
        plotOutput('likelihood')
      )
    )
    ),
  
  tabPanel("Covariates", 
    titlePanel("Covariates"),
    sidebarLayout(
      sidebarPanel(
        numericInput('beta0', 'Intercept (beta0) on logit scale', -1,
                     min=-5, max=5, step=0.05),  
        numericInput('beta1', 'Slope (beta1) on logit scale', 1,
                     min=-5, max=5, step=0.05)  
      ),
      mainPanel(
        fluidRow(  
          column(5, plotOutput('logistic_link')),
          column(5, plotOutput('logistic_prob'))
        )
        ## fluidRow(plotOutput('logistic_link')),
        ## fluidRow(plotOutput('logistic_prob'))
    )
    )

  )
)
