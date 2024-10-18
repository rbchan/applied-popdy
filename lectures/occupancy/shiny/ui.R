## pageWithSidebar(
navbarPage('Occupancy model',
    ## titlePanel('SCR state and observation models'),
    ## tabsetPanel(
    ##     ##    tabPanel("Plot", plotOutput("plot")),
    ##     tabPanel("Summary", verbatimTextOutput("summary")),
    ##     ##    tabPanel("Table", tableOutput("table")),
    ##     tabPanel("Table", verbatimTextOutput("table"))
    ## ),
##    tabsetPanel(
  tabPanel("Likelihood", ##verbatimTextOutput("State")),
    titlePanel("Likelihood"),
    sidebarLayout(
      sidebarPanel(
        numericInput('nsites', 'Number of sites', 50, min=0, max=1000, step=5),
        numericInput('nvisits', 'Number of survey occasions', 4, min=0, max=10, step=1), 
        numericInput('psi_actual', 'Occupancy probability (psi)', 0.5,
                     min=0.01, max=0.99, step=0.05),  
        numericInput('p_actual', 'Detection probability (p)', 0.4,
                     min=0.01, max=0.99, step=0.05),  
        numericInput('psi_guess', 'Occupancy probability guess', 0.2,
                     min=0.01, max=0.99, step=0.05),  
        numericInput('p_guess', 'Detection probability guess', 0.2,
                     min=0.01, max=0.99, step=0.05),  
        actionButton('simButton', 'Simulate', class = "btn-simulate"),
        ),
      mainPanel(
#        fluidRow(
#          column(7, plotOutput('likelihood'))
#        )
        plotOutput('likelihood')
      )
    ))  ##,
  ## tabPanel("Observation model", 
  ##          sidebarLayout(
  ##            sidebarPanel(
  ##              numericInput('g0', 'Baseline capture probability', 0.5, min=0, max=1, step=0.02),
  ##              numericInput('sigma', 'Scale parameter', 20, min=0, max=1000, step=5),
  ##              numericInput('buffer', 'Buffer (square)', 100, min=0, max=500, step=10),
  ##              numericInput('resolution', 'Spatial resolution', 1, min=0, max=10, step=0.1),
  ##              ),
  ##            mainPanel(
  ##              fluidRow(
  ##                column(5, plotOutput('gdist')),
  ##                column(7, plotOutput('gsp'))
  ##              )
  ##            )
  ##          )
  ##          ),
  ## ##    )
  ## tabPanel("Full model", 
  ##          sidebarLayout(
  ##            sidebarPanel(
  ##              numericInput('EDFull', 'Expected value of density', 50, min=5, max=1000, step=1),
  ##              numericInput('g0Full', 'Baseline capture probability', 0.1, min=0, max=1, step=0.01),
  ##              numericInput('sigmaFull', 'Scale parameter', 0.15, min=0.01, max=0.5, step=0.01),
  ##              actionButton('simButtonFull', 'Simulate', class = "btn-success"),
  ##              ),
  ##            mainPanel(
  ##              fluidRow(
  ##                column(7, plotOutput('spider'))
  ##              )
  ##              ## plotOutput('spider')
  ##            )
  ##          )
  ##          )
  )
