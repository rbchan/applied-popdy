library(shiny)
library(rsconnect)

## To deploy the app, follow instructions here:
## https://docs.rstudio.com/shinyapps.io/getting-started.html#deploying-applications


if(1==2) {
    runApp()
    deployApp(appName="sensitivity")
}
