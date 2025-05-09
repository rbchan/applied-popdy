library(shiny)
library(rsconnect)
library(DT)

## To deploy the app, follow instructions here:
## https://docs.rstudio.com/shinyapps.io/getting-started.html#deploying-applications


if(1==2) {
    ## If getwd() is same as script dir
    runApp()
    deployApp(appName="occupancy", account="richard-chandler")#, appId="13066173")

    ## If getwd() is up one level
    runApp("shiny")
    deployApp("shiny", appName="occupancy", account="richard-chandler")#, appId="13066173")
}
