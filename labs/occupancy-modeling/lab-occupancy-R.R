## ----eval=FALSE,include=FALSE-------------------------------------------------
## source("../../rnw2pdf.R")
## rnw2pdf("lab-occupancy-R")
## rnw2pdf("lab-occupancy-R",tangle=TRUE)


## ----install,eval=FALSE-------------------------------------------------------
## install.packages("unmarked")


## ----load---------------------------------------------------------------------
library(unmarked)


## ----getwd--------------------------------------------------------------------
getwd()


## ----setwd,eval=FALSE---------------------------------------------------------
## setwd("C:/Users/Richard/Documents/APD/") ## You will need to modify the path in quotes


## ----import-------------------------------------------------------------------
exampleData <- read.csv("example-data.csv")


## ----summary,size='footnotesize'----------------------------------------------
summary(exampleData)


## ----unmarkedFrame------------------------------------------------------------
## First, extract columns 2-4 with the occupancy data
occData <- exampleData[,c("season1visit1", "season1visit2",
                          "season2visit1", "season2visit2")]
## Next, extract the column with the site covariate.
## It must be formatted as a data.frame
habIndex <- exampleData[,c("habitatIndex"),drop=FALSE]
## Finally, put the pieces together
umf <- unmarkedFrameOccu(y=occData, siteCovs=habIndex)


## ----summary2-----------------------------------------------------------------
summary(umf)


## ----null---------------------------------------------------------------------
nullModel <- occu(~1 ~1, umf)


## ----summaryNull--------------------------------------------------------------
summary(nullModel)


## ----backTransform------------------------------------------------------------
## Occupancy estimate - Pr(site is occupied)
backTransform(nullModel, type="state") 
## Detection estimate - Pr(detection | site is occupied)
backTransform(nullModel, type="det") 


## ----occHab-------------------------------------------------------------------
detNullOccHab <- occu(~1 ~habitatIndex, umf)


## ----summaryOccHab------------------------------------------------------------
summary(detNullOccHab)


## ----predData-----------------------------------------------------------------
predData <- data.frame(habitatIndex=seq(from=1, to=5, length.out=10))


## ----pred---------------------------------------------------------------------
predOcc <- predict(detNullOccHab, newdata=predData,
                   type="state", append=TRUE)
predOcc


## ----plotOcc,out.width='80%',fig.align='center'-------------------------------
plot(Predicted ~ habitatIndex, data=predOcc, type="l", ylim=c(0,1),
     xlab="Habitat index", ylab="Occupancy")
lines(lower ~ habitatIndex, data=predOcc, lty=2)
lines(upper ~ habitatIndex, data=predOcc, lty=2)


## ----unmarkedFrameMS----------------------------------------------------------
umfMS <- unmarkedMultFrame(y=occData, siteCovs=habIndex, numPrimary=2)


## ----summaryMS----------------------------------------------------------------
summary(umfMS)


## ----nullMS-------------------------------------------------------------------
nullModelMS <- colext(~1, ~1, ~1, ~1, umfMS)


## ----summaryNullMS------------------------------------------------------------
summary(nullModelMS)


## ----backMS-------------------------------------------------------------------
backTransform(nullModelMS, type="psi")
backTransform(nullModelMS, type="col")
backTransform(nullModelMS, type="ext")
backTransform(nullModelMS, type="det")

