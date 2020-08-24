## ----build,eval=FALSE,include=FALSE-------------------------------------------
## source("../../rnw2pdf.R")
## rnw2pdf("lab-Excel-R-Basics")
## rnw2pdf("lab-Excel-R-Basics", tangle=TRUE)


## ----knitr-theme,include=FALSE------------------------------------------------
##knit_theme$set("navajo-night")
knit_theme$set("edit-kwrite")
##knit_theme$set("tabula")
##knit_theme$set("acid")
##knit_theme$set("breeze")
##knit_theme$set("peaksea")


## ----year,size='scriptsize'---------------------------------------------------
year <- 1950:1961 # A vector of integers
year              # Type the name of an object to see its values


## ----nYears,size='footnotesize'-----------------------------------------------
nYears <- length(year)
nYears


## ----females------------------------------------------------------------------
females <- rep(NA, nYears)
females[1] <- 100


## ----femaleLoop---------------------------------------------------------------
for(t in 2:nYears) {
    females[t] <- females[t-1] + females[t-1]*0.01
}


## ----males,size='footnotesize'------------------------------------------------
males <- females*0.8


## ----data, size='scriptsize'--------------------------------------------------
model1 <- data.frame(year, females, males)
model1


## ----plot,size='tiny',fig.width=7,fig.height=4.6------------------------------
plot(females ~ year, data=model1, type="o", xlab="Year", ylab="Abundance",
     lwd=2, pch=16, ylim=c(0, 120))
lines(males ~ year, data=model1, type="o", col="blue", lwd=2, pch=16)
legend(x=1950, y=40, legend=c("Females", "Males"), col=c("black", "blue"), lty=1, pch=16)

