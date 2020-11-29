library(survival)

deer.in <- read.csv("DeerSurvival.csv")
deer.in$CapDate <- as.Date(deer.in$Capture.Date, format="%m/%d/%Y")
deer.in$FateDate <- as.Date(deer.in$Fate.Date, format="%m/%d/%Y")
str(deer.in)

head(deer.in)

hist(deer.in$CapDate, breaks=20)


deer <- droplevels(subset(deer.in, Collar.On.Days>14 &
                          FateDate <= as.Date("2016-05-31")))
str(deer)

first.day <- min(deer$CapDate)
last.day <- max(deer$FateDate)

deer$Unique.ID <- factor(deer$Unique.ID)
deer$Fate2 <- NA
deer$Fate2[deer$Fate=="dead"] <- 1
deer$Fate2[deer$CapDate==first.day &
           deer$FateDate<=last.day &
           deer$Fate!="dead"] <- 0
deer$Fate2[deer$CapDate>first.day &
           deer$FateDate==last.day] <- 2
deer$Fate2[deer$CapDate>first.day &
           deer$FateDate<last.day &
           deer$Fate != "dead"] <- 3
deer$Fate2

table(deer$Fate2)

summary(deer)

hist(as.numeric(deer$FateDate - deer$CapDate), breaks=10)


S <- Surv(as.numeric(deer$FateDate-deer$CapDate),
          deer$Fate2==1)


plot(survfit(S~1))



## png("deerSurvSex.png", width=7, height=7, units="in", res=400)
plot(survfit(S~deer$Sex), lty=1:2,
     main="Survivorship curve",
     xlab="Days since capture", ylab="Proportion alive", cex.lab=1.5)
legend(400, 1, c("Female", "Male"), lty=c(1,2))
## dev.off()
## system("open deerSurvSex.png")



summary(coxph(S~deer$Sex))


##png("deerSurvSex.png", width=7, height=7, units="in", res=400)
plot(survfit(S~deer$Sex), lty=1:2,
     main="Survivorship curve",
     xlab="Date (time since 2014-12-30)",
     ylab="Proportion alive", cex.lab=1.5)
legend(400, 1, c("Female", "Male"), lty=c(1,2))
##dev.off()
##system("open deerSurvSex.png")


