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


## Times for interval censoring approach

deer$SurvTime <- as.numeric(deer$FateDate-deer$CapDate)
deer$Fate <- ifelse(deer$Fate2==1, 1, 0)

deer <- deer[,c("Unique.ID", "SurvTime", "Fate", "Sex")]

write.csv(deer, "deer_data.csv", row.names=FALSE)

## S1 <- Surv(time=ic.time1,
##            time2=ic.time2,
##            event=deer$Fate2, type="interval")
## S2 <- Surv(as.numeric(deer$FateDate-deer$CapDate),
##            deer$Fate2==1)
## S3 <- Surv(as.numeric(deer$CapDate-first.day),
##            as.numeric(deer$FateDate-first.day),
##            deer$Fate2==1, type="counting")


deer <- read.csv("deer_data.csv")

S <- Surv(deer$SurvTime, deer$Fate)

plot(survfit(S~1))


png("deerSurvSex.png", width=7, height=7, units="in", res=400)
plot(survfit(S~deer$Sex), lty=1:2,
     main="Survivorship curve",
     xlab="Days since capture (t)", ylab="Probability of surviving to time t", cex.lab=1.5)
legend(400, 1, c("Female", "Male"), lty=c(1,2))
dev.off()
system("gopen deerSurvSex.png")



summary(coxph(S3~deer$Sex))


##png("deerSurvSex.png", width=7, height=7, units="in", res=400)
plot(survfit(S3~deer$Sex), lty=1:2,
     main="Survivorship curve",
     xlab="Date (time since 2014-12-30)",
     ylab="Proportion alive", cex.lab=1.5)
legend(400, 1, c("Female", "Male"), lty=c(1,2))
##dev.off()
##system("open deerSurvSex.png")


