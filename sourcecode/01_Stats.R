new.data = read.csv(file.choose("LOCAL TEST FILE"),header=TRUE,na.strings="NA",sep=",")

attach(new.data)

cor.data <- new.data[,-c(1:2)]
cor.data <- cor.data[,-c(24:32)]
cor.data.m <- cor.data[ !apply(is.na(cor.data), 1,any), ]

mean.age <- aggregate(new.data$V1, list(Gender=new.data$V22), mean)
sd.age <- aggregate(new.data$V1, list(Gender=new.data$V22), sd)

t.age <- t.test(new.data$V1~new.data$V22,
                alternative ="two.sided", mu = 0, paired = FALSE, var.equal = FALSE)
mean.duration <- aggregate(new.data$V21, list(Gender=new.data$V22), mean)
sd.duration <- aggregate(new.data$V21, list(Gender=new.data$V22), sd)
t.duration <- t.test(new.data$V21~new.data$V22,
                     alternative ="two.sided", mu = 0, paired = FALSE, var.equal = FALSE)
mean.severity <- aggregate(ALSFRSr, list(Gender=new.data$V22), mean,na.rm=TRUE)
sd.severity <- aggregate(ALSFRSr, list(Gender=new.data$V22), sd,na.rm=TRUE)
t.severity <- t.test(new.data$ALSFRSr~new.data$V22,na.rm=TRUE,
                     alternative ="two.sided", mu = 0, paired = FALSE, var.equal = FALSE)
mean.severity2 <- aggregate(new.data$AALS, list(Gender=new.data$V22), mean,na.rm=TRUE)
sd.severity2 <- aggregate(new.data$AALS, list(Gender=new.data$V22), sd,na.rm=TRUE)
t.severity2 <- t.test(new.data$AALS~new.data$V22,na.rm=TRUE,
                      alternative ="two.sided", mu = 0, paired = FALSE, var.equal = FALSE)
mean.QoL <- aggregate(new.data$V3, list(Gender=new.data$V22), na.rm=TRUE, mean)
sd.QoL <- aggregate(new.data$V3, list(Gender=new.data$V22), na.rm=TRUE,sd)
t.QoL <- t.test(new.data$V3~new.data$V22,
                alternative ="two.sided", mu = 0, paired = FALSE, var.equal = FALSE)

mean.VAS <- aggregate(new.data$V23, list(Gender=new.data$V22), mean,na.rm=TRUE)
sd.VAS <- aggregate(new.data$V23, list(Gender=new.data$V22), sd,na.rm=TRUE)
t.VAS <- t.test(new.data$V23~new.data$V22,na.rm=TRUE,
                alternative ="two.sided", mu = 0, paired = FALSE, var.equal = FALSE)
block.lm <- summary(lm(new.data$V4~new.data$V18),na.rm=TRUE) #rAmp vs. Block-and-board
peg.lm <- summary(lm(new.data$V4~new.data$V17),na.rm=TRUE) #rAmp vs. Purdue peg bodard
grip.lm <- summary(lm(new.data$V4~new.data$V15),na.rm=TRUE) #rAmp vs. Gripforce
latpinch.lm <- summary(lm(new.data$V4~new.data$V16),na.rm=TRUE) #rAmp vs. Lat.Pinch
z<-new.data$V9
z[new.data$V9 <= 100]=0
z[new.data$V9 > 100]=1
sum.z <- aggregate(((z)/2), list(Bulbar=new.data$V24), sum)
b.CMAP<-t.test(new.data$V28~new.data$V24,na.rm=TRUE,
               alternative ="two.sided", mu = 0, paired = FALSE, var.equal = FALSE) #CMAP
b.MT<-t.test(new.data$V9~new.data$V24,na.rm=TRUE,
             alternative ="two.sided", mu = 0, paired = FALSE, var.equal = FALSE) #MT
b.rAmp<-t.test(new.data$V4~new.data$V24,na.rm=TRUE,
               alternative ="two.sided", mu = 0, paired = FALSE, var.equal = FALSE) #rAmp
b.MEP<-t.test(new.data$V30~new.data$V24,na.rm=TRUE,
              alternative ="two.sided", mu = 0, paired = FALSE, var.equal = FALSE) #MEP
b.ALSFRSr<-t.test(new.data$ALSFRSr~new.data$V24,na.rm=TRUE,
               alternative ="two.sided", mu = 0, paired = FALSE, var.equal = FALSE) #ALSFRSr
b.AALS<-t.test(new.data$AALS~new.data$V24,na.rm=TRUE,
               alternative ="two.sided", mu = 0, paired = FALSE, var.equal = FALSE) #AALS
CMCT.rAmp<-lm(new.data$V4~new.data$V8)
MT.rAmp<-lm(new.data$V4~new.data$V9)
ALSFRSr.rAmp<-(lm(new.data$ALSFRSr~new.data$V4)) # rAmp
AALS.rAmp<-(lm(new.data$AALS~new.data$V4)) # rAmp
ALSFRSr.CMCT<-(lm(new.data$ALSFRSr~new.data$V8)) # CMCT
AALS.CMCT<-(lm(new.data$AALS~new.data$V8)) # CMCT
ALSFRSr.MT<-(lm(new.data$ALSFRSr~new.data$V9)) # Motor threshold
AALS.MT<-(lm(new.data$AALS~new.data$V9)) # Motor threshold
tms.model <- (ALSFRSr ~ V4+V5+V6+V7+V8+V9)
lmmodel.tms <- (lm(tms.model, data=new.data,na.action=na.omit))
library(Hmisc)
library(reshape2)
library(ggplot2)
library(GGally)
tms <- cor.data.m[c(3,6,7,9,10,11)]
dta <- data.frame("ALSFRSr"=tms$ALSFRSr, "V8"=tms$V8, "rAMp"=tms$V4,"rArea"=tms$V5, "MEP-CMAP ratio"=tms$V7, "MT"=tms$V9)# get data
data <- dta
panel.cor <- function(x, y, digits=2, cex.cor)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  test <- cor.test(x,y)
  Signif <- ifelse(round(test$p.value,3)<0.001,"p<0.001",paste("p=",round(test$p.value,3)))
  text(0.5, 0.25, paste("r=",txt))
  text(.5, .75, Signif)
}
panel.smooth<-function (x, y, col = "blue", bg = NA, pch = 16,
                        cex = 0.8, col.smooth = "red", span = 2/3, iter = 3, ...)
{
  points(x, y, pch = pch, col = col, bg = bg, cex = cex)
  ok <- is.finite(x) & is.finite(y)
  if (any(ok))
    lines(stats::lowess(x[ok], y[ok], f = span, iter = iter),
          col = col.smooth, ...)
}
panel.hist <- function(x, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col="cyan", ...)
}
svg(file="ALSFRSr-TMS Matrix Summary.svg")
pairs(data,
      lower.panel=panel.smooth, upper.panel=panel.cor,diag.panel=panel.hist)
dev.off()
data <- cor.data.m
svg("correlation matrix_all.svg")
rplot <- qplot(x=Var1, y=Var2, data=melt(cor(data, use="p")), fill=value, geom="tile") +
  scale_fill_gradient2(limits=c(-1, 1))
rplot +
  theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=12),
        axis.text.y  = element_text(angle=21, vjust=0.5, size=12))
dev.off()
svg("Significance islands_all.svg")
rc <- rcorr(as.matrix(data), type="pearson")
print (rc$P, digits = 10)
pplot <- qplot(x=Var1, y=Var2, data=melt(rc$P), fill=value, geom="tile") +
  scale_fill_gradient2(limits=c(0, 0.049))
pplot +
  theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=12),
        axis.text.y  = element_text(angle=21, vjust=0.5, size=12))
dev.off()
# Create summary of stastics
sink("Stat.Summary.txt")
print(Total_females <- (nrow(new.data[new.data$V22==1,]))/2)
print("Age by gender")
print(mean.age)
print(sd.age)
print(t.age)
print("Disease duration by gender")
print(mean.duration)
print(sd.duration)
print(t.duration)
print("Disease severity (ALSFRSr) by gender")
print(mean.severity)
print(sd.severity)
print(t.severity)
print("Disease severity (AALS) by gender")
print(mean.severity2)
print(sd.severity2)
print(t.severity2)
print("MQoL-SIS by gender")
print(mean.QoL)
print(sd.QoL)
print(t.QoL)
print("VAS by gender")
print(mean.VAS)
print(sd.VAS)
print(t.VAS)
print("Simple linear regression diagnostics for rAmp")
print("rAmp and blocak-and-board performance")
print(block.lm)
print("rAmp and Purdue peg board test")
print(peg.lm)
print("rAmp and gripforce strength")
print(grip.lm)
print("rAmp and lateral pinch")
print(latpinch.lm)
print("Bulbar symptoms")
print("Inexcitable cortex among bulbar subjects")
print(sum.z)
print("Cortical changes accompanying bulbar symptoms")
print("Frequency of bulbar symptoms")
print(sum.z)
print("Bulbar symptoms versus ALSFRSr")
print(b.ALSFRSr)
print("Bulbar symptoms versus AALS")
print(b.AALS)
print("Bulbar symptoms versus CMAP")
print(b.CMAP)
print("Bulbar symptoms versus MT")
print(b.MT)
print("Bulbar symptoms versus rAmp")
print(b.rAmp)
print("Bulbar symptoms versus MEP")
print(b.MEP)
print("CMCT and rAmp")
summary(CMCT.rAmp)
print("Motor threshold and rAmp")
summary(MT.rAmp)
print("Disease severity and neurophysiology")
print("ALSFRSr versus rAmp")
summary(ALSFRSr.rAmp) # rAmp
print("AALS versus rAmp")
summary(AALS.rAmp) # rAmp
print("ALSFRSr versus CMCT")
summary(ALSFRSr.CMCT) # CMCT
print("AALS versus CMCT")
summary(AALS.CMCT) # CMCT
print("ALSFRSr versus MT")
summary(ALSFRSr.MT) # Motor threshold
print("AALS versus MT")
summary(AALS.MT) # Motor threshold
print("TMS model for disease severity prediction using MLR")
summary(lmmodel.tms)
sink()
sink("rc.csv")
rc
sink()
