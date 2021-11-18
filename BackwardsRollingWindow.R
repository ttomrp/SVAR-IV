library(tseries)
library(vars)
library(moments)
library(tidyverse)
library(readxl)
library(AER)
library(svars)
library(backtest)
library(stargazer)
library(xtable)
library(ggplot2)
library(Hmisc)
library(reshape2)
library(boot)
library(devtools)
library(varexternal)
library(varexternalinstrument)
setwd("C:/Users/Tom/OneDrive/Documents/Thesis Project/Thesis Data")
data = read_excel("Prelim_Dataset.xlsx", sheet = 1)

COD = ts(data[,2], start = 1987, frequency = 12) #Consumer Discretionary
ENE = ts(data[,3], start = 1987, frequency = 12) #Energy
FIN = ts(data[,4], start = 1987, frequency = 12) #Financials
UTL = ts(data[,5], start = 1987, frequency = 12) #Utilities
HCR = ts(data[,6], start = 1987, frequency = 12) #Healthcare
INT = ts(data[,7], start = 1987, frequency = 12) #Information Technology
RES = ts(data[,8], start = 1987, frequency = 12) #Real Estate
IND = ts(data[,9], start = 1987, frequency = 12) #Industrials
MAT = ts(data[,10], start = 1987, frequency = 12) #Materials
CST = ts(data[,11], start = 1987, frequency = 12) #Consumer Stationary
TEL = ts(data[,12], start = 1987, frequency = 12) #Telecommunications
COMP = ts(data[,13], start = 1987, frequency = 12) #US Composite
WTI = ts(data[,14], start = 1987, frequency = 12) #WTI Oil
HTO = ts(data[,15], start = 1987, frequency = 12) #No.2 Heating Oil
IP = ts(data[,16], start = 1987, frequency = 12) #Industrial Production
IR = ts(data[,17], start = 1987, frequency = 12) #Fed Funds Effective Interest Rate
CPI = ts(data[,18], start = 1987, frequency = 12) #CPI
WMKT = ts(data[,19], start = 1987, frequency = 12) #World Market Composite
TEN = ts(data[,20], start = 1987, frequency = 12) #10-Year Treasury Rate

y1 = c(NA, diff(log(COMP))) #y1 = COMP return
y2 = c(NA, diff(log(COD))) #y2 = COD return
y3 = c(NA, diff(log(CST))) #y3 = CST return
y4 = c(NA, diff(log(ENE))) #y4 = ENE return
y5 = c(NA, diff(log(FIN))) #y5 = FIN return
y6 = c(NA, diff(log(HCR))) #y6 = HCR return
y7 = c(NA, diff(log(IND))) #y7 = IND return
y8 = c(NA, diff(log(INT))) #y8 = INT return
y9 = c(NA, diff(log(MAT))) #y9 = MAT return
y10 = c(NA, diff(log(RES))) #y10 = RES return
y11 = c(NA, diff(log(TEL))) #y11 = TEL return
y12 = c(NA, diff(log(UTL))) #y12 = UTL return
x1 = c(NA, diff(log(WTI))) #x1 = WTI return
iv1 = c(NA, diff(log(HTO))) #iv1 = HTO return
c1 = c(NA, diff(log(IP))) #c1 = IP growth rate
c2 = c(NA, diff(IR)) #c2 = IR monthly change
c3 = c(NA, diff(log(CPI))) #c3 = Inflation Rate
c4 = c(NA, diff(log(WMKT))) #c4 = WMKT return
c5 = c(NA, diff(log(TEN))) #c5 = TEN monthly change


#----
# T = 96 + 1
#-----------------------------------------------------------------------------------------------------------------------
data_z = cbind(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, x1, iv1, c1, c3, c4, c5)
data_v = data_z[2:401,]
data.df = as.data.frame(data_v)
data.df$y = (1:nrow(data.df) - 1) %/% 12
data.df$y = c(data.df$y[2:400], 32)
year = data.df$y

#y1

func = function(mylist,ind){
  
  mydf = do.call(rbind,mylist[ind])
  gkvar <- VAR(mydf[, c("y1", "x1", "c1", "c3", "c4", "c5")], p = 1, type = "const")
  shockcol <- externalinstrument(gkvar, mydf$iv1, "x1")
  
  ma_representation <- Phi(gkvar, 12)
  irfs <- t(apply(ma_representation, 3, function(x) x %*% shockcol)) #unclear
  colnames(irfs) <- names(shockcol)
  irfs
  return(irfs)
}

cum_irfs = c()
s = seq(1:304)
#s = seq(2:3)
remove(n)
for (n in s){
  data_a = data_v[n:(96+n),]
  data.df = as.data.frame(data_a)
  data.df$year = year[n:(96+n)]
  
  irfs1 = func(split(data.df, data.df$year))*100
  res = boot(split(data.df,data.df$year),func,100)
  
  boot_est= sapply(1:ncol(res$t),function(i){
    boot.ci(res,index=i,type = "perc")$percent[4:5]
  })*100
  
  lb = matrix(boot_est[1,],ncol=ncol(res$t0))
  ub = matrix(boot_est[2,],ncol=ncol(res$t0))
  
  irfc1 = sum(irfs1[,1])
  lbc1 = sum(lb[,1])
  ubc1 = sum(ub[,1])
  ci = c(irfc1, lbc1, ubc1)
  cum_irfs = rbind(cum_irfs, ci)
}

cum_irfs

xticks = c(1,13,25,37,49,61,73,85,97,109,121,133,145,157,169,181,193,205,217,229,241,253,265,277,289,301)
xyears = c("1995","","","","","","","","","","","","2007","","","","","","","","","","","","","2020")
response.time = c(1:304)

plot(response.time, cum_irfs[,1], xlim=c(12, 293), ylim=c(-5.0, 8.0), xlab = "", ylab = "Market Return", main = "US Total Market", type = "l", cex.lab=1.2, cex.axis=1.2, xaxt="n")
axis(1, at=xticks, labels=xyears)
polygon(c(response.time, rev(response.time)), c(cum_irfs[,2], rev(cum_irfs[,3])), col='grey90', border=FALSE)
lines(response.time, cum_irfs[,1], lwd=2)
lines(response.time, cum_irfs[,2], col='red', lwd=2)
lines(response.time, cum_irfs[,3], col='red', lwd=2)
abline(h=0, col="blue", lwd=2, lty=2)
grid()


#y2

func = function(mylist,ind){
  
  mydf = do.call(rbind,mylist[ind])
  gkvar <- VAR(mydf[, c("y2", "x1", "c1", "c3", "c4", "c5")], p = 1, type = "const")
  shockcol <- externalinstrument(gkvar, mydf$iv1, "x1")
  
  ma_representation <- Phi(gkvar, 12)
  irfs <- t(apply(ma_representation, 3, function(x) x %*% shockcol)) #unclear
  colnames(irfs) <- names(shockcol)
  irfs
  return(irfs)
}

cum_irfs = c()
s = seq(1:304)
remove(n)
for (n in s){
  data_a = data_v[n:(96+n),]
  data.df = as.data.frame(data_a)
  data.df$year = year[n:(96+n)]
  
  irfs1 = func(split(data.df, data.df$year))*100
  res = boot(split(data.df,data.df$year),func,100)
  
  boot_est= sapply(1:ncol(res$t),function(i){
    boot.ci(res,index=i,type = "perc")$percent[4:5]
  })*100
  
  lb = matrix(boot_est[1,],ncol=ncol(res$t0))
  ub = matrix(boot_est[2,],ncol=ncol(res$t0))
  
  irfc1 = sum(irfs1[,1])
  lbc1 = sum(lb[,1])
  ubc1 = sum(ub[,1])
  ci = c(irfc1, lbc1, ubc1)
  cum_irfs = rbind(cum_irfs, ci)
}


xticks = c(1,13,25,37,49,61,73,85,97,109,121,133,145,157,169,181,193,205,217,229,241,253,265,277,289,301)
xyears = c("1995","","","","","","","","","","","","2007","","","","","","","","","","","","","2020")
response.time = c(1:304)

plot(response.time, cum_irfs[,1], xlim=c(12, 293), ylim=c(-6.5, 6.0), xlab = "", ylab = "Market Return", main = "Consumer Discretionary", type = "l", cex.lab=1.2, cex.axis=1.2, xaxt="n")
axis(1, at=xticks, labels=xyears)
polygon(c(response.time, rev(response.time)), c(cum_irfs[,2], rev(cum_irfs[,3])), col='grey90', border=FALSE)
lines(response.time, cum_irfs[,1], lwd=2)
lines(response.time, cum_irfs[,2], col='red', lwd=2)
lines(response.time, cum_irfs[,3], col='red', lwd=2)
abline(h=0, col="blue", lwd=2, lty=2)
grid()

#y3

func = function(mylist,ind){
  
  mydf = do.call(rbind,mylist[ind])
  gkvar <- VAR(mydf[, c("y3", "x1", "c1", "c3", "c4", "c5")], p = 1, type = "const")
  shockcol <- externalinstrument(gkvar, mydf$iv1, "x1")
  
  ma_representation <- Phi(gkvar, 12)
  irfs <- t(apply(ma_representation, 3, function(x) x %*% shockcol)) #unclear
  colnames(irfs) <- names(shockcol)
  irfs
  return(irfs)
}

cum_irfs = c()
s = seq(1:304)
remove(n)
for (n in s){
  data_a = data_v[n:(96+n),]
  data.df = as.data.frame(data_a)
  data.df$year = year[n:(96+n)]
  
  irfs1 = func(split(data.df, data.df$year))*100
  res = boot(split(data.df,data.df$year),func,100)
  
  boot_est= sapply(1:ncol(res$t),function(i){
    boot.ci(res,index=i,type = "perc")$percent[4:5]
  })*100
  
  lb = matrix(boot_est[1,],ncol=ncol(res$t0))
  ub = matrix(boot_est[2,],ncol=ncol(res$t0))
  
  irfc1 = sum(irfs1[,1])
  lbc1 = sum(lb[,1])
  ubc1 = sum(ub[,1])
  ci = c(irfc1, lbc1, ubc1)
  cum_irfs = rbind(cum_irfs, ci)
}


xticks = c(1,13,25,37,49,61,73,85,97,109,121,133,145,157,169,181,193,205,217,229,241,253,265,277,289,301)
xyears = c("1995","","","","","","","","","","","","2007","","","","","","","","","","","","","2020")
response.time = c(1:304)

plot(response.time, cum_irfs[,1], xlim=c(12, 293), ylim=c(-4.0, 4.5), xlab = "", ylab = "Market Return", main = "Consumer Stationary", type = "l", cex.lab=1.2, cex.axis=1.2, xaxt="n")
axis(1, at=xticks, labels=xyears)
polygon(c(response.time, rev(response.time)), c(cum_irfs[,2], rev(cum_irfs[,3])), col='grey90', border=FALSE)
lines(response.time, cum_irfs[,1], lwd=2)
lines(response.time, cum_irfs[,2], col='red', lwd=2)
lines(response.time, cum_irfs[,3], col='red', lwd=2)
abline(h=0, col="blue", lwd=2, lty=2)
grid()


#y4

func = function(mylist,ind){
  
  mydf = do.call(rbind,mylist[ind])
  gkvar <- VAR(mydf[, c("y4", "x1", "c1", "c3", "c4", "c5")], p = 1, type = "const")
  shockcol <- externalinstrument(gkvar, mydf$iv1, "x1")
  
  ma_representation <- Phi(gkvar, 12)
  irfs <- t(apply(ma_representation, 3, function(x) x %*% shockcol)) #unclear
  colnames(irfs) <- names(shockcol)
  irfs
  return(irfs)
}

cum_irfs = c()
s = seq(1:304)
remove(n)
for (n in s){
  data_a = data_v[n:(96+n),]
  data.df = as.data.frame(data_a)
  data.df$year = year[n:(96+n)]
  
  irfs1 = func(split(data.df, data.df$year))*100
  res = boot(split(data.df,data.df$year),func,100)
  
  boot_est= sapply(1:ncol(res$t),function(i){
    boot.ci(res,index=i,type = "perc")$percent[4:5]
  })*100
  
  lb = matrix(boot_est[1,],ncol=ncol(res$t0))
  ub = matrix(boot_est[2,],ncol=ncol(res$t0))
  
  irfc1 = sum(irfs1[,1])
  lbc1 = sum(lb[,1])
  ubc1 = sum(ub[,1])
  ci = c(irfc1, lbc1, ubc1)
  cum_irfs = rbind(cum_irfs, ci)
}


xticks = c(1,13,25,37,49,61,73,85,97,109,121,133,145,157,169,181,193,205,217,229,241,253,265,277,289,301)
xyears = c("1995","","","","","","","","","","","","2007","","","","","","","","","","","","","2020")
response.time = c(1:304)

plot(response.time, cum_irfs[,1], xlim=c(12, 293), ylim=c(-2.0, 14.0), xlab = "", ylab = "Market Return", main = "Energy", type = "l", cex.lab=1.2, cex.axis=1.2, xaxt="n")
axis(1, at=xticks, labels=xyears)
polygon(c(response.time, rev(response.time)), c(cum_irfs[,2], rev(cum_irfs[,3])), col='grey90', border=FALSE)
lines(response.time, cum_irfs[,1], lwd=2)
lines(response.time, cum_irfs[,2], col='red', lwd=2)
lines(response.time, cum_irfs[,3], col='red', lwd=2)
abline(h=0, col="blue", lwd=2, lty=2)
grid()


#y5

func = function(mylist,ind){
  
  mydf = do.call(rbind,mylist[ind])
  gkvar <- VAR(mydf[, c("y5", "x1", "c1", "c3", "c4", "c5")], p = 1, type = "const")
  shockcol <- externalinstrument(gkvar, mydf$iv1, "x1")
  
  ma_representation <- Phi(gkvar, 12)
  irfs <- t(apply(ma_representation, 3, function(x) x %*% shockcol)) #unclear
  colnames(irfs) <- names(shockcol)
  irfs
  return(irfs)
}

cum_irfs = c()
s = seq(1:304)
remove(n)
for (n in s){
  data_a = data_v[n:(96+n),]
  data.df = as.data.frame(data_a)
  data.df$year = year[n:(96+n)]
  
  irfs1 = func(split(data.df, data.df$year))*100
  res = boot(split(data.df,data.df$year),func,100)
  
  boot_est= sapply(1:ncol(res$t),function(i){
    boot.ci(res,index=i,type = "perc")$percent[4:5]
  })*100
  
  lb = matrix(boot_est[1,],ncol=ncol(res$t0))
  ub = matrix(boot_est[2,],ncol=ncol(res$t0))
  
  irfc1 = sum(irfs1[,1])
  lbc1 = sum(lb[,1])
  ubc1 = sum(ub[,1])
  ci = c(irfc1, lbc1, ubc1)
  cum_irfs = rbind(cum_irfs, ci)
}


xticks = c(1,13,25,37,49,61,73,85,97,109,121,133,145,157,169,181,193,205,217,229,241,253,265,277,289,301)
xyears = c("1995","","","","","","","","","","","","2007","","","","","","","","","","","","","2020")
response.time = c(1:304)

plot(response.time, cum_irfs[,1], xlim=c(12, 293), ylim=c(-7.0, 11.5), xlab = "", ylab = "Market Return", main = "Financials", type = "l", cex.lab=1.2, cex.axis=1.2, xaxt="n")
axis(1, at=xticks, labels=xyears)
polygon(c(response.time, rev(response.time)), c(cum_irfs[,2], rev(cum_irfs[,3])), col='grey90', border=FALSE)
lines(response.time, cum_irfs[,1], lwd=2)
lines(response.time, cum_irfs[,2], col='red', lwd=2)
lines(response.time, cum_irfs[,3], col='red', lwd=2)
abline(h=0, col="blue", lwd=2, lty=2)
grid()


#y6
# the interval of data for the plot is changed here

func = function(mylist,ind){
  
  mydf = do.call(rbind,mylist[ind])
  gkvar <- VAR(mydf[, c("y6", "x1", "c1", "c3", "c4", "c5")], p = 1, type = "const")
  shockcol <- externalinstrument(gkvar, mydf$iv1, "x1")
  
  ma_representation <- Phi(gkvar, 12)
  irfs <- t(apply(ma_representation, 3, function(x) x %*% shockcol)) #unclear
  colnames(irfs) <- names(shockcol)
  irfs
  return(irfs)
}

cum_irfs = c()
s = seq(1:304)
remove(n)
for (n in s){
  data_a = data_v[n:(96+n),]
  data.df = as.data.frame(data_a)
  data.df$year = year[n:(96+n)]
  
  irfs1 = func(split(data.df, data.df$year))*100
  res = boot(split(data.df,data.df$year),func,100)
  
  boot_est= sapply(1:ncol(res$t),function(i){
    boot.ci(res,index=i,type = "perc")$percent[4:5]
  })*100
  
  lb = matrix(boot_est[1,],ncol=ncol(res$t0))
  ub = matrix(boot_est[2,],ncol=ncol(res$t0))
  
  irfc1 = sum(irfs1[,1])
  lbc1 = sum(lb[,1])
  ubc1 = sum(ub[,1])
  ci = c(irfc1, lbc1, ubc1)
  cum_irfs = rbind(cum_irfs, ci)
}


xticks = c(1,13,25,37,49,61,73,85,97,109,121,133,145,157,169,181,193,205,217,229,241,253,265,277,289,301)
xyears = c("1995","","","","","","","","","","","","2007","","","","","","","","","","","","","2020")
response.time = c(1:303)

plot(response.time, cum_irfs[0:303,1], xlim=c(12, 292), ylim=c(-4.0, 5.0), xlab = "", ylab = "Market Return", main = "Healthcare", type = "l", cex.lab=1.2, cex.axis=1.2, xaxt="n")
axis(1, at=xticks, labels=xyears)
polygon(c(response.time, rev(response.time)), c(cum_irfs[0:303,2], rev(cum_irfs[0:303,3])), col='grey90', border=FALSE)
lines(response.time, cum_irfs[0:303,1], lwd=2)
lines(response.time, cum_irfs[0:303,2], col='red', lwd=2)
lines(response.time, cum_irfs[0:303,3], col='red', lwd=2)
abline(h=0, col="blue", lwd=2, lty=2)
grid()


#y7

func = function(mylist,ind){
  
  mydf = do.call(rbind,mylist[ind])
  gkvar <- VAR(mydf[, c("y7", "x1", "c1", "c3", "c4", "c5")], p = 1, type = "const")
  shockcol <- externalinstrument(gkvar, mydf$iv1, "x1")
  
  ma_representation <- Phi(gkvar, 12)
  irfs <- t(apply(ma_representation, 3, function(x) x %*% shockcol)) #unclear
  colnames(irfs) <- names(shockcol)
  irfs
  return(irfs)
}

cum_irfs = c()
s = seq(1:304)
remove(n)
for (n in s){
  data_a = data_v[n:(96+n),]
  data.df = as.data.frame(data_a)
  data.df$year = year[n:(96+n)]
  
  irfs1 = func(split(data.df, data.df$year))*100
  res = boot(split(data.df,data.df$year),func,100)
  
  boot_est= sapply(1:ncol(res$t),function(i){
    boot.ci(res,index=i,type = "perc")$percent[4:5]
  })*100
  
  lb = matrix(boot_est[1,],ncol=ncol(res$t0))
  ub = matrix(boot_est[2,],ncol=ncol(res$t0))
  
  irfc1 = sum(irfs1[,1])
  lbc1 = sum(lb[,1])
  ubc1 = sum(ub[,1])
  ci = c(irfc1, lbc1, ubc1)
  cum_irfs = rbind(cum_irfs, ci)
}


xticks = c(1,13,25,37,49,61,73,85,97,109,121,133,145,157,169,181,193,205,217,229,241,253,265,277,289,301)
xyears = c("1995","","","","","","","","","","","","2007","","","","","","","","","","","","","2020")
response.time = c(1:304)

plot(response.time, cum_irfs[,1], xlim=c(12, 293), ylim=c(-6.0, 8.5), xlab = "", ylab = "Market Return", main = "Industrials", type = "l", cex.lab=1.2, cex.axis=1.2, xaxt="n")
axis(1, at=xticks, labels=xyears)
polygon(c(response.time, rev(response.time)), c(cum_irfs[,2], rev(cum_irfs[,3])), col='grey90', border=FALSE)
lines(response.time, cum_irfs[,1], lwd=2)
lines(response.time, cum_irfs[,2], col='red', lwd=2)
lines(response.time, cum_irfs[,3], col='red', lwd=2)
abline(h=0, col="blue", lwd=2, lty=2)
grid()


#y8

func = function(mylist,ind){
  
  mydf = do.call(rbind,mylist[ind])
  gkvar <- VAR(mydf[, c("y8", "x1", "c1", "c3", "c4", "c5")], p = 1, type = "const")
  shockcol <- externalinstrument(gkvar, mydf$iv1, "x1")
  
  ma_representation <- Phi(gkvar, 12)
  irfs <- t(apply(ma_representation, 3, function(x) x %*% shockcol)) #unclear
  colnames(irfs) <- names(shockcol)
  irfs
  return(irfs)
}

cum_irfs = c()
s = seq(1:304)
remove(n)
for (n in s){
  data_a = data_v[n:(96+n),]
  data.df = as.data.frame(data_a)
  data.df$year = year[n:(96+n)]
  
  irfs1 = func(split(data.df, data.df$year))*100
  res = boot(split(data.df,data.df$year),func,100)
  
  boot_est= sapply(1:ncol(res$t),function(i){
    boot.ci(res,index=i,type = "perc")$percent[4:5]
  })*100
  
  lb = matrix(boot_est[1,],ncol=ncol(res$t0))
  ub = matrix(boot_est[2,],ncol=ncol(res$t0))
  
  irfc1 = sum(irfs1[,1])
  lbc1 = sum(lb[,1])
  ubc1 = sum(ub[,1])
  ci = c(irfc1, lbc1, ubc1)
  cum_irfs = rbind(cum_irfs, ci)
}


xticks = c(1,13,25,37,49,61,73,85,97,109,121,133,145,157,169,181,193,205,217,229,241,253,265,277,289,301)
xyears = c("1995","","","","","","","","","","","","2007","","","","","","","","","","","","","2020")
response.time = c(1:304)

plot(response.time, cum_irfs[,1], xlim=c(12, 293), ylim=c(-8.5, 7.5), xlab = "", ylab = "Market Return", main = "Information Technology", type = "l", cex.lab=1.2, cex.axis=1.2, xaxt="n")
axis(1, at=xticks, labels=xyears)
polygon(c(response.time, rev(response.time)), c(cum_irfs[,2], rev(cum_irfs[,3])), col='grey90', border=FALSE)
lines(response.time, cum_irfs[,1], lwd=2)
lines(response.time, cum_irfs[,2], col='red', lwd=2)
lines(response.time, cum_irfs[,3], col='red', lwd=2)
abline(h=0, col="blue", lwd=2, lty=2)
grid()


#y9

func = function(mylist,ind){
  
  mydf = do.call(rbind,mylist[ind])
  gkvar <- VAR(mydf[, c("y9", "x1", "c1", "c3", "c4", "c5")], p = 1, type = "const")
  shockcol <- externalinstrument(gkvar, mydf$iv1, "x1")
  
  ma_representation <- Phi(gkvar, 12)
  irfs <- t(apply(ma_representation, 3, function(x) x %*% shockcol)) #unclear
  colnames(irfs) <- names(shockcol)
  irfs
  return(irfs)
}

cum_irfs = c()
s = seq(1:304)
remove(n)
for (n in s){
  data_a = data_v[n:(96+n),]
  data.df = as.data.frame(data_a)
  data.df$year = year[n:(96+n)]
  
  irfs1 = func(split(data.df, data.df$year))*100
  res = boot(split(data.df,data.df$year),func,100)
  
  boot_est= sapply(1:ncol(res$t),function(i){
    boot.ci(res,index=i,type = "perc")$percent[4:5]
  })*100
  
  lb = matrix(boot_est[1,],ncol=ncol(res$t0))
  ub = matrix(boot_est[2,],ncol=ncol(res$t0))
  
  irfc1 = sum(irfs1[,1])
  lbc1 = sum(lb[,1])
  ubc1 = sum(ub[,1])
  ci = c(irfc1, lbc1, ubc1)
  cum_irfs = rbind(cum_irfs, ci)
}


xticks = c(1,13,25,37,49,61,73,85,97,109,121,133,145,157,169,181,193,205,217,229,241,253,265,277,289,301)
xyears = c("1995","","","","","","","","","","","","2007","","","","","","","","","","","","","2020")
response.time = c(1:304)

plot(response.time, cum_irfs[,1], xlim=c(12, 293), ylim=c(-5.0, 12.5), xlab = "", ylab = "Market Return", main = "Materials", type = "l", cex.lab=1.2, cex.axis=1.2, xaxt="n")
axis(1, at=xticks, labels=xyears)
polygon(c(response.time, rev(response.time)), c(cum_irfs[,2], rev(cum_irfs[,3])), col='grey90', border=FALSE)
lines(response.time, cum_irfs[,1], lwd=2)
lines(response.time, cum_irfs[,2], col='red', lwd=2)
lines(response.time, cum_irfs[,3], col='red', lwd=2)
abline(h=0, col="blue", lwd=2, lty=2)
grid()


#y10

func = function(mylist,ind){
  
  mydf = do.call(rbind,mylist[ind])
  gkvar <- VAR(mydf[, c("y10", "x1", "c1", "c3", "c4", "c5")], p = 1, type = "const")
  shockcol <- externalinstrument(gkvar, mydf$iv1, "x1")
  
  ma_representation <- Phi(gkvar, 12)
  irfs <- t(apply(ma_representation, 3, function(x) x %*% shockcol)) #unclear
  colnames(irfs) <- names(shockcol)
  irfs
  return(irfs)
}

cum_irfs = c()
s = seq(1:304)
remove(n)
for (n in s){
  data_a = data_v[n:(96+n),]
  data.df = as.data.frame(data_a)
  data.df$year = year[n:(96+n)]
  
  irfs1 = func(split(data.df, data.df$year))*100
  res = boot(split(data.df,data.df$year),func,100)
  
  boot_est= sapply(1:ncol(res$t),function(i){
    boot.ci(res,index=i,type = "perc")$percent[4:5]
  })*100
  
  lb = matrix(boot_est[1,],ncol=ncol(res$t0))
  ub = matrix(boot_est[2,],ncol=ncol(res$t0))
  
  irfc1 = sum(irfs1[,1])
  lbc1 = sum(lb[,1])
  ubc1 = sum(ub[,1])
  ci = c(irfc1, lbc1, ubc1)
  cum_irfs = rbind(cum_irfs, ci)
}


xticks = c(1,13,25,37,49,61,73,85,97,109,121,133,145,157,169,181,193,205,217,229,241,253,265,277,289,301)
xyears = c("1995","","","","","","","","","","","","2007","","","","","","","","","","","","","2020")
response.time = c(1:304)

plot(response.time, cum_irfs[,1], xlim=c(12, 293), ylim=c(-7.8, 11.8), xlab = "", ylab = "Market Return", main = "Real Estate", type = "l", cex.lab=1.2, cex.axis=1.2, xaxt="n")
axis(1, at=xticks, labels=xyears)
polygon(c(response.time, rev(response.time)), c(cum_irfs[,2], rev(cum_irfs[,3])), col='grey90', border=FALSE)
lines(response.time, cum_irfs[,1], lwd=2)
lines(response.time, cum_irfs[,2], col='red', lwd=2)
lines(response.time, cum_irfs[,3], col='red', lwd=2)
abline(h=0, col="blue", lwd=2, lty=2)
grid()


#y11

func = function(mylist,ind){
  
  mydf = do.call(rbind,mylist[ind])
  gkvar <- VAR(mydf[, c("y11", "x1", "c1", "c3", "c4", "c5")], p = 1, type = "const")
  shockcol <- externalinstrument(gkvar, mydf$iv1, "x1")
  
  ma_representation <- Phi(gkvar, 12)
  irfs <- t(apply(ma_representation, 3, function(x) x %*% shockcol)) #unclear
  colnames(irfs) <- names(shockcol)
  irfs
  return(irfs)
}

cum_irfs = c()
s = seq(1:304)
remove(n)
for (n in s){
  data_a = data_v[n:(96+n),]
  data.df = as.data.frame(data_a)
  data.df$year = year[n:(96+n)]
  
  irfs1 = func(split(data.df, data.df$year))*100
  res = boot(split(data.df,data.df$year),func,100)
  
  boot_est= sapply(1:ncol(res$t),function(i){
    boot.ci(res,index=i,type = "perc")$percent[4:5]
  })*100
  
  lb = matrix(boot_est[1,],ncol=ncol(res$t0))
  ub = matrix(boot_est[2,],ncol=ncol(res$t0))
  
  irfc1 = sum(irfs1[,1])
  lbc1 = sum(lb[,1])
  ubc1 = sum(ub[,1])
  ci = c(irfc1, lbc1, ubc1)
  cum_irfs = rbind(cum_irfs, ci)
}


xticks = c(1,13,25,37,49,61,73,85,97,109,121,133,145,157,169,181,193,205,217,229,241,253,265,277,289,301)
xyears = c("1995","","","","","","","","","","","","2007","","","","","","","","","","","","","2020")
response.time = c(1:304)

plot(response.time, cum_irfs[,1], xlim=c(12, 293), ylim=c(-7.0, 5.0), xlab = "", ylab = "Market Return", main = "Telecommunications", type = "l", cex.lab=1.2, cex.axis=1.2, xaxt="n")
axis(1, at=xticks, labels=xyears)
polygon(c(response.time, rev(response.time)), c(cum_irfs[,2], rev(cum_irfs[,3])), col='grey90', border=FALSE)
lines(response.time, cum_irfs[,1], lwd=2)
lines(response.time, cum_irfs[,2], col='red', lwd=2)
lines(response.time, cum_irfs[,3], col='red', lwd=2)
abline(h=0, col="blue", lwd=2, lty=2)
grid()



#y12

func = function(mylist,ind){
  
  mydf = do.call(rbind,mylist[ind])
  gkvar <- VAR(mydf[, c("y12", "x1", "c1", "c3", "c4", "c5")], p = 1, type = "const")
  shockcol <- externalinstrument(gkvar, mydf$iv1, "x1")
  
  ma_representation <- Phi(gkvar, 12)
  irfs <- t(apply(ma_representation, 3, function(x) x %*% shockcol)) #unclear
  colnames(irfs) <- names(shockcol)
  irfs
  return(irfs)
}

cum_irfs = c()
s = seq(1:304)
remove(n)
for (n in s){
  data_a = data_v[n:(96+n),]
  data.df = as.data.frame(data_a)
  data.df$year = year[n:(96+n)]
  
  irfs1 = func(split(data.df, data.df$year))*100
  res = boot(split(data.df,data.df$year),func,100)
  
  boot_est= sapply(1:ncol(res$t),function(i){
    boot.ci(res,index=i,type = "perc")$percent[4:5]
  })*100
  
  lb = matrix(boot_est[1,],ncol=ncol(res$t0))
  ub = matrix(boot_est[2,],ncol=ncol(res$t0))
  
  irfc1 = sum(irfs1[,1])
  lbc1 = sum(lb[,1])
  ubc1 = sum(ub[,1])
  ci = c(irfc1, lbc1, ubc1)
  cum_irfs = rbind(cum_irfs, ci)
}


xticks = c(1,13,25,37,49,61,73,85,97,109,121,133,145,157,169,181,193,205,217,229,241,253,265,277,289,301)
xyears = c("1995","","","","","","","","","","","","2007","","","","","","","","","","","","","2020")
response.time = c(1:304)

plot(response.time, cum_irfs[,1], xlim=c(12, 293), ylim=c(-4.0, 6.0), xlab = "", ylab = "Market Return", main = "Utilities", type = "l", cex.lab=1.2, cex.axis=1.2, xaxt="n")
axis(1, at=xticks, labels=xyears)
polygon(c(response.time, rev(response.time)), c(cum_irfs[,2], rev(cum_irfs[,3])), col='grey90', border=FALSE)
lines(response.time, cum_irfs[,1], lwd=2)
lines(response.time, cum_irfs[,2], col='red', lwd=2)
lines(response.time, cum_irfs[,3], col='red', lwd=2)
abline(h=0, col="blue", lwd=2, lty=2)
grid()
