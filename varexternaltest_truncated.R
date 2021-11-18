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
data = read_excel("Prelim_Dataset.xlsx", sheet = 2)

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
# Full Sample
#-----------------------------------------------------------------------------------------------------------------------
data_v = cbind(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, x1, iv1, c1, c3, c4, c5)
data_v = data_v[2:252,]
data.df = as.data.frame(data_v)
data.df$year = (1:nrow(data.df) - 1) %/% 12
data.df$year = c(data.df$year[2:251], 20)

y1.mat = cbind(y1, x1, c1, c3, c4, c5)
y1.mat = y1.mat[2:252,]

VARselect(y1.mat, lag.max = 12, type="const")

# the range data_v[2:396,] is for 1987-2019, data_v[2:401,] to include data up to May 2020
# if data_v[2,396,], then data.df$year = c(data.df$year[2:395], 32)
# if data_v[2:401,], then data.df$year = c(data.df$year[2:400], 33)
# the range for data.df$year needs to be one less than data_v

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
irfs1 = func(split(data.df, data.df$year))*100
res = boot(split(data.df,data.df$year),func,100)

boot_est= sapply(1:ncol(res$t),function(i){
  boot.ci(res,index=i,type = "perc")$percent[4:5]
})*100

lb = matrix(boot_est[1,],ncol=ncol(res$t0))
ub = matrix(boot_est[2,],ncol=ncol(res$t0))
response.time = c(1:13)

plot(response.time, irfs1[,1], xlim=c(1.4, 12), ylim=c(-1.3, 0.1), xlab = "", ylab = "Market Return", main = "US Total Market", type = "l", cex.lab=1.2, cex.axis=1.2)
polygon(c(response.time, rev(response.time)), c(lb[,1], rev(ub[,1])), col='grey90', border=FALSE)
lines(response.time, irfs1[,1], lwd=2)
lines(response.time, lb[,1], col='red', lwd=2, lty=2)
lines(response.time, ub[,1], col='red', lwd=2, lty=2)
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
irfs2 = func(split(data.df, data.df$year))*100
res = boot(split(data.df,data.df$year),func,100)

boot_est= sapply(1:ncol(res$t),function(i){
  boot.ci(res,index=i,type = "perc")$percent[4:5]
})*100

lb = matrix(boot_est[1,],ncol=ncol(res$t0))
ub = matrix(boot_est[2,],ncol=ncol(res$t0))
response.time = c(1:13)


plot(response.time, irfs2[,1], xlim=c(1.4, 12), ylim=c(-2.0, 0.1), xlab = "", ylab = "Market Return", main = "Consumer Discretionary", type = "l", cex.lab=1.2, cex.axis=1.2)
polygon(c(response.time, rev(response.time)), c(lb[,1], rev(ub[,1])), col='grey90', border=FALSE)
lines(response.time, irfs2[,1], lwd=2)
lines(response.time, lb[,1], col='red', lwd=2, lty=2)
lines(response.time, ub[,1], col='red', lwd=2, lty=2)
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
irfs3 = func(split(data.df, data.df$year))*100
res = boot(split(data.df,data.df$year),func,100)

boot_est= sapply(1:ncol(res$t),function(i){
  boot.ci(res,index=i,type = "perc")$percent[4:5]
})*100

lb = matrix(boot_est[1,],ncol=ncol(res$t0))
ub = matrix(boot_est[2,],ncol=ncol(res$t0))
response.time = c(1:13)


plot(response.time, irfs3[,1], xlim=c(1.4, 12), ylim=c(-1.45, 0.1), xlab = "", ylab = "Market Return", main = "Consumer Stationary", type = "l", cex.lab=1.2, cex.axis=1.2)
polygon(c(response.time, rev(response.time)), c(lb[,1], rev(ub[,1])), col='grey90', border=FALSE)
lines(response.time, irfs3[,1], lwd=2)
lines(response.time, lb[,1], col='red', lwd=2, lty=2)
lines(response.time, ub[,1], col='red', lwd=2, lty=2)
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
irfs4 = func(split(data.df, data.df$year))*100
res = boot(split(data.df,data.df$year),func,100)

boot_est= sapply(1:ncol(res$t),function(i){
  boot.ci(res,index=i,type = "perc")$percent[4:5]
})*100

lb = matrix(boot_est[1,],ncol=ncol(res$t0))
ub = matrix(boot_est[2,],ncol=ncol(res$t0))
response.time = c(1:13)


plot(response.time, irfs4[,1], xlim=c(1.4, 12), ylim=c(-0.4, 2.0), xlab = "", ylab = "Market Return", main = "Energy", type = "l", cex.lab=1.2, cex.axis=1.2)
polygon(c(response.time, rev(response.time)), c(lb[,1], rev(ub[,1])), col='grey90', border=FALSE)
lines(response.time, irfs4[,1], lwd=2)
lines(response.time, lb[,1], col='red', lwd=2, lty=2)
lines(response.time, ub[,1], col='red', lwd=2, lty=2)
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
irfs5 = func(split(data.df, data.df$year))*100
res = boot(split(data.df,data.df$year),func,100)

boot_est= sapply(1:ncol(res$t),function(i){
  boot.ci(res,index=i,type = "perc")$percent[4:5]
})*100

lb = matrix(boot_est[1,],ncol=ncol(res$t0))
ub = matrix(boot_est[2,],ncol=ncol(res$t0))
response.time = c(1:13)


plot(response.time, irfs5[,1], xlim=c(1.4, 12), ylim=c(-2.2, 0.1), xlab = "", ylab = "Market Return", main = "Financials", type = "l", cex.lab=1.2, cex.axis=1.2)
polygon(c(response.time, rev(response.time)), c(lb[,1], rev(ub[,1])), col='grey90', border=FALSE)
lines(response.time, irfs5[,1], lwd=2)
lines(response.time, lb[,1], col='red', lwd=2, lty=2)
lines(response.time, ub[,1], col='red', lwd=2, lty=2)
abline(h=0, col="blue", lwd=2, lty=2)
grid()

#y6
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
irfs6 = func(split(data.df, data.df$year))*100
res = boot(split(data.df,data.df$year),func,100)

boot_est= sapply(1:ncol(res$t),function(i){
  boot.ci(res,index=i,type = "perc")$percent[4:5]
})*100

lb = matrix(boot_est[1,],ncol=ncol(res$t0))
ub = matrix(boot_est[2,],ncol=ncol(res$t0))
response.time = c(1:13)


plot(response.time, irfs6[,1], xlim=c(1.4, 12), ylim=c(-1.5, 0.1), xlab = "", ylab = "Market Return", main = "Healthcare", type = "l", cex.lab=1.2, cex.axis=1.2)
polygon(c(response.time, rev(response.time)), c(lb[,1], rev(ub[,1])), col='grey90', border=FALSE)
lines(response.time, irfs6[,1], lwd=2)
lines(response.time, lb[,1], col='red', lwd=2, lty=2)
lines(response.time, ub[,1], col='red', lwd=2, lty=2)
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
irfs7 = func(split(data.df, data.df$year))*100
res = boot(split(data.df,data.df$year),func,100)

boot_est= sapply(1:ncol(res$t),function(i){
  boot.ci(res,index=i,type = "perc")$percent[4:5]
})*100

lb = matrix(boot_est[1,],ncol=ncol(res$t0))
ub = matrix(boot_est[2,],ncol=ncol(res$t0))
response.time = c(1:13)


plot(response.time, irfs7[,1], xlim=c(1.4, 12), ylim=c(-1.35, 0.1), xlab = "", ylab = "Market Return", main = "Industrials", type = "l", cex.lab=1.2, cex.axis=1.2)
polygon(c(response.time, rev(response.time)), c(lb[,1], rev(ub[,1])), col='grey90', border=FALSE)
lines(response.time, irfs7[,1], lwd=2)
lines(response.time, lb[,1], col='red', lwd=2, lty=2)
lines(response.time, ub[,1], col='red', lwd=2, lty=2)
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
irfs8 = func(split(data.df, data.df$year))*100
res = boot(split(data.df,data.df$year),func,100)

boot_est= sapply(1:ncol(res$t),function(i){
  boot.ci(res,index=i,type = "perc")$percent[4:5]
})*100

lb = matrix(boot_est[1,],ncol=ncol(res$t0))
ub = matrix(boot_est[2,],ncol=ncol(res$t0))
response.time = c(1:13)


plot(response.time, irfs8[,1], xlim=c(1.4, 12), ylim=c(-2.3, 0.1), xlab = "", ylab = "Market Return", main = "Information Technology", type = "l", cex.lab=1.2, cex.axis=1.2)
polygon(c(response.time, rev(response.time)), c(lb[,1], rev(ub[,1])), col='grey90', border=FALSE)
lines(response.time, irfs8[,1], lwd=2)
lines(response.time, lb[,1], col='red', lwd=2, lty=2)
lines(response.time, ub[,1], col='red', lwd=2, lty=2)
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
irfs9 = func(split(data.df, data.df$year))*100
res = boot(split(data.df,data.df$year),func,100)

boot_est= sapply(1:ncol(res$t),function(i){
  boot.ci(res,index=i,type = "perc")$percent[4:5]
})*100

lb = matrix(boot_est[1,],ncol=ncol(res$t0))
ub = matrix(boot_est[2,],ncol=ncol(res$t0))
response.time = c(1:13)


plot(response.time, irfs9[,1], xlim=c(1.4, 12), ylim=c(-1.25, 0.2), xlab = "", ylab = "Market Return", main = "Materials", type = "l", cex.lab=1.2, cex.axis=1.2)
polygon(c(response.time, rev(response.time)), c(lb[,1], rev(ub[,1])), col='grey90', border=FALSE)
lines(response.time, irfs9[,1], lwd=2)
lines(response.time, lb[,1], col='red', lwd=2, lty=2)
lines(response.time, ub[,1], col='red', lwd=2, lty=2)
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
irfs10 = func(split(data.df, data.df$year))*100
res = boot(split(data.df,data.df$year),func,100)

boot_est= sapply(1:ncol(res$t),function(i){
  boot.ci(res,index=i,type = "perc")$percent[4:5]
})*100

lb = matrix(boot_est[1,],ncol=ncol(res$t0))
ub = matrix(boot_est[2,],ncol=ncol(res$t0))
response.time = c(1:13)


plot(response.time, irfs10[,1], xlim=c(1.4, 12), ylim=c(-1.7, 0.1), xlab = "", ylab = "Market Return", main = "Real Estate", type = "l", cex.lab=1.2, cex.axis=1.2)
polygon(c(response.time, rev(response.time)), c(lb[,1], rev(ub[,1])), col='grey90', border=FALSE)
lines(response.time, irfs10[,1], lwd=2)
lines(response.time, lb[,1], col='red', lwd=2, lty=2)
lines(response.time, ub[,1], col='red', lwd=2, lty=2)
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
irfs11 = func(split(data.df, data.df$year))*100
res = boot(split(data.df,data.df$year),func,100)

boot_est= sapply(1:ncol(res$t),function(i){
  boot.ci(res,index=i,type = "perc")$percent[4:5]
})*100

lb = matrix(boot_est[1,],ncol=ncol(res$t0))
ub = matrix(boot_est[2,],ncol=ncol(res$t0))
response.time = c(1:13)


plot(response.time, irfs11[,1], xlim=c(1.4, 12), ylim=c(-1.55, 0.1), xlab = "", ylab = "Market Return", main = "Telecommunications", type = "l", cex.lab=1.2, cex.axis=1.2)
polygon(c(response.time, rev(response.time)), c(lb[,1], rev(ub[,1])), col='grey90', border=FALSE)
lines(response.time, irfs11[,1], lwd=2)
lines(response.time, lb[,1], col='red', lwd=2, lty=2)
lines(response.time, ub[,1], col='red', lwd=2, lty=2)
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
irfs12 = func(split(data.df, data.df$year))*100
res = boot(split(data.df,data.df$year),func,100)

boot_est= sapply(1:ncol(res$t),function(i){
  boot.ci(res,index=i,type = "perc")$percent[4:5]
})*100

lb = matrix(boot_est[1,],ncol=ncol(res$t0))
ub = matrix(boot_est[2,],ncol=ncol(res$t0))
response.time = c(1:13)


plot(response.time, irfs12[,1], xlim=c(1.4, 12), ylim=c(-0.65, 1.05), xlab = "", ylab = "Market Return", main = "Utilities", type = "l", cex.lab=1.2, cex.axis=1.2)
polygon(c(response.time, rev(response.time)), c(lb[,1], rev(ub[,1])), col='grey90', border=FALSE)
lines(response.time, irfs12[,1], lwd=2)
lines(response.time, lb[,1], col='red', lwd=2, lty=2)
lines(response.time, ub[,1], col='red', lwd=2, lty=2)
abline(h=0, col="blue", lwd=2, lty=2)
grid()

irfs_all = cbind(irfs1[,1], irfs2[,1], irfs3[,1], irfs4[,1], irfs5[,1], irfs6[,1], irfs7[,1], irfs8[,1], irfs9[,1], irfs10[,1], irfs11[,1], irfs12[,1])
irf.df = as.data.frame(irfs_all)
stargazer(irf.df, summary=FALSE)
#----
# First 6 years (48 months - first month) 
#-----------------------------------------------------------------------------------------------------------------------
data_v = cbind(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, x1, iv1, c1, c3, c4, c5)
data_v = data_v[2:72,]
data.df = as.data.frame(data_v)
data.df$year = (1:nrow(data.df) - 1) %/% 12
data.df$year = c(data.df$year[2:71], 5)

#y1

func = function(mylist,ind){
  
  mydf = do.call(rbind,mylist[ind])
  gkvar <- VAR(mydf[, c("y1", "x1", "c1", "c3", "c4", "c5")], p = 3, type = "const")
  shockcol <- externalinstrument(gkvar, mydf$iv1, "x1")
  
  ma_representation <- Phi(gkvar, 12)
  irfs <- t(apply(ma_representation, 3, function(x) x %*% shockcol)) #unclear
  colnames(irfs) <- names(shockcol)
  irfs
  return(irfs)
}
irfs = func(split(data.df, data.df$year))*100
res = boot(split(data.df,data.df$year),func,100)

boot_est= sapply(1:ncol(res$t),function(i){
  boot.ci(res,index=i,type = "perc")$percent[4:5]
})*100

lb = matrix(boot_est[1,],ncol=ncol(res$t0))
ub = matrix(boot_est[2,],ncol=ncol(res$t0))
response.time = c(1:13)

plot(response.time, irfs[,1], xlim=c(1.4, 12), ylim=c(-0.023, 0.014), xlab = "", ylab = "Response", main = "Orthogonal Impulse Response of US Total Market to Oil Price Shock", type = "l")
polygon(c(response.time, rev(response.time)), c(lb[,1], rev(ub[,1])), col='grey90', border=FALSE)
lines(response.time, irfs[,1], lwd=2)
lines(response.time, lb[,1], col='red', lwd=2, lty=2)
lines(response.time, ub[,1], col='red', lwd=2, lty=2)
abline(h=0, col="blue", lwd=2, lty=2)
grid()

#y2
func = function(mylist,ind){
  
  mydf = do.call(rbind,mylist[ind])
  gkvar <- VAR(mydf[, c("y2", "x1", "c1", "c3", "c4", "c5")], p = 3, type = "const")
  shockcol <- externalinstrument(gkvar, mydf$iv1, "x1")
  
  ma_representation <- Phi(gkvar, 12)
  irfs <- t(apply(ma_representation, 3, function(x) x %*% shockcol)) #unclear
  colnames(irfs) <- names(shockcol)
  irfs
  return(irfs)
}
irfs = func(split(data.df, data.df$year))*100
res = boot(split(data.df,data.df$year),func,100)

boot_est= sapply(1:ncol(res$t),function(i){
  boot.ci(res,index=i,type = "perc")$percent[4:5]
})*100

lb = matrix(boot_est[1,],ncol=ncol(res$t0))
ub = matrix(boot_est[2,],ncol=ncol(res$t0))
response.time = c(1:13)


plot(response.time, irfs[,1], xlim=c(1.4, 12), ylim=c(-0.032, 0.014), xlab = "", ylab = "Response", main = "Orthogonal Impulse Response of Consumer Discretionary Sector to Oil Price Shock", type = "l")
polygon(c(response.time, rev(response.time)), c(lb[,1], rev(ub[,1])), col='grey90', border=FALSE)
lines(response.time, irfs[,1], lwd=2)
lines(response.time, lb[,1], col='red', lwd=2, lty=2)
lines(response.time, ub[,1], col='red', lwd=2, lty=2)
abline(h=0, col="blue", lwd=2, lty=2)
grid()

#y3
func = function(mylist,ind){
  
  mydf = do.call(rbind,mylist[ind])
  gkvar <- VAR(mydf[, c("y3", "x1", "c1", "c3", "c4", "c5")], p = 3, type = "const")
  shockcol <- externalinstrument(gkvar, mydf$iv1, "x1")
  
  ma_representation <- Phi(gkvar, 12)
  irfs <- t(apply(ma_representation, 3, function(x) x %*% shockcol)) #unclear
  colnames(irfs) <- names(shockcol)
  irfs
  return(irfs)
}
irfs = func(split(data.df, data.df$year))*100
res = boot(split(data.df,data.df$year),func,100)

boot_est= sapply(1:ncol(res$t),function(i){
  boot.ci(res,index=i,type = "perc")$percent[4:5]
})*100

lb = matrix(boot_est[1,],ncol=ncol(res$t0))
ub = matrix(boot_est[2,],ncol=ncol(res$t0))
response.time = c(1:13)


plot(response.time, irfs[,1], xlim=c(1.4, 12), ylim=c(-0.023, 0.01), xlab = "", ylab = "Response", main = "Orthogonal Impulse Response of Consumer Stationary Sector to Oil Price Shock", type = "l")
polygon(c(response.time, rev(response.time)), c(lb[,1], rev(ub[,1])), col='grey90', border=FALSE)
lines(response.time, irfs[,1], lwd=2)
lines(response.time, lb[,1], col='red', lwd=2, lty=2)
lines(response.time, ub[,1], col='red', lwd=2, lty=2)
abline(h=0, col="blue", lwd=2, lty=2)
grid()

#y4
func = function(mylist,ind){
  
  mydf = do.call(rbind,mylist[ind])
  gkvar <- VAR(mydf[, c("y4", "x1", "c1", "c3", "c4", "c5")], p = 3, type = "const")
  shockcol <- externalinstrument(gkvar, mydf$iv1, "x1")
  
  ma_representation <- Phi(gkvar, 12)
  irfs <- t(apply(ma_representation, 3, function(x) x %*% shockcol)) #unclear
  colnames(irfs) <- names(shockcol)
  irfs
  return(irfs)
}
irfs = func(split(data.df, data.df$year))*100
res = boot(split(data.df,data.df$year),func,100)

boot_est= sapply(1:ncol(res$t),function(i){
  boot.ci(res,index=i,type = "perc")$percent[4:5]
})*100

lb = matrix(boot_est[1,],ncol=ncol(res$t0))
ub = matrix(boot_est[2,],ncol=ncol(res$t0))
response.time = c(1:13)


plot(response.time, irfs[,1], xlim=c(1.4, 12), ylim=c(-0.01, 0.023), xlab = "", ylab = "Response", main = "Orthogonal Impulse Response of Energy Sector to Oil Price Shock", type = "l")
polygon(c(response.time, rev(response.time)), c(lb[,1], rev(ub[,1])), col='grey90', border=FALSE)
lines(response.time, irfs[,1], lwd=2)
lines(response.time, lb[,1], col='red', lwd=2, lty=2)
lines(response.time, ub[,1], col='red', lwd=2, lty=2)
abline(h=0, col="blue", lwd=2, lty=2)
grid()

#y5
func = function(mylist,ind){
  
  mydf = do.call(rbind,mylist[ind])
  gkvar <- VAR(mydf[, c("y5", "x1", "c1", "c3", "c4", "c5")], p = 3, type = "const")
  shockcol <- externalinstrument(gkvar, mydf$iv1, "x1")
  
  ma_representation <- Phi(gkvar, 12)
  irfs <- t(apply(ma_representation, 3, function(x) x %*% shockcol)) #unclear
  colnames(irfs) <- names(shockcol)
  irfs
  return(irfs)
}
irfs = func(split(data.df, data.df$year))*100
res = boot(split(data.df,data.df$year),func,100)

boot_est= sapply(1:ncol(res$t),function(i){
  boot.ci(res,index=i,type = "perc")$percent[4:5]
})*100

lb = matrix(boot_est[1,],ncol=ncol(res$t0))
ub = matrix(boot_est[2,],ncol=ncol(res$t0))
response.time = c(1:13)


plot(response.time, irfs[,1], xlim=c(1.4, 12), ylim=c(-0.031, 0.015), xlab = "", ylab = "Response", main = "Orthogonal Impulse Response of Financial Sector to Oil Price Shock", type = "l")
polygon(c(response.time, rev(response.time)), c(lb[,1], rev(ub[,1])), col='grey90', border=FALSE)
lines(response.time, irfs[,1], lwd=2)
lines(response.time, lb[,1], col='red', lwd=2, lty=2)
lines(response.time, ub[,1], col='red', lwd=2, lty=2)
abline(h=0, col="blue", lwd=2, lty=2)
grid()

#y6
func = function(mylist,ind){
  
  mydf = do.call(rbind,mylist[ind])
  gkvar <- VAR(mydf[, c("y6", "x1", "c1", "c3", "c4", "c5")], p = 3, type = "const")
  shockcol <- externalinstrument(gkvar, mydf$iv1, "x1")
  
  ma_representation <- Phi(gkvar, 12)
  irfs <- t(apply(ma_representation, 3, function(x) x %*% shockcol)) #unclear
  colnames(irfs) <- names(shockcol)
  irfs
  return(irfs)
}
irfs = func(split(data.df, data.df$year))*100
res = boot(split(data.df,data.df$year),func,100)

boot_est= sapply(1:ncol(res$t),function(i){
  boot.ci(res,index=i,type = "perc")$percent[4:5]
})*100

lb = matrix(boot_est[1,],ncol=ncol(res$t0))
ub = matrix(boot_est[2,],ncol=ncol(res$t0))
response.time = c(1:13)


plot(response.time, irfs[,1], xlim=c(1.4, 12), ylim=c(-0.024, 0.012), xlab = "", ylab = "Response", main = "Orthogonal Impulse Response of Healthcare Sector to Oil Price Shock", type = "l")
polygon(c(response.time, rev(response.time)), c(lb[,1], rev(ub[,1])), col='grey90', border=FALSE)
lines(response.time, irfs[,1], lwd=2)
lines(response.time, lb[,1], col='red', lwd=2, lty=2)
lines(response.time, ub[,1], col='red', lwd=2, lty=2)
abline(h=0, col="blue", lwd=2, lty=2)
grid()

#y7
func = function(mylist,ind){
  
  mydf = do.call(rbind,mylist[ind])
  gkvar <- VAR(mydf[, c("y7", "x1", "c1", "c3", "c4", "c5")], p = 3, type = "const")
  shockcol <- externalinstrument(gkvar, mydf$iv1, "x1")
  
  ma_representation <- Phi(gkvar, 12)
  irfs <- t(apply(ma_representation, 3, function(x) x %*% shockcol)) #unclear
  colnames(irfs) <- names(shockcol)
  irfs
  return(irfs)
}
irfs = func(split(data.df, data.df$year))*100
res = boot(split(data.df,data.df$year),func,100)

boot_est= sapply(1:ncol(res$t),function(i){
  boot.ci(res,index=i,type = "perc")$percent[4:5]
})*100

lb = matrix(boot_est[1,],ncol=ncol(res$t0))
ub = matrix(boot_est[2,],ncol=ncol(res$t0))
response.time = c(1:13)


plot(response.time, irfs[,1], xlim=c(1.4, 12), ylim=c(-0.027, 0.011), xlab = "", ylab = "Response", main = "Orthogonal Impulse Response of Industrial Sector to Oil Price Shock", type = "l")
polygon(c(response.time, rev(response.time)), c(lb[,1], rev(ub[,1])), col='grey90', border=FALSE)
lines(response.time, irfs[,1], lwd=2)
lines(response.time, lb[,1], col='red', lwd=2, lty=2)
lines(response.time, ub[,1], col='red', lwd=2, lty=2)
abline(h=0, col="blue", lwd=2, lty=2)
grid()

#y8
func = function(mylist,ind){
  
  mydf = do.call(rbind,mylist[ind])
  gkvar <- VAR(mydf[, c("y8", "x1", "c1", "c3", "c4", "c5")], p = 3, type = "const")
  shockcol <- externalinstrument(gkvar, mydf$iv1, "x1")
  
  ma_representation <- Phi(gkvar, 12)
  irfs <- t(apply(ma_representation, 3, function(x) x %*% shockcol)) #unclear
  colnames(irfs) <- names(shockcol)
  irfs
  return(irfs)
}
irfs = func(split(data.df, data.df$year))*100
res = boot(split(data.df,data.df$year),func,100)

boot_est= sapply(1:ncol(res$t),function(i){
  boot.ci(res,index=i,type = "perc")$percent[4:5]
})*100

lb = matrix(boot_est[1,],ncol=ncol(res$t0))
ub = matrix(boot_est[2,],ncol=ncol(res$t0))
response.time = c(1:13)


plot(response.time, irfs[,1], xlim=c(1.4, 12), ylim=c(-0.039, 0.014), xlab = "", ylab = "Response", main = "Orthogonal Impulse Response of Information Technology Sector to Oil Price Shock", type = "l")
polygon(c(response.time, rev(response.time)), c(lb[,1], rev(ub[,1])), col='grey90', border=FALSE)
lines(response.time, irfs[,1], lwd=2)
lines(response.time, lb[,1], col='red', lwd=2, lty=2)
lines(response.time, ub[,1], col='red', lwd=2, lty=2)
abline(h=0, col="blue", lwd=2, lty=2)
grid()

#y9
func = function(mylist,ind){
  
  mydf = do.call(rbind,mylist[ind])
  gkvar <- VAR(mydf[, c("y9", "x1", "c1", "c3", "c4", "c5")], p = 3, type = "const")
  shockcol <- externalinstrument(gkvar, mydf$iv1, "x1")
  
  ma_representation <- Phi(gkvar, 12)
  irfs <- t(apply(ma_representation, 3, function(x) x %*% shockcol)) #unclear
  colnames(irfs) <- names(shockcol)
  irfs
  return(irfs)
}
irfs = func(split(data.df, data.df$year))*100
res = boot(split(data.df,data.df$year),func,100)

boot_est= sapply(1:ncol(res$t),function(i){
  boot.ci(res,index=i,type = "perc")$percent[4:5]
})*100

lb = matrix(boot_est[1,],ncol=ncol(res$t0))
ub = matrix(boot_est[2,],ncol=ncol(res$t0))
response.time = c(1:13)


plot(response.time, irfs[,1], xlim=c(1.4, 12), ylim=c(-0.027, 0.01), xlab = "", ylab = "Response", main = "Orthogonal Impulse Response of Materials Sector to Oil Price Shock", type = "l")
polygon(c(response.time, rev(response.time)), c(lb[,1], rev(ub[,1])), col='grey90', border=FALSE)
lines(response.time, irfs[,1], lwd=2)
lines(response.time, lb[,1], col='red', lwd=2, lty=2)
lines(response.time, ub[,1], col='red', lwd=2, lty=2)
abline(h=0, col="blue", lwd=2, lty=2)
grid()

#y10
func = function(mylist,ind){
  
  mydf = do.call(rbind,mylist[ind])
  gkvar <- VAR(mydf[, c("y10", "x1", "c1", "c3", "c4", "c5")], p = 3, type = "const")
  shockcol <- externalinstrument(gkvar, mydf$iv1, "x1")
  
  ma_representation <- Phi(gkvar, 12)
  irfs <- t(apply(ma_representation, 3, function(x) x %*% shockcol)) #unclear
  colnames(irfs) <- names(shockcol)
  irfs
  return(irfs)
}
irfs = func(split(data.df, data.df$year))*100
res = boot(split(data.df,data.df$year),func,100)

boot_est= sapply(1:ncol(res$t),function(i){
  boot.ci(res,index=i,type = "perc")$percent[4:5]
})*100

lb = matrix(boot_est[1,],ncol=ncol(res$t0))
ub = matrix(boot_est[2,],ncol=ncol(res$t0))
response.time = c(1:13)


plot(response.time, irfs[,1], xlim=c(1.4, 12), ylim=c(-0.040, 0.01), xlab = "", ylab = "Response", main = "Orthogonal Impulse Response of Real Estate Sector to Oil Price Shock", type = "l")
polygon(c(response.time, rev(response.time)), c(lb[,1], rev(ub[,1])), col='grey90', border=FALSE)
lines(response.time, irfs[,1], lwd=2)
lines(response.time, lb[,1], col='red', lwd=2, lty=2)
lines(response.time, ub[,1], col='red', lwd=2, lty=2)
abline(h=0, col="blue", lwd=2, lty=2)
grid()

#y11
func = function(mylist,ind){
  
  mydf = do.call(rbind,mylist[ind])
  gkvar <- VAR(mydf[, c("y11", "x1", "c1", "c3", "c4", "c5")], p = 3, type = "const")
  shockcol <- externalinstrument(gkvar, mydf$iv1, "x1")
  
  ma_representation <- Phi(gkvar, 12)
  irfs <- t(apply(ma_representation, 3, function(x) x %*% shockcol)) #unclear
  colnames(irfs) <- names(shockcol)
  irfs
  return(irfs)
}
irfs = func(split(data.df, data.df$year))*100
res = boot(split(data.df,data.df$year),func,100)

boot_est= sapply(1:ncol(res$t),function(i){
  boot.ci(res,index=i,type = "perc")$percent[4:5]
})*100

lb = matrix(boot_est[1,],ncol=ncol(res$t0))
ub = matrix(boot_est[2,],ncol=ncol(res$t0))
response.time = c(1:13)


plot(response.time, irfs[,1], xlim=c(1.4, 12), ylim=c(-0.013, 0.008), xlab = "", ylab = "Response", main = "Orthogonal Impulse Response of Telecommunications Sector to Oil Price Shock", type = "l")
polygon(c(response.time, rev(response.time)), c(lb[,1], rev(ub[,1])), col='grey90', border=FALSE)
lines(response.time, irfs[,1], lwd=2)
lines(response.time, lb[,1], col='red', lwd=2, lty=2)
lines(response.time, ub[,1], col='red', lwd=2, lty=2)
abline(h=0, col="blue", lwd=2, lty=2)
grid()

#y12
func = function(mylist,ind){
  
  mydf = do.call(rbind,mylist[ind])
  gkvar <- VAR(mydf[, c("y12", "x1", "c1", "c3", "c4", "c5")], p = 3, type = "const")
  shockcol <- externalinstrument(gkvar, mydf$iv1, "x1")
  
  ma_representation <- Phi(gkvar, 12)
  irfs <- t(apply(ma_representation, 3, function(x) x %*% shockcol)) #unclear
  colnames(irfs) <- names(shockcol)
  irfs
  return(irfs)
}
irfs = func(split(data.df, data.df$year))*100
res = boot(split(data.df,data.df$year),func,100)

boot_est= sapply(1:ncol(res$t),function(i){
  boot.ci(res,index=i,type = "perc")$percent[4:5]
})*100

lb = matrix(boot_est[1,],ncol=ncol(res$t0))
ub = matrix(boot_est[2,],ncol=ncol(res$t0))
response.time = c(1:13)


plot(response.time, irfs[,1], xlim=c(1.4, 12), ylim=c(-0.006, 0.0055), xlab = "", ylab = "Response", main = "Orthogonal Impulse Response of Utilities Sector to Oil Price Shock", type = "l")
polygon(c(response.time, rev(response.time)), c(lb[,1], rev(ub[,1])), col='grey90', border=FALSE)
lines(response.time, irfs[,1], lwd=2)
lines(response.time, lb[,1], col='red', lwd=2, lty=2)
lines(response.time, ub[,1], col='red', lwd=2, lty=2)
abline(h=0, col="blue", lwd=2, lty=2)
grid()