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

y1 = ts(c(NA, diff(log(COMP))), start=1987, frequency=12) #y1 = COMP return
y2 = ts(c(NA, diff(log(COD))), start=1987, frequency=12) #y2 = COD return
y3 = ts(c(NA, diff(log(CST))), start=1987, frequency=12) #y3 = CST return
y4 = ts(c(NA, diff(log(ENE))), start=1987, frequency=12) #y4 = ENE return
y5 = ts(c(NA, diff(log(FIN))), start=1987, frequency=12) #y5 = FIN return
y6 = ts(c(NA, diff(log(HCR))), start=1987, frequency=12) #y6 = HCR return
y7 = ts(c(NA, diff(log(IND))), start=1987, frequency=12) #y7 = IND return
y8 = ts(c(NA, diff(log(INT))), start=1987, frequency=12) #y8 = INT return
y9 = ts(c(NA, diff(log(MAT))), start=1987, frequency=12) #y9 = MAT return
y10 = ts(c(NA, diff(log(RES))), start=1987, frequency=12) #y10 = RES return
y11 = ts(c(NA, diff(log(TEL))), start=1987, frequency=12) #y11 = TEL return
y12 = ts(c(NA, diff(log(UTL))), start=1987, frequency=12) #y12 = UTL return
x1 = ts(c(NA, diff(log(WTI))), start=1987, frequency=12) #x1 = WTI return
iv1 = ts(c(NA, diff(log(HTO))), start=1987, frequency=12) #iv1 = HTO return
c1 = ts(c(NA, diff(log(IP))), start=1987, frequency=12) #c1 = IP growth rate
c2 = ts(c(NA, diff(IR)), start=1987, frequency=12) #c2 = IR monthly change
c3 = ts(c(NA, diff(log(CPI))), start=1987, frequency=12) #c3 = Inflation Rate
c4 = ts(c(NA, diff(log(WMKT))), start=1987, frequency=12) #c4 = WMKT return
c5 = ts(c(NA, diff(log(TEN))), start=1987, frequency=12) #c5 = TEN monthly change

variables.df = data.frame("y1", 'y2', 'y3', 'y4', 'y5', 'y6', 'y7', 'y8', 'y9', 'y10', 'y11', 'y12', 'x1', 'iv1', 'c1', 'c3', 'c4', 'c5')


# y1
tsmat = window(ts.union(y1, x1, c1, c3, c4, c5), start = c(1987, 2), frequency = 12)

VARselect(tsmat, lag.max = 12, type="const")
var1 = VAR(y = tsmat, p=1)
amat = diag(6)
diag(amat) = NA
svar1 = SVAR(var1, estmethod="direct", Amat = amat)
irfs = irf(svar1, n.ahead = 12, cumulative = FALSE, ci = 0.95, boot=TRUE, runs=100)
irfy = irfs$irf[1]
irfy.df = as.data.frame(irfy)
sel = irfs$Lower[1]
sel.df = as.data.frame(sel)
seu = irfs$Upper[1]
seu.df = as.data.frame(seu)
irfs1 = irfy.df[2]
irfs1 = irfs1[2:13,]*100
lb = sel.df[2]
lb = lb[2:13,]*100
ub = seu.df[2]
ub = ub[2:13,]*100
response.time = c(1:12)


plot(response.time, irfs1, xlim=c(1.4, 11.6), ylim=c(-1.95, 1.48), xlab = "", ylab = "Market Return", main = "US Total Market", type = "l", cex.lab=1.2, cex.axis=1.2)
polygon(c(response.time, rev(response.time)), c(lb, rev(ub)), col='grey90', border=FALSE)
lines(response.time, irfs1, lwd=2)
lines(response.time, lb, col='red', lwd=2, lty=2)
lines(response.time, ub, col='red', lwd=2, lty=2)
abline(h=0, col="blue", lwd=2, lty=2)
grid()

# y2
tsmat = window(ts.union(y2, x1, c1, c3, c4, c5), start = c(1987, 2), frequency = 12)

var1 = VAR(y = tsmat, p=1)
amat = diag(6)
diag(amat) = NA
svar1 = SVAR(var1, estmethod="direct", Amat = amat)
irfs = irf(svar1, n.ahead = 12, cumulative = FALSE, ci = 0.95, boot=TRUE, runs=100)
irfy = irfs$irf[1]
irfy.df = as.data.frame(irfy)
sel = irfs$Lower[1]
sel.df = as.data.frame(sel)
seu = irfs$Upper[1]
seu.df = as.data.frame(seu)
irfs2 = irfy.df[2]
irfs2 = irfs2[2:13,]*100
lb = sel.df[2]
lb = lb[2:13,]*100
ub = seu.df[2]
ub = ub[2:13,]*100
response.time = c(1:12)


plot(response.time, irfs2, xlim=c(1.4, 11.6), ylim=c(-1.3, 0.5), xlab = "", ylab = "Market Return", main = "Consumer Discretionary", type = "l", cex.lab=1.2, cex.axis=1.2)
polygon(c(response.time, rev(response.time)), c(lb, rev(ub)), col='grey90', border=FALSE)
lines(response.time, irfs2, lwd=2)
lines(response.time, lb, col='red', lwd=2, lty=2)
lines(response.time, ub, col='red', lwd=2, lty=2)
abline(h=0, col="blue", lwd=2, lty=2)
grid()


# y3
tsmat = window(ts.union(y3, x1, c1, c3, c4, c5), start = c(1987, 2), frequency = 12)

var1 = VAR(y = tsmat, p=1)
amat = diag(6)
diag(amat) = NA
svar1 = SVAR(var1, estmethod="direct", Amat = amat)
irfs = irf(svar1, n.ahead = 12, cumulative = FALSE, ci = 0.95, boot=TRUE, runs=100)
irfy = irfs$irf[1]
irfy.df = as.data.frame(irfy)
sel = irfs$Lower[1]
sel.df = as.data.frame(sel)
seu = irfs$Upper[1]
seu.df = as.data.frame(seu)
irfs3 = irfy.df[2]
irfs3 = irfs3[2:13,]*100
lb = sel.df[2]
lb = lb[2:13,]*100
ub = seu.df[2]
ub = ub[2:13,]*100
response.time = c(1:12)


plot(response.time, irfs3, xlim=c(1.4, 11.6), ylim=c(-1.7, 0.85), xlab = "", ylab = "Market Return", main = "Consumer Stationary", type = "l", cex.lab=1.2, cex.axis=1.2)
polygon(c(response.time, rev(response.time)), c(lb, rev(ub)), col='grey90', border=FALSE)
lines(response.time, irfs3, lwd=2)
lines(response.time, lb, col='red', lwd=2, lty=2)
lines(response.time, ub, col='red', lwd=2, lty=2)
abline(h=0, col="blue", lwd=2, lty=2)
grid()

# y4
tsmat = window(ts.union(y4, x1, c1, c3, c4, c5), start = c(1987, 2), frequency = 12)

var1 = VAR(y = tsmat, p=1)
amat = diag(6)
diag(amat) = NA
svar1 = SVAR(var1, estmethod="direct", Amat = amat)
irfs = irf(svar1, n.ahead = 12, cumulative = FALSE, ci = 0.95, boot=TRUE, runs=100)
irfy = irfs$irf[1]
irfy.df = as.data.frame(irfy)
sel = irfs$Lower[1]
sel.df = as.data.frame(sel)
seu = irfs$Upper[1]
seu.df = as.data.frame(seu)
irfs4 = irfy.df[2]
irfs4 = irfs4[2:13,]*100
lb = sel.df[2]
lb = lb[2:13,]*100
ub = seu.df[2]
ub = ub[2:13,]*100
response.time = c(1:12)


plot(response.time, irfs4, xlim=c(1.4, 11.6), ylim=c(-0.25, 3.6), xlab = "", ylab = "Market Return", main = "Energy", type = "l", cex.lab=1.2, cex.axis=1.2)
polygon(c(response.time, rev(response.time)), c(lb, rev(ub)), col='grey90', border=FALSE)
lines(response.time, irfs4, lwd=2)
lines(response.time, lb, col='red', lwd=2, lty=2)
lines(response.time, ub, col='red', lwd=2, lty=2)
abline(h=0, col="blue", lwd=2, lty=2)
grid()

# y5
tsmat = window(ts.union(y5, x1, c1, c3, c4, c5), start = c(1987, 2), frequency = 12)

var1 = VAR(y = tsmat, p=1)
amat = diag(6)
diag(amat) = NA
svar1 = SVAR(var1, estmethod="direct", Amat = amat)
irfs = irf(svar1, n.ahead = 12, cumulative = FALSE, ci = 0.95, boot=TRUE, runs=100)
irfy = irfs$irf[1]
irfy.df = as.data.frame(irfy)
sel = irfs$Lower[1]
sel.df = as.data.frame(sel)
seu = irfs$Upper[1]
seu.df = as.data.frame(seu)
irfs5 = irfy.df[2]
irfs5 = irfs5[2:13,]*100
lb = sel.df[2]
lb = lb[2:13,]*100
ub = seu.df[2]
ub = ub[2:13,]*100
response.time = c(1:12)


plot(response.time, irfs5, xlim=c(1.4, 11.6), ylim=c(-0.95, 1.05), xlab = "", ylab = "Market Return", main = "Financials", type = "l", cex.lab=1.2, cex.axis=1.2)
polygon(c(response.time, rev(response.time)), c(lb, rev(ub)), col='grey90', border=FALSE)
lines(response.time, irfs5, lwd=2)
lines(response.time, lb, col='red', lwd=2, lty=2)
lines(response.time, ub, col='red', lwd=2, lty=2)
abline(h=0, col="blue", lwd=2, lty=2)
grid()

# y6
tsmat = window(ts.union(y6, x1, c1, c3, c4, c5), start = c(1987, 2), frequency = 12)

var1 = VAR(y = tsmat, p=1)
amat = diag(6)
diag(amat) = NA
svar1 = SVAR(var1, estmethod="direct", Amat = amat)
irfs = irf(svar1, n.ahead = 12, cumulative = FALSE, ci = 0.95, boot=TRUE, runs=100)
irfy = irfs$irf[1]
irfy.df = as.data.frame(irfy)
sel = irfs$Lower[1]
sel.df = as.data.frame(sel)
seu = irfs$Upper[1]
seu.df = as.data.frame(seu)
irfs6 = irfy.df[2]
irfs6 = irfs6[2:13,]*100
lb = sel.df[2]
lb = lb[2:13,]*100
ub = seu.df[2]
ub = ub[2:13,]*100
response.time = c(1:12)


plot(response.time, irfs6, xlim=c(1.4, 11.6), ylim=c(-0.75, 1.38), xlab = "", ylab = "Market Return", main = "Healthcare", type = "l", cex.lab=1.2, cex.axis=1.2)
polygon(c(response.time, rev(response.time)), c(lb, rev(ub)), col='grey90', border=FALSE)
lines(response.time, irfs6, lwd=2)
lines(response.time, lb, col='red', lwd=2, lty=2)
lines(response.time, ub, col='red', lwd=2, lty=2)
abline(h=0, col="blue", lwd=2, lty=2)
grid()

# y7
tsmat = window(ts.union(y7, x1, c1, c3, c4, c5), start = c(1987, 2), frequency = 12)

var1 = VAR(y = tsmat, p=1)
amat = diag(6)
diag(amat) = NA
svar1 = SVAR(var1, estmethod="direct", Amat = amat)
irfs = irf(svar1, n.ahead = 12, cumulative = FALSE, ci = 0.95, boot=TRUE, runs=100)
irfy = irfs$irf[1]
irfy.df = as.data.frame(irfy)
sel = irfs$Lower[1]
sel.df = as.data.frame(sel)
seu = irfs$Upper[1]
seu.df = as.data.frame(seu)
irfs7 = irfy.df[2]
irfs7 = irfs7[2:13,]*100
lb = sel.df[2]
lb = lb[2:13,]*100
ub = seu.df[2]
ub = ub[2:13,]*100
response.time = c(1:12)


plot(response.time, irfs7, xlim=c(1.4, 11.6), ylim=c(-0.85, 1.153), xlab = "", ylab = "Market Return", main = "Industrials", type = "l", cex.lab=1.2, cex.axis=1.2)
polygon(c(response.time, rev(response.time)), c(lb, rev(ub)), col='grey90', border=FALSE)
lines(response.time, irfs7, lwd=2)
lines(response.time, lb, col='red', lwd=2, lty=2)
lines(response.time, ub, col='red', lwd=2, lty=2)
abline(h=0, col="blue", lwd=2, lty=2)
grid()

# y8
tsmat = window(ts.union(y8, x1, c1, c3, c4, c5), start = c(1987, 2), frequency = 12)

var1 = VAR(y = tsmat, p=1)
amat = diag(6)
diag(amat) = NA
svar1 = SVAR(var1, estmethod="direct", Amat = amat)
irfs = irf(svar1, n.ahead = 12, cumulative = FALSE, ci = 0.95, boot=TRUE, runs=100)
irfy = irfs$irf[1]
irfy.df = as.data.frame(irfy)
sel = irfs$Lower[1]
sel.df = as.data.frame(sel)
seu = irfs$Upper[1]
seu.df = as.data.frame(seu)
irfs8 = irfy.df[2]
irfs8 = irfs8[2:13,]*100
lb = sel.df[2]
lb = lb[2:13,]*100
ub = seu.df[2]
ub = ub[2:13,]*100
response.time = c(1:12)


plot(response.time, irfs8, xlim=c(1.4, 11.6), ylim=c(-3.9, 0.25), xlab = "", ylab = "Market Return", main = "Information Technology", type = "l", cex.lab=1.2, cex.axis=1.2)
polygon(c(response.time, rev(response.time)), c(lb, rev(ub)), col='grey90', border=FALSE)
lines(response.time, irfs8, lwd=2)
lines(response.time, lb, col='red', lwd=2, lty=2)
lines(response.time, ub, col='red', lwd=2, lty=2)
abline(h=0, col="blue", lwd=2, lty=2)
grid()

# y9
tsmat = window(ts.union(y9, x1, c1, c3, c4, c5), start = c(1987, 2), frequency = 12)

var1 = VAR(y = tsmat, p=1)
amat = diag(6)
diag(amat) = NA
svar1 = SVAR(var1, estmethod="direct", Amat = amat)
irfs = irf(svar1, n.ahead = 12, cumulative = FALSE, ci = 0.95, boot=TRUE, runs=100)
irfy = irfs$irf[1]
irfy.df = as.data.frame(irfy)
sel = irfs$Lower[1]
sel.df = as.data.frame(sel)
seu = irfs$Upper[1]
seu.df = as.data.frame(seu)
irfs9 = irfy.df[2]
irfs9 = irfs9[2:13,]*100
lb = sel.df[2]
lb = lb[2:13,]*100
ub = seu.df[2]
ub = ub[2:13,]*100
response.time = c(1:12)


plot(response.time, irfs9, xlim=c(1.4, 11.6), ylim=c(-0.25, 3.0), xlab = "", ylab = "Market Return", main = "Materials", type = "l", cex.lab=1.2, cex.axis=1.2)
polygon(c(response.time, rev(response.time)), c(lb, rev(ub)), col='grey90', border=FALSE)
lines(response.time, irfs9, lwd=2)
lines(response.time, lb, col='red', lwd=2, lty=2)
lines(response.time, ub, col='red', lwd=2, lty=2)
abline(h=0, col="blue", lwd=2, lty=2)
grid()

# y10
tsmat = window(ts.union(y10, x1, c1, c3, c4, c5), start = c(1987, 2), frequency = 12)

var1 = VAR(y = tsmat, p=1)
amat = diag(6)
diag(amat) = NA
svar1 = SVAR(var1, estmethod="direct", Amat = amat)
irfs = irf(svar1, n.ahead = 12, cumulative = FALSE, ci = 0.95, boot=TRUE, runs=100)
irfy = irfs$irf[1]
irfy.df = as.data.frame(irfy)
sel = irfs$Lower[1]
sel.df = as.data.frame(sel)
seu = irfs$Upper[1]
seu.df = as.data.frame(seu)
irfs10 = irfy.df[2]
irfs10 = irfs10[2:13,]*100
lb = sel.df[2]
lb = lb[2:13,]*100
ub = seu.df[2]
ub = ub[2:13,]*100
response.time = c(1:12)


plot(response.time, irfs10, xlim=c(1.4, 11.6), ylim=c(-0.55, 1.25), xlab = "", ylab = "Market Return", main = "Real Estate", type = "l", cex.lab=1.2, cex.axis=1.2)
polygon(c(response.time, rev(response.time)), c(lb, rev(ub)), col='grey90', border=FALSE)
lines(response.time, irfs10, lwd=2)
lines(response.time, lb, col='red', lwd=2, lty=2)
lines(response.time, ub, col='red', lwd=2, lty=2)
abline(h=0, col="blue", lwd=2, lty=2)
grid()

# y11
tsmat = window(ts.union(y11, x1, c1, c3, c4, c5), start = c(1987, 2), frequency = 12)

var1 = VAR(y = tsmat, p=1)
amat = diag(6)
diag(amat) = NA
svar1 = SVAR(var1, estmethod="direct", Amat = amat)
irfs = irf(svar1, n.ahead = 12, cumulative = FALSE, ci = 0.95, boot=TRUE, runs=100)
irfy = irfs$irf[1]
irfy.df = as.data.frame(irfy)
sel = irfs$Lower[1]
sel.df = as.data.frame(sel)
seu = irfs$Upper[1]
seu.df = as.data.frame(seu)
irfs11 = irfy.df[2]
irfs11 = irfs11[2:13,]*100
lb = sel.df[2]
lb = lb[2:13,]*100
ub = seu.df[2]
ub = ub[2:13,]*100
response.time = c(1:12)


plot(response.time, irfs11, xlim=c(1.4, 11.6), ylim=c(-1.3, 0.65), xlab = "", ylab = "Market Return", main = "Telecommunications", type = "l", cex.lab=1.2, cex.axis=1.2)
polygon(c(response.time, rev(response.time)), c(lb, rev(ub)), col='grey90', border=FALSE)
lines(response.time, irfs11, lwd=2)
lines(response.time, lb, col='red', lwd=2, lty=2)
lines(response.time, ub, col='red', lwd=2, lty=2)
abline(h=0, col="blue", lwd=2, lty=2)
grid()

# y12
tsmat = window(ts.union(y12, x1, c1, c3, c4, c5), start = c(1987, 2), frequency = 12)

var1 = VAR(y = tsmat, p=1)
amat = diag(6)
diag(amat) = NA
svar1 = SVAR(var1, estmethod="direct", Amat = amat)
irfs = irf(svar1, n.ahead = 12, cumulative = FALSE, ci = 0.95, boot=TRUE, runs=100)
irfy = irfs$irf[1]
irfy.df = as.data.frame(irfy)
sel = irfs$Lower[1]
sel.df = as.data.frame(sel)
seu = irfs$Upper[1]
seu.df = as.data.frame(seu)
irfs12 = irfy.df[2]
irfs12 = irfs12[2:13,]*100
lb = sel.df[2]
lb = lb[2:13,]*100
ub = seu.df[2]
ub = ub[2:13,]*100
response.time = c(1:12)


plot(response.time, irfs12, xlim=c(1.4, 11.6), ylim=c(-0.7, 1.45), xlab = "", ylab = "Market Return", main = "Utilities", type = "l", cex.lab=1.2, cex.axis=1.2)
polygon(c(response.time, rev(response.time)), c(lb, rev(ub)), col='grey90', border=FALSE)
lines(response.time, irfs12, lwd=2)
lines(response.time, lb, col='red', lwd=2, lty=2)
lines(response.time, ub, col='red', lwd=2, lty=2)
abline(h=0, col="blue", lwd=2, lty=2)
grid()

irfs_all = cbind(irfs1, irfs2, irfs3, irfs4, irfs5, irfs6, irfs7, irfs8, irfs9, irfs10, irfs11, irfs12)
irf.df = as.data.frame(irfs_all)
stargazer(irf.df, summary=FALSE)