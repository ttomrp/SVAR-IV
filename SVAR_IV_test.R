library(tseries)
library(vars)
library(moments)
library(tidyverse)
library(readxl)
library(AER)
library(svars)
library(backtest)
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
c5 = ts(c(NA, diff(TEN)), start=1987, frequency=12) #c5 = TEN monthly change

#Preliminary VAR and SVAR runs using cholesky decomp
#-----------------------------------------------------------------------------------------------------------------------
y1.tsmat = window(ts.union(y1, x1, c1, c3, c4, c5), start = c(1987, 2), frequency = 12)


VARselect(y1.tsmat, lag.max = 12, type="const")

var.y1aic = VAR(y = y1.tsmat, p=3)
summary(var.y1aic)
roots(var.y1aic)
serial.test(var.y1aic, lags.pt = 12, type = "BG")

var.y1sbc = VAR(y = y1.tsmat, p=1)
summary(var.y1sbc)
roots(var.y1sbc)
serial.test(var.y1sbc, lags.pt = 12, type = "BG")

y1.c = id.chol(var.y1sbc, order_k = c("y1", "x1", "c1", "c3", "c4", "c5"))
summary(y1.c)
irf_y1 = irf(y1.c, n.ahead = 12, cumulative = FALSE, ci = 0.95)
irf_y1
plot(irf_y1, plot.type = "multiple")
irfc_y1 = irf(y1.c, n.ahead = 12, cumulative = TRUE, ci = 0.95)
irfc_y1

fevd.y1 = fevd(y1.c, n.ahead = 12)
fevd.y1


#----
#Attempting to run SVAR-IV
#-----------------------------------------------------------------------------------------------------------------------
y1.tsmat = window(ts.union(y1, x1, c1, c3, c4, c5), start = c(1987, 2), frequency = 12)

#Step 1)
var.y1sbc = VAR(y = y1.tsmat, p=1)
summary(var.y1sbc)
resid.y1 = residuals(var.y1sbc)
summary(resid.y1)

u1 = resid.y1[,1]
up = resid.y1[,2] #residuals of x1.ts = Yi + Up, we want to extract the shock from oil prices
u2 = resid.y1[,3]
u3 = resid.y1[,4]
u4 = resid.y1[,5]
u5 = resid.y1[,6]

#Step 2)
m1 = ivreg(u1~up+iv1[3:396] | iv1[3:396])
summary(m1)
v1 = residuals(m1)

m2 = ivreg(u2~up+iv1[3:396] | iv1[3:396])
summary(m2)
v2 = residuals(m2)

m3 = ivreg(u3~up+iv1[3:396] | iv1[3:396])
summary(m3)
v3 = residuals(m3)

m4 = ivreg(u4~up+iv1[3:396] | iv1[3:396])
summary(m4)
v4 = residuals(m4)

m5 = ivreg(u5~up+iv1[3:396] | iv1[3:396])
summary(m5)
v5 = residuals(m5)

#Step 3)
mp = ivreg(up~u1+u2+u3+u4+u5+v1+v2+v3+v4+v5 | v1+v2+v3+v4+v5)
summary(mp)
vp = residuals(mp)
summary(vp)

#Step 4)
variance.vp = var(vp)
sigma.p = sqrt(variance.vp) #The isolated effect of the shock, the shock has been identified

#----
#Attempt 2 removing controls exept for industrial production
#-----------------------------------------------------------------------------------------------------------------------
y1.tsmat = window(ts.union(y1, x1, c1), start = c(1987, 2), frequency = 12)

#Step 1)
var.y1sbc = VAR(y = y1.tsmat, p=1)
summary(var.y1sbc)
resid.y1 = residuals(var.y1sbc)
summary(resid.y1)

u1 = resid.y1[,1]
up = resid.y1[,2] #residuals of x1.ts = Yi + Up, we want to extract the shock from oil prices
u2 = resid.y1[,3]

#Step 2)
m1 = ivreg(u1~up+iv1[3:396] | iv1[3:396])
summary(m1)
v1 = residuals(m1)

m2 = ivreg(u2~up+iv1[3:396] | iv1[3:396])
summary(m2)
v2 = residuals(m2)

#Step 3)
mp = ivreg(up~u1+v1+v2 | v1+v2)
summary(mp)
vp = residuals(mp)
summary(vp)

#Step 4)
variance.vp = var(vp)
sigma.p = sqrt(variance.vp) #The isolated effect of the shock, the shock has been identified
