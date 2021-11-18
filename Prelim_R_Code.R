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

y1.1 = lag(y1)
y2.1 = lag(y2)
y3.1 = lag(y3)
y4.1 = lag(y4)
y5.1 = lag(y5)
y6.1 = lag(y6)
y7.1 = lag(y7)
y8.1 = lag(y8)
y9.1 = lag(y9)
y10.1 = lag(y10)
y11.1 = lag(y11)
y12.1 = lag(y12)
x1.1 = lag(x1)
iv1.1 = lag(iv1)
c1.1 = lag(c1)
c2.1 = lag(c2)
c3.1 = lag(c3)
c4.1 = lag(c4)
c5.1 = lag(c5)

variables.df = data.frame("y1", 'y2', 'y3', 'y4', 'y5', 'y6', 'y7', 'y8', 'y9', 'y10', 'y11', 'y12', 'x1', 'iv1', 'c1', 'c3', 'c4', 'c5')

recessions.df = read.table(textConnection(
  "Peak, Trough
1887-03-01, 1888-04-01
1890-07-01, 1891-05-01
1893-01-01, 1894-06-01
1895-12-01, 1897-06-01
1899-06-01, 1900-12-01
1902-09-01, 1904-08-01
1907-05-01, 1908-06-01
1910-01-01, 1912-01-01
1913-01-01, 1914-12-01
1918-08-01, 1919-03-01
1920-01-01, 1921-07-01
1923-05-01, 1924-07-01
1926-10-01, 1927-11-01
1929-08-01, 1933-03-01
1937-05-01, 1938-06-01
1945-02-01, 1945-10-01
1948-11-01, 1949-10-01
1953-07-01, 1954-05-01
1957-08-01, 1958-04-01
1960-04-01, 1961-02-01
1969-12-01, 1970-11-01
1973-11-01, 1975-03-01
1980-01-01, 1980-07-01
1981-07-01, 1982-11-01
1990-07-01, 1991-03-01
2001-03-01, 2001-11-01
2007-12-01, 2009-06-01
2020-02-01, 2020-04-01"), sep=',',
  colClasses=c('Date', 'Date'), header=TRUE)

ts.plot(log(WMKT), log(COMP), type='l', lty=c(1,1), col=c(1,2))
par(new=TRUE)
plot(log(WTI), type='l', lty=1, col=4, axes=FALSE, xlab="", main="")
axis(side=4, xlab="")
legend("topleft", border=NULL, legend=c("World Market","U.S. Market", "WTI Oil"), lty=c(1,1,1), col=c(1,2,4))
grid()

a = log(WMKT)
b = log(COMP)
c = log(WTI)
df = data.frame('a', 'b', 'c')
ggplot(data=variables.df)

#Descriptive Statistics & ADF-test
#-----------------------------------------------------------------------------------------------------------------------

#COMP
ggplot(data, aes(y=COMP, x=Date)) + geom_line()
summary(COMP)
#y1
ggplot(data, aes(y=y1, x=Date)) + geom_line()
adf.test(y1[2:length(y1)], alternative = c("s"), k=1)
summary(y1)
var(y1[2:length(y1)])
sqrt(var(y1[2:length(y1)]))

#COD
ggplot(data, aes(y=COD, x=Date)) + geom_line()
summary(COD)
#y2
ggplot(data, aes(y=y2, x=Date)) + geom_line()
adf.test(y2[2:length(y2)], alternative = c("s"), k=1)
summary(y2)
var(y2[2:length(y2)])
sqrt(var(y2[2:length(y2)]))

#CST
ggplot(data, aes(y=CST, x=Date)) + geom_line()
summary(CST)
#y3
ggplot(data, aes(y=y3, x=Date)) + geom_line()
adf.test(y3[2:length(y3)], alternative = c("s"), k=1)
summary(y3)
var(y3[2:length(y3)])
sqrt(var(y3[2:length(y3)]))

#ENE
ggplot(data, aes(y=ENE, x=Date)) + geom_line()
summary(ENE)
#y4
ggplot(data, aes(y=y4, x=Date)) + geom_line()
adf.test(y4[2:length(y4)], alternative = c("s"), k=1)
summary(y4)
var(y4[2:length(y4)])
sqrt(var(y4[2:length(y4)]))

#FIN
ggplot(data, aes(y=FIN, x=Date)) + geom_line()
summary(FIN)
#y5
ggplot(data, aes(y=y5, x=Date)) + geom_line()
adf.test(y5[2:length(y5)], alternative = c("s"), k=1)
summary(y5)
var(y5[2:length(y5)])
sqrt(var(y5[2:length(y5)]))

#HCR
ggplot(data, aes(y=HCR, x=Date)) + geom_line()
summary(HCR)
#y6
ggplot(data, aes(y=y6, x=Date)) + geom_line()
adf.test(y6[2:length(y6)], alternative = c("s"), k=1)
summary(y6)
var(y6[2:length(y6)])
sqrt(var(y6[2:length(y6)]))

#IND
ggplot(data, aes(y=IND, x=Date)) + geom_line()
summary(IND)
#y7
ggplot(data, aes(y=y7, x=Date)) + geom_line()
adf.test(y7[2:length(y7)], alternative = c("s"), k=1)
summary(y7)
var(y7[2:length(y7)])
sqrt(var(y7[2:length(y7)]))

#INT
ggplot(data, aes(y=INT, x=Date)) + geom_line()
summary(INT)
#y8
ggplot(data, aes(y=y8, x=Date)) + geom_line()
adf.test(y8[2:length(y8)], alternative = c("s"), k=1)
summary(y8)
var(y8[2:length(y8)])
sqrt(var(y8[2:length(y8)]))

#MAT
ggplot(data, aes(y=MAT, x=Date)) + geom_line()
summary(MAT)
#y9
ggplot(data, aes(y=y9, x=Date)) + geom_line()
adf.test(y9[2:length(y9)], alternative = c("s"), k=1)
summary(y9)
var(y9[2:length(y9)])
sqrt(var(y9[2:length(y9)]))

#RES
ggplot(data, aes(y=RES, x=Date)) + geom_line()
summary(RES)
#y10
ggplot(data, aes(y=y10, x=Date)) + geom_line()
adf.test(y10[2:length(y10)], alternative = c("s"), k=1)
summary(y10)
var(y10[2:length(y10)])
sqrt(var(y10[2:length(y10)]))

#TEL
ggplot(data, aes(y=TEL, x=Date)) + geom_line()
summary(TEL)
#y11
ggplot(data, aes(y=y11, x=Date)) + geom_line()
adf.test(y11[2:length(y11)], alternative = c("s"), k=1)
summary(y11)
var(y11[2:length(y11)])
sqrt(var(y11[2:length(y11)]))

#UTL
ggplot(data, aes(y=UTL, x=Date)) + geom_line()
summary(UTL)
#y12
ggplot(data, aes(y=y12, x=Date)) + geom_line()
adf.test(y12[2:length(y12)], alternative = c("s"), k=1)
summary(y12)
var(y12[2:length(y12)])
sqrt(var(y12[2:length(y12)]))

#WTI
ggplot(data, aes(y=WTI, x=Date)) + geom_line()
summary(WTI)
#x1
ggplot(data, aes(y=x1, x=Date)) + geom_line()
adf.test(x1[2:length(x1)], alternative = c("s"), k=1)
summary(x1)
var(x1[2:length(x1)])
sqrt(var(x1[2:length(x1)]))

#HTO
ggplot(data, aes(y=HTO, x=Date)) + geom_line()
summary(HTO)
#iv1
ggplot(data, aes(y=iv1, x=Date)) + geom_line()
adf.test(iv1[2:length(iv1)], alternative = c("s"), k=1)
summary(iv1)
var(iv1[2:length(iv1)])
sqrt(var(iv1[2:length(iv1)]))

#IP
ggplot(data, aes(y=IP, x=Date)) + geom_line()
summary(IP)
#c1
ggplot(data, aes(y=c1, x=Date)) + geom_line()
adf.test(c1[2:length(c1)], alternative = c("s"), k=1)
summary(c1)
var(c1[2:length(c1)])
sqrt(var(c1[2:length(c1)]))

#IR
ggplot(data, aes(y=IR, x=Date)) + geom_line()
summary(IR)
#c2
ggplot(data, aes(y=c2, x=Date)) + geom_line()
adf.test(c2[2:length(c2)], alternative = c("s"), k=1)
summary(c2)
var(c2[2:length(c2)])
sqrt(var(c2[2:length(c2)]))

#CPI
ggplot(data, aes(y=CPI, x=Date)) + geom_line()
summary(CPI)
#c3
ggplot(data, aes(y=c3, x=Date)) + geom_line()
adf.test(c3[2:length(c3)], alternative = c("s"), k=1)
summary(c3)
var(c3[2:length(c3)])
sqrt(var(c3[2:length(c3)]))

#WMKT
ggplot(data, aes(y=WMKT, x=Date)) + geom_line()
summary(WMKT)
#c4
ggplot(data, aes(y=c4, x=Date)) + geom_line()
adf.test(c4[2:length(c4)], alternative = c("s"), k=1)
summary(c4)
var(c4[2:length(c4)])
sqrt(var(c4[2:length(c4)]))

#TEN
ggplot(data, aes(y=TEN, x=Date)) + geom_line()
summary(TEN)
#c5
ggplot(data, aes(y=c5, x=Date)) + geom_line()
adf.test(c5[2:length(c5)], alternative = c("s"), k=1)
summary(c5)
var(c5[2:length(c5)])
sqrt(var(c5[2:length(c5)]))

#----
#First-Stage Regressions
#-----------------------------------------------------------------------------------------------------------------------
model.fs1 = lm(x1~iv1+c1+c3+c4+c5) #edit this function to test with different controls
summary(model.fs1)

fs1res = c(NA, model.fs1$res) #residuals of model fs1

Ftest = linearHypothesis(model.fs1, c("iv1=0")) #Stock-Yogo Test:  F-stat>10  =>  Reject null, IV not weak
#F-stat = 492.13

model.fs1 = lm(x1~x1.1+iv1+iv1.1+c1+c1.1+c3+c3.1+c4+c4.1+c5+c5.1)
summary(model.fs1)

fs1res = c(NA,NA, model.fs1$res)

Ftest1=linearHypothesis(model.fs1, c("iv1=0"))
Ftest2=linearHypothesis(model.fs1, c("iv1.1=0"))
Ftest3=linearHypothesis(model.fs1, c("iv1=0","iv1.1=0"))
Ftest1
Ftest2
Ftest3
stargazer(Ftest2)
stargazer(Ftest3)





model.iv1 = ivreg(y1~y1.1+x1+x1.1+c1+c1.1+c3+c3.1+c4+c4.1+c5+c5.1 | iv1+iv1.1+y1.1+c1+c1.1+c3+c3.1+c4+c4.1+c5+c5.1)
summary(model.iv1, vcov = sandwich, diagnostics = TRUE)

model.iv2 = ivreg(y2~y2.1+x1+x1.1+c1+c1.1+c3+c3.1+c4+c4.1+c5+c5.1 | iv1+iv1.1+y2.1+c1+c1.1+c3+c3.1+c4+c4.1+c5+c5.1)
summary(model.iv2, vcov = sandwich, diagnostics = TRUE)

model.iv3 = ivreg(y3~y3.1+x1+x1.1+c1+c1.1+c3+c3.1+c4+c4.1+c5+c5.1 | iv1+iv1.1+y3.1+c1+c1.1+c3+c3.1+c4+c4.1+c5+c5.1)
summary(model.iv3, vcov = sandwich, diagnostics = TRUE)

model.iv4 = ivreg(y4~y4.1+x1+x1.1+c1+c1.1+c3+c3.1+c4+c4.1+c5+c5.1 | iv1+iv1.1+y4.1+c1+c1.1+c3+c3.1+c4+c4.1+c5+c5.1)
summary(model.iv4, vcov = sandwich, diagnostics = TRUE)

model.iv5 = ivreg(y5~y5.1+x1+x1.1+c1+c1.1+c3+c3.1+c4+c4.1+c5+c5.1 | iv1+iv1.1+y5.1+c1+c1.1+c3+c3.1+c4+c4.1+c5+c5.1)
summary(model.iv5, vcov = sandwich, diagnostics = TRUE)

model.iv6 = ivreg(y6~y6.1+x1+x1.1+c1+c1.1+c3+c3.1+c4+c4.1+c5+c5.1 | iv1+iv1.1+y6.1+c1+c1.1+c3+c3.1+c4+c4.1+c5+c5.1)
summary(model.iv6, vcov = sandwich, diagnostics = TRUE)

model.iv7 = ivreg(y7~y7.1+x1+x1.1+c1+c1.1+c3+c3.1+c4+c4.1+c5+c5.1 | iv1+iv1.1+y7.1+c1+c1.1+c3+c3.1+c4+c4.1+c5+c5.1)
summary(model.iv7, vcov = sandwich, diagnostics = TRUE)

model.iv8 = ivreg(y8~y8.1+x1+x1.1+c1+c1.1+c3+c3.1+c4+c4.1+c5+c5.1 | iv1+iv1.1+y8.1+c1+c1.1+c3+c3.1+c4+c4.1+c5+c5.1)
summary(model.iv8, vcov = sandwich, diagnostics = TRUE)

model.iv9 = ivreg(y9~y9.1+x1+x1.1+c1+c1.1+c3+c3.1+c4+c4.1+c5+c5.1 | iv1+iv1.1+y9.1+c1+c1.1+c3+c3.1+c4+c4.1+c5+c5.1)
summary(model.iv9, vcov = sandwich, diagnostics = TRUE)

model.iv10 = ivreg(y10~y10.1+x1+x1.1+c1+c1.1+c3+c3.1+c4+c4.1+c5+c5.1 | iv1+iv1.1+y10.1+c1+c1.1+c3+c3.1+c4+c4.1+c5+c5.1)
summary(model.iv10, vcov = sandwich, diagnostics = TRUE)

model.iv11 = ivreg(y11~y11.1+x1+x1.1+c1+c1.1+c3+c3.1+c4+c4.1+c5+c5.1 | iv1+iv1.1+y11.1+c1+c1.1+c3+c3.1+c4+c4.1+c5+c5.1)
summary(model.iv11, vcov = sandwich, diagnostics = TRUE)

model.iv12 = ivreg(y12~y12.1+x1+x1.1+c1+c1.1+c3+c3.1+c4+c4.1+c5+c5.1 | iv1+iv1.1+y12.1+c1+c1.1+c3+c3.1+c4+c4.1+c5+c5.1)
summary(model.iv12, vcov = sandwich, diagnostics = TRUE)


#----
#y1(COMP RETURN) as Independent Variable
#-----------------------------------------------------------------------------------------------------------------------

#Hausman Tests
model.y1 = lm(y1~y1.1+x1+x1.1+c1+c1.1+c3+c3.1+c4+c4.1+c5+c5.1+fs1res)
summary(model.y1)
linearHypothesis(model.y1, c("fs1res=0")) #Hausman Test:  fs1res is significant at 5% level  =>  OLS is not sufficient

model.iv1 = ivreg(y1~x1+c1+c3+c4+c5 | iv1+c1+c3+c4+c5)
summary(model.iv1, vcov = sandwich, diagnostics = TRUE)
#Weak Instruments: F-stat>10  =>  Reject null, IV not weak
#Hausman Test: Residuals significant  =>  IV needed

#----
#y2(COD RETURN) as Independent Variable
#-----------------------------------------------------------------------------------------------------------------------
#Hausman Tests
model.y2 = lm(y2~y2.1+x1+x1.1+c1+c1.1+c3+c3.1+c4+c4.1+c5+c5.1+fs1res)
summary(model.y2)
linearHypothesis(model.y2, c("fs1res=0")) #Hausman Test:  fr1res is significant at 5% level  =>  OLS is not sufficient

model.iv2 = ivreg(y2~x1+c1+c3+c4+c5 | iv1+c1+c3+c4+c5)
summary(model.iv2, vcov = sandwich, diagnostics = TRUE)

#----
#y3(CST RETURN) as Independent Variable
#-----------------------------------------------------------------------------------------------------------------------
#Hausman Tests
model.y3 = lm(y3~y3.1+x1+x1.1+c1+c1.1+c3+c3.1+c4+c4.1+c5+c5.1+fs1res)
summary(model.y3)
linearHypothesis(model.y3, c("fs1res=0")) #Hausman Test:  fr1res is not significant  =>  OLS is sufficient

model.iv3 = ivreg(y3~x1+c1+c3+c4+c5 | iv1+c1+c3+c4+c5)
summary(model.iv3, vcov = sandwich, diagnostics = TRUE)

#----
#y4(ENE RETURN) as Independent Variable
#-----------------------------------------------------------------------------------------------------------------------
#Hausman Tests
model.y4 = lm(y4~y4.1+x1+x1.1+c1+c1.1+c3+c3.1+c4+c4.1+c5+c5.1+fs1res)
summary(model.y4)
linearHypothesis(model.y4, c("fs1res=0")) #Hausman Test:  fr1res is significant at 1% level  =>  OLS is not sufficient

model.iv4 = ivreg(y4~x1+c1+c3+c4+c5 | iv1+c1+c3+c4+c5)
summary(model.iv4, vcov = sandwich, diagnostics = TRUE)

#----
#y5(FIN RETURN) as Independent Variable
#-----------------------------------------------------------------------------------------------------------------------
#Hausman Tests
model.y5 = lm(y5~y5.1+x1+x1.1+c1+c1.1+c3+c3.1+c4+c4.1+c5+c5.1+fs1res)
summary(model.y5)
linearHypothesis(model.y5, c("fs1res=0")) #Hausman Test:  fr1res is significant at 5% level  =>  OLS is not sufficient

model.iv5 = ivreg(y5~x1+c1+c3+c4+c5 | iv1+c1+c3+c4+c5)
summary(model.iv5, vcov = sandwich, diagnostics = TRUE)

#----
#y6(HCR RETURN) as Independent Variable
#-----------------------------------------------------------------------------------------------------------------------
#Hausman Tests
model.y6 = lm(y6~y6.1+x1+x1.1+c1+c1.1+c3+c3.1+c4+c4.1+c5+c5.1+fs1res)
summary(model.y6)
linearHypothesis(model.y6, c("fs1res=0")) #Hausman Test:  fr1res is not significant  =>  OLS is sufficient

model.iv6 = ivreg(y6~x1+c1+c3+c4+c5 | iv1+c1+c3+c4+c5)
summary(model.iv6, vcov = sandwich, diagnostics = TRUE)

#----
#y7(IND RETURN) as Independent Variable
#-----------------------------------------------------------------------------------------------------------------------
#Hausman Tests
model.y7 = lm(y7~y7.1+x1+x1.1+c1+c1.1+c3+c3.1+c4+c4.1+c5+c5.1+fs1res)
summary(model.y7)
linearHypothesis(model.y7, c("fs1res=0")) #Hausman Test:  fr1res is not significant  =>  OLS is sufficient

model.iv7 = ivreg(y7~x1+c1+c3+c4+c5 | iv1+c1+c3+c4+c5)
summary(model.iv7, vcov = sandwich, diagnostics = TRUE)

#----
#y8(INT RETURN) as Independent Variable
#-----------------------------------------------------------------------------------------------------------------------
#Hausman Tests
model.y8 = lm(y8~y8.1+x1+x1.1+c1+c1.1+c3+c3.1+c4+c4.1+c5+c5.1+fs1res)
summary(model.y8)
linearHypothesis(model.y8, c("fs1res=0")) #Hausman Test:  fr1res is significant at 5% level  =>  OLS is not sufficient

model.iv8 = ivreg(y8~x1+c1+c3+c4+c5 | iv1+c1+c3+c4+c5)
summary(model.iv8, vcov = sandwich, diagnostics = TRUE)

#----
#y9(MAT RETURN) as Independent Variable
#-----------------------------------------------------------------------------------------------------------------------
#Hausman Tests
model.y9 = lm(y9~y9.1+x1+x1.1+c1+c1.1+c3+c3.1+c4+c4.1+c5+c5.1+fs1res)
summary(model.y9)
linearHypothesis(model.y9, c("fs1res=0")) #Hausman Test:  fr1res is not significant  =>  OLS is sufficient

model.iv9 = ivreg(y9~x1+c1+c3+c4+c5 | iv1+c1+c3+c4+c5)
summary(model.iv9, vcov = sandwich, diagnostics = TRUE)

#----
#y10(RES RETURN) as Independent Variable
#-----------------------------------------------------------------------------------------------------------------------
#Hausman Tests
model.y10 = lm(y10~y10.1+x1+x1.1+c1+c1.1+c3+c3.1+c4+c4.1+c5+c5.1+fs1res)
summary(model.y10)
linearHypothesis(model.y10, c("fs1res=0")) #Hausman Test:  fr1res is not significant  =>  OLS is sufficient

model.iv10 = ivreg(y10~x1+c1+c3+c4+c5 | iv1+c1+c3+c4+c5)
summary(model.iv10, vcov = sandwich, diagnostics = TRUE)

#----
#y11(TEL RETURN) as Independent Variable
#-----------------------------------------------------------------------------------------------------------------------
#Hausman Tests
model.y11 = lm(y11~y11.1+x1+x1.1+c1+c1.1+c3+c3.1+c4+c4.1+c5+c5.1+fs1res)
summary(model.y11)
linearHypothesis(model.y11, c("fs1res=0")) #Hausman Test:  fr1res is significant at 10% level  =>  OLS might be sufficient

model.iv11 = ivreg(y11~x1+c1+c3+c4+c5 | iv1+c1+c3+c4+c5)
summary(model.iv11, vcov = sandwich, diagnostics = TRUE)

#----
#y12(UTL RETURN) as Independent Variable
#-----------------------------------------------------------------------------------------------------------------------
#Hausman Tests
model.y12 = lm(y12~y12.1+x1+x1.1+c1+c1.1+c3+c3.1+c4+c4.1+c5+c5.1+fs1res)
summary(model.y12)
linearHypothesis(model.y12, c("fs1res=0")) #Hausman Test:  fr1res is significant at 5% level  =>  OLS is not sufficient

model.iv12 = ivreg(y12~x1+c1+c3+c4+c5 | iv1+c1+c3+c4+c5)
summary(model.iv12, vcov = sandwich, diagnostics = TRUE)

#----
#Vector Autoregression Testing
#-----------------------------------------------------------------------------------------------------------------------

y1.testmat = window(ts.union(y1.ts, x1.ts), start = c(1987, 2), frequency = 12)

#var test
VARselect(y1.testmat, lag.max=10, type="const") #parsimonious specification(Schwarz) = 1 lag, (AIC suggests 2)
var.y1 = VAR(y = y1.testmat, p=1)
summary(var.y1)
roots(var.y1)
serial.test(var.y1, lags.pt = 8, type = "BG") #p-value = 0.066  =>  At 10% level, resids are white noise
causality(var.y1, cause="y1.ts") #cannot reject null
causality(var.y1, cause="x1.ts") #cannot reject null

#svar test
svar.y1 = BQ(var.y1)
summary(svar.y1)

irf_y1 = irf(svar.y1, n.ahead = 10, cumulative = FALSE, ci = 0.95) #noncumulative IRFs of SVAR
irf_y1
irf_y1_cum = irf(svar.y1, n.ahead = 10, cumulative = TRUE, ci = 0.95) #cumulative IRF of SVAR

plot(irf_y1_cum, plot.type = "multiple")

#another way
amat = diag(6)
diag(amat) = NA
svar.test = SVAR(var.y1, Amat = amat)
summary(svar.test)
irf.svartest = irf(svar.test, n.ahead = 10, cumulative = FALSE, ci = 0.95)
irf.svartest

#another way (svars package)
var.y1test = vars::VAR(y1.tsmat, lag.max = 1, ic = "AIC")
y1_c = id.chol(var.y1test)
y1_c
y1_chol = id.chol(var.y1test, order_k = c("y1.ts", "x1.ts"))
y1_chol

#----
#Preliminary VAR and SVAR runs using cholesky decomp
#-----------------------------------------------------------------------------------------------------------------------
y1.tsmat = window(ts.union(y1, x1, c1, c3, c4, c5), start = c(1987, 2), frequency = 12)
amat = diag(6)
diag(amat) = NA

VARselect(y1.tsmat, lag.max = 12, type="const")
var.y1aic = VAR(y = y1.tsmat, p=3)
summary(var.y1aic)
roots(var.y1aic)
stargazer(var.y1aic[['varresult']], type = 'text')
stargazer(var.y1aic$varresult$y1, var.y1aic$varresult$x1)

serial.test(var.y1aic, lags.pt = 12, type = "BG")
var.y1sbc = VAR(y = y1.tsmat, p=1)
tmp = summary(var.y1sbc)
roots(var.y1sbc)
serial.test(var.y1sbc, lags.pt = 12, type = "BG")
stargazer(var.y1sbc[['varresult']], type = 'text')
stargazer(var.y1sbc$varresult$y1, var.y1sbc$varresult$x1)

y1.bq = BQ(var.y1sbc)

y1.c = id.chol(var.y1sbc, order_k = c("y1", "x1", "c1", "c3", "c4", "c5"))
summary(y1.c)
irf_y1 = irf(y1.c, n.ahead = 24, cumulative = FALSE, ci = 0.95)
irf_y1
plot(irf_y1, plot.type = "multiple")

irfc_y1 = irf(y1.c, n.ahead = 24, cumulative = TRUE, ci = 0.95)
irfc_y1
plot(irfc_y1, plot.type = "multiple")

fevd.y1 = fevd(y1.c, n.ahead = 24)
fevd.y1


#----
#Attempt Simplified SVAR-IV
#-----------------------------------------------------------------------------------------------------------------------


fs1 = lm(x1~iv1+c1+c3+c4+c5)
fs1res = c(NA, fs1$res)
fs1.fitted = ts(c(NA, fitted(fs1)), start = 1987, frequency = 12)

y1.ivmat = window(ts.union(y1, fs1.fitted, c1, c3, c4, c5), start = c(1987, 2), frequency = 12)

VARselect(y1.ivmat, lag.max=12, type="const")
var.y1ivsbc = VAR(y = y1.ivmat, p=1)
var.y1ivaic = VAR(y = y1.ivmat, p=3)

y1.ivc = id.chol(var.y1ivaic, order_k = c("y1", "fs1.fitted", "c1", "c3", "c4", "c5"))
summary(y1.ivc)

irf_y1iv = irf(y1.ivc, n.ahead = 24, cumulative = FALSE, ci = 0.95)
irf_y1iv
summary(irf_y1iv)
plot(irf_y1iv)
irf.y1_x1 = unlist(irf_y1iv$irf[3]) # Impulse Response of y1 to x1
irf.x1_y1 = unlist(irf_y1iv$irf[8]) # Impulse Response of x1 to 71
response.time = c(1:24)
plot(response.time, irf.y1_x1, xlab = "Time", ylab = "Response", main = "Response of US Total Market to Oil Price Shock", type = "l")


irfc.y1iv = irf(y1.ivc, n.ahead = 24, cumulative = TRUE, ci = 0.95)
irfc.y1iv
plot(irfc.y1iv)
irfc.y1_x1 = unlist(irfc_y1iv$irf[3]) # Impulse Response of y1 to x1
irfc.x1_y1 = unlist(irfc_y1iv$irf[8]) # Impulse Response of x1 to y1
response.time = c(1:24)
plot(response.time, irfc.y1_x1, xlab = "Time", ylab = "Response", main = "Response of US Total Market to Oil Price Shock", type = "l")


fevd.y1iv = fevd(y1.ivc, n.ahead  = 24)
fevd.y1iv