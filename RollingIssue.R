library(tseries)

data = read_excel("data_test.xlsx", sheet = 1)

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



#-----------------------------------------------------------------------------------------------------------------------
data_z = cbind(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, x1, iv1, c1, c3, c4, c5)
data_v = data_z[2:401,]
data.df = as.data.frame(data_v)
data.df$y = (1:nrow(data.df) - 1) %/% 12
data.df$y = c(data.df$y[2:400], 33)
year = data.df$y

remove(n)
for (n in 1:304){
  data_a = data_z[n:(96+n),]
  data.df = as.data.frame(data_a)
  data.df$year = year[n:(96+n)]  
}

remove(n)
for (n in 1:80){
  data_a = data_z[n:(96+n),]
  data.df = as.data.frame(data_a)
  data.df$year = year[n:(96+n)]  
}
data_a
data_z[1:96+1,]

data_z
w = 1
data_z[w:96+w,] #takes the first 96 elements of each column, excludes the NAs in the first element of each column 
w = 2
data_z[w:(96+w),]
w = 3
data_z[w:(96+w),]
w = 4
data_z[w:(96+w),]
