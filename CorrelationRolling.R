http://eranraviv.com/blog/live-rolling-correlation-plot/
          
fun = function(){
library(quantmod)
symetf = c('TLT','SPY')
end<- format(Sys.Date(),"%Y-%m-%d") 
start<-format(Sys.Date() - 4000,"%Y-%m-%d")
dat0 = (getSymbols(symetf[1], src="google", from=start, to=end, auto.assign = F, warnings = FALSE,symbol.lookup = F))
n = NROW(dat0)  ; l = length(symetf)
dat = array(dim = c(n,NCOL(dat0),l)) ; ret = matrix(nrow = n, ncol = l) 
for (i in 1:l){
 dat0 = (getSymbols(symetf[i], src="google", from=start, to=end, auto.assign = F,warnings = FALSE,symbol.lookup = F))
dat[1:n,,i] = dat0 
ret[2:n,i] = dat[2:n,4,i]/dat[1:(n-1),4,i] - 1
 }
rolcor = NULL ; h = 15 # 10 bussiness days is two weeks
for (i in 2:(n-h)){
rolcor[i+h] = mean(cor(ret[i:(i+h),])[lower.tri(cor(ret[i:(i+h),]))]) # just the rolling average correlation
}

rolcor1 = NULL ; h = 63 # 10 bussiness days is two weeks
for (i in 2:(n-h)){
rolcor1[i+h] = mean(cor(ret[i:(i+h),])[lower.tri(cor(ret[i:(i+h),]))]) # just the rolling average correlation
}

par( mfrow = c(3,1), bg = "white", bty ="n", fg = gray(0.3) ,font.lab = 6, font.axis = 6, #xaxp =  c(x1, x2, n = 2)
font.main = 6, col.axis = gray(0.3) , col.lab = gray(0.3) , pch = 21,  tck = -0.02, #tck is the length of the axis spikes 
 xaxs = "r")  # Graph parameters
lwd1 = 2.5
plot(rolcor~index(dat0), ty = "l",lwd = lwd1, xlab = "Time",ylab = "Average Correlation", main = "TLT-SPY 3 Weeks Rolling Correlation")
plot(rolcor1~index(dat0), ty = "l",lwd = lwd1, xlab = "Time",ylab = "Average Correlation", main = "TLT-SPY 3 Months Rolling Correlation")
plot(dat[1:n,4,2]~index(dat0), ty = "l", lwd = lwd1, xlab = "Time", ylab = "SPY Price Level",main ="SPY Price Level")
}
 
fun()
## When increasing and decreasing 3 month trends are fast (middle 2013, early 2014), 
#be patient, buy smaller lots on leader weakness, more agressive on sideways 
#trend of 3 month correlation. 
##







##Sector specific 

http://rbresearch.wordpress.com/2012/05/24/quick-view-on-correlations-of-different-asset-classes/
#Correlations of Sector ETFs to benchmarked against SPY

#Load the packages used
require(PerformanceAnalytics)
require(quantmod)

#create a list of symbols
symbols = c("XLY", "XLP", "XLE", "XLF", "XLV", "XLI", "XLK", "XLB", "XLU")
retsymbols <- paste("ret", symbols, sep = ".")

#Downlad the data from yahoo
getSymbols(symbols, src = 'yahoo', index.class = c("POSIXt","POSIXct"), from = '2005-01-01')
getSymbols("SPY", src = 'yahoo', index.class = c("POSIXt","POSIXct"), from = '2005-01-01')

#The benchmark is the return vector of which the other assets will be benchmarked against
benchmark <- ROC(Ad(SPY), n=1, type="continuous", na.pad=TRUE)
colnames(benchmark) <- "SPY"

#Loop to create new xts objects with just the returns
for (symbol in symbols){
  x <- get(symbol)
  x1 <- ROC(Ad(x), n=1, type="continuous", na.pad=TRUE)
  colnames(x1)<-gsub("x",symbol,colnames(x1))
  assign(paste("ret", symbol, sep = "."),x1)
}

#this merges all of the objects in 'retsymbols' into one object named 'ret'
ret <- do.call(merge, lapply(retsymbols, get))

mfrow = c(1,1)
suppressWarnings(chart.RollingCorrelation(ret[,1:ncol(ret)], benchmark, width = 252, xaxis = TRUE, 
                          colorset = rich8equal, legend.loc = "bottomright",
                         main = "Rolling 252 Day Correlation"))