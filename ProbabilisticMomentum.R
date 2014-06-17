###############################################################################
# Load Systematic Investor Toolbox (SIT)
# http://systematicinvestor.wordpress.com/systematic-investor-toolbox/
###############################################################################
setInternet2(TRUE)
con = gzcon(file('/Users/juaneshutte/Tech/R/Trading/sit2014.gz', 'rb'))
source(con)
close(con)
    #*****************************************************************
    # Load historical data
    #****************************************************************** 
    load.packages('quantmod')
         
    tickers = spl('SPY,TLT')
         
    data <- new.env()
    getSymbols(tickers, src = 'yahoo', from = '1980-01-01', env = data, auto.assign = T)
        for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)
    bt.prep(data, align='remove.na', dates='1980::')
  
     
    #*****************************************************************
    # Setup
    #****************************************************************** 
    lookback.len = 60
     
    prices = data$prices
     
    models = list()
     
    #*****************************************************************
    # Simple Momentum
    #****************************************************************** 
    momentum = prices / mlag(prices, lookback.len)
    data$weight[] = NA
        data$weight$SPY[] = momentum$SPY > momentum$TLT
        data$weight$TLT[] = momentum$SPY <= momentum$TLT
    models$Simple  = bt.run.share(data, clean.signal=T, trade.summary=T)   

#*****************************************************************
# Probabilistic Momentum
#****************************************************************** 
confidence.level = 60/100
ret = prices / mlag(prices) - 1 
 
ir = sqrt(lookback.len) * runMean(ret$SPY - ret$TLT, lookback.len) / runSD(ret$SPY - ret$TLT, lookback.len)
momentum.p = pt(ir, lookback.len - 1)
     
data$weight[] = NA
    data$weight$SPY[] = iif(cross.up(momentum.p, confidence.level), 1, iif(cross.dn(momentum.p, (1 - confidence.level)), 0,NA))
    data$weight$TLT[] = iif(cross.dn(momentum.p, (1 - confidence.level)), 1, iif(cross.up(momentum.p, confidence.level), 0,NA))
models$Probabilistic  = bt.run.share(data, clean.signal=T, trade.summary=T)  

#*****************************************************************
# Probabilistic Momentum + SPY Leverage 
#****************************************************************** 
data$weight[] = NA
    data$weight$SPY[] = iif(cross.up(momentum.p, confidence.level), 1, iif(cross.up(momentum.p, (1 - confidence.level)), 0,NA))
    data$weight$TLT[] = iif(cross.dn(momentum.p, (1 - confidence.level)), 1, iif(cross.up(momentum.p, confidence.level), 0,NA))
models$Probabilistic.Leverage = bt.run.share(data, clean.signal=T, trade.summary=T)  
## tail(models$Probabilistic.Leverage[["trade.summary"]]) exit entry dates not right so ingore ##


#*****************************************************************
# Create Report
#******************************************************************    
strategy.performance.snapshoot(models, T)
plotbt.custom.report(models$Probabilistic)
#*****************************************************************
# Visualize Signal
#******************************************************************        
cols = spl('steelblue1,steelblue')
prices = scale.one(data$prices)
 
layout(1:3)
 
plota(prices$SPY, type='l', ylim=range(prices), plotX=F, col=cols[1], lwd=2)
plota.lines(prices$TLT, type='l', plotX=F, col=cols[2], lwd=2)
    plota.legend('SPY,TLT',cols,as.list(prices))
 
highlight = models$Probabilistic$weight$SPY > 0
    plota.control$col.x.highlight = iif(highlight, cols[1], cols[2])
plota(models$Probabilistic$equity, type='l', plotX=F, x.highlight = highlight | T)
    plota.legend('Probabilistic,SPY,TLT',c('black',cols))
             
highlight = models$Simple$weight$SPY > 0
    plota.control$col.x.highlight = iif(highlight, cols[1], cols[2])
plota(models$Simple$equity, type='l', plotX=T, x.highlight = highlight | T)
    plota.legend('Simple,SPY,TLT',c('black',cols)) 

tail(models$Probabilistic$weight)
tail(models$Probabilistic$share)
tail(models$Probabilistic$ret)
tail(models$Probabilistic$equity)
tail(models$Probabilistic[["trade.summary"]])
tail(momentum.p,20)


# Buy on leaders' weakness, a breakout after consolidation and on greater than 
# 10% drawdown of strategy. Sell after big leader move cause of consolidation and sell if strategy says so, ie stick to IT!!
# watch for short and medium term correlation moving toward -1 as sign of poor 
# strategy performance, trend toward 0 more confidence better performance
# 2012 performance poor as correlation around -0.6 to -1 (buy low sell high of strategy) 
# 2013 better perf as short and medium term correlation trended toward 0 state (buy on weakness agressively)
# Biggest risks to strategy: both consolidating, good returns too quickly (viscous move),
# correlation in -0.6 to -1 state if not higher highs, switching into TLT (no formation) when
# SPY (bull formation) needs consolidation 