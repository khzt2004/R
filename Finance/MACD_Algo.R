require(quantmod)
require(PerformanceAnalytics)

# https://www.quantinsti.com/blog/an-example-of-a-trading-strategy-coded-in-r/

getSymbols('LOXO', src='yahoo')
chartSeries(LOXO, TA=NULL)
data=LOXO[,4]
macd = MACD(data, nFast=12, nSlow=26,nSig=9,maType=SMA,percent = FALSE)
chartSeries(data, TA='addMACD()')
signal = Lag(ifelse(macd$macd < macd$signal, -1, 1))
returns = ROC(data)*signal
returns = returns['2015-01-01/2017-06-25']
portfolio = exp(cumsum(returns))
plot(portfolio)
table.Drawdowns(returns, top=10)
table.DownsideRisk(returns)
charts.PerformanceSummary(returns)