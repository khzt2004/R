library(alphavantager)

av_api_key("Q2T7PJKSA1F923PS")

nvda <- av_get(symbol = "NVDA", av_fun = "TIME_SERIES_INTRADAY", interval = "1min", outputsize = "compact")
as.data.frame(nvda)
nvda[1,1:2]

