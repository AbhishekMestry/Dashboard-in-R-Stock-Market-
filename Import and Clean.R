library(rvest)
library(dplyr)
library(readr)
library(tidyr)
#Import Tickers
setwd('E:/Drive E/shares_project')
bse_url="https://en.wikipedia.org/wiki/List_of_BSE_SENSEX_companies"
bsedata=read_html(bse_url) %>%
  html_node("table") %>%
  html_table()

bsedata=bsedata %>% select(`Exchange ticker`, Symbol, Companies, Sector)
colnames(bsedata) <- c("Exchange_ticker", "Ticker", "Companies", "Sector")
save(bsedata, file='bsedata.RData')

AXISBANK.BO=read_csv('https://query1.finance.yahoo.com/v7/finance/download/AXISBANK.BO?period1=1476921600&period2=1634688000&interval=1d&events=history&includeAdjustedClose=true')
HDFC.BO=read_csv('https://query1.finance.yahoo.com/v7/finance/download/HDFC.BO?period1=1476921600&period2=1634688000&interval=1d&events=history&includeAdjustedClose=true')

returns <- as.data.frame(matrix(NA, ncol = 8, nrow = 0))
names(returns) <- c("Date", "Open", "High", "Low", "Close", "Adj_Close", "Volume", "Ticker")



for(symbol in bsedata$Ticker){
  print(symbol)
  url=paste0("https://query1.finance.yahoo.com/v7/finance/download/", symbol ,"?period1=1476921600&period2=1634688000&interval=1d&events=history&includeAdjustedClose=true")
  print(url)



ret <- try(read_csv(url))

if(mode(ret) != "character"){
  ret$Ticker <- symbol
  returns <- rbind(returns, ret)
}
}


returns <- returns %>% select("Date", "Ticker", "Open", "High", "Low", "Close")
returns <- returns %>% mutate(
  Open = as.numeric(Open),
  High = as.numeric(High),
  Low = as.numeric(Low),
  Close = as.numeric(Close),
)



returns <- returns %>% mutate(
  Movement = ifelse(Close > Open, "Up", "Down")
)

#save(returns, file = "returns.RData")


returns_long <- returns %>% gather("Series", "Value", -Date, -Ticker, -Movement)
returns_long <- returns_long %>% left_join(bsedata %>% select(Ticker, Companies, Sector), by = c( "Ticker"="Ticker"))

#save(returns_long, file = "returns_long.RData")




## Performance calcs

performance_summary <- as.data.frame(matrix(NA, ncol = 6, nrow = 0))
names(performance_summary) <- c("Ticker", "Thirty_days", "Ninety_days", "One_year", "Three_years", "Five_years")

i <- 1
for(ticker in unique(returns_long$Ticker)){
  print(ticker)
  
  returns_long_by_ticker <- returns_long %>% filter(Ticker == ticker, Series == "Close") %>% arrange(desc(Date))
  
  thrity_day <- (returns_long_by_ticker$Value[1] - returns_long_by_ticker$Value[21])/returns_long_by_ticker$Value[21]
  ninety_day <- (returns_long_by_ticker$Value[1] - returns_long_by_ticker$Value[63])/returns_long_by_ticker$Value[63]
  one_year <- (returns_long_by_ticker$Value[1] - returns_long_by_ticker$Value[253])/returns_long_by_ticker$Value[253]
  three_year <- (1 + ((returns_long_by_ticker$Value[1] - returns_long_by_ticker$Value[759])/returns_long_by_ticker$Value[759]))^(1/3)-1
  five_year <- (1 + ((returns_long_by_ticker$Value[1] - returns_long_by_ticker$Value[1265])/returns_long_by_ticker$Value[1265]))^(1/5)-1
  
  performance_summary[i, 1] <- ticker
  performance_summary[i, 2] <- thrity_day
  performance_summary[i, 3] <- ninety_day
  performance_summary[i, 4] <- one_year
  performance_summary[i, 5] <- three_year
  performance_summary[i, 6] <- five_year
  
  i <- i + 1
}

load("bsedata.RData")

performance_summary <- performance_summary %>% left_join(bsedata, by = c("Ticker" = "Ticker"))
#save(performance_summary, file = "performance_summary.RData")

