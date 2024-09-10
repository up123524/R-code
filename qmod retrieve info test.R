##Using predict function on stock market data
#extreme gradient boosting
install.packages("xgboost")
library(quantmod); library(TTR); library(xgboost); library(dplyr) ;library(ggplot2)

symbol<-c("^GSPC","AAPL","MSFT")
getSymbols(symbol,
           src="yahoo",
           from=as.Date("2017-1-1"),
           to=as.Date("2020-6-6"),
           periodicity="daily",
           verbose = TRUE)
AAPL<-as.data.frame(AAPL)
GSPC<-as.data.frame(GSPC)
MSFT<-as.data.frame(MSFT)
AAPL<-AAPL %>%
  rename(Open=AAPL.Open, High= AAPL.High,
       Low=AAPL.Low,Close= AAPL.Close,Volume= AAPL.Volume,
       Adjusted=AAPL.Adjusted)
GSPC<-GSPC %>%
  rename(Open=GSPC.Open, High= GSPC.High,
         Low=GSPC.Low,Close= GSPC.Close,Volume= GSPC.Volume,
         Adjusted=GSPC.Adjusted)
MSFT<-MSFT %>%
  rename(Open=MSFT.Open, High= MSFT.High,
         Low=MSFT.Low,Close= MSFT.Close,Volume= MSFT.Volume,
         Adjusted=MSFT.Adjusted)
head(GSPC)
head(AAPL)
head(MSFT)
####################
##sources#yahoo#google#oanda#FRED()

#new env
library(quantmod); library(TTR); library(xgboost); library(dplyr)
ENV.STOCK<-new.env()
symbol<-c("^GSPC","AAPL","MSFT", "INTC")
getSymbols(symbol,
           src="yahoo",
           from=as.Date("2017-1-1"),
           to=as.Date("2020-6-6"),
           periodicity="daily",
           verbose = TRUE,
           env=ENV.STOCK)
Cl(ENV.STOCK$AAPL)
head(Ad(ENV.STOCK$AAPL))
?Ad()
plot(Cl(ENV.STOCK$AAPL), col = "black")


XTS.ADJUSTED<-do.call(merge,eapply(ENV.STOCK,Ad))
head(XTS.ADJUSTED)
plot(XTS.ADJUSTED)

