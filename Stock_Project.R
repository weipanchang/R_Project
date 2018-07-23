library(lubridate)
#rm(df_stock)
#
#read from csv file
#
df_stock <-read.csv("/home/wchang/Downloads/quantquote_daily_sp500_83986/daily/table_amzn.csv", header = FALSE)
df_stock$Date <- c(ymd(df_stock[,1]))
df_stock$Week_Day<- c(wday(df_stock$Date))

names(df_stock) <- c("S_Date","","Open","High","Low","Close","Volume", "Date", "Week_Day")
#
# Remove second column
#
df_stock <- df_stock[c(-2)]

#str(df_stock)
#
# Buy on Friday,  Sell on Monday ----- Strategic
#
selector <- function(df_stock) {
#
# Initial buy stock data frame
#
  df_buy_stock <- data.frame(
    S_Date=integer(),
    Open=double(), 
    High=double(),
    Low=double(),
    Close=double(),
    Volume=double(),
    Date =as.Date(character()),
    Week_Day=double()
  ) 
#
# Initial sell stock data frame
#
  df_sell_stock <- data.frame(
    S_Date=integer(),
    Open=double(), 
    High=double(),
    Low=double(),
    Close=double(),
    Volume=double(),
    Date =as.Date(character()),
    Week_Day=double(),stringsAsFactors=FALSE
  ) 

  for (i in 1:(nrow(df_stock)-1)) {
    if (df_stock$Week_Day[[i]] >= df_stock$Week_Day[[i+1]]) {
       df_buy_stock <- rbind(df_buy_stock,df_stock[i,])
       i <- i + 1
       df_sell_stock <- rbind(df_sell_stock,df_stock[i,])
    }
   }
  mylist <- list("df_buy_stock"=df_buy_stock, "df_sell_stock"=df_sell_stock)
  return(mylist)
}
myreturn <- selector(df_stock)
df_buy_stock <- myreturn$df_buy_stock
df_sell_stock <- myreturn$df_sell_stock

#
# Initial sell stock data frame
#
#df_buy_sell <- data.frame(
#  Buy_Date=character(),
#  Buy_Price=double(),
#  Sell_Date=character(),
#  Sell_Price=double()
#  Profit=double(),stringsAsFactors=FALSE
#) 

df_buy_sell <- data.frame()

for (i in 1:nrow(df_buy_stock)) {
  df_buy_sell <- rbind(df_buy_sell, rbind(c("Buy_Date"=as.character(df_buy_stock$Date[[i]]), "Buy_Price"=df_buy_stock$Open[[i]], 
                 "Sell_Date"=as.character(df_sell_stock$Date[[i]]), "Sell_Price"=df_sell_stock$Open[[i]],
                 "Profit" = as.double(df_sell_stock$Open[[i]]-df_buy_stock$Open[[i]]) / df_buy_stock$Open[[i]])))
}

length(df_buy_sell$Profit)
sum(as.numeric(as.character(df_buy_sell$Profit)))
mean(as.numeric(as.character(df_buy_sell$Profit)))
sd(as.numeric(as.character(df_buy_sell$Profit)))
