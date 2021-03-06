library(lubridate)
options(stringsAsFactors = FALSE)
df_stock <- data.frame()
df_buy_stock <- data.frame()
df_sell_stock<- data.frame()
df_buy_sell <- data.frame()
#
#read from csv file
#
df_stock <-read.csv("/home/wchang/Downloads/quantquote_daily_sp500_83986/daily/MSFT.csv", header = TRUE)

df_stock$Week_Day<- c(wday(df_stock[,1]))

names(df_stock) <- c("Date","Open","High","Low","Close","Adj_Close","Volume", "Week_Day")
df_stock$Date <- c(as.Date(df_stock$Date))
#
# Remove second column
#
#df_stock <- df_stock[c(-2)]

str(df_stock)
#
# Buy on Friday,  Sell on Monday ----- Strategic
#
selector <- function(df_stock) {

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


df_buy_sell <- data.frame()

for (i in 1:nrow(df_buy_stock)) {
  df_buy_sell <- rbind(df_buy_sell, rbind(c("Buy_Date"=df_buy_stock$Date[[i]], 
                                            "Buy_Price"=df_buy_stock$Open[[i]], 
                                            "Sell_Date"=df_sell_stock$Date[[i]],
                                            "Sell_Price"=df_sell_stock$Open[[i]],
                                            "Profit" = (df_sell_stock$Open[[i]]-df_buy_stock$Open[[i]])/df_buy_stock$Open[[i]])))
}

df_buy_sell$Buy_Date <- as.Date(df_buy_stock$Date)
df_buy_sell$Sell_Date <- as.Date(df_sell_stock$Date)

str(df_buy_sell)
length(df_buy_sell$Profit)
sum(as.numeric(as.character(df_buy_sell$Profit)))
mean(as.numeric(as.character(df_buy_sell$Profit)))
sd(as.numeric(as.character(df_buy_sell$Profit)))
