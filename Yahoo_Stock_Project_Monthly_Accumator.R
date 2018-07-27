#install.packages("wesanderson")
library(wesanderson)
library(lubridate)
library(dplyr)
library(plyr)
library(tidyverse)
#library(gapminder)
rm(list=ls())

options(stringsAsFactors = FALSE)
df_stock <- data.frame()
df_buy_stock <- data.frame()
df_sell_stock<- data.frame()
df_buy_sell<- data.frame()

#
#read from csv file
#
df_stock <-read.csv("/home/wchang/Downloads/data/AMZN.csv", header = TRUE)
#df_stock <-read.csv("/home/wchang/Downloads/data/GOOG.csv", header = TRUE)
df_stock$Year<- c(substr(df_stock$Date, 1,4))
df_stock$Month<- c(substr(df_stock$Date, 6,7))

names(df_stock) <- c("Date","Open","High","Low","Close","Adj_Close","Volume", "Year", "Month")
df_stock$Date <- c(as.Date(df_stock$Date))


str(df_stock)
#
# Split for each Month
# 
spliter <- function(df_stock, i) {
  
  df_temp <- data.frame()
  df_temp <- df_stock %>%
    filter(substr(df_stock$Date, 6,7) == v_month)
  return(df_temp)
}  


lst.dataframe = list()
for (i in 1:12) {
  if (nchar(i) == 1) {
    v_month <- paste("0", i, sep="")
  }
  else{
    v_month <- i
  }
  x <- spliter(df_stock, v_month)
  lst.dataframe <- c(lst.dataframe, list(x))
}

names(lst.dataframe)[c(1:9)] <- paste("df_stock", "_0",1:9, sep="") 
names(lst.dataframe)[c(10:12)] <- paste("df_stock", "_",10:12, sep="")

for (i in 1:12) {
  for (j in c(min(range(lst.dataframe[[i]]$Year)):max(range(lst.dataframe[[i]]$Year))) ){
    df_buy_stock <- rbind(df_buy_stock, lst.dataframe[[i]] %>%
                            filter(Year == j) %>%
                          head(1))

        df_sell_stock <- rbind(df_sell_stock, lst.dataframe[[i]] %>%
                             filter(Year == j) %>%
                             tail(1))
  }
}

for (i in 1:nrow(df_buy_stock)) {
  df_buy_sell <- rbind(df_buy_sell, rbind(c("Buy_Date"=as.character(df_buy_stock$Date[[i]]), 
                                            "Buy_Price"=df_buy_stock$Open[[i]], 
                                            "Sell_Date"=as.character(df_sell_stock$Date[[i]]),
                                            "Sell_Price"=df_sell_stock$Open[[i]],
                                            "Buy_Month"=df_buy_stock$Month[[i]], 
                                            "Profit" = (as.numeric(df_sell_stock$Open[[i]])-as.numeric(df_buy_stock$Open[[i]]))/as.numeric(df_buy_stock$Open[[i]]) + 1
  )))
}

df_buy_sell$Buy_Date <- as.Date(df_buy_sell$Buy_Date)
df_buy_sell$Sell_Date <- as.Date(df_buy_sell$Sell_Date)
df_buy_sell$Buy_Price <- as.numeric(df_buy_sell$Buy_Price)
df_buy_sell$Sell_Price <- as.numeric(df_buy_sell$Sell_Price)
df_buy_sell$Profit <- as.numeric(df_buy_sell$Profit)
df_buy_sell$Buy_Month <- as.integer(df_buy_sell$Buy_Month)
df_buy_sell$Year <- substr(df_buy_sell$Buy_Date, 1,4)

# 
df_buy_sell <- df_buy_sell %>%
  group_by(Buy_Month) %>%
  mutate_at(vars(Profit),funs(Cum_Profit = cumprod(Profit))) %>%
  ungroup()


#str(df_buy_sell)


#ddply(df_buy_sell, c("Buy_Month"), summarise, mean=mean(cumsum(Profit)), sd=sd(cumsum(Profit)))
ddply(df_buy_sell, c("Buy_Month"), summarise, mean=mean(Profit), sd=sd(Profit))

# ggplot(data = df_buy_sell, aes(x = Year , y = Profit, group = c(Buy_Month),  color = Buy_Month)) +
#  geom_line() +
#  scale_color_gradientn(colours = rainbow(12))



#ddply(df_buy_sell, c("Buy_Month"), summarise, mean=mean(Cum_Profit), sd=sd(Cum_Profit))

ggplot(data = df_buy_sell, aes(x = Year , y = Cum_Profit, group = c(Buy_Month),  color = Buy_Month)) +
  geom_line() +
  scale_color_gradientn(colours = rainbow(12))
