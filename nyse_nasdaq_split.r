#library("wesanderson")
library(lubridate)
library(dplyr)
library(plyr)
library(tidyverse)
library("caTools", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.2")
options(stringsAsFactors = FALSE)
options(digits=15)
df_stock <- data.frame()
df_buy_stock <- data.frame()
df_sell_stock<- data.frame()
df_buy_sell<- data.frame()
df_result <- data.frame()
df_nyse <- read.csv("/home/wchang/Downloads/r_regression/^NYA.csv", header = TRUE)
df_nasdaq <- read.csv("/home/wchang/Downloads/r_regression/^IXIC.csv", header = TRUE)

df_nyse <- df_nyse[, c(-6, -7)]
df_nasdaq <- df_nasdaq[, c(-6, -7)]

df_nyse$Date <- as.Date(df_nyse$Date)
df_nyse$Open <- c(as.numeric(df_nyse$Open))
df_nyse$Close <- c(as.numeric(df_nyse$Close))
df_nyse <- df_nyse[complete.cases(df_nyse),]
names(df_nyse) <- c("nyse_date", "nyse_open", "nyse_high", "nyse_low", "nyse_close")

df_nasdaq$Date <- as.Date(df_nasdaq$Date)
df_nasdaq$Open <- c(as.numeric(df_nasdaq$Open))
df_nasdaq$Close <- c(as.numeric(df_nasdaq$Close))
df_nasdaq <- df_nasdaq[complete.cases(df_nasdaq),]
names(df_nasdaq) <- c("nasdaq_date", "nasdaq_open", "nasdaq_high", "nasdaq_low", "nasdaq_close")

df_stock <- merge(df_nyse, df_nasdaq, by.x="nyse_date", by.y="nasdaq_date" )
df_stock$split <- df_stock[,2] - df_stock[,6]
df_nasdaq_nyse_split <- subset(df_stock, select=c(1,10))
#df_nasdaq_nyse_split$split <- sqrt(df_nasdaq_nyse_split$split)

svm_regressor = svm(formula = split ~ ., data = df_nasdaq_nyse_split, type= "eps-regression" )
summary(svm_regressor)

# df_nasdaq_nyse_split$level2 <- df_nasdaq_nyse_split[,1] ^2
# df_nasdaq_nyse_split$level3 <- df_nasdaq_nyse_split[,1] ^3
# df_nasdaq_nyse_split$level4 <- df_nasdaq_nyse_split[,1] ^4
# df_nasdaq_nyse_split$level5 <- df_nasdaq_nyse_split[,1] ^5
# df_nasdaq_nyse_split$level6 <- df_nasdaq_nyse_split[,1] ^6
# df_nasdaq_nyse_split$level7 <- df_nasdaq_nyse_split[,1] ^7
# df_nasdaq_nyse_split$level8 <- df_nasdaq_nyse_split[,1] ^8
# df_nasdaq_nyse_split$level9 <- df_nasdaq_nyse_split[,1] ^9
# df_nasdaq_nyse_split$level10 <- df_nasdaq_nyse_split[,1] ^10
# 
# poly_regressor <- lm(formula = nasdaq_close ~ ., data = df_nasdaq_nyse_split)
# summary(poly_regressor)

ggplot() +
geom_point(aes(x=df_nasdaq_nyse_split$nyse_date, y=df_nasdaq_nyse_split$split), colour="red") +
geom_line(aes(x=df_nasdaq_nyse_split$nyse_date, y=predict(svm_regressor, newdata =  df_nasdaq_nyse_split)),  
               colour="blue")  +
    xlab("Year") +
    ylab("Index Split") +
    ggtitle("SVM Regression: NYSE Index vs Nasdaq Index Split")

#y_pred <- predict(svm_regressor, data.frame(split = 12874.36))
#print(y_pred)
