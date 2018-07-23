#install.packages("e1071")
library("wesanderson")
library(lubridate)
library(dplyr)
library(plyr)
library(tidyverse)
library("caTools", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.2")
#library(gapminder)
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
#df_result <- subset(df_stock, select=c(2,6))
df_nasdaq_nyse <- subset(df_stock, select=c(5,9))


# set.seed(123456)
# split <- sample.split(df_nasdaq_nyse$nasdaq_close, SplitRatio = 0.95)
# df_nasdaq_nyse  = subset(df_nasdaq_nyse, split == TRUE)
# test_set = subset(df_nasdaq_nyse, split == FALSE)


# df_nasdaq_nyse[, 1:2] <- scale(df_nasdaq_nyse[1:2])
# test_set [,1:2]= scale(test_set[1:2])
#line_regressor = lm(formula = nasdaq_close ~ poly(nyse_close,8), data = df_nasdaq_nyse )


svm_regressor = svm(formula = nasdaq_close ~ ., data = df_nasdaq_nyse, type= "eps-regression" )

#y_pred <- predict(line_regressor, newdata =  test_set)
summary(svm_regressor)
y_pred <- predict(svm_regressor, data.frame(nyse_close = 12874.36 ))

ggplot() +
geom_point(aes(x=df_nasdaq_nyse$nyse_close, y=df_nasdaq_nyse$nasdaq_close), colour="red") +
geom_line(aes(x=df_nasdaq_nyse$nyse_close, y=predict(svm_regressor, newdata =  df_nasdaq_nyse)),  
               colour="blue")  +
#  geom_point(aes(x=test_set$nyse_close, y=test_set$nasdaq_close), colour="black")  +
  
  xlab("NYSE Index") +
  ylab("Nasdaq Index") +
  ggtitle("Ploynomial Regression: NYSE Index vs Nasdaq Index")

print(y_pred)
