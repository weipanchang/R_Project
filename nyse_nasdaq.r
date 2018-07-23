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


set.seed(123456)
split <- sample.split(df_nasdaq_nyse$nasdaq_close, SplitRatio = 0.95)
training_set = subset(df_nasdaq_nyse, split == TRUE)
test_set = subset(df_nasdaq_nyse, split == FALSE)


# training_set[, 1:2] <- scale(training_set[1:2])
# test_set [,1:2]= scale(test_set[1:2])
#line_regressor = lm(formula = nasdaq_close ~ poly(nyse_close,8), data = training_set )


line_regressor = lm(formula = nasdaq_close ~ ., data = training_set )

#y_pred <- predict(line_regressor, newdata =  test_set)
summary(line_regressor)

#plot(df_result$nasdaq_close,df_result$nyse_close,col = "blue",main = "Height & Weight Regression",
#     abline(lm(formula = nyse_close ~ nasdaq_close, data = df_result )),xlab = "NASDAQ",ylab = "NYSE")

training_set$level2 <- training_set[,1] ^2
training_set$level3 <- training_set[,1] ^3
training_set$level4 <- training_set[,1] ^4
training_set$level5 <- training_set[,1] ^5
training_set$level6 <- training_set[,1] ^6
training_set$level7 <- training_set[,1] ^7
training_set$level8 <- training_set[,1] ^8
training_set$level9 <- training_set[,1] ^9
training_set$level10 <- training_set[,1] ^10

poly_regressor <- lm(formula = nasdaq_close ~ ., data = training_set)
summary(poly_regressor)

ggplot() +
geom_point(aes(x=training_set$nyse_close, y=training_set$nasdaq_close), colour="red") +
geom_line(aes(x=training_set$nyse_close, y=predict(poly_regressor, newdata =  training_set)),  
               colour="blue")  +
#  geom_point(aes(x=test_set$nyse_close, y=test_set$nasdaq_close), colour="black")  +
  
  xlab("NYSE Index") +
  ylab("Nasdaq Index") +
  ggtitle("Ploynomial Regression: NYSE Index vs Nasdaq Index")


# ggplot() +
#   geom_point(aes(x=training_set$nyse_close, y=training_set$nasdaq_close), colour="red") +
#   geom_line(aes(x=training_set$nyse_close, y=predict(line_regressor, y = predict(line_regressor, newdata =  training_set))), 
#             colour="blue")  +
#   geom_point(aes(x=test_set$nyse_close, y=test_set$nasdaq_close), colour="black")  +
#   
#   xlab("NYSE Index") +
#   ylab("Nasdaq Index") +
#   ggtitle("Ploynomial Regression: NYSE Index vs Nasdaq Index")

y_pred <- predict(poly_regressor, data.frame(nyse_close = 12874.36,
                                             level2 = 12874.36 ^2,
                                             level3 = 12874.36 ^3,
                                             level4 = 12874.36 ^4,
                                             level5 = 12874.36 ^5,
                                             level6 = 12874.36 ^6,
                                             level7 = 12874.36 ^7,
                                             level8 = 12874.36 ^8,
                                             level9 = 12874.36 ^9,
                                             level10 = 12874.36 ^10))
print(y_pred)
