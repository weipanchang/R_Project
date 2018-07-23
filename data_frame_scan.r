#install.packages("chron")
#install.packages("hash")
library(lubridate)
library(dplyr)
library(plyr)
library(tidyverse)
library(hash)
#NYA.csv
df <- read.csv("/home/wchang/Downloads/data/AAPL.csv", header = TRUE)
#df <- read.csv("/home/wchang/Downloads/BerkeleyPD_Calls_for_Service.csv", header = TRUE)

### Make some cell blank or 
df[[7,1]] <- NA
#Cell [7,1] [10,6] [16,6] are NA, and   [17,9][4,5] are blank
#seq(0.1,1.0,length=10)seq(0.1,1.0,length=10)seq(0.1,1.0,length=10)seq(0.1,1.0,length=10)
df_type <- c(hash())
m <- ncol(df)
n <- nrow(df)
for (i in (1:m)) {
  df_type <- c(df_type, hash()) 
}

for (column_number in (1:m)) {
#  print (column_number)
  for (row_number in (1:n)) {
    if (is.na(df[row_number, column_number])) {
      type_found <- "NA"
    } else {
      if (df[row_number, column_number] == "") {
        type_found <- "blank"
      } else {
        type_found <- as.character(typeof(df[row_number, column_number]))
      }
    }

    if (is.null(df_type[[column_number]][[type_found]])) {
      df_type[[column_number]][type_found] <- 1

    }  else {
      temp <- as.integer(df_type[[column_number]][[type_found]])
      temp <- temp + 1
      df_type[[column_number]][[type_found]] <- temp

    }
  }
}

print (paste('Data Frame: df has ',n, "x ", m, " dimension"))
k <- length(df_type)
k <- k - 1
print ("===========================================================")
for (i in c(1:k)) {
  temp <- df_type[[i]]
#  print(" ")
  print ("===========================================================")

  for (j in 1:length(keys(temp))) {
  print(paste('column ',i,": " , names(df)[i], ' has ', keys(temp)[[j]],": ", temp[[keys(temp)[[j]]]] ))

  }
}

