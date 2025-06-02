library(forecast)
library(tseries)
library(astsa)
library(readr)
library(dplyr)
library(lubridate)
library(tidyr)

# Load the dataset
data <- read.csv("SnP500.csv")

# Convert the DATE column to Date format
data$DATE <- as.Date(data$DATE, format = "%m/%d/%y")

# Create a full sequence of dates from the minimum to the maximum date in the dataset
full_dates <- data.frame(DATE = seq(min(data$DATE), max(data$DATE), by = "day"))

# Merge the original dataset with the full sequence of dates
data_full <- full_dates %>%
  left_join(data, by = "DATE")

# Replace '.' in the SP500 column with NA
data_full$SP500 <- ifelse(data_full$SP500 == ".", NA, data_full$SP500)

# Fill missing SP500 values with the previous available value
data_full <- data_full %>%
  mutate(SP500 = as.numeric(SP500)) %>%
  fill(SP500, .direction = "down")

# Optional: Save the cleaned dataset to a CSV file
write.csv(data_full, "SnP500_cleaned.csv", row.names = FALSE)



DATE = data_full$DATE
SP500 = ts(data_full$SP500)



#time series plot of clean
tsplot(DATE, SP500)
adf.test(SP500) # Not Stationary

#Log Transformation
tsplot(diff(SP500)) 
tsplot(diff(log(SP500)))




diff_log_ts = diff(log(SP500))
diff_log_ts_new = ts(diff_log_ts)
tsplot(diff_log_ts_new)
adf.test(diff_log_ts_new)
acf2(diff_log_ts_new, max.lag = 70)



snp_1 = sarima(diff_log_ts_new, p=1,d=1,q=1,P=0,D=1,Q=1,S=7)
snp_2 = sarima(diff_log_ts_new, p=1,d=1,q=1,P=1,D=1,Q=2,S=7)

forecast_snp_test = sarima.for(SP500,n.ahead=5,p=1,d=1,q=1,P=0,D=1,Q=1,S=7)
forecast_snp_test1 = sarima.for(SP500,n.ahead=5,p=1,d=1,q=1,P=1,D=1,Q=2,S=7)
print(forecast_snp_test)
print(forecast_snp_test1)
