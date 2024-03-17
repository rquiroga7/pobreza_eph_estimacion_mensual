library(dplyr)
library(lubridate)
# Create a sequence of months
months <- c(4:12, rep(1:12, 6), 1:9)
# Create a sequence of years
years <- c(rep(2016, 9), rep(2017:2022, each = 12), rep(2023, 9))
# Create the data frame
df <- data.frame(month_begin = months, year_begin = years, poverty = 0)
# Add columns for the end month and year
df<-df %>% mutate(month_end=ifelse(month_begin+5>12,month_begin+5-12,month_begin+5),year_end=ifelse(month_begin+5>12,year_begin+1,year_begin))
#combine year and month into a single date using lubriudate, end date should be last day of the month_end month
df <- df %>% mutate(
  date_begin = ymd(paste0(year_begin, "-", month_begin, "-01")),
  date_end = ceiling_date(ymd(paste0(year_end, "-", month_end, "-01")), "month") - days(1)
)
#Load Rozada poverty data
p2016<-c(31.4,31,30.5,30.3,29.3,29.1,29.1,28.4,28.6)
p2017<-c(28.6,28,27.3,26.9,26.7,25.8,25.7,25.3,25.6,25.9,26.3,26.8)
p2018<-c(27.3,27.6,27.9,28.5,29.3,30.3,32,32.5,34.5,35.1,35,34.8)
p2019<-c(35.4,35,35,34.8,34.8,35,35.5,35.9,36.1,36.4,38,40)
p2020<-c(40.9,41.3,42.3,42.9,42.4,42.2,42,42.5,42.5,42.5,41.9,41.4)
p2021<-c(40.6,40.3,39.8,39.9,39,38.4,37.3,37.2,36,35.4,35.1,36.2)
p2022<-c(36.5,37.1,37.6,37.9,38.4,38.5,39.2,39.4,39.9,39.8,40.1,40)
p2023<-c(40.1,40.1,40.1,40,40.6,41.1,42.4,43.7,46.3)
df$poverty<-c(p2016,p2017,p2018,p2019,p2020,p2021,p2022,p2023)

# Convert the date columns to Date type
df$date_begin <- as.Date(df$date_begin)
df$date_end <- as.Date(df$date_end)

#Calculate the median day between the begin and end date
df$median_date<-df$date_begin+(df$date_end-df$date_begin)/2

#Get first day of month for the month in end dates
df$first_day<-df$date_end-day(df$date_end)+1
# Create a sequence of monthly dates from the minimum to the maximum date
date_seq <- seq(from = min(df$date_begin), to = max(df$first_day), by = "month")

###PREDICTION

# Initialize a vector to store the predicted values
predicted_values <- numeric(nrow(df))

# Loop over each date in the df$median_date column
for (i in 3:nrow(df)) {
  # Subset the data to include only the rows up to and including the current date
  df_subset <- df[1:i, ]
  
  # Create a spline interpolation function using the subsetted data
  spline_interpolation <- splinefun(x = as.numeric(df_subset$median_date), y = df_subset$poverty, method = "monoH.FC")
  #spline_interpolation <- splinefun(x = as.numeric(df_subset$median_date), y = df_subset$poverty, method = "fmm")
  
  # Predict the poverty value for the date 90 days in the future
  future_date <- df$median_date[i] + 90
  predicted_values[i] <- spline_interpolation(as.numeric(future_date))
}

# Add the predicted values to the data frame
df$predicted_poverty <- predicted_values
#This should equal df$poverty[69]
mean(df$predicted_poverty[76:81])
df$poverty[81]
#Change 0 values in predicted_poverty to NA
df$predicted_poverty[df$predicted_poverty==0]<-NA

# Load the necessary library
library(zoo)

# Calculate the rolling mean of the predicted_poverty column with a window size of 6
rolling_mean <- rollmean(df$predicted_poverty, k = 6, fill = NA, align = "right")

# Add the calculated rolling mean to the dataframe as a new column
df$rolling_mean <- rolling_mean


#Plot the original and interpolated data
library(ggplot2)
ggplot() +
  geom_line(data = df, aes(x = first_day, y = poverty), color = "blue") +
  geom_line(data = df, aes(x = first_day, y = predicted_poverty), color = "red") +
  geom_line(data = df, aes(x = first_day, y = rolling_mean), color = "orange") +
  #Add breaks at every 2 points of poverty and add the % symbol
  scale_y_continuous(breaks = seq(0, 60, by = 2.5),minor_breaks = NULL, labels = function(x) paste0(x, "%")) +
  #limit x scale to 2018-2024
    scale_x_date(limits = c(as.Date("2017-01-01"), as.Date("2024-03-01")), date_labels = "%Y") +
  labs(title = "Predicted Poverty Values", x = "Date", y = "Poverty Value") +
  theme_minimal()




#Instead of using spline to predict, calculate the monthly change in poverty and add it to the poverty of 6 months before to get the predicted poverty
df$monthly_change<-c(NA,diff(df$poverty))*6
df$predicted_poverty2<-df$monthly_change+lag(df$poverty,6)

# Calculate the rolling mean of the predicted_poverty column with a window size of 6
rolling_mean2 <- rollmean(df$predicted_poverty2, k = 6, fill = NA, align = "right")

df$predicted_poverty3<-(df$predicted_poverty2+df$predicted_poverty)/2
rolling_mean3 <- rollmean(df$predicted_poverty3, k = 6, fill = NA, align = "right")
#Plot the original and interpolated data

#Calculate the RMSE of the predictions py elevating to the power of 2
sqrt(sum((df$poverty-df$predicted_poverty)^2,na.rm = TRUE))
sqrt(sum((df$poverty-df$predicted_poverty2)^2,na.rm = TRUE))
sqrt(sum((df$poverty-df$predicted_poverty3)^2,na.rm = TRUE))





library(ggplot2)
ggplot() +
  geom_line(data = df, aes(x = first_day, y = poverty), color = "purple") +
  #geom_line(data = df, aes(x = first_day, y = predicted_poverty2), color = "red") +
  geom_line(data = df, aes(x = first_day, y = rolling_mean2), color = "orange") +
    #geom_line(data = df, aes(x = first_day, y = predicted_poverty), color = "blue") +
    geom_line(data = df, aes(x = first_day, y = rolling_mean), color = "cyan") +
      #geom_line(data = df, aes(x = first_day, y = predicted_poverty3), color = "red") +
      geom_line(data = df, aes(x = first_day, y = rolling_mean3), color = "green") +
  #Add breaks at every 2 points of poverty and add the % symbol
  scale_y_continuous(breaks = seq(0, 60, by = 2.5), labels = function(x) paste0(x, "%")) +
  #limit x scale to 2018-2024
  scale_x_date(limits = c(as.Date("2017-01-01"), as.Date("2024-04-01")), date_labels = "%Y") +
  labs(title = "Predicted Poverty Values", x = "Date", y = "Poverty Value") +
  theme_light()
ggsave("poverty_prediction.png")