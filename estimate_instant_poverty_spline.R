library(dplyr)
library(lubridate)
library(zoo)
library(ggplot2)
library(ggborderline)
library(tidyverse)
library(stringr)
library(ggrepel)

# Create a sequence of months
months <- c(4:12, rep(1:12, 6), 1:10)
# Create a sequence of years
years <- c(rep(2016, 9), rep(2017:2022, each = 12), rep(2023, 10))
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
p2023<-c(40.1,40.1,40.1,40,40.6,41.1,42.4,43.7,46.3,48.3)
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
  
  # Predict the poverty value for the date 75 days in the future (fecha mediana del mes +2 meses y medio para estimar dato promedio del último mes)
  future_date <- df$median_date[i] + 75
  predicted_values[i] <- spline_interpolation(as.numeric(future_date))
}

# Add the predicted values to the data frame
df$predicted_poverty <- predicted_values
#Change 0 values in predicted_poverty to NA
df$predicted_poverty[df$predicted_poverty==0]<-NA
# Calculate the rolling mean of the predicted_poverty column with a window size of 6
rolling_mean <- rollmean(df$predicted_poverty, k = 6, fill = NA, align = "right")
# Add the calculated rolling mean to the dataframe as a new column
df$rolling_mean <- rolling_mean

#Correlation between predicted poverty and actual poverty
cor(df$rolling_mean[10:91],df$poverty[10:91])
#Coefficient of determination between predicted poverty and actual poverty (r2)
summary(lm(df$rolling_mean[10:91]~df$poverty[10:91]))$r.squared
#This should equal df$poverty[69]
mean(df$predicted_poverty[85:90])
df$poverty[90]



# Reshape the data to long format
df_long <- df %>% pivot_longer(cols = c(poverty, predicted_poverty, rolling_mean), names_to = "variable", values_to = "value")
df_long$variable <- factor(df_long$variable, levels = c("poverty", "predicted_poverty", "rolling_mean"), 
                           labels = c("Nowcast pobreza semestral (Rozada)", "Predicción instantánea mensual", "Promedio semestral de la predicción mensual"))
# Plot the data
ggplot(df_long, aes(x = first_day, y = value, color = variable)) +
  geom_borderline(size=1) +
  scale_y_continuous(breaks = seq(0, 60, by = 2.5), minor_breaks = NULL, labels = function(x) paste0(x, "%")) +
  scale_x_date(limits = c(as.Date("2017-01-01"), as.Date("2024-03-01")), date_labels = "%Y-%m", date_breaks = "2 months",expand=c(0,15)) +
  labs(title = "Predicción instantánea mensual de la Pobreza en Argentina utilizando Splines", x = "Mes", y = "Pobreza") +
  labs(caption = str_wrap("Por R. Quiroga tomando como base la estimación central del Nowcast de pobreza semestral de Martín Rozada. Se puede observar la predicción mensual en rojo, utilizando el método de Splines 'monoH.FC'. En naranja, se observa el promedio de los últimos seis valores predichos, es decir un rolling mean alineado a la derecha. Código disponible en https://github.com/rquiroga7/pobreza_eph_estimacion_mensual", width = 120)) +
  scale_color_manual(values = c("black", "red", "orange")) +
  geom_label_repel(data = df_long %>% group_by(variable) %>% filter(row_number()==n()), 
                 aes(label = paste0(round(value,1), "%")), box.padding = 0.35, point.padding = 0.5, segment.color = 'grey50', 
                 segment.size = 0.5, size=5, direction="x",show.legend=FALSE) +
  theme_light(base_size=14) +
  theme(legend.position = "bottom", legend.title = element_blank(), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# Save the plot
ggsave("prediccion_pobreza_mensual.png")





